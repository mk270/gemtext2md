/* gemtext2md, A gemtext to markdown, mom and PDF converter, by Martin Keegan

   To the extent (if any) permissible by law, Copyright (C) 2023  Martin Keegan

   This programme is free software; you may redistribute and/or modify it under
   the terms of the Apache Software Licence v2.0. */

/* Usage: the binaries in src/bin are commandline filters. Each reads Gemtext
   on stdin; gemtext2md outputs Markdown, hopefully in CommonMark format,
   gemtext2mom outputs groff mom, and gemtext2pdf outputs PDF. */

/* The code below is a fairly bloody-minded hand translation from
   the OCaml original into somewhat unidiomatic Rust. */

use std::sync::mpsc::{self, Sender, Receiver};
use std::thread;
use std::io::{self, BufRead, Write};
use std::fmt;
use std::error::Error;
use std::process::ExitCode;

pub mod mom;

type StageResult = Result<(), String>;

#[derive(Debug)]
pub enum HeadingLevel {
    H1,
    H2,
    H3
}

#[derive(Debug)]
enum Malformed {
    MLink,
    MHeading
}

#[derive(Debug)]
pub struct Heading(pub HeadingLevel, pub String);

#[derive(Debug)]
pub struct Link(pub String, pub Option<String>);

#[derive(Debug)]
enum Line {
    PreformattedL(Option<String>, Vec<String>),
    ParaL(String),
    LinkL(Link),
    ListItemL(String),
    QuoteL(String),
    HeadingL(Heading),
    BlankL
}

#[derive(Debug)]
pub enum Block {
    PreformattedB(Option<String>, Vec<String>),
    ParaB(String),
    LinksB(Vec<Link>),
    ListB(Vec<String>),
    QuoteB(Vec<String>),
    HeadingB(Heading)
}

// wrapper for including line numbers
struct NumString(String, usize);

impl TryFrom<&str> for Line {
    type Error = Malformed;

    // corresponds to OCaml function 'line_of_string : string -> line'
    // modulo parsing quirks that violated the spec
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use Line::*;
        use Malformed::*;
        use HeadingLevel::*;

        // links: "=>" [whitespace] URL [whitespace label]
        if let Some(rest) = s.strip_prefix("=>") {
            let rest = rest.trim();
            return match rest.split_once(char::is_whitespace) {
                None if rest.is_empty() => Err(MLink),
                None => Ok(LinkL(Link(rest.to_string(), None))),
                Some((url, label)) => Ok(LinkL(Link(
                    url.to_string(),
                    Some(label.trim_start().to_string())
                ))),
            };
        }

        // headings: longest prefix first, whitespace after the hashes optional
        let heading = if let Some(r) = s.strip_prefix("###") {
            Some((H3, r))
        } else if let Some(r) = s.strip_prefix("##") {
            Some((H2, r))
        } else if let Some(r) = s.strip_prefix('#') {
            Some((H1, r))
        } else {
            None
        };

        if let Some((level, text)) = heading {
            let text = text.trim();
            return if text.is_empty() {
                Err(MHeading)
            } else {
                Ok(HeadingL(Heading(level, text.to_string())))
            };
        }

        // lists
        if let Some(rest) = s.strip_prefix("* ") {
            return Ok(ListItemL(rest.trim().to_string()));
        }

        // quotes: ">" [whitespace] text
        if let Some(rest) = s.strip_prefix('>') {
            return Ok(QuoteL(rest.trim().to_string()));
        }

        // paragraphs / blanks
        let text = s.trim();
        Ok(if text.is_empty() { BlankL } else { ParaL(text.to_string()) })
    }
}

impl fmt::Display for Malformed {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Malformed::*;

        let s = match self {
            MLink    => "link line with no URL",
            MHeading => "heading with no text"
        };

        write!(f, "{}", s)
    }
}


impl fmt::Display for Heading {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", heading_chars(&self.0), self.1)
    }
}

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Link(url, tag) = self;
        let caption = tag.as_deref().unwrap_or(url);
        writeln!(f, "* [{}]({})", caption, url)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Block::*;

        match self {
            ParaB(p)            => write!(f, "{}\n\n", p),

            HeadingB(h)         => write!(f, "{}\n\n", h),

            PreformattedB(alt, prpr) => {
                writeln!(f, "```{}", alt.as_deref().unwrap_or(""))?;
                for line in prpr {
                    writeln!(f, "{}", line)?;
                }
                write!(f, "```\n\n")
            },

            LinksB(ll)          => {
                if ll.is_empty() {
                    return Ok(());
                }
                for l in ll {
                    write!(f, "{}", l)?;
                }
                writeln!(f)
            },

            ListB(items)        => {
                for item in items {
                    writeln!(f, "- {}", item)?;
                }
                writeln!(f)
            },

            QuoteB(paras)       => {
                for (i, para) in paras.iter().enumerate() {
                    if i > 0 {
                        writeln!(f, ">")?;
                    }
                    writeln!(f, "> {}", para)?;
                }
                writeln!(f)
            }
        }
    }
}

fn heading_chars(h: &HeadingLevel) -> &'static str {
    use HeadingLevel::*;

    match h {
        H1 => "#",
        H2 => "##",
        H3 => "###"
    }
}

/*

Everything below this point is a five-stage pipeline.

  read_lines:          get stdin input stream line by line as strings
  gather_preformatted: tag the strings if they're in a preformatted block
  decode_lines:        turn the strings into Lines
  blocks_of_lines:     turn the Lines into Blocks
  consume_blocks:      print the Blocks to stdout

*/

macro_rules! send_or_stop {
    ($sender:ident, $payload:expr) => {
        if $sender.send($payload).is_err() { return Ok(()); }
    }
}

fn read_lines(tx: Sender<NumString>)
              -> thread::JoinHandle<StageResult>
{
    thread::spawn(move || {
        let stdin = io::stdin();

        for (lineno_0, line) in stdin.lock().lines().enumerate() {
            let lineno = lineno_0 + 1; // index from 1, not 0

            match line {
                Ok(l) => send_or_stop!(tx, NumString(l, lineno)),
                Err(e) => return Err(
                    format!("line {}: {}", lineno, e)
                )
            }
        }

        Ok(())
    })
}

// annotate lines with whether they occur within preformatted blocks;
// pass the opening fence on (it carries the alt text), drop the closing one
fn gather_preformatted(rx: Receiver<NumString>,
                       tx: Sender<(bool, NumString)>)
                       -> thread::JoinHandle<StageResult>
{
    thread::spawn(move || {
        let mut pref = false;

        for i in rx {
            if i.0.starts_with("```") {
                pref = !pref;
                if pref {
                    send_or_stop!(tx, (true, i));   // opening fence: keep
                }
            } else {
                send_or_stop!(tx, (pref, i));
            }
        }

        Ok(())
    })
}

// convert the incoming strings to Lines
fn decode_lines(rx: Receiver<(bool, NumString)>,
                tx: Sender<Line>)
                -> thread::JoinHandle<StageResult>
{
    thread::spawn(move || {
        use Line::PreformattedL;

        // the preformatted block being collected, if any: (alt text, lines)
        let mut block: Option<(Option<String>, Vec<String>)> = None;

        for (pref, ns) in rx {
            let NumString(s, lineno) = ns;

            if pref {
                if let Some(alt) = s.strip_prefix("```") {
                    // opening fence: finish any block it directly follows
                    if let Some((a, lines)) = block.take() {
                        send_or_stop!(tx, PreformattedL(a, lines));
                    }
                    let alt = alt.trim();
                    let alt = (!alt.is_empty()).then(|| alt.to_string());
                    block = Some((alt, vec![]));
                } else if let Some((_, lines)) = block.as_mut() {
                    lines.push(s);
                }
                continue;
            }

            if let Some((a, lines)) = block.take() {
                send_or_stop!(tx, PreformattedL(a, lines));
            }

            let line = Line::try_from(s.as_str())
                .map_err(|m| format!("line {}: {}", lineno, m))?;
            send_or_stop!(tx, line);
        }

        if let Some((a, lines)) = block {
            send_or_stop!(tx, PreformattedL(a, lines));
        }

        Ok(())
    })
}

impl Block {
    // the Block a Line starts, if any; a blank line starts nothing
    fn of_line(line: Line) -> Option<Block> {
        use Block::*;
        use Line::*;

        match line {
            LinkL(l)            => Some(LinksB(vec![l])),
            ListItemL(i)        => Some(ListB(vec![i])),
            QuoteL(q)           => Some(QuoteB(vec![q])),
            ParaL(p)            => Some(ParaB(p)),
            HeadingL(h)         => Some(HeadingB(h)),
            PreformattedL(a, p) => Some(PreformattedB(a, p)),
            BlankL              => None
        }
    }

    // blocks that consecutive lines of the same kind are added to
    fn is_group(&self) -> bool {
        use Block::*;
        matches!(self, LinksB(_) | ListB(_) | QuoteB(_))
    }

    // add a line to this group if it belongs here; otherwise hand it back
    fn absorb(&mut self, line: Line) -> Result<(), Line> {
        use Block::*;
        use Line::*;

        match (self, line) {
            (LinksB(v), LinkL(l))     => { v.push(l); Ok(()) },
            (ListB(v),  ListItemL(i)) => { v.push(i); Ok(()) },
            (QuoteB(v), QuoteL(q))    => { v.push(q); Ok(()) },
            (_, line)                 => Err(line)
        }
    }
}

// aggregate Lines into Blocks
fn blocks_of_lines(rx: Receiver<Line>,
                   tx: Sender<Block>)
                   -> thread::JoinHandle<StageResult>
{
    thread::spawn(move || {
        // the group (links, list or quote) being built, if any
        let mut group: Option<Block> = None;

        for line in rx {
            // a line that belongs to the group in progress joins it
            let line = match group.as_mut() {
                Some(g) => match g.absorb(line) {
                    Ok(())    => continue,
                    Err(line) => line
                },
                None => line
            };

            // anything else ends the group in progress ...
            if let Some(g) = group.take() {
                send_or_stop!(tx, g);
            }

            // ... and starts a new group, or is a block of its own
            match Block::of_line(line) {
                Some(b) if b.is_group() => group = Some(b),
                Some(b)                 => send_or_stop!(tx, b),
                None                    => {}
            }
        }

        if let Some(g) = group {
            send_or_stop!(tx, g);
        }

        Ok(())
    })
}

enum Outcome { Complete, ReaderGone }

fn consume_blocks<W>(rx: Receiver<Block>, write: W)
                     -> Result<Outcome, Box<dyn Error>>
    where W: FnOnce(Receiver<Block>) -> io::Result<()>
{
    match write(rx) {
        Ok(())
            => Ok(Outcome::Complete),

        Err(e) if e.kind() == io::ErrorKind::BrokenPipe
            => Ok(Outcome::ReaderGone),

        Err(e)
            => Err(e.into())
    }
}

// write the Blocks to stdout as Markdown, as they arrive
pub fn write_markdown(rx: Receiver<Block>) -> io::Result<()> {
    let mut out = io::stdout().lock();

    for b in rx {
        write!(out, "{}", b)?;
    }
    out.flush()
}

// run the pipeline on stdin, handing its Blocks to `write`
pub fn run<W>(write: W) -> Result<(), Box<dyn Error>>
    where W: FnOnce(Receiver<Block>) -> io::Result<()>
{
    let (tx1, rx1) = mpsc::channel();
    let handle1    = read_lines(tx1);

    let (tx2, rx2) = mpsc::channel();
    let handle2    = gather_preformatted(rx1, tx2);

    let (tx3, rx3) = mpsc::channel();
    let handle3    = decode_lines(rx2, tx3);

    let (tx4, rxlast) = mpsc::channel();
    let handle4       = blocks_of_lines(rx3, tx4);

    if let Outcome::ReaderGone = consume_blocks(rxlast, write)? {
        return Ok(());
    }

    /* Join from the end of the pipeline backwards and stop at the first
       error: stages after a failed one finish normally, but stages before
       it may still be blocked reading. */
    for h in [handle4, handle3, handle2, handle1] {
        match h.join() {
            Ok(Ok(())) => {},
            Ok(Err(e)) => return Err(e.into()),
            Err(_)     => return Err("internal error: a thread panicked"
                                     .into())
        }
    }

    Ok(())
}

// report the outcome of `run` the way a commandline filter should
pub fn exit_code(name: &str, result: Result<(), Box<dyn Error>>)
                 -> ExitCode
{
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{name}: {e}");
            ExitCode::FAILURE
        }
    }
}
