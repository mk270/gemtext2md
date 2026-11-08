/* gemtext2md, A gemtext to markdown converter, by Martin Keegan

   To the extent (if any) permissible by law, Copyright (C) 2023  Martin Keegan

   This programme is free software; you may redistribute and/or modify it under
   the terms of the Apache Software Licence v2.0. */

/* Usage: as a commandline filter. The program reads Gemtext on stdin and
   outputs Markdown, hopefully in CommonMark format. */

/* The author is perfectly aware that this code is unidiomatic,
   inefficient, inelegant, unprincipled, undocumented, depends on
   loading the whole input stream into memory, makes your curtains
   fade, etc, etc. It is simply not worth polishing it. */

/* The code below is a fairly bloody-minded hand translation from
   the OCaml original into unidiomatic Rust, right down to the function
   names. */

use std::sync::mpsc;
use std::sync::mpsc::{Sender, Receiver};
use std::thread;
use std::io::{self, BufRead, Write};
use std::fmt;
use std::mem::take;
use std::error::Error;
use std::process::ExitCode;

#[derive(Debug)]
enum HeadingLevel {
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
struct Heading(HeadingLevel, String);

#[derive(Debug,Clone)]
struct Link(String, Option<String>);

#[derive(Debug)]
enum Line {
    PreformattedL(Vec<String>),
    ParaL(String),
    LinkL(Link),
    HeadingL(Heading),
    BlankL,
    MalformedL(Malformed)
}

#[derive(Debug)]
enum Block {
    PreformattedB(Vec<String>),
    ParaB(String),
    LinksB(Vec<Link>),
    HeadingB(Heading)
}

// wrappers for including line numbers
struct NumString(String, usize);
struct NumLine(Line, usize);

impl From<String> for Line {
    // corresponds to OCaml function 'line_of_string : string -> line'
    fn from(s: String) -> Self {
        use Line::*;
        use Malformed::*;
        use HeadingLevel::*;

        // links: "=>" [whitespace] URL [whitespace label]
        if let Some(rest) = s.strip_prefix("=>") {
            let rest = rest.trim();
            return match rest.split_once(char::is_whitespace) {
                None if rest.is_empty() => MalformedL(MLink),
                None => LinkL(Link(rest.to_string(), None)),
                Some((url, label)) => LinkL(Link(
                    url.to_string(),
                    Some(label.trim_start().to_string())
                )),
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
                MalformedL(MHeading)
            } else {
                HeadingL(Heading(level, text.to_string()))
            };
        }

        // paragraphs / blanks
        let text = s.trim();
        if text.is_empty() { BlankL } else { ParaL(text.to_string()) }
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

            PreformattedB(prpr) => {
                writeln!(f, "```")?;
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
            }
        }
    }
}

fn heading_chars(h: &HeadingLevel) -> &'static str {
    use HeadingLevel::*;

    let s = match h {
        H1 => "#",
        H2 => "##",
        H3 => "###"
    };

    s
}

/*

Everything below this point is a five-stage pipeline.

  read_lines:          get stdin input stream line by line as strings
  gather_preformatted: tag the strings if they're in a preformatted block
  decode_lines:        turn the strings into Lines
  blocks_of_lines:     turn the Lines into Blocks
  consume_blocks:      print the Blocks to stdout

*/

fn read_lines(tx: Sender<NumString>) {
    thread::spawn(move || {
        let stdin = io::stdin();
        for (lineno, line) in stdin.lock().lines().enumerate() {
            match line {
                Ok(l) => tx.send(NumString(l, lineno)).unwrap(),
                Err(e) => panic!("couldn't read line {}, {}", lineno + 1, e)
            }
        }
    });
}

// annotate lines with whether they occur within preformatted blocks
// discard the lines beginning with "```"
fn gather_preformatted(rx: Receiver<NumString>,
                       tx: Sender<(bool, NumString)>) {
    thread::spawn(move || {
        let mut pref = false;

        for i in rx {
            match i.0.get(..3) {
                Some("```") => pref = !pref,
                _ => tx.send((pref, i)).unwrap()
            }
        }
    });
}

// convert the incoming strings to Lines
fn decode_lines(rx: Receiver<(bool, NumString)>,
                tx: Sender<NumLine>) {
    thread::spawn(move || {
        use Line::PreformattedL;
        let mut pref_acc: Vec<String> = vec![];

        for (pref, ns) in rx {
            let NumString(s, lineno) = ns;
            if pref {
                pref_acc.push(s);
            } else {
                if !pref_acc.is_empty() {
                    tx.send(NumLine(PreformattedL(take(&mut pref_acc)),
                                    lineno)).
                        unwrap();
                }
                tx.send(NumLine(Line::from(s), lineno)).unwrap();
            }
        }

        if !pref_acc.is_empty() {
            tx.send(NumLine(PreformattedL(pref_acc), 0)).unwrap();
        }
    });
}

// aggregate Lines into Blocks
fn blocks_of_lines(rx: Receiver<NumLine>,
                   tx: Sender<Block>) {
    thread::spawn(move || {
        use Block::*;
        use Line::*;

        let mut links: Vec<Link> = vec![];

        for NumLine(line, lineno) in rx {
            let (flush_links, payload) = match line {
                MalformedL(m)    => panic!("error: {:?} at line {}", m, lineno),
                LinkL(link)      => { links.push(link); (false, None) },
                BlankL           => (true, None),
                ParaL(p)         => (true, Some(ParaB(p))),
                HeadingL(h)      => (true, Some(HeadingB(h))),
                PreformattedL(p) => (true, Some(PreformattedB(p)))
            };

            if flush_links {
                tx.send(LinksB(take(&mut links))).unwrap();
            }

            if let Some(p) = payload {
                tx.send(p).unwrap();
            }
        }

        if !links.is_empty() {
            tx.send(LinksB(links)).unwrap();
        }
    });
}

fn consume_blocks(rx: Receiver<Block>) -> Result<(), Box<dyn Error>>{
    for i in rx {
        print!("{}", i);
        io::stdout().flush()?;
    }

    Ok(())
}

fn run() -> Result<(), Box<dyn Error>> {
    let (tx1, rx1) = mpsc::channel();
    read_lines(tx1);

    let (tx2, rx2) = mpsc::channel();
    gather_preformatted(rx1, tx2);

    let (tx3, rx3) = mpsc::channel();
    decode_lines(rx2, tx3);

    let (tx4, rxlast) = mpsc::channel();
    blocks_of_lines(rx3, tx4);

    consume_blocks(rxlast)
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("gemtext2md: {e}");
            ExitCode::FAILURE
        }
    }
}
