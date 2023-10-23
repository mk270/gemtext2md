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

impl From<String> for Line {
    // corresponds to OCaml function 'line_of_string : string -> line'
    fn from(s: String) -> Self {
        use Line::*;
        use Malformed::*;
        use HeadingLevel::*;

        match s.chars().collect::<Vec<char>>()[..] {
            // links
            ['=', '>']               => MalformedL(MLink),
            ['=', '>', ' ', ..]      => link_of_line(s),
            ['=', '>', ..]           => MalformedL(MLink),

            // headings
            ['#', '#', '#']          => MalformedL(MHeading),
            ['#', '#', '#', ' ']     => MalformedL(MHeading),
            ['#', '#', '#', ' ', ..] => make_heading(s, H3, 4),
            ['#', '#', '#', ..]      => make_heading(s, H3, 3),

            ['#', '#', _]            => MalformedL(MHeading),
            ['#', '#']               => MalformedL(MHeading),
            ['#', '#', ' ', ..]      => make_heading(s, H2, 3),
            ['#', '#', ..]           => MalformedL(MHeading),

            ['#', ' ']               => MalformedL(MHeading),
            ['#']                    => MalformedL(MHeading),
            ['#', ' ', ..]           => make_heading(s, H1, 2),
            ['#', ..]                => MalformedL(MHeading),

            // paragraphs / blanks
            []                       => BlankL,
            _                        => ParaL(trim(s))
        }
    }
}

impl fmt::Display for Heading {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", heading_chars(&self.0), self.1)
    }
}

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Link(url, tag) = self.clone();
        let caption = match tag {
            Some(c) => c,
            None => url.clone()
        };

        write!(f, "* [{}]({})\n", caption, url)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Block::*;

        let s = match self {
            ParaB(p)            => format!("{}\n\n", p),
            PreformattedB(prpr) => format!("```\n{}\n```\n\n", prpr.join("\n")),
            LinksB(ll)          => string_of_links(ll.to_vec()),
            HeadingB(h)         => format!("{}\n\n", h)
        };

        write!(f, "{}", s)
    }
}

fn heading_chars(h: &HeadingLevel) -> String {
    use HeadingLevel::*;

    let s = match h {
        H1 => "#",
        H2 => "##",
        H3 => "###"
    };

    s.to_string()
}

fn string_of_links(ll: Vec<Link>) -> String {
    if ll.is_empty() {
        return String::from("");
    }

    let links: Vec<String> = ll.into_iter()
        .map(|l| l.to_string()).collect();

    format!("{}\n", links.join(""))
}

fn link_of_line(line: String) -> Line {
    use Line::*;

    let parts: Vec<&str> = line.splitn(3, " ").collect();
    match parts.as_slice() {
        [ "=>", "" ] => MalformedL(Malformed::MLink),
        [ "=>", url ] => LinkL(Link(url.to_string(), None)),
        [ "=>", url, tag ] => LinkL(
            Link(url.to_string(), Some(tag.to_string()))
        ),
        _ => MalformedL(Malformed::MLink)
    }
}

// FIXME - broken in the original
fn trim(s: String) -> String { s.to_string().trim().to_string() }

fn make_heading(s: String, level: HeadingLevel, offset: usize) -> Line {
    let trimmed = trim(s);
    let after_hashes = &trimmed[offset..];
    Line::HeadingL(Heading(level, after_hashes.to_string()))
}

fn read_lines(tx: Sender<String>) {
    thread::spawn(move || {
        let stdin = io::stdin();
        for (lineno, line) in stdin.lock().lines().enumerate() {
            match line {
                Ok(l) => tx.send(l).unwrap(),
                Err(e) => panic!("couldn't read line {}, {}", lineno, e)
            }
        }
    });
}

// annotate lines with whether they occur within preformatted blocks
// discard the lines beginning with "```"
fn gather_preformatted(rx: Receiver<String>, tx: Sender<(bool, String)>) {
    thread::spawn(move || {
        let mut pref = false;

        for i in rx {
            match i.get(..3) {
                Some("```") => pref = !pref,
                _ => tx.send((pref, i)).unwrap()
            }
        }
    });
}

fn decode_lines(rx: Receiver<(bool, String)>, tx: Sender<Line>) {
    thread::spawn(move || {
        let mut pref_acc: Vec<String> = vec![];
        let mut _acc: Vec<Line> = vec![];

        for (pref, line) in rx {
            match (pref_acc.as_slice(), pref, line) {
                // in a preformatted block
                ([], true, l) => { // first line of a preformatted block
                    pref_acc.push(l);
                },
                (_pls, true, l) => { // first line of a preformatted block
                    pref_acc.push(l);
                },

                // outside preformatted block
                ([], false, l) => { // usual case
                    pref_acc.clear();
                    tx.send(Line::from(l)).unwrap();
                },
                (pls, false, l) => { // first line after a preformatted block
                    tx.send(Line::PreformattedL(pls.to_vec())).unwrap();
                    pref_acc.clear();
                    tx.send(Line::from(l)).unwrap();
                }
            }
        }

        if !pref_acc.is_empty() {
            tx.send(Line::PreformattedL(pref_acc)).unwrap();
        }
    });
}

fn blocks_of_lines(rx: Receiver<Line>, tx: Sender<Block>) {
    thread::spawn(move || {
        use Block::*;
        use Line::*;

        let mut links: Vec<Link> = vec![];

        for line in rx {
            match line {
                BlankL => {
                    tx.send(LinksB(links.clone())).unwrap();
                    links.clear();
                },
                ParaL(p) => {
                    tx.send(LinksB(links.clone())).unwrap();
                    links.clear();
                    tx.send(ParaB(p)).unwrap();
                },
                HeadingL(h) => {
                    tx.send(LinksB(links.clone())).unwrap();
                    links.clear();
                    tx.send(HeadingB(h)).unwrap();
                },
                PreformattedL(p) => {
                    tx.send(LinksB(links.clone())).unwrap();
                    links.clear();
                    tx.send(PreformattedB(p)).unwrap();
                },
                LinkL(link) => {
                    links.push(link);
                },
                MalformedL(m) => {
                    panic!("malformed line: {:?}", m);
                }
            }
        }

        if !links.is_empty() {
            tx.send(LinksB(links)).unwrap();
        }
    });
}

fn consume_blocks(rx: Receiver<Block>) -> Result<(), ()>{
    for i in rx {
        print!("{}", i);
        io::stdout().flush().unwrap();
    }

    Ok(())
}

fn main() -> Result<(), ()> {
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
