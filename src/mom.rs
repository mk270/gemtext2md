/* gemtext2md, A gemini text converter, by Martin Keegan

   To the extent (if any) permissible by law, Copyright (C) 2022  Martin Keegan

   This programme is free software; you may redistribute and/or modify it under
   the terms of the Apache Software Licence v2.0. */

/* Rendering Blocks as groff mom.

   The title is the text of the first heading, if any. mom needs it before
   .START, and the first heading may come after other text, so the whole
   document is collected before anything is written. If the document opens
   with that heading, it becomes the title only; otherwise it is the title
   and also stays as a heading where it is. */

use std::io::{self, Write};
use std::sync::mpsc::Receiver;

use crate::{Block, Heading, HeadingLevel, Link};

// text on a line of its own: a leading . or ' would make roff read the
// line as a request, and a backslash starts an escape
fn text(s: &str) -> String {
    let s = s.replace('\\', "\\e");
    if s.starts_with('.') || s.starts_with('\'') {
        format!("\\&{}", s)
    } else {
        s
    }
}

/* Italics: *like this*. An opening * follows the start of the text,
   whitespace or opening punctuation, and is followed by a non-space; a
   closing * follows a non-space and is followed by the end of the text,
   whitespace or closing punctuation. Each opener pairs with the nearest
   valid closer. ** is never italics. Anything unpaired stays literal. */

fn opens(c: &[char], i: usize) -> bool {
    let before = i == 0 || c[i - 1].is_whitespace()
        || "([{\"'".contains(c[i - 1]);
    let after = c.get(i + 1).is_some_and(|&n| !n.is_whitespace() && n != '*');
    c[i] == '*' && before && after
}

fn closes(c: &[char], i: usize) -> bool {
    let before = c[i - 1] != '*' && !c[i - 1].is_whitespace();
    let after = c.get(i + 1)
        .is_none_or(|&n| n.is_whitespace() || ".,;:!?)]}\"'".contains(n));
    c[i] == '*' && before && after
}

// split into (is_italic, text) runs
fn italic_runs(s: &str) -> Vec<(bool, String)> {
    let c: Vec<char> = s.chars().collect();
    let mut runs: Vec<(bool, String)> = Vec::new();
    let mut plain = String::new();
    let mut i = 0;

    while i < c.len() {
        if opens(&c, i) {
            if let Some(j) = (i + 2..c.len()).find(|&j| closes(&c, j)) {
                if !plain.is_empty() {
                    runs.push((false, std::mem::take(&mut plain)));
                }
                runs.push((true, c[i + 1..j].iter().collect()));
                i = j + 1;
                continue;
            }
        }
        plain.push(c[i]);
        i += 1;
    }
    if !plain.is_empty() {
        runs.push((false, plain));
    }
    runs
}

// running text, with italics if enabled
fn prose(s: &str, italics: bool) -> String {
    if !italics {
        return text(s);
    }
    let mut out = String::new();
    for (ital, t) in italic_runs(s) {
        let t = t.replace('\\', "\\e");
        if ital {
            out.push_str(&format!("\\*[IT]{}\\*[PREV]", t));
        } else {
            out.push_str(&t);
        }
    }
    if out.starts_with('.') || out.starts_with('\'') {
        out.insert_str(0, "\\&");
    }
    out
}

// text as a quoted argument to a macro
fn arg(s: &str) -> String {
    format!("\"{}\"", s.replace('\\', "\\e").replace('"', "\\(dq"))
}

fn level(h: &HeadingLevel) -> u8 {
    use HeadingLevel::*;

    match h {
        H1 => 1,
        H2 => 2,
        H3 => 3
    }
}

fn write_header(out: &mut impl Write, title: Option<&str>,
                st: &Style) -> io::Result<()> {
    // PAPER must come first, before anything else
    writeln!(out, ".PAPER A4")?;
    if let Some(t) = title {
        writeln!(out, ".TITLE {}", arg(t))?;
        writeln!(out, ".PDF_TITLE {}", arg(t))?;
    }
    writeln!(out, ".DOCTYPE DEFAULT")?;
    writeln!(out, ".PRINTSTYLE TYPESET")?;
    write_style(out, st)?;
    writeln!(out, ".START")
}

// All the knobs for the look of the output. Sizes are groff
// measurements: a number plus a unit, e.g. "1v" (a line) or "12p"
// (points).
struct Style {
    family: &'static str,         // mom font family: T = Times, H = Helvetica
    para_space: &'static str,     // between paragraphs
    para_indent: &'static str,    // first-line indent of paragraphs
    heading_after: &'static str,  // between a heading and what follows
    list_before: &'static str,    // between text and a list
    list_after: &'static str,     // extra after a list (para_space adds)
    list_indent: &'static str,    // shift of a whole list to the right
    bullet_gap: u32,              // extra points between bullet and text
    title_font: &'static str,     // R = roman, B = bold
    heading_font: &'static str,   // R = roman, B = bold
    small_caps_heads: bool,       // set headings in small caps
}

const DEFAULT: Style = Style {
    family: "T",
    para_space: ".6v",
    para_indent: "0",
    heading_after: ".4v",
    list_before: ".6v",
    list_after: "0",
    list_indent: "24p",
    bullet_gap: 9,
    title_font: "B",
    heading_font: "B",
    small_caps_heads: false,
};

const SANS_SERIF: Style = Style {
    family: "H",
    ..DEFAULT
};

const CAP_HEADS: Style = Style {
    title_font: "R",
    heading_font: "R",
    small_caps_heads: true,
    ..DEFAULT
};

// the styles that can be chosen on the command line
#[derive(Clone, Copy, Default, clap::ValueEnum)]
pub enum StyleName {
    #[default]
    Default,
    SansSerif,
    CapHeads
}

impl StyleName {
    fn style(self) -> &'static Style {
        match self {
            StyleName::Default   => &DEFAULT,
            StyleName::SansSerif => &SANS_SERIF,
            StyleName::CapHeads  => &CAP_HEADS
        }
    }
}

fn write_style(out: &mut impl Write, st: &Style) -> io::Result<()> {
    writeln!(out, ".FAMILY {}", st.family)?;
    writeln!(out, ".DOCHEADER_FAMILY {}", st.family)?;
    writeln!(out, ".PAGENUM_FAMILY {}", st.family)?;
    writeln!(out, ".TITLE_FONT {}", st.title_font)?;
    writeln!(out, ".PAGENUM_HYPHENS OFF")?;
    writeln!(out, ".HEADERS OFF")?;
    for n in 1..=3 {
        write!(out, ".HEADING_STYLE {} FONT {}", n, st.heading_font)?;
        if st.small_caps_heads {
            write!(out, " SMALLCAPS")?;
        }
        writeln!(out)?;
    }
    writeln!(out, ".PARA_SPACE {}", st.para_space)?;
    writeln!(out, ".PARA_INDENT {}", st.para_indent)
}

fn list_open(out: &mut impl Write, st: &Style) -> io::Result<()> {
    writeln!(out, ".SP {}", st.list_before)?;
    writeln!(out, ".LIST USER \"\\[bu]\\h'{}p'\"", st.bullet_gap)?;
    writeln!(out, ".SHIFT_LIST {}", st.list_indent)
}

fn list_close(out: &mut impl Write, st: &Style) -> io::Result<()> {
    writeln!(out, ".LIST OFF")?;
    writeln!(out, ".SP {}", st.list_after)
}

fn write_block(out: &mut impl Write, b: &Block, st: &Style,
               italics: bool)
    -> io::Result<()> {
    use Block::*;

    match b {
        ParaB(p) => {
            writeln!(out, ".PP")?;
            writeln!(out, "{}", prose(p, italics))
        },

        HeadingB(Heading(l, t)) => {
            writeln!(out, ".HEADING {} {}", level(l), arg(t))?;
            writeln!(out, ".SP {}", st.heading_after)
        },

        PreformattedB(alt, lines) => {
            if let Some(a) = alt {
                writeln!(out, ".\\\" {}", a)?;
            }
            writeln!(out, ".QUOTE")?;
            writeln!(out, ".CODE")?;
            for line in lines {
                writeln!(out, "{}", text(line))?;
            }
            writeln!(out, ".CODE OFF")?;
            writeln!(out, ".QUOTE OFF")
        },

        LinksB(ll) => {
            list_open(out, st)?;
            for Link(url, tag) in ll {
                let caption = tag.as_deref().unwrap_or(url);
                writeln!(out, ".ITEM")?;
                writeln!(out, ".PDF_WWW_LINK {} {}", url, arg(caption))?;
            }
            list_close(out, st)
        },

        ListB(items) => {
            list_open(out, st)?;
            for item in items {
                writeln!(out, ".ITEM")?;
                writeln!(out, "{}", prose(item, italics))?;
            }
            list_close(out, st)
        },

        QuoteB(paras) => {
            writeln!(out, ".BLOCKQUOTE")?;
            for (i, para) in paras.iter().enumerate() {
                if i > 0 {
                    writeln!(out, ".PP")?;
                }
                writeln!(out, "{}", prose(para, italics))?;
            }
            writeln!(out, ".BLOCKQUOTE OFF")
        }
    }
}

// write the whole document as mom to `out`
pub fn write_mom(rx: Receiver<Block>, mut out: impl Write,
                 style: StyleName, italics: bool) -> io::Result<()> {
    let blocks: Vec<Block> = rx.into_iter().collect();

    let title = blocks.iter().find_map(|b| match b {
        Block::HeadingB(Heading(_, t)) => Some(t.as_str()),
        _                              => None
    });

    // a document that opens with its title heading doesn't repeat it
    let skip = matches!(blocks.first(), Some(Block::HeadingB(_)));

    let st = style.style();
    write_header(&mut out, title, st)?;
    for b in blocks.iter().skip(skip as usize) {
        write_block(&mut out, b, st, italics)?;
    }
    out.flush()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> String {
        prose(s, true)
    }

    #[test]
    fn simple() {
        assert_eq!(p("a *b c* d"), "a \\*[IT]b c\\*[PREV] d");
    }

    #[test]
    fn punctuation() {
        assert_eq!(p("(*b*), *c*."),
                   "(\\*[IT]b\\*[PREV]), \\*[IT]c\\*[PREV].");
    }

    #[test]
    fn literal() {
        for s in ["2 * 3 * 4", "a*b*c", "**b**", "*a", "a* b", "* *"] {
            assert_eq!(p(s), s);
        }
    }

    #[test]
    fn off() {
        assert_eq!(prose("a *b*", false), "a *b*");
    }
}
