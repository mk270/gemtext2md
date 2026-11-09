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

fn write_header(out: &mut impl Write, title: Option<&str>) -> io::Result<()> {
    if let Some(t) = title {
        writeln!(out, ".TITLE {}", arg(t))?;
        writeln!(out, ".PDF_TITLE {}", arg(t))?;
    }
    writeln!(out, ".DOCTYPE DEFAULT")?;
    writeln!(out, ".PRINTSTYLE TYPESET")?;
    writeln!(out, ".START")
}

fn write_block(out: &mut impl Write, b: &Block) -> io::Result<()> {
    use Block::*;

    match b {
        ParaB(p) => {
            writeln!(out, ".PP")?;
            writeln!(out, "{}", text(p))
        },

        HeadingB(Heading(l, t)) => {
            writeln!(out, ".HEADING {} {}", level(l), arg(t))
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
            writeln!(out, ".LIST BULLET")?;
            for Link(url, tag) in ll {
                let caption = tag.as_deref().unwrap_or(url);
                writeln!(out, ".ITEM")?;
                writeln!(out, ".PDF_WWW_LINK {} {}", url, arg(caption))?;
            }
            writeln!(out, ".LIST OFF")
        },

        ListB(items) => {
            writeln!(out, ".LIST BULLET")?;
            for item in items {
                writeln!(out, ".ITEM")?;
                writeln!(out, "{}", text(item))?;
            }
            writeln!(out, ".LIST OFF")
        },

        QuoteB(paras) => {
            writeln!(out, ".BLOCKQUOTE")?;
            for (i, para) in paras.iter().enumerate() {
                if i > 0 {
                    writeln!(out, ".PP")?;
                }
                writeln!(out, "{}", text(para))?;
            }
            writeln!(out, ".BLOCKQUOTE OFF")
        }
    }
}

// write the whole document as mom to `out`
pub fn write_mom(rx: Receiver<Block>, mut out: impl Write) -> io::Result<()> {
    let blocks: Vec<Block> = rx.into_iter().collect();

    let title = blocks.iter().find_map(|b| match b {
        Block::HeadingB(Heading(_, t)) => Some(t.as_str()),
        _                              => None
    });

    // a document that opens with its title heading doesn't repeat it
    let skip = matches!(blocks.first(), Some(Block::HeadingB(_)));

    write_header(&mut out, title)?;
    for b in blocks.iter().skip(skip as usize) {
        write_block(&mut out, b)?;
    }
    out.flush()
}
