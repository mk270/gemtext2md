/* gemtext2mom: read Gemtext on stdin, write groff mom to stdout */

use std::io;
use std::process::ExitCode;
use std::sync::mpsc::Receiver;

use clap::Parser;

use gemtext2md::{exit_code, run, mom::{write_mom, StyleName}, Block};


#[derive(Parser)]
#[command(about = "Convert Gemtext on stdin to groff mom")]
struct Args {
    /// Visual style of the document
    #[arg(long, value_enum, default_value_t = StyleName::Default)]
    style: StyleName,

    /// Leave *asterisks* alone instead of setting the text in italics
    #[arg(long)]
    no_italics: bool
}

// the last stage of the pipeline: write the Blocks to stdout as mom
fn write_to_stdout(rx: Receiver<Block>, style: StyleName,
                   italics: bool) -> io::Result<()> {
    let output = io::stdout().lock();
    write_mom(rx, output, style, italics)
}

fn main() -> ExitCode {
    let args = Args::parse();
    let italics = !args.no_italics;
    let pipeline = run(|rx| write_to_stdout(rx, args.style, italics));
    exit_code("gemtext2mom", pipeline)
}
