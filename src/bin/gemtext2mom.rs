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
    style: StyleName
}

// the last stage of the pipeline: write the Blocks to stdout as mom
fn write_to_stdout(rx: Receiver<Block>, style: StyleName) -> io::Result<()> {
    let output = io::stdout().lock();
    write_mom(rx, output, style)
}

fn main() -> ExitCode {
    let args = Args::parse();
    let pipeline = run(|rx| write_to_stdout(rx, args.style));
    exit_code("gemtext2mom", pipeline)
}
