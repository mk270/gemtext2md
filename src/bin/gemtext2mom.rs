/* gemtext2mom: read Gemtext on stdin, write groff mom to stdout */

use std::io;
use std::process::ExitCode;
use std::sync::mpsc::Receiver;

use gemtext2md::{exit_code, run, mom::write_mom, Block};


// the last stage of the pipeline: write the Blocks to stdout as mom
fn write_to_stdout(rx: Receiver<Block>) -> io::Result<()> {
    let output = io::stdout().lock();
    write_mom(rx, output)
}

fn main() -> ExitCode {
    let pipeline = run(write_to_stdout);
    exit_code("gemtext2mom", pipeline)
}
