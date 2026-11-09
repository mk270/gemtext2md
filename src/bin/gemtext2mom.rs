/* gemtext2mom: read Gemtext on stdin, write groff mom to stdout */

use std::io;
use std::process::ExitCode;

use gemtext2md::{exit_code, run, mom::write_mom};

fn main() -> ExitCode {
    exit_code("gemtext2mom", run(|rx| write_mom(rx, io::stdout().lock())))
}
