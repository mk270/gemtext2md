/* gemtext2md: read Gemtext on stdin, write Markdown to stdout */

use std::process::ExitCode;

use gemtext2md::{exit_code, run, write_markdown};

fn main() -> ExitCode {
    exit_code("gemtext2md", run(write_markdown))
}
