/* gemtext2md, A gemini text converter, by Martin Keegan

   To the extent (if any) permissible by law, Copyright (C) 2022  Martin Keegan

   This programme is free software; you may redistribute and/or modify it under
   the terms of the Apache Software Licence v2.0. */

/* gemtext2md: read Gemtext on stdin, write Markdown to stdout */

use std::process::ExitCode;

use gemtext2md::{exit_code, run, write_markdown};

fn main() -> ExitCode {
    exit_code("gemtext2md", run(write_markdown))
}
