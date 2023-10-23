gemtext2md
==========

A gemini text to Markdown convertor, by Martin Keegan.

Some care is taken to get reasonable whitespace around the various blocks,
and to consolidate consecutive links into lists.

Usage
-----

As a commandline filter. The program reads Gemtext on stdin and
outputs Markdown, hopefully in CommonMark format.

Use:

    $ cargo run -q < myfile.gemini > myfile.md

The code has been rewritten from OCaml to Rust. `cargo` is part of the
Rust toolchain. At some point, native binaries may be provided for
download.
