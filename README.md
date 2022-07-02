gemtext2md
==========

A gemini text to Markdown convertor, by Martin Keegan.

Usage
-----

As a commandline filter. The program reads Gemtext on stdin and
outputs Markdown, hopefully in CommonMark format.

Compile:

    $ ocamlc -o gemtext2md gemtext2md.ml

Use:

    $ gemtext2md < myfile.gemini > myfile.md

or, for the intensely lazy:

    $ ocaml gemtext2md.ml < myfile.gemini > myfile.md

