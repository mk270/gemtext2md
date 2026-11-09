gemtext2md suite
================

A convertor for Gemini-formatted text, by Martin Keegan.

The tools generate PDF, Markdown and groff mom.

The three tools:

* gemtext2md
* gemtext2pdf
* gemtext2mom

Some care is taken to get reasonable whitespace around the various blocks,
and to consolidate consecutive links into lists.

Usage
-----

As a commandline filter. The programs read Gemtext on stdin and
output Markdown, hopefully in CommonMark format, or groff mom, or PDF.

Use:

    $ gemtext2md < myfile.gemini > myfile.md
    $ gemtext2mom < myfile.gemini > myfile.mom
    $ gemtext2pdf -o myfile.pdf < myfile.gemini

The mom and PDF programs take an optional `--style` argument:
`default` (serif fonts), `sans-serif`, or `cap-heads` (default,
with small-caps headings).

Text between asterisks, like *this*, is set in italics in the mom and PDF
output, subject to some rules: the opening asterisk must follow whitespace
or an opening bracket or quote, the closing one must precede whitespace or
punctuation, and `**` is left alone. Use `--no-italics` to leave asterisks
as they are.

Dependencies
------------

The PDF generation pathway requires `groff` to be on the PATH.

Licence
-------

Available under the Apache licence.
