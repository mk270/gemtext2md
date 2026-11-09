gemtext2md suite
================

A convertor for Gemini-formatted text, by Martin Keegan.

Some care is taken to get reasonable whitespace around the various blocks,
and to consolidate consecutive links into lists.

The three tools:

* gemtext2md
* gemtext2pdf
* gemtext2mom

Usage
-----

As a commandline filter. The programs read Gemtext on stdin and
output Markdown, hopefully in CommonMark format, or groff mom, or PDF.

Use:

    $ gemtext2md < myfile.gemini > myfile.md
    $ gemtext2mom < myfile.gemini > myfile.mom
    $ gemtext2pdf -o myfile.pdf < myfile.gemini

Dependencies
------------

The PDF generation pathway requires `groff` to be on the PATH.

Known bugs
----------

The formatting of mom and PDF is currently quite bad.
