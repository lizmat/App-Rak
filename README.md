[![Actions Status](https://github.com/lizmat/App-Rak/actions/workflows/test.yml/badge.svg)](https://github.com/lizmat/App-Rak/actions)

NAME
====

App::Rak - a CLI for searching strings in files

SYNOPSIS
========

```bash
$ rak foo      # look for "foo" in current directory recursively

$ rak foo bar  # look for "foo" in directory "bar" recursively

$ rak '/ << foo >> /'    # look for "foo" as word in current directory

$ raku foo --files-only  # look for "foo", only produce filenames

$ raku foo --before=2 --after=2  # also produce 2 lines before and after
```

DESCRIPTION
===========

App::Rak provides a CLI called `rak` that allows you to look for a needle in (a selection of files) from a given directory recursively.

Note: this is still very much in alpha development phase. Comments and suggestions are more than welcome!

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/App-Rak . Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2022 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

