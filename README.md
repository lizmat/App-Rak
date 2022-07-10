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

To a large extent, the arguments are the same as with the `grep` utility provided on most Unixes.

Note: this is still very much in alpha development phase. Comments and suggestions are more than welcome!

POSITIONAL ARGUMENTS
====================

pattern
-------

The pattern to search for. This can either be a string, or a regular expression (indicated by a string starting and ending with **/**), or a Callable (indicated by a string starting with **{** and ending with **}**. 

path
----

Optional. Either indicates the path of the directory (and its sub-directories), or the file that will be searched. By default, all directories that do not start with a period, will be recursed into (but this can be changed with the `--dir` named argument).

By default, all files will be searched in the directories. This can be changed with the `--file` named argument.

SUPPORTED NAMED ARGUMENTS
=========================

All named arguments are optional. Any unexpected named arguments, will cause an exception with the unexpected named arguments listed.

-A --after --after-context
--------------------------

Indicate the number of lines that should be shown **after** any line that matches. Defaults to **0**. Will be overridden by a `-C` or `--context` argument.

-B --before --before-context
----------------------------

Indicate the number of lines that should be shown **before** any line that matches. Defaults to **0**. Will be overridden by a `-C` or `--context` argument.

-C --context
------------

Indicate the number of lines that should be shown **around** any line that matches. Defaults to **0**. Overrides any a `-A`, `--after`, `--after-context`, `-B`, `--before` or `--before-context` argument. argument.

--highlight
-----------

Indicate whether the pattern should be highlighted in the line in which it was found. Defaults to `True` if `--human` is (implicitely) set to `True`, else defaults to `False`.

--highlight--after
------------------

Indicate the string that should be used at the end of the pattern found in a line. Only makes sense if `--highlight` is (implicitely) set to `True`. Defaults to the empty string if `-o` or `--only-matching` is specified with a `True` value, or to the terminal code to end **bold** otherwise.

--highlight--before
-------------------

Indicate the string that should be used at the end of the pattern found in a line. Only makes sense if `--highlight` is (implicitely) set to `True`. Defaults to a space if `-o` or `--only-matching` is specified with a `True` value, or to the terminal code to start **bold** otherwise.

--human
-------

Indicate that search results should be presented in a human readable manner. This means: filenames shown on a separate line, line numbers shown, and highlighting performed. Defaults to `True` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `False`.

-l --files-only --files-with-matches
------------------------------------

If specified with a true value, will only produce the filenames of the files in which the pattern was found. Defaults to `False`.

-o --only-matching
------------------

Indicate whether only the matched pattern should be produced, rather than the line in which the pattern was found. Defaults to `False`.

--output-file
-------------

Indicate the path of the file in which the result of the search should be placed. Defaults to `STDOUT`.

--sum --summary-if-larger-than
------------------------------

Indicate the maximum size a line may have before it will be summarized. Defaults to `160` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `Inf` effectively (indicating no summarization will ever occur).

-S --follow-symlinks
--------------------

Indicate whether symbolic links to directories should be followed. Defaults to `False`.

--trim
------

Indicate whether lines that have the pattern, should have any whitespace at the start and/or end of the line removed. Defaults to `True` if no context for lines was specified, else defaults to `False`.

-V --version
------------

If the only argument, shows the name and version of the script, and the system it is running on.

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/App-Rak . Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2022 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

