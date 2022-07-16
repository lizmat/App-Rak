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

Can also be specified with the `--pattern` named argument, in which case all the positional arguments are considered to be a path specification.

path(s)
-------

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

--edit
------

Indicate whether the patterns found should be fed into an editor for inspection and/or changes. Defaults to `False`.

-h --no-filename
----------------

Indicate whether filenames should **not** be shown. Defaults to `False` if `--human` is (implicitely) set to `True`, else defaults to `True`.

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

--list-tags
-----------

```bash
$ rak --list-tags
fs: --'follow-symlinks'
im: --ignorecase --ignoremark
```

If specified with a true value and as the only named argument, will list all saved tags.

-n --line-number
----------------

Indicate whether line numbers should be shown. Defaults to `True` if `--human` is (implicitely) set to `True` and <-h> is **not** set to `True`, else defaults to `False`.

-o --only-matching
------------------

Indicate whether only the matched pattern should be produced, rather than the line in which the pattern was found. Defaults to `False`.

--output-file
-------------

Indicate the path of the file in which the result of the search should be placed. Defaults to `STDOUT`.

--pattern
---------

Alternative way to specify the pattern to search for. If (implicitely) specified, will assume the first positional parameter specified is actually a path specification, rather than a pattern. This allows the pattern to be searched for to be saved with `--save`.

--replace-files
---------------

Only makes sense if the specified pattern is a `Callable`. Indicates whether the output of the pattern should be applied to the file in which it was found. Defaults to `False`.

--save
------

Save all named arguments with the given tag in the configuration file (`~/.rak-config.json`), and exit with a message that the named arguments have been saved with the given tag.

This feature can used to both create shortcuts for specific (long) named arguments, or just as a convenient way to combine often used named arguments.

```bash
$ rak --ignorecase --ignoremark --save=im
Saved configuration for 'im'

$ rak --follow-symlinks --save=fs
Saved configuration for 'fs'

$ rak --save=foo
Removed configuration for 'foo'
```

See `--with` to add saved named arguments to a query. Please note that no validity checking on the named arguments is being performed at the moment of saving, as validity may depend on other arguments having been specified.

To remove a saved set of named arguments, use `--save` as the only argument.

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

--with
------

```bash
# run search with --ignorecase --ignoremark --follow-symlinks
$ rak foo --with=im,fs
```

Add all named arguments previously saved with `--save` with the given tag(s) from the configuration file (`~/.rak-config.json`). Multiple tags can be specified, separated by commas. See `--save` to saved named arguments with a tag.

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/App-Rak . Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2022 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

