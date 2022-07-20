[![Actions Status](https://github.com/lizmat/App-Rak/actions/workflows/test.yml/badge.svg)](https://github.com/lizmat/App-Rak/actions)

NAME
====

App::Rak - a CLI for searching strings in files and more

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

The pattern to search for. This can either be a string, or a [Raku regular expression](https://docs.raku.org/language/regexes) (indicated by a string starting and ending with `/`), a `Callable` (indicated by a string starting with `{` and ending with `}`), or a a result of [`Whatever` currying](https://docs.raku.org/type/Whatever) (indicated by a string starting with `*.`).

Can also be specified with the `--pattern` option, in which case **all** the positional arguments are considered to be a path specification.

path(s)
-------

Optional. Either indicates the path of the directory (and its sub-directories), or the file that will be searched. By default, all directories that do not start with a period, will be recursed into (but this can be changed with the `--dir` option).

By default, all files will be searched in the directories. This can be changed with the `--file` option

SUPPORTED OPTIONS
=================

All options are optional. Any unexpected options, will cause an exception to be thrown with the unexpected options listed.

--after-context=N
-----------------

Indicate the number of lines that should be shown **after** any line that matches. Defaults to **0**. Will be overridden by a `--context` argument.

--before-context=N
------------------

Indicate the number of lines that should be shown **before** any line that matches. Defaults to **0**. Will be overridden by a `--context` argument.

--break[=string]
----------------

Indicate whether there should be a visible division between matches of different files. Can also be specified as a string to be used as the divider. Defaults to `True` (using an empty line as a divider) if `--human` is (implicitly) set to `True`, else defaults to `False`.

--context=N
-----------

Indicate the number of lines that should be shown **around** any line that matches. Defaults to **0**. Overrides any a `--after-context` or `--before-context` arguments.

--count-only
------------

Indicate whether just the number of lines with matches should be calculated. When specified with a `True` value, will show a "N matches in M files" by default, and if the `:files-with-matches` option is also specified with a `True` value, will also list the file names with their respective counts.

--edit[=editor]
---------------

Indicate whether the patterns found should be fed into an editor for inspection and/or changes. Defaults to `False`. Optionally takes the name of the editor to be used.

--file-separator-null
---------------------

Indicate to separate filenames by null bytes rather than newlines if the `--files-with-matches` option is specified with a `True` value.

--group-matches
---------------

Indicate whether matches of a file should be grouped together by mentioning the filename only once (instead of on every line). Defaults to `True` if `--human` is (implicitly) set to `True`, else defaults to `False`.

--highlight
-----------

Indicate whether the pattern should be highlighted in the line in which it was found. Defaults to `True` if `--human` is (implicitly) set to `True`, else defaults to `False`.

--help [area-of-interest]
-------------------------

Show argument documentation, possibly extended by giving the area of interest, which are:

  * pattern

  * string

  * code

  * haystack

  * result

  * listing

  * resource

  * edit

  * option

  * general

  * philosophy

  * examples

--highlight--after[=string]
---------------------------

Indicate the string that should be used at the end of the pattern found in a line. Only makes sense if `--highlight` is (implicitly) set to `True`. Defaults to the empty string if `--only-matching` is specified with a `True` value, or to the terminal code to end **bold** otherwise.

--highlight--before[=string]
----------------------------

Indicate the string that should be used at the end of the pattern found in a line. Only makes sense if `--highlight` is (implicitly) set to `True`. Defaults to a space if `--only-matching` is specified with a `True` value, or to the terminal code to start **bold** otherwise.

--human
-------

Indicate that search results should be presented in a human readable manner. This means: filenames shown on a separate line, line numbers shown, and highlighting performed. Defaults to `True` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `False`.

--files-with-matches
--------------------

If specified with a true value, will only produce the filenames of the files in which the pattern was found. Defaults to `False`.

--list-additional-options
-------------------------

```bash
$ rak --list-additional-options
fs: --'follow-symlinks'
im: --ignorecase --ignoremark
```

If specified with a true value and as the only option, will list all additional options previously saved with `--save`.

--modify-files
--------------

Only makes sense if the specified pattern is a `Callable`. Indicates whether the output of the pattern should be applied to the file in which it was found. Defaults to `False`.

The `Callable` will be called for each line, giving the line (**including** its line ending). It is then up to the `Callable` to return:

### False

Remove this line from the file. NOTE: this means the exact `False` value.

### True

Keep this line unchanged the file. NOTE: this means the exact `True` value.

### any other value

Inserts this value in the file instead of the given line. The value can either be a string, or a list of strings.

--module=foo
------------

Indicate the Raku module that should be loaded. Only makes sense if the pattern is executable code.

--only-matching
---------------

Indicate whether only the matched pattern should be produced, rather than the line in which the pattern was found. Defaults to `False`.

--output-file=filename
----------------------

Indicate the path of the file in which the result of the search should be placed. Defaults to `STDOUT`.

--pattern=foo
-------------

Alternative way to specify the pattern to search for. If (implicitly) specified, will assume the first positional parameter specified is actually a path specification, rather than a pattern. This allows the pattern to be searched for to be saved with `--save`.

--repository=dir
----------------

Indicate the directory that should be searched for Raku module loading. Only makes sense if the pattern is executable code.

Note that you can create a familiar shortcut for the most common arguments of the `--repository` option:

```bash
$ rak --repository=. --save=I.
Saved option '--I.' as: --repository='.'

$ rak --repository=lib --save=Ilib
Saved option '--Ilib' as: --repository=lib
```

--save=name
-----------

Save all named arguments with the given name in the configuration file (`~/.rak-config.json`), and exit with a message that these options have been saved with the given name.

This feature can used to both create shortcuts for specific (long) named arguments, or just as a convenient way to combine often used named arguments.

```bash
$ rak --ignorecase --ignoremark --save=im
Saved option '--im' as: --ignorecase --ignoremark

# same as --ignorecase --ignoremark
$ rak foo --im

$ rak --follow-symlinks --save=fs
Saved option '--fs' as: --follow-symlinks

$ rak --save=foo
Removed configuration for 'foo'
```

Any saved named arguments can be accessed as if it is a standard named boolean argument. Please note that no validity checking on the named arguments is being performed at the moment of saving, as validity may depend on other arguments having been specified.

To remove a saved set of named arguments, use `--save` as the only named argument.

--show-filename
---------------

Indicate whether filenames should be shown. Defaults to `True` if `--human` is (implicitly) set to `True`, else defaults to `False`.

--show-line-number
------------------

Indicate whether line numbers should be shown. Defaults to `True` if `--human` is (implicitly) set to `True` and <-h> is **not** set to `True`, else defaults to `False`.

--summary-if-larger-than=N
--------------------------

Indicate the maximum size a line may have before it will be summarized. Defaults to `160` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `Inf` effectively (indicating no summarization will ever occur).

  * --type[=words|starts-with|ends-with|contains]

Only makes sense if the pattern is a string. With `words` specified, will look for pattern as a word in a line, with `starts-with` will look for the pattern at the beginning of a line, with `ends-with` will look for the pattern at the end of a line, with `contains` will look for the pattern at any position in a line.

--follow-symlinks
-----------------

Indicate whether symbolic links to directories should be followed. Defaults to `False`.

--trim
------

Indicate whether lines that have the pattern, should have any whitespace at the start and/or end of the line removed. Defaults to `True` if no context for lines was specified, else defaults to `False`.

--version
---------

If the only argument, shows the name and version of the script, and the system it is running on.

CREATING YOUR OWN OPTIONS
=========================

You can use the `--save` option to save a set of options and than later access them with the given name:

```bash
$ rak --ignorecase --ignoremark --save=im
Saved option '--im' as: --ignorecase --ignoremark

# same as --ignorecase --ignoremark
$ rak foo --im
```

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/App-Rak . Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2022 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

