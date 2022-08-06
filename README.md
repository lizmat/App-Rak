[![Actions Status](https://github.com/lizmat/App-Rak/actions/workflows/test.yml/badge.svg)](https://github.com/lizmat/App-Rak/actions)

NAME
====

App::Rak - 21st century grep / find / ack / ag / rg on steroids

SYNOPSIS
========

```bash
$ rak foo      # look for "foo" in current directory recursively

$ rak foo bar  # look for "foo" in directory "bar" recursively

$ rak '/ << foo >> /'    # look for "foo" as word in current directory

$ rak foo --files-only  # look for "foo", only produce filenames

$ rak foo --before=2 --after=2  # also produce 2 lines before and after

$ rak '{.contains("foo") && .contains("bar")}'  # lines with foo AND bar
```

DESCRIPTION
===========

App::Rak provides a CLI called `rak` that allows you to look for a pattern in (a selection of files) from one or more directories recursively. It has been modelled after utilities such as `grep`, `ack`, `ag` and `rg`, with a little bit of `find` mixed in, and `-n` and `-p` parameters of many programming languages.

Note: this is still very much in alpha development phase. Comments, suggestions and bug reports are more than welcome!

POSITIONAL ARGUMENTS
====================

pattern
-------

The pattern to search for. This can either be a string, or a [Raku regular expression](https://docs.raku.org/language/regexes) (indicated by a string starting and ending with `/`), a `Callable` (indicated by a string starting with `{` and ending with `}`), or a a result of [`Whatever` currying](https://docs.raku.org/type/Whatever) (indicated by a string starting with `*.`).

Can also be specified with the `--pattern` option, in which case **all** the positional arguments are considered to be a path specification.

If the pattern is a `Callable`, then the dynamic variable `$*IO` will contain the `IO::Path` object of the file being processed. Note that pattern `Callable`s will be called in a thread **unsafe** manner.

path(s)
-------

Optional. Either indicates the path of the directory (and its sub-directories), or the file that will be searched. By default, all directories that do not start with a period, will be recursed into (but this can be changed with the `--dir` option).

By default, all files will be searched in the directories. This can be changed with the `--file` option

ON CALLABLES AS PATTERN
=======================

The Raku Programming Language has a number of unique features that can be used with patterns that are so-called `Callable`s. One of them is the use of so-called [phasers](https://docs.raku.org/language/phasers) (pieces of code that will be executed automatically when a certain condition has been met.

`App::Rak` currently supports the [loop phasers](https://docs.raku.org/language/phasers#FIRST):

  * FIRST - code to run when searching starts

  * NEXT - code to run when searching a file is done

  * LAST - code to run when searching is done

These phasers will be called in a thread-safe manner.

```bash
$ rak '{ FIRST state $seen = 0; NEXT $seen++; LAST say "$seen files"; .contains("pattern")}'
```

Any other phasers that do not require special attention by `App::Rak` are also supported in any code specified.

CREATING YOUR OWN OPTIONS
=========================

App::Rak provides **many** options. If you are happy with a set of options for a certain workflow, You can use the `--save` option to save that set of options and than later access them with the given name:

```bash
$ rak --ignorecase --ignoremark --save=im
Saved option '--im' as: --ignorecase --ignoremark

# same as --ignorecase --ignoremark
$ rak foo --im
```

You can use the `--list-custom-options` to see what options you have saved before.

SUPPORTED OPTIONS
=================

All options are optional. Any unexpected options, will cause an exception to be thrown with the unexpected options listed.

--after-context=N
-----------------

Indicate the number of lines that should be shown **after** any line that matches. Defaults to **0**. Will be overridden by a `--context` argument.

--backup[=extension]
--------------------

Indicate whether backups should be made of files that are being modified. If specified without extension, the extension `.bak` will be used.

--before-context=N
------------------

Indicate the number of lines that should be shown **before** any line that matches. Defaults to **0**. Will be overridden by a `--context` argument.

--blame-per-line
----------------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a `True` value, indicates that each line from the selected files will be provided as [`Git::Blame::Line`](https://raku.land/zef:lizmat/Git::Blame::File#accessors-on-gitblameline) objects if `git blame` can be performed on the a selected file. If that is not possible, then the selected file will be ignored.

If <git blame> information can be obtained, then the associated `Git::Blame::Line` object will be presented to the pattern `Callable`. If the Callable returns a true value, then the short representation of the `git blame` information will be shown. If the returned value is a string, then that string will be shown.

```bash
$ rak '{ .author eq "Scooby Doo" }' --blame-per-line
```

--break[=string]
----------------

Indicate whether there should be a visible division between matches of different files. Can also be specified as a string to be used as the divider. Defaults to `True` (using an empty line as a divider) if `--human` is (implicitly) set to `True`, else defaults to `False`.

--context=N
-----------

Indicate the number of lines that should be shown **around** any line that matches. Defaults to **0**. Overrides any a `--after-context` or `--before-context` arguments.

--count-only
------------

Flag. Indicate whether just the number of lines with matches should be calculated. When specified with a `True` value, will show a "N matches in M files" by default, and if the `:files-with-matches` option is also specified with a `True` value, will also list the file names with their respective counts.

--dryrun
--------

Flag. Indicate to **not** actually make any changes to any content modification if specified with a `True` value. Only makes sense together with the `--modify-files` option.

--edit[=editor]
---------------

Indicate whether the patterns found should be fed into an editor for inspection and/or changes. Defaults to `False`. Optionally takes the name of the editor to be used.

--extensions=spec
-----------------

Indicate the extensions of the filenames that should be inspected. By default, no limitation on filename extensions will be done.

Extensions can be specified as a comma-separated list, or one of the predefined groups, indicated by `#name`.

```bash
# inspect files with extensions used by Raku
$ rak foo --extensions=#raku

# inspect files with Markdown content
$ rak foo --extensions=md,markdown

# inspect files without extension
$ rak foo --extensions=
```

Predefined groups are `#raku`, `#perl`, `#c`, `#c++`, `#yaml`, <#ruby> `#python`, `#markdown` and `#text`.

--file-separator-null
---------------------

Flag. Indicate to separate filenames by null bytes rather than newlines if the `--files-with-matches` option is specified with a `True` value.

--files-from=filename
---------------------

Indicate the path of the file to read filenames from instead of the expansion of paths from any positional arguments. "-" can be specified to read filenames from STDIN.

--files-with-matches
--------------------

Flag. If specified with a true value, will only produce the filenames of the files in which the pattern was found. Defaults to `False`.

--find
------

Flag. If specified with a true value, will **not** look at the contents of the selected paths, but instead consider the selected paths as lines in a virtual file.

--follow-symlinks
-----------------

Flag. Indicate whether symbolic links to directories should be followed. Defaults to `False`.

--group-matches
---------------

Flag. Indicate whether matches of a file should be grouped together by mentioning the filename only once (instead of on every line). Defaults to `True` if `--human` is (implicitly) set to `True`, else defaults to `False`.

--highlight
-----------

Flag. Indicate whether the pattern should be highlighted in the line in which it was found. Defaults to `True` if `--human` is (implicitly) set to `True`, else defaults to `False`.

--help [area-of-interest]
-------------------------

Show argument documentation, possibly extended by giving the area of interest, which are:

  * pattern

  * string

  * code

  * input

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

Flag. Indicate that search results should be presented in a human readable manner. This means: filenames shown on a separate line, line numbers shown, and highlighting performed. Defaults to `True` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `False`.

--json-per-file
---------------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a `True` value, indicates that each selected file will be interpreted as JSON, and if valid, will then be given to the pattern for introspection. If the Callable returns a true value, the filename will be shown. If the returned value is a string, that string will also be mentioned. For example:

```bash
$ rak '{ $_ with .<auth> }' --json-per-file
```

--json-per-line
---------------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a `True` value, indicates that each line from the selected files will be interpreted as JSON, and if valid, will then be given to the pattern for introspection. If the Callable returns a true value, the filename and line number will be shown. If the returned value is a string, that string will also be mentioned. For example:

```bash
$ rak '{ $_ with .<auth> }' --json-per-line
```

--known-extensions
------------------

Flag. Indicate that only files with known extensions (occuring in any of the `#groups`) should be searched. Defaults to `True` if a human is watching.

--list-custom-options
---------------------

```bash
$ rak --list-custom-options
fs: --'follow-symlinks'
im: --ignorecase --ignoremark
```

Flag. If specified with a true value and as the only option, will list all additional options previously saved with `--save`.

--list-expanded-options
-----------------------

```bash
$ rak --im --list-expanded-options
--ignorecase --ignoremark
```

If specified with a true value, will show all actual options being activated after having been recursively expanded, and then exit. Intended as a debugging aid if you have many custom options defined.

--modify-files
--------------

Flag. Only makes sense if the specified pattern is a `Callable`. Indicates whether the output of the pattern should be applied to the file in which it was found. Defaults to `False`.

The `Callable` will be called for each line, giving the line (**including** its line ending). It is then up to the `Callable` to return:

### False

Remove this line from the file. NOTE: this means the exact `False` value.

### True

Keep this line unchanged the file. NOTE: this means the exact `True` value.

### Empty

Keep this line unchanged the file. NOTE: this means the exact `Empty` value. This is typically returned as the result of a failed condition. For example, only change the string "foo" into "bar" if the line starts with "#":

```bash
$ rak '{ .subst("foo","bar") if .starts-with("#") }' --modify-files
```

### any other value

Inserts this value in the file instead of the given line. The value can either be a string, or a list of strings.

--module=foo
------------

Indicate the Raku module that should be loaded. Only makes sense if the pattern is executable code.

--only-matching
---------------

Flag. Indicate whether only the matched pattern should be produced, rather than the line in which the pattern was found. Defaults to `False`.

--output-file=filename
----------------------

Indicate the path of the file in which the result of the search should be placed. Defaults to `STDOUT`.

--pager
-------

Indicate the name (and arguments) of a pager program to be used to page through the generated output. Defaults to the `RAK_PAGER` environment variable. If that isn't specified either, then no pager program will be run.

```bash
$ RAK_PAGER='more -r' rak foo

$ rak foo --pager='less -r'
```

--paragraph-context
-------------------

Flag. Indicate all lines that are part of the same paragraph **around** any line that matches. Defaults to `False`.

--passthru
----------

Flag. Indicate whether **all** lines from source should be shown, even if they do **not** match the pattern. Highlighting will still be performed, if so (implicitely) specified.

```bash
# Watch a log file, and highlight a certain IP address.
$ tail -f ~/access.log | rak --passthru 123.45.67.89
```

--paths-from=filename
---------------------

Indicate the path of the file to read path specifications from instead of from any positional arguments. "-" can be specified to read path specifications from STDIN.

--pattern=foo
-------------

Alternative way to specify the pattern to search for. If (implicitly) specified, will assume the first positional parameter specified is actually a path specification, rather than a pattern. This allows the pattern to be searched for to be saved with `--save`.

--quietly
---------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a true value, will catch all **warnings** that are emitted when executing the pattern's `Callable`. Defaults to False.

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

--save=shortcut-name
--------------------

Save all options with the given name in the configuration file (`~/.rak-config.json`), and exit with a message that these options have been saved with the given name.

This feature can used to both create shortcuts for specific (long) options, or just as a convenient way to combine often used options.

```bash
$ rak --ignorecase --ignoremark --save=im
Saved option '--im' as: --ignorecase --ignoremark

# same as --ignorecase --ignoremark
$ rak foo --im

$ rak --follow-symlinks --save=fs
Saved option '--fs' as: --follow-symlinks

$ rak --break='[---]' --save=B
Saved option '--B' as: --break='[---]'

$ rak --pattern=! --save=P
Saved option '--P' as: --pattern='!'

$ rak --save=foo
Removed configuration for 'foo'
```

Any options can be accessed as if it is a standard option. Please note that no validity checking on the options is being performed at the moment of saving, as validity may depend on other options having been specified.

One option can be marked as requiring a value to be specified (with "!") or have a default value (with "[default-value]").

To remove a saved set of options, use `--save` as the only option.

--show-blame
------------

Flag. Indicate whether to show `git blame` information for matching lines if possible, instead of just the line. Defaults to `False`.

--show-filename
---------------

Flag. Indicate whether filenames should be shown. Defaults to `True` if `--human` is (implicitly) set to `True`, else defaults to `False`.

--show-line-number
------------------

Flag. Indicate whether line numbers should be shown. Defaults to `True` if `--human` is (implicitly) set to `True` and <-h> is **not** set to `True`, else defaults to `False`.

--silently[=out,err]
====================

Flag and option. Only applicable if the pattern is a `Callable`. Indicates whether any output from the `Callable` pattern should be caught. Defaults to `False`. If specified as a flag, will catch both STDOUT as well as STDERR output from the pattern's execution. When specified as an option, will accept:

  * out - only capture STDOUT

  * err - only capture STDERR

  * out,err - capture both STDIN as well as STDERR

  * err,out - capture both STDIN as well as STDERR

--smartcase
-----------

Flag. An intelligent version of `--ignorecase`. If the pattern does **not** contain any uppercase characters, it will act as if `--ignorecase` was specified. Otherwise it is ignored.

--summary-if-larger-than=N
--------------------------

Indicate the maximum size a line may have before it will be summarized. Defaults to `160` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `Inf` effectively (indicating no summarization will ever occur).

  * --type[=words|starts-with|ends-with|contains]

Only makes sense if the pattern is a string. With `words` specified, will look for pattern as a word in a line, with `starts-with` will look for the pattern at the beginning of a line, with `ends-with` will look for the pattern at the end of a line, with `contains` will look for the pattern at any position in a line.

--trim
------

Flag. Indicate whether lines that have the pattern, should have any whitespace at the start and/or end of the line removed. Defaults to `True` if no context for lines was specified, else defaults to `False`.

--version
---------

Flag. If the only argument, shows the name and version of the script, and the system it is running on.

--vimgrep
---------

Flag. If specified with a true value, will output search results in the format "filename:linenumber:column:line". This allows integration with the `:grep` action in vim-like editors.

IN APPLICATION USAGE
====================

You can also load the `App::Rak` module as a module in your own application. It will then export a `rak` subroutine with the same arguments as the `rak` command line interface.

This is still a bit experimental.

Currently calling this subroutine will return `Nil`. That may change in the future.

```raku
use App::Rak;

rak 'foo';  # look for "foo" in all files with known extensions from "."
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

