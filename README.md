[![Actions Status](https://github.com/lizmat/App-Rak/actions/workflows/linux.yml/badge.svg)](https://github.com/lizmat/App-Rak/actions) [![Actions Status](https://github.com/lizmat/App-Rak/actions/workflows/macos.yml/badge.svg)](https://github.com/lizmat/App-Rak/actions) [![Actions Status](https://github.com/lizmat/App-Rak/actions/workflows/windows.yml/badge.svg)](https://github.com/lizmat/App-Rak/actions)

NAME
====

App::Rak - 21st century grep / find / ack / ag / rg on steroids

SYNOPSIS
========

```bash
# look for "foo" in current directory recursively
$ rak foo

# look for "foo" in directory "bar" recursively
$ rak foo bar

# look for "foo" as word in current directory
$ rak '/ << foo >> /'

# look for "foo", only produce filenames
$ rak foo --files-with-matches

# also produce 2 lines before and after
$ rak foo --before=2 --after=2

# lines with foo AND bar
$ rak '{.contains("foo") && .contains("bar")}'
```

DESCRIPTION
===========

App::Rak provides a CLI called `rak` that allows you to look for a pattern in (a selection of files) from one or more directories recursively. It has been modelled after utilities such as `grep`, `ack`, `ag` and `rg`, with a little bit of `find` mixed in, and `-n` and `-p` parameters of many programming languages.

Note: this project is now in beta-development phase. Comments, suggestions and bug reports continue to be more than welcome!

POSITIONAL ARGUMENTS
====================

pattern
-------

The pattern to search for.

Can also be specified with the `--pattern` option, in which case **all** the positional arguments are considered to be a path specification.

Patterns will be interpreted in the following ways if **no** `--type` has been specified, or `--type=auto` has been specified.

Multiple patterns, stored in a file or read from STDIN, can also be specified with the <C--patterns-from> argument.

### / regex /

If the pattern starts and ends with `/`, then it indicates a Raku [regex](https://docs.raku.org/language/regexes). **No** special processing of the given string between slashes will be done: the given pattern will be parsed as a regex verbatim. During the search process, each item will be matched against this regex. Any `--ignorecase` or `--ignoremark` arguments will be honoured.

### { code }

If the pattern starts with `{` and ends with `}`, then it indicates Raku code to be executed. **No** special processing of the given string between the curly braces will be done: the given code will be compiled as Raku code. During the search process, this code will be run for each item, available in `$_`. To facilitate the use of libraries that wish to access that topic, it is also available as the `$*_` dynamic variable.

The dynamic variable `$*SOURCE` will contain the `IO::Path` object of the file being processed. Note that the Raku code will be called in a thread **unsafe** manner.

The dynamic variable `$*_` will contain the topic with which the code was called. This to allow custom libraries to easily obtain the topic without the user needing to specify that again.

### *code

If the pattern starts with `*`, then this is a short way of specifying Raku code as a pattern, using [Whatever-currying](https://docs.raku.org/type/Whatever#index-entry-Whatever-currying). Otherwise the same as `{ code }`.

### jp:path

If the pattern start with 'jp:', then interpret the rest of the pattern as a `JSON path`. Only makes sense when used together with `--json-per-file`, `--json-per-line` or `--json-per-elem`. Requires that the [`JSON::Path`](https://raku.land/cpan:JNTHN/JSON::Path) module is installed. Basically a shortcut to specifying `path --type=json-path`.

<table class="pod-table">
<caption>Supported JSON path syntax</caption>
<thead><tr>
<th>expression</th> <th>meaning</th>
</tr></thead>
<tbody>
<tr> <td>$</td> <td>root node</td> </tr> <tr> <td>.key</td> <td>index hash key</td> </tr> <tr> <td>[&#39;key&#39;]</td> <td>index hash key</td> </tr> <tr> <td>[2]</td> <td>index array element</td> </tr> <tr> <td>[0,1]</td> <td>index array slice</td> </tr> <tr> <td>[4:5]</td> <td>index array range</td> </tr> <tr> <td>[:5]</td> <td>index from the beginning</td> </tr> <tr> <td>[-3:]</td> <td>index to the end</td> </tr> <tr> <td>.*</td> <td>index all elements</td> </tr> <tr> <td>[*]</td> <td>index all elements</td> </tr> <tr> <td>[?(expr)]</td> <td>filter on Raku expression</td> </tr> <tr> <td>..key</td> <td>search all descendants for hash key</td> </tr>
</tbody>
</table>

A query that is not rooted from $ or specified using .. will be evaluated from the document root (that is, same as an explicit $ at the start).

#### Full Raku support

The `jp:path` and `--type=json-path` syntax are actually syntactic sugar for calling a dedicated `jp` macro that takes an unquoted JSON path as its argument, and returns an instantiated `JP` object.

This means that:

```bash
$ rak --json-per-file jp:foo
$ rak --json-per-file --type=json-path foo
```

are a different way of saying:

```bash
$ rak --json-per-file '{ jp(path).Slip }'
```

using the "pattern is Raku code" syntax.

The following methods can be called on the `JP` object:

<table class="pod-table">
<thead><tr>
<th>method</th> <th>selected</th>
</tr></thead>
<tbody>
<tr> <td>.value</td> <td>The first selected value.</td> </tr> <tr> <td>.values</td> <td>All selected values as a Seq.</td> </tr> <tr> <td>.paths</td> <td>The paths of all selected values as a Seq.</td> </tr> <tr> <td>.paths-and-values</td> <td>Interleaved selected paths and values.</td> </tr>
</tbody>
</table>

Without listing all of the methods that can be called on the `JP` object, one should note that all efforts have been made to make the `JP` object act like a `Seq`.

### ^string

If the pattern starts with `^`, then it indicates the string should be at the **start** of each item. Basically a shortcut to specifying `string --type=starts-with`. Any `--smartcase`, `--smartmark`, `--ignorecase` or `--ignoremark` arguments will be honoured.

### string$

If the pattern ends with `$`, then it indicates the string should be at the **end** of each item. Basically a shortcut to specifying `string --type=ends-with`. Any `--smartcase`, `--smartmark`, `--ignorecase` or `--ignoremark` arguments will be honoured.

### ^string$

If the pattern starts with `^` and ends with `$`, then it indicates that the string should be equal to the item. Basically a shortcut to specifying `string --type=equal`. Any `--smartcase`, `--ignorecase` or `--ignoremark` arguments will be honoured.

### §string

If the pattern starts with `§`, then it indicates that the string should occur as a word (with word-boundaris on both ends) in the item. Basically a shortcut to specifying `string --type=words`. Any `--smartcase`, `--smartmark`, `--ignorecase` or `--ignoremark` arguments will be honoured.

### string

If there are no special start or end markers, then it indicates that the string should occur somewhere in the item. Basically a shortcut to specifying `string --type=contains`. Any `--smartcase`, `--smartmark`, `--ignorecase` or `--ignoremark` arguments will be honoured.

path(s)
-------

Optional. Either indicates the path of the directory (and its sub-directories), or the file that will be searched, or a URL that will produce a file to be searched. By default, all directories that do not start with a period, and which are not symbolic links, will be recursed into (but this can be changed with the `--dir` option).

By default, all files with known extensions will be searched in the directories. This can be changed with the `--file` option, or specialized version of that like `--extensions`.

Paths can also be specified with the `--paths` option, in which case there should only be a positional argument for the pattern, or none if `--pattern` option was used for the pattern specification.

ON CALLABLES AS PATTERN
=======================

`Callables` can be specified by a string starting with `*.` (so-called [Whatever currying](https://docs.raku.org/type/Whatever), or as a string starting with `{` and ending with `}`.

Note that if a `Callable` is specified as a pattern, then no highlighting can be performed as it cannot signal why or where a match occurred.

The return value of the pattern `Callable` match is interpreted in the following way:

True
----

If the `Bool`ean True value is returned, assume the pattern is found. Produce the item unless `--invert-match` was specified.

False
-----

If the `Bool`ean False value is returned, assume the pattern is **not** found. Do **not** produce the item unless `--invert-match` was specified.

Nil
---

If `Nil` is returned, assume the pattern is **not** found.

This typically happens when a `try` is used in a pattern, and an execution error occurred. Do **not** produce the item unless `--invert-match` was specified.

Empty
-----

If the empty `Slip` is returned, assume the pattern is **not** found. Do **not** produce the item unless `--invert-match` was specified. Shown in stats as a `passthru`.

any other Slip
--------------

If a non-empty `Slip` is returned, produce the values of the `Slip` separately for the given item (each with the same item number).

### any other value

Produce that value.

PHASERS IN CALLABLE PATTERNS
============================

The Raku Programming Language has a number of unique features that can be used with patterns that are executable code. One of them is the use of so-called [phasers](https://docs.raku.org/language/phasers) (pieces of code that will be executed automatically when a certain condition has been met.

`App::Rak` currently supports all of Raku's [loop phasers](https://docs.raku.org/language/phasers#FIRST):

<table class="pod-table">
<thead><tr>
<th>phaser</th> <th>event</th>
</tr></thead>
<tbody>
<tr> <td>FIRST</td> <td>code to run when searching starts</td> </tr> <tr> <td>NEXT</td> <td>code to run when searching a file is done</td> </tr> <tr> <td>LAST</td> <td>code to run when searching is done</td> </tr>
</tbody>
</table>

These phasers will be called in a **thread-safe** manner.

```bash
# show number of files inspected before the search result
$ rak '{ state $s = 0; NEXT $s++; LAST say "$s files"; .contains("foo")}'

# show number of files inspected after of the search result
$ rak '{ state $s = 0; NEXT $s++; END say "$s files"; .contains("foo")}'
```

Note that the use of the `LAST` phaser will make the search run eagerly, meaning that no results will be shown until the search has been completed.

Any other phasers that do not require special attention by `App::Rak` are also supported in any code specified (such as `BEGIN` and `END`).

ON THE INTERPRETATION OF OPTIONS
================================

All options when using App::Rak, start with either one dash `-` or two dashes `--`.

If an option starts with two dashes, it is a so-called "long option". Any characters after the dashes are considered to be the single name of the option.

If an option starts with a single dash, then it is considered to be a collection of "short options", each of 1 letter. If the number of short options is 1, then it can be followed by a numerical value (without equal sign).

If the specification of the option does **not** contain an equal sign `=`, then the option is interpreted as a boolean option. By default, such a flag is considered to represent `True`. The value can be negated in two ways:

  * a slash before the name

  * the string "no-" before the name

Some examples:

  * -i

Option "i" is True.

  * -j5

Option "j" has the value 5.

  * -im

Options "i" and "m" are True.

  * -/i

Option "i" is False.

  * -/im

Options "i" and "m" are False.

  * -no-i

Option "i" is False.

  * --foo

Option "foo" is True.

  * --/foo

Option "foo" is False.

  * --no-foo

Option "foo" is False.

If the specification of an option contains an equal sign after the name, then whatever follows that, is considered the value of that option. Whether or not that value needs to be quoted, and how they are to be quoted, really depends on the shell that you use to access `rak`. Generally, if the value consists of alphanumeric characters only, no quoting is necessary. Otherwise it's better to quote your values.

Some examples:

  * -s=foo

Option "s" has the value "foo".

  * -t='foo bar'

Option "t" has the value "foo bar".

  * --frobnicate=yes

Option "frobnicate" has the value "yes".

CREATING YOUR OWN OPTIONS
=========================

App::Rak provides **many** options. If you are happy with a set of options for a certain workflow, You can use the `--save` option to save that set of options and than later access them with the given name:

```bash
# create -i shortcut for ignoring case
$ rak --ignorecase --save=i
Saved option '-i' as: --ignorecase

# create -m shortcut for ignoring accents
$ rak --ignoremark --save=m
Saved option '-m' as: --ignoremark

# same as --ignorecase --ignoremark
$ rak foo -im
```

Generally speaking, the most used boolean options can be saved as single letter options: this allows multiple options to be specified in a single, short manner (as shown above).

To better document / remember what a particular custom option is meant to do, you can add a description with the `--description` option.

```bash
# add a description to the -i custom option
$ rak --description='Search without caring for uppercase' --save=i
Saved '--description='Search without caring for uppercase'' as: -i

# add an option -g for searching only git files
$ rak --description='Committed files only' --under-version-control --save=g
Saved '--description='Committed files only' --under-version-control' as: -g
```

There is also a special named option that indicates the options that will be automatically activated on any invocation: `(default)`.

```bash
# enable --smartcase by default on every run
$ rak --smartcase --save='(default)'
Saved '--smartcase' as: (default)
```

You can use the `--list-custom-options` to see what options you have saved before.

Custom options are saved in `~/.rak-config.json`. You can override this by specifying the `RAK_CONFIG` environment variable.

```bash
# read custom options from ".custom.json" (in the current directory)
$ RAK_CONFIG=.custom.json rak foo
```

You can also use the `RAK_CONFIG` variable to disable loading any configuration by not specifying a value:

```bash
# start rak without any custom configuration
$ RAK_CONFIG= rak foo
```

SUPPORTED OPTIONS
=================

All options are optional. Any unexpected options, will cause an exception to be thrown with the unexpected options listed and possible alternatives mentioned. Unless specifically indicated otherwise, using the negation of a flag has the same effect as **not** specifying it.

--absolute
----------

Flag. If specified indicates that whenever paths are shown, they will be shown as absolute paths. Defaults to `False`, which will cause paths to be produced as paths relative to the current directory.

--accept=code
-------------

Specifies the code that should be executed that should return `True` if the path is acceptable, given an `IO::Path` object of the path. See also `--deny`.

```bash
# Include files that have "use Test" in them
$ rak --accept='*.slurp.contains("use Test")'
```

--accessed=condition
--------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The access time of the file (number of seconds since epoch, as a `Num` value) will be passed as the only argument. Note that many file systems do not actually support this reliably.

See "CHECKING TIMES ON FILES" for more information about features that can be used inside the `Callable`.

--ack
-----

Attempt to interpret following arguments as if they were arguments to the [`ack`](https://metacpan.org/pod/ack) utility. This is incomplete and may be subtly different in behaviour. Intended as a temporary measure for people used to using `ack`, while they train their muscle memory to use **rak** instead.

If you'd like a list of the option configuration, you can use the `--list-custom-options` argument.

```bash
# Show the current mappinng of "ack" to "rak" options
$ rak --ack --list-custom-options
```

--after-context=N
-----------------

Indicate the number of lines that should be shown **after** any line that matches. Defaults to **0**. Will be overridden by a `--context` argument.

--allow-loose-escapes
---------------------

Only applicable if `--csv-per-line` has been specified. Flag. If specified, indicates that **any** character may be escaped.

--allow-loose-quotes
--------------------

Only applicable if `--csv-per-line` has been specified. Flag. If specified, indicates that fields do not need to be quoted to be acceptable.

--allow-whitespace
------------------

Only applicable if `--csv-per-line` has been specified. Flag. If specified, indicates that whitespace is allowed around separators.

--auto-decompress
-----------------

Flag. If specified with a True value, will accept compressed files with the `.gz` (gzip) or `.bz2` (bzip2) extension, if the extension was otherwise acceptable. Will automatically decompress files for inspection.

--auto-diag
-----------

Only applicable if `--csv-per-line` has been specified. Flag. If (implicitly) specified, will show diagnostic information about problems that occurred during parsing of the CSV file. The default is `True`.

--backtrace
-----------

Flag. When specified with a True value, will interpret either standard input, or a single file, as a Raku backtrace. And produce a result containing the lines of source code from that backtrace. Can be used together with `--context`, `--before-context`, `--after-context`, `--edit` and `--vimgrep`. Any pattern specification will only be used for highlighting. If **not** used in combination with `--edit` or `--vimgrep`, will assume a context of 2 lines.

```bash
# look at source of a stacktrace
$ raku script 2>&1 | rak --backtrace

# inspect the source of a stacktrace in an editor
$ raku script 2>&1 | rak --backtrace --edit

# inspect a backtrace stored in a file
$ rak --backtrace filename
```

--backup[=extension]
--------------------

Indicate whether backups should be made of files that are being modified. If specified without extension, the extension `.bak` will be used.

--batch[=N]
-----------

Indicate the number of files that should be checked per thread. If specified as a flag, will assue `1`. Defaults to `64` if not specified. See also <--degree>.

--before-context=N
------------------

Indicate the number of lines that should be shown **before** any line that matches. Defaults to **0**. Will be overridden by a `--context` argument.

--blame-per-file
----------------

Flag. Only makes sense if the pattern is a `Callable`. If specified, indicates that each of the selected files will be provided as [`Git::Blame::File`](https://raku.land/zef:lizmat/Git::Blame::File#methods-on-gitblamefile) objects if `git blame` can be performed on the a selected file. If that is not possible, then the selected file will be ignored.

If <git blame> information can be obtained, then the associated `Git::Blame::File` object will be presented to the pattern `Callable`. If the Callable returns `True`, then the filename will be produced. If anything else is returned, then the stringification of that object will be produced.

Requires the [`Git::Blame::File`](https://raku.land/zef:lizmat/Git::Blame::File) Raku module to be installed.

```bash
# show files with more than 10 commits
$ rak '*.commits > 10' --blame-per-file --files-with-matches
```

--blame-per-line
----------------

Flag. Only makes sense if the pattern is a `Callable`. If specified, indicates that each line from the selected files will be provided as [`Git::Blame::Line`](https://raku.land/zef:lizmat/Git::Blame::File#accessors-on-gitblameline) objects if `git blame` can be performed on the a selected file. If that is not possible, then the selected file will be ignored.

If <git blame> information can be obtained, then the associated `Git::Blame::Line` object will be presented to the pattern `Callable`. If the Callable returns `True`, then the short representation of the `git blame` information will be produced. If the returned value is anything else, then the stringification of that object will be produced.

```bash
# show git blame on lines of which the author is "Scooby Doo"
$ rak '{ .author eq "Scooby Doo" }' --blame-per-line
```

Requires that the [`Git::Blame::File`](https://raku.land/zef:lizmat/Git::Blame::File) module is installed.

--blocks=condition
------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The number of logical blocks that a file takes up in the filesystem, will be passed as the only argument.

```bash
# show files that consist of at least 3 blocks
$ rak --find --blocks='* >= 3'
```

--break[=string]
----------------

Indicate whether there should be a visible division between matches of different files. Can also be specified as a string to be used as the divider. Defaults to `True` (using an empty line as a divider) if `--group-matches` is (implicitly) set to `True`, else defaults to `False`.

--checkout=branch
-----------------

Only valid if the current directory is under git version control. Indicate the branch to checkout by the general matching logic of App::Rak. Will produce listing of matching branches if more than one, or say that there is no match. Branches need not have been checked out locally yet.

--categorize=categorizer
------------------------

If specified, indicates the `Callable` that should return zero or more keys for a given line to have it categorized. This effectively replaces the filename if a line by its key in the result. See also `--classify`.

```bash
# categorize by the first two letters of a line
$ rak --categorize='*.substr(0,2).comb'
```

--classify=classifier
---------------------

If specified, indicates the `Callable` that should return a key for a given line to have it classified. This effectively replaces the filename if a line by its key in the result. See also `--categorize`.

```bash
# classify by the last letter of a line
$ rak --classify='*.substr(*-1)'
```

--context=N
-----------

Indicate the number of lines that should be produced **around** any line that matches. Defaults to **0**.

--count-only
------------

Flag. Indicate whether just the number of lines with matches should be calculated. When specified with a `True` value, will show a "N matches in M files" by default, and if the `:files-with-matches` (or `files-without matches`) option is also specified with a `True` value, will just show total counts. See also `--stats-only`.

--created=condition
-------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The creation time of the file (number of seconds since epoch, as a `Num` value) will be passed as the only argument.

See "CHECKING TIMES ON FILES" for more information about features that can be used inside the `Callable`.

--csv-per-line
--------------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a `True` value, indicates that selected files should be interpreted as comma separated values (CSV). Each row from the selected files will be provided as a list of strings (or as `CSV::Field` objects if `--keep-meta` was specified).

Attempt to interpret file as a CSV file, and pass each row as a List to to the pattern Callable. Only files with extensions from the `#csv` group will be tried, unless overridden by any explicit extension specification.

More documentation can be found with the [Text::CSV](https://raku.land/github:Tux/Text::CSV) module itself.

```bash
# Show the values of the column named "foo" of the rows in the "info.csv"
# file if the column named "bar" is equal to "foo"
$ rak --csv-per-line '{.<foo> if .<bar> eq "foo"}' info.csv

# Show the values of the first column of the rows in the "info.csv" file
# if the second column is equal to "foo"
$ rak --csv-per-line --/headers '{.[0] if .[1] eq "foo"}' info.csv
```

--degree[=N | code]
-------------------

Indicate the number of worker threads that should be maximally. Defaults to the number of cores minus 1 if not specified. Assumes `1` if specified as a flag. Can also take a `Callable` specification, in which case the number of CPU cores will be presented to that Callable as the only argument. See also <--batch>.

--deny=code
-----------

Specifies the code that should be executed that should return `True` if the path is **NOT** acceptable, given an `IO::Path` object of the path. See also `--accept`.

```bash
# Include files that **NOT** have "use Test" in them
$ rak --deny='*.slurp.contains("use Test")'
```

--description=text
------------------

Specify a description to be saved with the custom option. This will be shown prominently with --list-custom-options. If it is the only argument apart from --save, then the discription will be added (if there was no description yet) or replace the current description of the option.

--device-number=condition
-------------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The device number of the filesystem on which the file is located, will be passed as the only argument.

--dir=condition
---------------

If specified, indicates the `Callable` that should return True to have a directory be included for further recursions in file selection. The basename of the directory will be passed as the only argument. Defaults to all directories that do not start with a period. Can specify as a flag to include **all** directories for recursion.

--dont-catch
------------

Flag. If specified as a flag, will **not** catch any error during processing, but will throw any error again. Defaults to `False`, making sure that errors **will** be caught. Mainly intended for debugging and error reporting.

--dryrun
--------

Flag. Indicate to **not** actually make any changes to any content modification if specified with a `True` value. Only makes sense together with the `--modify-files` and the `--rename-files` option.

--ecosystem[=name1,name2]
-------------------------

Intended to be used by Raku ecosystem maintainers. Indicates the name of zero or more Raku ecosystems of which to inspect the `META6.json` information of all its modules. Currently supported names are:

<table class="pod-table">
<thead><tr>
<th>name</th> <th>description</th>
</tr></thead>
<tbody>
<tr> <td>p6c</td> <td>the original git ecosystem (deprecated)</td> </tr> <tr> <td>cpan</td> <td>the ecosystem piggybacking on PAUSE / CPAN (deprecated)</td> </tr> <tr> <td>fez</td> <td>the currently recommended ecosystem for new modules / updates</td> </tr> <tr> <td>rea</td> <td>the Raku Ecosystem Archive</td> </tr>
</tbody>
</table>

Defaults to `rea` if specified as a flag. Implies `--json-per-elem`.

```bash
# show all unique module names by an author from the REA
$ rak '{ .author eq "Scooby Doo" }' --ecosystem

# same, but now from the p6c and cpan ecosystems
$ rak '{ .author eq "Scooby Doo" }' --ecosystem=p6c,cpan
```

Assumes `zef` is installed and its meta information is available.

--edit[=editor]
---------------

Indicate whether the patterns found should be fed into an editor for inspection and/or changes. Defaults to `False`. Optionally takes the name of the editor to be used. If no editor is specified, will use what is in the `EDITOR` environment variable. If that is not specified either, will call "vim".

Requires the [`Edit::Files`](https://raku.land/zef:lizmat/Edit::Files) Raku module to be installed.

--encoding[=utf8-c8]
--------------------

Indicate the encoding to be used when reading text files. Defaults to [`utf8-c8`](https://docs.raku.org/language/unicode#UTF8-C8). Other encodings are e.g. `utf8` and `ascii`.

--eol=lf|cr|crlf
----------------

Only applicable if `--csv-per-line` has been specified. Indicate a line ending different from the standard line ending assumed by the system. Can be specified as `lf` for a single LineFeed character, `cr` for a single CarriageReturn character, or `crlf` for a combination of a CarriageReturn and a LineFeed character.

--escape=char
-------------

Only applicable if `--csv-per-line` has been specified. Indicates the escape character to be used to escape characters in a field. Defaults to **double quote**.

--exec=invocation
-----------------

If specified, indicates the name of a program and its arguments to be executed. Any `$_` in the invocation string will be replaced by the file being checked. The file will be included if the program runs to a successful conclusion.

--execute-raku[=code]
---------------------

Flag or code specification. When specified with a True value, will use the pattern as the name of a script to execute. If code is specified will execute that code. If the code consists of "-", then will read code from STDIN to execute. Any execution error's backtrace will be used to produce a result with the lines of source code of that backtrace.

Can be used together with `--context`, `--before-context`, `--after-context`, `--edit` and `--vimgrep`. Will assume a context of 2 lines if **not** used in combination with `--edit` or `--vimgrep`,

If `--verbose` is specified, will try to create an extended (--ll-exception) backtrace.

```bash
# look at source of a stacktrace after running script
$ rak --execute-raku script

# inspect the source of a stacktrace in an editor
$ rak --execute-raku script --edit

# inspect a backtrace from execution of code read from STDIN
$ cat script | rak --execute-raku=-
```

--extensions=spec
-----------------

Indicate the extensions of the filenames that should be inspected. By default, only files with known extensions, will be searched.

Extensions can be specified as a comma-separated list of either a a predefined group of extensions (indicated by `#name`), a single extension, or `*` to indicate all known extensions.

```bash
# inspect files with extensions used by Raku and Perl
$ rak foo --extensions=#raku,#perl

# inspect files with presumable Markdown content
$ rak foo --extensions=md,markdown

# inspect files without extension
$ rak foo --extensions=

# inspect files without extension or with the extension "foo"
$ rak foo --extensions=,foo
```

Predefined groups are `#raku`, `#perl`, `#cro`, `#text`, `#c`, `#c++`, `#yaml`, `#ruby`, `#python`, `#r`, `#wl`, `#html`, `#markdown`, `#js`, `#json`, `#jsonl`, `#csv`, `#config`, and `#text`.

The `--list-known-extensions` argument can be used to see which predefined groups of extensions are supported, and which extensions they cover.

--file=condition
----------------

If specified, indicates the `Callable` that should return True to have a file be included in the file selection process. The basename of the file will be passed as the only argument. Defaults to `True`, indicating that all files should be included.

If `--/file` is specified, then only directory paths will be accepted. This only makes sense if `--find` is also specified.

--file-separator-null
---------------------

Flag. Indicate to separate filenames by null bytes rather than newlines if the `--files-with-matches` or `--files-without-matches` option are specified with a `True` value.

--files-from=filename
---------------------

Indicate the path of the file to read filenames from instead of the expansion of paths from any positional arguments. "-" can be specified to read filenames from STDIN.

--files-with-matches
--------------------

Flag. If specified, will only produce the filenames of the files in which the pattern was found. Defaults to `False`.

--files-without-matches
-----------------------

Flag. If specified, will only produce the filenames of the files in which the pattern was **not** found. Defaults to `False`.

--filesize=condition
--------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The number of bytes of data in the file, will be passed as the only argument. See also `--is-empty`.

```bash
# show files that consist of at 30 bytes
$ rak --find --filesize='* >= 30'
```

--find
------

Flag. If specified, will **not** look at the contents of the selected paths, but instead consider the selected paths as lines in a virtual file. And as such will always only produce filenames.

--only-first[=N]
----------------

Indicate the **overall** number of matches to show. If specified without a value, will default to **1**. Defaults to **1000** if a human is watching, otherwise defaults to returning all possible matches. Can be used to tweak search results, before letting it loose to get all possible results.

Special values that are allowed to produce all possible results are `∞` (aka `221E INFINITY`), `*` and `Inf`.

--formula=[none]
----------------

Only applicable if `--csv-per-line` has been specified. If specified, indicates the action to be taken when a field starts with an equal sign (indicating a formula of some kind in many spreadsheets). The following values are recognized:

<table class="pod-table">
<thead><tr>
<th>type</th> <th>action</th>
</tr></thead>
<tbody>
<tr> <td>none</td> <td>take not action, just pass on</td> </tr> <tr> <td>die</td> <td>throw an exception</td> </tr> <tr> <td>diag</td> <td>report line and position where formula was found</td> </tr> <tr> <td>empty</td> <td>replace the formula by an empty string</td> </tr>
</tbody>
</table>

--frequencies
-------------

Flag. If specified, will produce a frequency table of the matches with the most frequent match first. Default is `False`. See also `--unique`. Usually used in conjunction with `--matches-only` and/or `Callable` patterns returning something other than True/False/Nil/Empty.

--gid=condition
---------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The numeric `gid` of the file will be passed as the only argument. Can also be specified as a single numeric argument. See also `--group`.

```bash
# show files of which the numeric group id is greater than 20
$ rak --find --gid='* > 20'

# show files of which the numeric group id is 20
$ rak --find --gid=20
```

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--group=condition
-----------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The name of the group associated with the `gid` of the file will be passed as the only argument.

Can also be specified as a list of comma separated names to (not) select on. To select all names **except** the listed named, prefix with a `!`.

See also `--gid`. Requires the [P5getgrnam](https://raku.land/zef:lizmat/P5getgrnam) module to be installed.

```bash
# files of which the name associated with the user id starts with underscore
$ rak --find --group='*.starts-with("_")'

# show files of which the group is "staff"
$ rak --find --group=staff

# show files of which the group is NOT "staff"
$ rak --find --group='!staff'
```

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--group-matches
---------------

Flag. Indicate whether matches of a file should be grouped together by mentioning the filename only once (instead of on every line). Defaults to `True` if a human is watching, else `False`.

--hard-links=condition
----------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The number of hard-links to the file on the filesystem, will be passed as the only argument.

--has-setgid
------------

Flag. If specified, will only select files that do have the SETGID bit set in their attributes. Use negation `--/has-setgid` to only select files that do **not** have the SETGID bit set.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--has-setuid
------------

Flag. If specified, will only select files that do have the SETUID bit set in their attributes. Use negation `--/has-setuid` to only select files that do **not** have the SETUID bit set.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--headers
---------

Only applicable when `--csv-per-line` is also specified. It defaults to "auto". It can have the following values:

  * --headers

Boolean True, same as "auto"

  * --/headers

Boolean False, assume comma separator and no header line, produce a list of column values for each line in the CSV file.

  * --headers=auto

Automatically determine separator, first line is header with column names, produce a hash with values keyed to column names for each line in the CSV file.

  * --headers=skip

Assume the first line is a header, but skip it. Produce a lust of column values for each line in the CSV file.

  * --headers=uc

Same as "auto", but uppercase the column names found in the header line.

  * --headers=lc

Same as "auto", but lowercase the column names found in the header line.

  * --headers='<a b c>'

Specifies a list of column names to associate with columns, in order. Assumes no header line is available.

  * --headers=':a<foo>, :b<bar>'

Indicates a list of `Pair`s with column name mapping to use instead of the column names found in the header line of the CSV file.

  * --headers='code'

Any Raku code that produces one of the above values. Also supports a `Map` or `Hash` instead of a list of `Pair`s.

```bash
# Use uppercase column names
$ rak --csv-per-line --headers=uc '{.<FOO> if .<BAR> eq "foo"}' info.csv

# Use alternate column names in order of columns
$ rak --csv-per-line --headers='<a b>' '{.<a> if .<n> eq "foo"}' info.csv

# Use alternate column names using mapping
$ rak --csv-per-line --headers=':foo<a>, :bar<b>' '{.<a> if .<n> eq "foo"}' info.csv
```

--help[=area-of-interest]
-------------------------

Show argument documentation, possibly extended by giving the area of interest, which are:

  * argument

  * code

  * content

  * debugging

  * examples

  * faq

  * filesystem

  * general

  * haystack

  * item

  * listing

  * option

  * pattern

  * philosophy

  * resource

  * result

  * special

  * string

If no area of interest is given, then the overview will be shown.

Any pattern specification will be used to search the help subjects, and only show the logical paragraphs with matches.

--highlight
-----------

Flag. Indicate whether the pattern should be highlighted in the line in which it was found. Defaults to `True` if a human is watching (aka STDOUT connected to a terminal), or `--highlight-before` or `highlight-after` are explicitely specified, or `False` otherwise.

--highlight--after[=string]
---------------------------

Indicate the string that should be used at the end of the pattern found in a line. Specifying implies `--highlight`ing implicitely. If `--highlight` or `--highlight-before` are explicitely specified, will default to whatever is specified with `--highlight-before`, or to the ANSI code to end **bold**.

--highlight--before[=string]
----------------------------

Indicate the string that should be used at the end of the pattern found in a line. Specifying implies `--highlight`ing implicitly. If `highlight` is explicitely specified, will default to the terminal code to start **bold**.

--human
-------

Flag. Indicate that search results should be presented in a human readable manner. This means: filenames shown on a separate line, line numbers shown, and highlighting performed. Defaults to `True` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `False`.

--ignorecase
------------

Flag. If specified, indicates that any matching using a literal string or a regex, should be done case insensitively. Default is `False`.

--ignoremark
------------

Flag. If specified, indicates that any matching using a literal string or a regex, should be done without consideration of any accents. Default is `False`.

--inode=condition
-----------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The inode number of the file on the filesystem, will be passed as the only argument.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--invert-match
--------------

Flag. If specified, will negate the result of any match if it has a logical meaning:

<table class="pod-table">
<thead><tr>
<th>given value</th> <th>result</th>
</tr></thead>
<tbody>
<tr> <td>True</td> <td>False</td> </tr> <tr> <td>False</td> <td>True</td> </tr> <tr> <td>Nil</td> <td>True</td> </tr> <tr> <td>Empty</td> <td>True</td> </tr> <tr> <td>none of the above</td> <td>just that</td> </tr>
</tbody>
</table>

--is-empty
----------

Flag. If specified, will only select files that do not contain any data. Use negation `--/is-empty` to only select files that **do** contain data. Special case of `--filesize`.

--is-executable
---------------

Flag. If specified, will only select files that can be executed by the current user. Use negation `--/is-executable` to only select files that are **not** executable by the current user.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-group-executable
---------------------

Flag. If specified, will only select files that can be executed by members of the group of the owner. Use negation `--/is-group-executable` to only select files that are **not** executable by the members of the group of the owner.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-group-readable
-------------------

Flag. If specified, will only select files that can be read by members of the group of the owner. Use negation `--/is-group-readable` to only select files that are **not** readable by the members of the group of the owner.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-group-writable
-------------------

Flag. If specified, will only select files that can be written to by members of the group of the owner. Use negation `--/is-group-writable` to only select files that are **not** writable by the members of the group of the owner.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-moarvm
-----------

Flag. If specified, will only select files that appear to be MoarVM bytecode files. Defaults to `False`.

--is-owned-by-group
-------------------

Flag. If specified, will only select files that are owned by the group of the current user. Use negation `--/is-owned-by-group` to only select files that are **not** owned by the group of the current user.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-owned-by-user
------------------

Flag. If specified, will only select files that are owned by current user. Use negation `--/is-owned-by-user` to only select files that are **not** owned by the current user.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-owner-executable
---------------------

Flag. If specified, will only select files that can be executed by the owner. Use negation `--/is-owner-executable` to only select files that are **not** executable by the owner.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-owner-readable
-------------------

Flag. If specified, will only select files that can be read by the owner. Use negation `--/is-owner-readable` to only select files that are **not** readable by the owner.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-owner-writable
-------------------

Flag. If specified, will only select files that can be written to by the owner. Use negation `--/is-owner-writable` to only select files that are **not** writable by the owner.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-pdf
--------

Flag. If specified, will only select files that appear to be PDF files. Defaults to `False`.

--is-readable
-------------

Flag. If specified, will only select files that can be read by the current user. Use negation `--/is-readable` to only select files that are **not** readable by the current user.

--is-sticky
-----------

Flag. If specified, will only select files that do have the STICKY bit set in their attributes. Use negation `--/is-sticky` to only select files that do **not** have the STICKY bit set.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-symbolic-link
------------------

Flag. If specified, will only select files that are symbolic links. Use negation `--/is-symbolic-link` to only select files that are **not** symbolic links.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-text
---------

Flag. If specified, will only select files that appear to contain text (rather than binary data). Defaults to `True` if no other file filter has been specified, and `--find` is not specified either. Use negation `--/is-text` to only select files with binary data.

Note: support for searching for binary data is not yet implemented, so `--/is-text` can only be used in conjunction with --find.

--is-world-executable
---------------------

Flag. If specified, will only select files that can be executed by anybody. Use negation `--/is-group-executable` to only select files that are **not** executable by anybody.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-world-readable
-------------------

Flag. If specified, will only select files that can be read by anybody. Use negation `--/is-world-readable` to only select files that are **not** readable by anybody.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-world-writable
-------------------

Flag. If specified, will only select files that can be written to by anybody. Use negation `--/is-world-writable` to only select files that can **not** be written to by anybody.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--is-writable
-------------

Flag. If specified, will only select files that can be written to by the current user. Use negation `--/is-writable` to only select files that can **not** be written to by the current user.

--json-per-elem
---------------

Flag. Only makes sense if the pattern is a `Callable`. If specified, indicates that each selected file will be interpreted as JSON, and if valid, will then produce all elements of the outermost data structure to the pattern for introspection. If the data structure is a hash, then key/value `Pair`s will be produced.

If the Callable returns `True`, the stringification of the element will be produced. If the returned value is a string, that string will be produced. For example when searching the list of modules in the zef ecosystem (which consists of an array of hashes):

```bash
# Show all defined "auth" values of top elemens in JSON file
$ rak '{ .<auth> // False }' META.json --json-per-elem
```

--json-per-file
---------------

Flag. Only makes sense if the pattern is a `Callable`. If specified, indicates that each selected file will be interpreted as JSON, and if valid, will then be given to the pattern for introspection. If the Callable returns `True`, the filename will be produced. If anything else is returned, then the stringification of that object will be produced. For example:

```bash
# show the "auth" value from all JSON files
$ rak '*<auth> // False' --json-per-file
```

--json-per-line
---------------

Flag. Only makes sense if the pattern is a `Callable`. If specified, indicates that each line from the selected files will be interpreted as JSON, and if valid, will then be given to the pattern for introspection. If the Callable returns `True`, the filename and line number will be produced. If the returned value is anything else, then the stringification of that object will be be produced. For example:

```bash
# show the "auth" value from the JSON blob on each line
$ rak '{ $_ with .<auth> }' --json-per-line
```

--keep-meta
-----------

Flag. Only applicable if `--csv-per-line` has been specified. If specified, indicates that meta-information will be kept for each field, by presenting each field as a `CSV::Field|https://github.com/Tux/CSV/blob/master/doc/Text-CSV.md#csvfield` object rather than as a string. The most important methods that can be called on a `CSV::Field` object are:

<table class="pod-table">
<thead><tr>
<th>method</th> <th>meaning</th>
</tr></thead>
<tbody>
<tr> <td>is-quoted</td> <td>field was quoted</td> </tr> <tr> <td>is-binary</td> <td>field contains undecodable data</td> </tr> <tr> <td>is-utf8</td> <td>field contains decodable data beyond ASCII</td> </tr> <tr> <td>is-formula</td> <td>field looks like it contains a spreadsheet formula</td> </tr>
</tbody>
</table>

--list-custom-options
---------------------

Flag. If specified as the only option, will list all additional options previously saved with `--save`.

```bash
# show all of the custom options
$ rak --list-custom-options
fs: --'follow-symlinks'
im: --ignorecase --ignoremark
```

--list-expanded-options
-----------------------

Flag. If specified, will show all actual options being activated after having been recursively expanded, and then exit. Intended as a debugging aid if you have many custom options defined.

```bash
# show how custom option "--im" expands
$ rak --im --list-expanded-options
--ignorecase --ignoremark
```

--list-known-extensions
-----------------------

Flag. If specified, will show all known extension groups and the extensions they represent. Intended as an informational aid.

```bash
# show the filename extensions that "rak" knows about
$ rak --list-known-extensions
       #c: c h hdl
     #c++: cpp cxx hpp hxx
  #config: ini
     #cro: (none) crotmp
     #csv: (none) csv psv tsv
    #html: htm html css
      #js: js ts tsx
    #json: json
   #jsonl: jsonl
#markdown: md markdown
    #perl: (none) pl pm t
  #python: py ipynb
       #r: (none) r R Rmd
    #raku: (none) raku rakumod rakutest rakudoc nqp t pm6 pl6 pod6 t6
    #ruby: rb
    #text: (none) txt
      #wl: (none) wl m wlt mt nb
    #yaml: yaml yml
```

--matches-only
--------------

Flag. Indicate whether only the matched pattern should be produced, rather than the line in which the pattern was found. Defaults to `False`. Frequently used in conjunction with `--per-file`. Will show separated by space if multiple matches are found on the same line.

--max-matches-per-file[=N]
--------------------------

Indicate the maximum number of matches that should be produced per file. If specified as a flag, will assume **1** for its value. By default, will produce **all** possible matches in a file.

--mbc
-----

Flag. Indicates that a [`MoarVM::Bytecode`](https://raku.land/zef:lizmat/MoarVM::Bytecode) object should be produced for each MoarVM bytecode file, to be presented to the matcher. Only makes sense if the pattern is a `Callable`. Will also set the `is-moarvm` flag to only select MoarVM bytecode files, unless reading from STDIN.

Requires the [`MoarVM::Bytecode`](https://raku.land/zef:lizmat/MoarVM::Bytecode) Raku module to be installed.

--mbc-frames
------------

Flag. Indicates that the frames in a MoarVM bytecode file should be produced as a [`MoarVM::Bytecode::Frame`](https://raku.land/zef:lizmat/MoarVM::Bytecode#frame) to the matcher. Only makes sense if the pattern is a `Callable`. Will also set the `is-moarvm` flag to only select MoarVM bytecode files, unless reading from STDIN.

Requires the [`MoarVM::Bytecode`](https://raku.land/zef:lizmat/MoarVM::Bytecode) Raku module to be installed.

--mbc-strings
-------------

Flag. Indicates that the strings in the string hap in a MoarVM bytecode file should be produced to the matcher. Will also set the `is-moarvm` flag to only select MoarVM bytecode files, unless reading from STDIN.

Requires the [`MoarVM::Bytecode`](https://raku.land/zef:lizmat/MoarVM::Bytecode) Raku module to be installed.

---meta-modified=condition
--------------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The modification time of meta information of the file (number of seconds since epoch, as a `Num` value) will be passed as the only argument.

See "CHECKING TIMES ON FILES" for more information about features that can be used inside the `Callable`.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--mode=condition
----------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The full numeric mode value of the file on the filesystem, will be passed as the only argument.

```bash
# list files with sticky bit set
$ rak --find --mode='{ $_ +& 0o1000 }'
```

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--modified=condition
--------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The modification time of the file (number of seconds since epoch, as a `Num` value) will be passed as the only argument.

See "CHECKING TIMES ON FILES" for more information about features that can be used inside the `Callable`.

--modify-files
--------------

Flag. Only makes sense if the specified pattern is a `Callable`. Indicates whether the output of the pattern should be applied to the file in which it was found. Defaults to `False`.

The `Callable` will be called for each file (in sorted order) and each line of the file, giving the line (**including** its line ending). The `$*N` dynamic variable is available inside the `Callable` and is initialized to 0 (in case modifications require keeping numeric state between calls). It is then up to the `Callable` to return:

### False

Remove this line from the file. NOTE: this means the exact `False` value.

### True

Keep this line unchanged the file. NOTE: this means the exact `True` value.

### Nil

Keep this line unchanged the file. NOTE: this means the exact `Nil` value.

### Empty

Keep this line unchanged the file. NOTE: this means the exact `Empty` value. This is typically returned as the result of a failed condition. For example, only change the string "foo" into "bar" if the line starts with "#":

```bash
# replace "foo" by "bar" in all files
$ rak '*.subst("foo","bar")' --modify-files

# replace "foo" by "bar" in all comment lines
$ rak '{ .subst("foo","bar") if .starts-with("#") }' --modify-files
```

### any other value

Inserts this value in the file instead of the given line. The value can either be a string, or a list of strings (which would add lines to the file).

--module=Foo
------------

Indicate the Raku module that should be loaded. Only makes sense if the pattern is a `Callable`.

--output-dir=directory
----------------------

Specify the name of the directory to store the results in. For each group, a separate file will be created. Usually used in conjunction with `--classify` or `--categorize`, but can also be used for normal search results. In that case, the basename of a file with results, will be taken as the name of the file to create in that output directory. The directory must **not** exist beforehand.

--output-file=filename
----------------------

Indicate the path of the file in which the result of the search should be placed. Defaults to `STDOUT`.

--pager=name
------------

Indicate the name (and arguments) of a pager program to be used to page through the generated output. Defaults to the `RAK_PAGER` environment variable. If that isn't specified either, then no pager program will be run.

```bash
# use the "more" pager to page the output
$ RAK_PAGER='more -r' rak foo

# use the "less" pager to page the output
$ rak foo --pager='less -r'
```

--paragraph-context
-------------------

Flag. Indicate all lines that are part of the same paragraph **around** any line that matches. Defaults to `False`. Paragraph boundaries are:

  * the start of the file

  * an empty line

  * the end of the file

--passthru
----------

Flag. Indicate whether **all** lines from source should be shown always. Highlighting will still be performed, if so (implicitely) specified.

```bash
# watch a log file, and highlight a IP address 123.45.67.89
$ tail -f ~/access.log | rak --passthru 123.45.67.89
```

--passthru-context
------------------

Flag. Indicate whether **all** lines from source should be produced if at least one line matches. Highlighting will still be performed, if so (implicitely) specified.

--paths=path1,path2
-------------------

Indicates the path specification to be used instead of from any positional arguments. Multiple path specifications should be separated by comma's.

--paths-from=filename
---------------------

Indicate the path of the file to read path specifications from instead of from any positional arguments. "-" can be specified to read path specifications from STDIN.

--pattern=foo
-------------

Alternative way to specify the pattern to search for. If (implicitly) specified, will assume the first positional parameter specified is actually a path specification, rather than a pattern. This allows the pattern to be saved with `--save`, and thus freeze a specific pattern as part of a custom option.

--patterns-from=file
--------------------

Alternative way to specify one or more patterns to search for. Reads the indicated file and interprets each line as a pattern according to the rules (implicitly) set with the `--type` argument. If the file specification is `"-"`, then the patterns will be read from STDIN.

--pdf-info
----------

Flag. Indicates that the meta-info of any PDF file should be produced as single hash to the matcher. Only makes sense if the pattern is a `Callable`. Will also set the `is-pdf` flag to only select PDF files, unless reading from STDIN.

Requires the [`PDF::Extract`](https://raku.land/zef:librasteve/PDF::Extract) Raku module to be installed.

--pdf-per-file
--------------

Flag. Indicates that any PDF file text contents should be produced as single text to the matcher. Will also set the `is-pdf` flag to only select PDF files, unless reading from STDIN.

Requires the [`PDF::Extract`](https://raku.land/zef:librasteve/PDF::Extract) Raku module to be installed.

--pdf-per-line
--------------

Flag. Indicates that any PDF file text contents should be produced as lines to the matcher. Will also set the `is-pdf` flag to only select PDF files, unless reading from STDIN.

Requires the [`PDF::Extract`](https://raku.land/zef:librasteve/PDF::Extract) Raku module to be installed.

--per-file[=code]
-----------------

Indicate whether matching should be done per file, rather than per line. If specified as a flag, will slurp a file with the indicated `--encoding` and present that to the matcher. Optionally takes a `Callable` specification: this will be given an `IO::Path` object of the file: whatever it produces will be presented to the matcher. Usually used in conjunction with `--matches-only` and/or `count-only`.

```bash
# look for foo in only the first 10 lines of each file
$ rak foo --per-file='*.lines(:!chomp).head(10).join'
```

--per-line[=code]
-----------------

Indicate whether matching should be done per line. If specified as a flag, will read lines with the indicated `--encoding` and present each line to the matcher (which is actually the default if no action was specified).i

Optionally takes a `Callable` specification: this will be given an `IO::Path` object of the file: that is then expected to produce lines that will be presented to the matcher.

```bash
# look for foo in only the last 10 lines of each file
$ rak foo --per-line='*.lines.tail(10)'
```

--progress
----------

Flag. If specified, will produce a progress indicator on STDERR that indicates the number of files checked, number of lines checked and number of matches, updated 5 times per second. Defaults to `False`.

--proximate=[N]
---------------

Indicates whether matched lines should be grouped together that are within N lines of each other. This is useful for visually picking out matches that appear close to other matches. If specified as a flag, indicates a proximation of **1**. Defaults to <0> otherwise (indicating no proximation check should be performed).

--quietly
---------

Flag. Only makes sense if the pattern is a `Callable`. If specified, will catch all **warnings** that are emitted when executing the pattern's `Callable`. Defaults to `False`.

--quote=["]
-----------

Only applicable if `--csv-per-line` has been specified. Indicates the character that should be used for quoting fields. Defaults to **double quote**.

--rak
-----

Flag. Intended for debugging purposes only. When specified, will show the named arguments sent to the `rak` plumbing subroutine just before it isi being called.

--recurse-unmatched-dir
-----------------------

Flag. Indicate whether directories that didn't match the `--dir` specification, should be recursed into anyway. Will not produce files from such directories, but may recurse further if directories are encountered. Defaults to `False`.

--recurse-symlinked-dir
-----------------------

Flag. Indicate whether directories that are actually symbolic links, should be recursed into. Defaults to `False`.

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--rename-files
--------------

Flag. Only makes sense if the specified pattern is a `Callable`. Feeds all selected files, sorted by absolute path, as `IO::Path` objects to the pattern, and uses the result (if different from the original) as the new name of the file.

The `--dryrun` argument can be used to run through the whole process **except** doing actually any renaming.

The `--verbose` argument can be used to get more verbose feedback on the operation.

The `Callable` will be called for each line, giving the file as an `IO::Path` object. The `$*N` dynamic variable is available inside the `Callable` and is initialized to 0. It is then up to the `Callable` to return:

### False

Don't change the name of the file NOTE: this means the exact `False` value.

### True

Don't change the name of the file. NOTE: this means the exact `True` value.

### Nil

Don't change the name of the file. NOTE: this means the exact `Nil` value.

### Empty

Don't change the name of the file. NOTE: this means the exact `Empty` value. This is typically returned as the result of a failed condition.

### any other value

Use this value as the new name of the file. It can either be a string or an `IO::Path` object. Only when the returned value is different from the given value, will a rename actually be attempted. To make this easier on the user, any `Str` returned, will be automatically converted to an `IO::Path` object before being compared using `eqv`.

```bash
# change the extension of all .t files to .rakutest
$ rak '*.subst(/ \.t $/,".rakutest")' --rename-files

# Rename files with 3 digits word bounded with an interval of 10
$ rak '*.subst(/ << \d ** 3 >> /, { ($*N += 10).fmt("%03d") })' --rename-files
```

Note that files that are under git revision control will be renamed using `git mv`: if that fails for any reason, a normal rename will be performed.

--repository=dir
----------------

Indicate the directory that should be searched for Raku module loading. Only makes sense if the pattern is executable code.

--save=shortcut-name
--------------------

Save all options with the given name in the configuration file (`~/.rak-config.json`), and exit with a message that these options have been saved with the given name.

This feature can used to both create shortcuts for specific (long) options, or just as a convenient way to combine often used options, or both.

```bash
# save options as "--im"
$ rak --ignorecase --ignoremark --save=im
Saved option '--im' as: --ignorecase --ignoremark

# can use shortcut to --ignorecase --ignoremark
$ rak foo --im

# save options as "--rsd"
$ rak --recurse-symlinked-dir --save=rsd
Saved option '--rsd' as: --recurse-symlinked-dir

# save as "--B" with a default of '---'
$ rak --break='[---]' --save=B
Saved option '--B' as: --break='[---]'

# save as "--P" requiring a value
$ rak --pattern=! --save=P
Saved option '--P' as: --pattern='!'

# remove shortcut "--foo"
$ rak --save=foo
Removed configuration for 'foo'
```

Any options can be accessed as if it is a standard option. Please note that no validity checking on the options is being performed at the moment of saving, as validity may depend on other options having been specified.

One option can be marked as requiring a value to be specified (with "!") or have a default value (with "[default-value]").

To remove a saved set of options, use `--save`=foo as the only option to remove the "foo" set of options.

--sep=,
-------

Only applicable if `--csv-per-line` has been specified. Indicates the character to indicate the field separator. Defaults to the **comma**.

--show-blame
------------

Flag. Indicate whether to show `git blame` information for matching lines if possible, instead of just the line. Defaults to `False`.

Requires that the [`Git::Blame::File`](https://raku.land/zef:lizmat/Git::Blame::File) module is installed.

--show-item-number
------------------

Flag. Indicate whether item numbers should be shown. Defaults to `True`.

--show-filename
---------------

Flag. Indicate whether filenames should be shown. Defaults to `True`.

--shell=invocation
------------------

If specified, indicates the command(s) to be executed in a shell. Any `$_` in the invocation string will be replaced by the file being checked. The file will be included if the shell command(s) run to a successful conclusion.

--silently[=out,err]
--------------------

Flag and option. Only applicable if the pattern is a `Callable`. Indicates whether any output from the `Callable` pattern should be caught. Defaults to `False`. If specified as a flag, will catch both STDOUT as well as STDERR output from the pattern's execution. When specified as an option, will accept:

<table class="pod-table">
<thead><tr>
<th>flag(s)</th> <th>action</th>
</tr></thead>
<tbody>
<tr> <td>out</td> <td>only capture STDOUT</td> </tr> <tr> <td>err</td> <td>only capture STDERR</td> </tr> <tr> <td>out,err</td> <td>capture both STDIN as well as STDERR</td> </tr> <tr> <td>err,out</td> <td>capture both STDIN as well as STDERR</td> </tr>
</tbody>
</table>

--smartcase
-----------

Flag. An intelligent version of `--ignorecase`. If the pattern does **not** contain any uppercase letters, it will act as if `--ignorecase` was specified. Otherwise it is ignored.

--smartmark
-----------

Flag. An intelligent version of `--ignoremark`. If the pattern does **not** contain any accented letters, it will act as if `--ignoremark` was specified. Otherwise it is ignored.

--sourcery
----------

Flag. Mainly intended for Raku Programming Language core developers. If specified, indicates that the pattern should be interpreted as code specifying a simple call to a subroutine, or a simple call to a method, optionally with arguments. The search result will then contain the source locations of subroutine / method that is expected to be able to handle that call.

Compatible with the `--edit`, `--vimgrep` and the implicit `per-line` option.

```bash
# edit the location(s) of the "say" sub handling a single string
$ rak --sourcery 'say "foo"' --edit
```

Requires the [`sourcery`](https://raku.land/zef:lizmat/sourcery) module to be installed.

--stats
-------

Flag. Also show statistics about the search operation after having shown the full search result.

--stats-only
------------

Flag. **Only** show statistics about the search operation. See also `--count-only`.

--strict
--------

Flag. Only applicable if `--csv-per-line` has been specified. If specified, then each line in the CSV file **must** have the same number of fields. Default is to allow different numbers of fields.

--summary-if-larger-than=N
--------------------------

Indicate the maximum size a line may have before it will be summarized. Defaults to `160` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `Inf` effectively (indicating no summarization will ever occur).

--type=string
-------------

The `--type` argument indicates how any pattern, as specified on the commmand line, or from previously saved options, should be interpreted. If not specified specified, will assume `auto`.

The following strings can be specified:

### auto

If `--type=auto` is (implicitely) specified, will look for cues in a specified pattern to understand what functionality is requested. See the [pattern](pattern) for more information.

### regex

If `--type=regex` is specified, then a pattern will be interpreted as a regex, as if it was surrounded by slashes.

### code

If `--type=code` is specified, then a pattern will be interpreted as Raku source code, as if it was surrounded by curly braces.

### json-path

If `--type=json-path` is specified, then a pattern will be interpreted as a `JSON path`. Only makes sense when used together with `--json-per-file`, `--json-per-line` or `--json-per-elem`. Requires the [`JSON::Path`](https://raku.land/cpan:JNTHN/JSON::Path) module to be installed.

### contains

If `--type=contains` is specified, then a pattern will be interpreted as a literal string, while honouring any `--smartcase`, `--smartmark`, `--ignorecase` and `--ignoremark` specifications.

### words

If `--type=words` is specified, then a pattern will be interpreted as a literal string that should be bounded by word boundares at both ends, while honouring any `--smartcase`, `--smartmark`, `--ignorecase` and `--ignoremark` specifications.

### starts-with

If `--type=starts-with` is specified, then a pattern will be interpreted as a literal string that should occur at the **start** of a line, while honouring any `--smartcase`, `--smartmark`, `--ignorecase` and `--ignoremark` specifications.

### ends-with

If `--type=ends-with` is specified, then a pattern will be interpreted as a literal string that should occur at the **end** of a line, while honouring any `--smartcase`, `--smartmark`, `--ignorecase` and `--ignoremark` specifications.

### equal

If `--type=equal` is specified, then a pattern will be interpreted as a literal string that should be **equal** to the line, while honouring any `--smartcase`, `--smartmark`, `--ignorecase` and `--ignoremark` specifications.

--trim
------

Flag. Indicate whether lines that have the pattern, should have any whitespace at the start and/or end of the line removed. Defaults to `True` if no context for lines was specified, else defaults to `False`.

--uid=condition
---------------

If specified, indicates the `Callable` that should return `True` to include a file in the selection of files to be checked. The numeric `uid` of the file will be passed as the only argument. Can also be specified as a single numeric argument. See also `--user`.

```bash
# select files of which the numeric user id is greater than 500
$ rak --find --uid='* > 500'

# select files of which the numeric user id is 501
$ rak --find --uid=501
```

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--under-version-control[=git]
-----------------------------

Indicate whether to only select files that are under some form of version control. If specified as a flag, will assume files that are under `git` version control. Can also specify the name of the version control system as the value: currently only **git** is supported.

--unicode
---------

Flag. If specified, will search the unicode database for defined codepoints by name. Default is `False`.

--unique
--------

Flag. If specified, will only produce unique lines of output. Default is `False`. See also `--frequencies`.

--user=condition
----------------

If specified, indicates the `Callable` that should return `True` to include a file in the selection of files to be checked. The user name associated with the `uid` of the file will be passed as the only argument.

Can also be specified as a list of comma separated names to (not) select on. To select all names **except** the listed named, prefix with a `!`.

See also `--uid`. Requires the [P5getpwnam](https://raku.land/zef:lizmat/P5getpwnam) module to be installed.

```bash
# files of which the name associated with the user id starts with underscore
$ rak --find --user='*.starts-with("_")'

# select files of which the owner is liz or wendy
$ rak --find --user=liz,wendy

# select files of which the owner is NOT liz or wendy
$ rak --find --user='!liz,wendy'
```

NOTE: support of this feature depends on Raku supporting that feature on the current operating system.

--version
---------

Flag. If the only argument, shows the name and version of the script, and the system it is running on. Additionally specify `--verbose` to see more information.

--vimgrep
---------

Flag. If specified, will output search results in the format "filename:linenumber:column:line". This allows integration with the `:grep` action in vim-like editors.

CHECKING TIMES ON FILES
=======================

The `--accessed`, `--created`, `--modified` and `--meta-modified` options expect `Callable` to perform the check to include a file in the search process. It is passed the **epoch** (number of seconds since 1 January 1970 UTC) value of the file being checked for the indicated option, and it should return `True` to include that file in any search.

To facilitate checks, some extra features are activated for these `Callable`s, allowing you to more easily craft your conditions.

Automatic conversion to epoch
-----------------------------

In Raku, the `.accessed`, `.created`, `.changed` and `.modified` methods on the `IO::Path` object return [`Instant`](https://docs.raku.org/type/Instant) objects, which are atomic time rather than epoch. Within these special `Callables`, these values are automatically converted to epoch values, to ease comparisons.

Specifying some time ago
------------------------

Within these special `Callable`s, one can also indicate an epoch value in the past by using the `.ago` method in a specially formatted string. This string is formatted similarly to time specifications of the Unix `find` command: one of more digits followed by "s" for seconds, "m" for minutes, "h" for hours, "d" for days and "w" for weeks. "+" and "-" may also be used, but do not have any special meaning other than negating the value they apply to.

On method naming
----------------

For `rak` it was decided to name the option for checking the meta information of a file as `--meta-modified`. In Raku, the associated method on the `IO::Path` object is (probably for historical reasons) called `.changed`. To facilitate the creation of the `Callable`s for these options, one can use both `.meta-modified` as well as `.changed` as methods.

Examples
--------

```bash
# select all files that were modified later than an hour ago
$ rak --find --modified='* > "1h".ago'

# select all files that were created before 2.5 hours ago
$ rak --find --created='* < "2h30m".ago'

# select all files that were modified after "Changes" was created
$ rak --find --modified='* > "Changes".IO.created'
```

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/App-Rak . Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2022, 2023, 2024 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

