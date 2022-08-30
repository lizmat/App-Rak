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

$ rak foo --files-with-matches  # look for "foo", only produce filenames

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

If the pattern is a `Callable`, then the dynamic variable `$*SOURCE` will contain the `IO::Path` object of the file being processed. Note that pattern `Callable`s will be called in a thread **unsafe** manner.

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

--allow-loose-escapes
---------------------

Only applicable if `--csv-per-line` has been specified. Flag. If specified, indicates that **any** character may be escaped.

--allow-loose-quotes
--------------------

Only applicable if `--csv-per-line` has been specified. Flag. If specified, indicates that fields do not need to be quoted to be acceptable.

--allow-whitespace
------------------

Only applicable if `--csv-per-line` has been specified. Flag. If specified, indicates that whitespace is allowed around separators.

--backup[=extension]
--------------------

Indicate whether backups should be made of files that are being modified. If specified without extension, the extension `.bak` will be used.

--batch=N
---------

Indicate the number of files that should be checked per thread. Defaults to `64` if not specified. See also <--degree>.

--before-context=N
------------------

Indicate the number of lines that should be shown **before** any line that matches. Defaults to **0**. Will be overridden by a `--context` argument.

--blame-per-file
----------------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a `True` value, indicates that each of the selected files will be provided as [`Git::Blame::File`](https://raku.land/zef:lizmat/Git::Blame::File#methods-on-gitblamefile) objects if `git blame` can be performed on the a selected file. If that is not possible, then the selected file will be ignored.

If <git blame> information can be obtained, then the associated `Git::Blame::File` object will be presented to the pattern `Callable`. If the Callable returns a true value, then filename will be shown. If the returned value is a string, then that string will be shown.

```bash
# show files with more than 10 commits
$ rak '*.commits > 10' --blame-per-file --files-with-matches
```

Requires that the [`Git::Blame::File`](https://raku.land/zef:lizmat/Git::Blame::File) module is installed.

--blame-per-line
----------------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a `True` value, indicates that each line from the selected files will be provided as [`Git::Blame::Line`](https://raku.land/zef:lizmat/Git::Blame::File#accessors-on-gitblameline) objects if `git blame` can be performed on the a selected file. If that is not possible, then the selected file will be ignored.

If <git blame> information can be obtained, then the associated `Git::Blame::Line` object will be presented to the pattern `Callable`. If the Callable returns a true value, then the short representation of the `git blame` information will be shown. If the returned value is a string, then that string will be shown.

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

--context=N
-----------

Indicate the number of lines that should be shown **around** any line that matches. Defaults to **0**. Overrides any a `--after-context` or `--before-context` arguments.

--count-only
------------

Flag. Indicate whether just the number of lines with matches should be calculated. When specified with a `True` value, will show a "N matches in M files" by default, and if the `:files-with-matches` (or `files-without matches`) option is also specified with a `True` value, will just show total counts.

--csv-per-line
--------------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a `True` value, indicates that selected files should be interpreted as comma separated values (CSV). Each row from the selected files will be provided as a list of strings (or of `CSV::Field` objects).

Attempt to interpret file as a CSV file, and pass each row as a List to to the pattern Callable. Only files with extensions from the `#csv` group will be tried, unless overridden by any explicit extension specification.

More documentation can be found with the [Text::CSV](https://raku.land/github:Tux/Text::CSV) module itself.

--degree=N
----------

Indicate the number of worker threads that should be maximally. Defaults to the number of cores minus 1 if not specified. See also <--batch>.

--device-number=condition
-------------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The device number of the filesystem on which the file is located, will be passed as the only argument.

--dir=condition
---------------

If specified, indicates the `Callable` that should return True to have a directory be included for further recursions in file selection. The basename of the directory will be passed as the only argument. Defaults to all directories that do not start with a period. Can specify as a flag to include **all** directories for recursion.

--dryrun
--------

Flag. Indicate to **not** actually make any changes to any content modification if specified with a `True` value. Only makes sense together with the `--modify-files` option.

--edit[=editor]
---------------

Indicate whether the patterns found should be fed into an editor for inspection and/or changes. Defaults to `False`. Optionally takes the name of the editor to be used.

--eol=[lf|cr|crlf]
------------------

Only applicable if `--csv-per-line` has been specified. Indicate a line ending different from the standard line ending assumed by the system. Can be specified as `lf` for a single LineFeed character, `cr` for a single CarriageReturn character, or `crlf` for a combination of a CarriageReturn and a LineFeed character.

--escape=["]
------------

Only applicable if `--csv-per-line` has been specified. Indicates the escape character to be used to escape characters in a field. Defaults to **double quote**.

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

--file=condition
----------------

If specified, indicates the `Callable` that should return True to have a file be included in the file selection process. The basename of the file will be passed as the only argument. Defaults to `True`, indicating that all files should be included.

--file-separator-null
---------------------

Flag. Indicate to separate filenames by null bytes rather than newlines if the `--files-with-matches` option is specified with a `True` value.

--files-from=filename
---------------------

Indicate the path of the file to read filenames from instead of the expansion of paths from any positional arguments. "-" can be specified to read filenames from STDIN.

--files-with-matches
--------------------

Flag. If specified with a true value, will only produce the filenames of the files in which the pattern was found. Defaults to `False`.

--files-without-matches
-----------------------

Flag. If specified with a true value, will only produce the filenames of the files in which the pattern was **not** found. Defaults to `False`.

--filesize=condition
--------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The number of bytes of data in the file, will be passed as the only argument.

```bash
# show files that consist of at 30 bytes
$ rak --find --filesize='* >= 30'
```

--find
------

Flag. If specified with a true value, will **not** look at the contents of the selected paths, but instead consider the selected paths as lines in a virtual file.

--find-all
----------

Flag. If specified with a true value, will override any file or directory filter settings and include all possible files for inspection.

--first-only[=N]
----------------

Indicate the number of matches to show. If specified without a value, will default to **1**. Defaults to show all possible matches.

--formula=[none]
----------------

Only applicable if `--csv-per-line` has been specified. If specified, indicates the action to be taken when a field starts with an equal sign (indicating a formula of some kind in many spreadsheets). The following values are recognized:

  * none - take not action, just pass on

  * die - throw an exception

  * diag - report line and position where formula was found

  * empty - replace the formula by an empty string

--frequencies
-------------

Flag. If specified, will produce a frequency table of the matches with the most frequent match first. Default is `False`. See also `--unique`;

--gid=condition
---------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The numeric `gid` of the file will be passed as the only argument. Can also be specified as a single numeric argument. See also `--group`.

```bash
# show files of which the numeric group id is greater than 20
$ rak --find --gid='* > 20'

# show files of which the numeric group id is 20
$ rak --find --gid=20
```

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

--group-matches
---------------

Flag. Indicate whether matches of a file should be grouped together by mentioning the filename only once (instead of on every line). Defaults to `True`.

--hard-links=condition
----------------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The number of hard-links to the file on the filesystem, will be passed as the only argument.

--has-setgid
------------

Flag. If specified with a trueish value, will only select files that do have the SETGID bit set in their attributes. Use negation `--/has-setgid` to only select files that do **not** have the SETGID bit set.

--has-setuid
------------

Flag. If specified with a trueish value, will only select files that do have the SETUID bit set in their attributes. Use negation `--/has-setuid` to only select files that do **not** have the SETUID bit set.

--help [area-of-interest]
-------------------------

Show argument documentation, possibly extended by giving the area of interest, which are:

  * pattern

  * string

  * code

  * input

  * haystack

  * filesystem

  * result

  * listing

  * resource

  * special

  * option

  * general

  * philosophy

  * examples

--highlight
-----------

Flag. Indicate whether the pattern should be highlighted in the line in which it was found. Defaults to `True` if a human is watching (aka STDOUT connected to a terminal), or `--highlight-before` or `highlight-after` are explicitely specified, or `False` otherwise.

--highlight--after[=string]
---------------------------

Indicate the string that should be used at the end of the pattern found in a line. Specifying implies `--highlight`ing implicitely. If `--highlight` or `--highlight-before` are explicitely specified, will default to whatever is specified with `--highlight-before`, or to the ANSI code to end **bold**.

--highlight--before[=string]
----------------------------

Indicate the string that should be used at the end of the pattern found in a line. Specifying implies `--highlight`ing implicitly. If `highlight` is explicitely specified with a trueish value, will default to the terminal code to start **bold**.

--ignorecase
------------

Flag. If specified with a trueish value, indicates that any matching should be done case insensitively. Default is `False`.

--ignoremark
------------

Flag. If specified with a trueish value, indicates that any matching should be done without consideration of any accents. Default is `False`.

--inode=condition
-----------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The inode number of the file on the filesystem, will be passed as the only argument.

--is-empty
----------

Flag. If specified with a trueish value, will only select files that do not contain any data. Use negation `--/is-empty` to only select files that **do** contain data.

--is-executable
---------------

Flag. If specified with a trueish value, will only select files that can be executed by the current user. Use negation `--/is-executable` to only select files that are **not** executable by the current user.

--is-group-executable
---------------------

Flag. If specified with a trueish value, will only select files that can be executed by members of the group of the owner. Use negation `--/is-group-executable` to only select files that are **not** executable by the members of the group of the owner.

--is-group-readable
-------------------

Flag. If specified with a trueish value, will only select files that can be read by members of the group of the owner. Use negation `--/is-group-readable` to only select files that are **not** readable by the members of the group of the owner.

--is-group-writable
-------------------

Flag. If specified with a trueish value, will only select files that can be written to by members of the group of the owner. Use negation `--/is-group-writable` to only select files that are **not** writable by the members of the group of the owner.

--is-owned-by-group
-------------------

Flag. If specified with a trueish value, will only select files that are owned by the group of the current user. Use negation `--/is-owned-by-group` to only select files that are **not** owned by the group of the current user.

--is-owned-by-user
------------------

Flag. If specified with a trueish value, will only select files that are owned by current user. Use negation `--/is-owned-by-user` to only select files that are **not** owned by the current user.

--is-owner-executable
---------------------

Flag. If specified with a trueish value, will only select files that can be executed by the owner. Use negation `--/is-owner-executable` to only select files that are **not** executable by the owner.

--is-owner-readable
-------------------

Flag. If specified with a trueish value, will only select files that can be read by the owner. Use negation `--/is-owner-readable` to only select files that are **not** readable by the owner.

--is-owner-writable
-------------------

Flag. If specified with a trueish value, will only select files that can be written to by the owner. Use negation `--/is-owner-writable` to only select files that are **not** writable by the owner.

--is-readable
-------------

Flag. If specified with a trueish value, will only select files that can be read by the the current user. Use negation `--/is-readable` to only select files that are **not** readable by the current user.

--is-sticky
-----------

Flag. If specified with a trueish value, will only select files that do have the STICKY bit set in their attributes. Use negation `--/is-sticky` to only select files that do **not** have the STICKY bit set.

--is-symbolic-link
------------------

Flag. If specified with a trueish value, will only select files that are symbolic links. Use negation `--/is-symbolic-link` to only select files that are **not** symbolic links.

--is-world-executable
---------------------

Flag. If specified with a trueish value, will only select files that can be executed by anybody. Use negation `--/is-group-executable` to only select files that are **not** executable by anybody.

--is-world-readable
-------------------

Flag. If specified with a trueish value, will only select files that can be read by anybody. Use negation `--/is-world-readable` to only select files that are **not** readable by anybody.

--is-world-writable
-------------------

Flag. If specified with a trueish value, will only select files that can be written to by anybody. Use negation `--/is-world-writable` to only select files that can **not** be written to by anybody.

--is-writable
-------------

Flag. If specified with a trueish value, will only select files that can be written to by the current user. Use negation `--/is-writable` to only select files that can **not** be written to by the current user.

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

--keep-meta
-----------

Only applicable if `--csv-per-line` has been specified. Flag. If specified, indicates that meta-information will be kept for each field, by presenting each field as a `CSV::Field|https://github.com/Tux/CSV/blob/master/doc/Text-CSV.md#csvfield` object rather than as a string. The most important methods that can be called on a `CSV::Field` object are:

  * is-quoted - field was quoted

  * is-binary - field contains undecodable data

  * is-utf8 - field contains decodable data beyond ASCII

  * is-formula = field looks like it contains a spreadsheet formula

--known-extensions
------------------

Flag. Indicate that only files with known extensions (occuring in any of the `#groups`) should be searched. Defaults to `True` if a human is watching (aka STDOUT is connected to a terminal).

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

Flag. If specified with a true value, will show all actual options being activated after having been recursively expanded, and then exit. Intended as a debugging aid if you have many custom options defined.

--list-known-extensions
-----------------------

```bash
$ rak --list-known-extensions
       #c: c h hdl
     #c++: cpp cxx hpp hxx
  #config: ini
#markdown: md markdown
    #perl: (none) pl pm t
  #python: py
    #raku: (none) raku rakumod rakutest rakudoc nqp t pm6 pl6 pod6 t6
    #ruby: rb
    #text: (none) txt
    #yaml: yaml yml
```

Flag. If specified with a true value, will show all known extension groups and the extensions they represent. Intended as an informational aid.

--mode=condition
----------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The full numeric mode value of the file on the filesystem, will be passed as the only argument.

```bash
# list files with sticky bit set
$ rak --find --mode='{ $_ +& 0o1000 }'
```

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

--matches-only
--------------

Flag. Indicate whether only the matched pattern should be produced, rather than the line in which the pattern was found. Defaults to `False`. Frequently used in conjunction with `--per-file`. Will show separated by space if multiple matches are found on the same line.

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

Flag. Indicate whether **all** lines from source should be shown always. Highlighting will still be performed, if so (implicitely) specified.

```bash
# Watch a log file, and highlight a certain IP address.
$ tail -f ~/access.log | rak --passthru 123.45.67.89
```

--passthru-context
------------------

Flag. Indicate whether **all** lines from source should be shown if at least one line matches. Highlighting will still be performed, if so (implicitely) specified.

--paths-from=filename
---------------------

Indicate the path of the file to read path specifications from instead of from any positional arguments. "-" can be specified to read path specifications from STDIN.

--pattern=foo
-------------

Alternative way to specify the pattern to search for. If (implicitly) specified, will assume the first positional parameter specified is actually a path specification, rather than a pattern. This allows the pattern to be searched for to be saved with `--save`.

--per-file[=code]
-----------------

Indicate whether matching should be done per file, rather than per line. If specified as a flag, will slurp a file with the indicated `--encoding` and present that to the matcher. Optionally takes a `Callable` specification: this will be given an `IO::Path` object of the file: whatever it produces will be presented to the matcher. Usually used in conjunction with `--matches-only` and/or `count-only`.

```bash
# look for foo in only the first 10 lines of each file
$ rak foo --per-file='*.lines(:!chomp).head(10).join'
```

--quietly
---------

Flag. Only makes sense if the pattern is a `Callable`. If specified with a true value, will catch all **warnings** that are emitted when executing the pattern's `Callable`. Defaults to False.

--quote=["]
-----------

Only applicable if `--csv-per-line` has been specified. Indicates the character that should be used for quoting fields. Defaults to **double quote**.

--recurse-unmatched-dir
-----------------------

Flag. Indicate whether directories that didn't match the `--dir` specification, should be recursed into anyway. Will not produce files from such directories, but may recurse further if directories are encountered. Defaults to `False`.

--recurse-symlinked-dir
-----------------------

Flag. Indicate whether directories that are actually symbolic links, should be recursed into. Defaults to `False`.

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

--sep=[,]
---------

Only applicable if `--csv-per-line` has been specified. Indicates the character to indicate the field separator. Defaults to the **comma**.

--show-blame
------------

Flag. Indicate whether to show `git blame` information for matching lines if possible, instead of just the line. Defaults to `False`.

Requires that the [`Git::Blame::File`](https://raku.land/zef:lizmat/Git::Blame::File) module is installed.

--show-filename
---------------

Flag. Indicate whether filenames should be shown. Defaults to `True`.

--show-line-number
------------------

Flag. Indicate whether line numbers should be shown. Defaults to `True`.

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

--strict
--------

Only applicable if `--csv-per-line` has been specified. Flag. If specified with a trueish value, then each line in the CSV file **must** have the same number of fields. Default is to allow different numbers of fields.

--summary-if-larger-than=N
--------------------------

Indicate the maximum size a line may have before it will be summarized. Defaults to `160` if `STDOUT` is a TTY (aka, someone is actually watching the search results), otherwise defaults to `Inf` effectively (indicating no summarization will ever occur).

  * --type[=words|starts-with|ends-with|contains]

Only makes sense if the pattern is a string. With `words` specified, will look for pattern as a word in a line, with `starts-with` will look for the pattern at the beginning of a line, with `ends-with` will look for the pattern at the end of a line, with `contains` will look for the pattern at any position in a line.

--trim
------

Flag. Indicate whether lines that have the pattern, should have any whitespace at the start and/or end of the line removed. Defaults to `True` if no context for lines was specified, else defaults to `False`.

--uid=condition
---------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The numeric `uid` of the file will be passed as the only argument. Can also be specified as a single numeric argument. See also `--user`.

```bash
# show files of which the numeric user id is greater than 500
$ rak --find --uid='* > 500'

# show files of which the numeric user id is 501
$ rak --find --uid=501
```

--under-version-control[=git]
-----------------------------

Indicate whether to only select files that are under some form of version control. If specified with a trueish value, will assume files that are under `git` version control. Can also specify the name of the version control system as the value: currently only **git** is supported.

--user=condition
----------------

If specified, indicates the `Callable` that should return True to include a file in the selection of files to be checked. The user name associated with the `uid` of the file will be passed as the only argument.

Can also be specified as a list of comma separated names to (not) select on. To select all names **except** the listed named, prefix with a `!`.

See also `--uid`. Requires the [P5getpwnam](https://raku.land/zef:lizmat/P5getpwnam) module to be installed.

```bash
# files of which the name associated with the user id starts with underscore
$ rak --find --user='*.starts-with("_")'

# show files of which the owner is liz or wendy
$ rak --find --user=liz,wendy

# show files of which the owner is NOT liz or wendy
$ rak --find --user='!liz,wendy'
```

--unique
--------

Flag. If specified with a true value, will only produce unique lines of output. Default is `False`. See also `--frequencies`.

--version
---------

Flag. If the only argument, shows the name and version of the script, and the system it is running on.

--vimgrep
---------

Flag. If specified with a true value, will output search results in the format "filename:linenumber:column:line". This allows integration with the `:grep` action in vim-like editors.

AUTHOR
======

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/App-Rak . Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a [small sponsorship](https://github.com/sponsors/lizmat/) would mean a great deal to me!

COPYRIGHT AND LICENSE
=====================

Copyright 2022 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

