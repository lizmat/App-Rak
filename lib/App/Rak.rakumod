use highlighter:ver<0.0.5>:auth<zef:lizmat>;
use paths:ver<10.0.5>:auth<zef:lizmat>;
use Files::Containing:ver<0.0.6>:auth<zef:lizmat>;

my constant BON  = "\e[1m";
my constant BOFF = "\e[22m";

# Make sure we remember if there's a human watching (terminal connected)
my $isa-tty := $*OUT.t;

my constant @raku-extensions = <
   raku rakumod rakutest nqp t pm6 pl6
>;

# sane way of quitting
my sub meh($message) { exit note $message }

# quit if unexpected named arguments
my sub meh-if-unexpected(%_) {
    if %_.keys -> @unexpected {
        meh "Unexpected parameters: @unexpected[]";
    }
}

# is a needle a simple Callable?
my sub is-simple-Callable($needle) {
    Callable.ACCEPTS($needle) && !Regex.ACCEPTS($needle)
}

# process all alternate names / values into a single value
my sub named-arg(%args, *@names) {
    return %args.DELETE-KEY($_) if %args.EXISTS-KEY($_) for @names;
    Nil
}

# process all alternate names / values into a Map
my sub named-args(%args, *%wanted) {
    Map.new: %wanted.kv.map: -> $name, $keys {
        if $keys =:= True {
            Pair.new($name, %args.DELETE-KEY($name))
              if %args.EXISTS-KEY($name);
        }
        orwith $keys.first: { %args.EXISTS-KEY($_) }, :k {
            Pair.new($name, %args.DELETE-KEY($keys.AT-POS($_)))
        }
        elsif %args.EXISTS-KEY($name) {
            Pair.new($name, %args.DELETE-KEY($name))
        }
    }
}

# add any lines before / after in a result
my sub add-before-after($io, @initially-selected, int $before, int $after) {
    my str @lines = $io.lines;
    @lines.unshift: "";   # make 1-base indexing natural
    my int $last-linenr = @lines.end;

    my int8 @seen;
    my @selected;
    for @initially-selected {
        my int $linenr = .key;
        if $before {
            for max($linenr - $before, 1) ..^ $linenr -> int $_ {
                @selected.push: Pair.new($_, @lines.AT-POS($_))
                  unless @seen.AT-POS($_)++;
            }
        }

        @selected.push: $_ unless @seen.AT-POS($linenr)++;

        if $after {
            for $linenr ^.. min($linenr + $after, $last-linenr ) -> int $_ {
                @selected.push: Pair.new($_,@lines.AT-POS($_))
                  unless @seen.AT-POS($_)++;
            }
        }
    }

    @selected
}

use CLI::Version:ver<0.0.1>:auth<zef:lizmat>
  $?DISTRIBUTION,
  my proto sub MAIN(|) is export {*}

my multi sub MAIN($needle is copy, $root = ".", *%_) {
    $needle .= trim;
    if $needle.starts-with('/') && $needle.ends-with('/')
      || $needle.indices('*') == 1 {
        $needle .= EVAL;
    }
    elsif $needle.starts-with('{') && $needle.ends-with('}') {
        $needle = ('-> $_ ' ~ $needle).EVAL;
    }

    temp $*OUT;
    with named-arg %_, <output-file> -> $path {
        $*OUT = open($path, :w) if $path ne "-";
    }

    my $file;
    my $dir;

    named-arg(%_, <l files-only files-with-matches>)
      ?? files-only($needle, $root, $file, $dir, %_)
      !! want-lines($needle, $root, $file, $dir, %_)
}

my sub files-only($needle, $root, $file, $dir, %_ --> Nil) {
    my $additional := named-args %_,
      :ignorecase<i ignore-case>,
      :ignoremark<m ignore-mark>,
      :invert-match<v>,
      :batch,
      :degree,
    ;
    meh-if-unexpected(%_);

    .relative.say for files-containing
       $needle, $root, :$file, :$dir, :files-only, |$additional;
}

my sub want-lines($needle, $root, $file, $dir, %_ --> Nil) {
    my $ignorecase = named-arg %_, <i ignorecase ignore-case>;
    my $ignoremark = named-arg %_, <m ignoremark ignore-mark>;
    my $seq := files-containing
      $needle, $root, :$file, :$dir, :offset(1), :$ignorecase, :$ignoremark,
      |named-args %_,
        :invert-match<v>,
        :max-count,
        :batch,
        :degree,
    ;

    my UInt() $before = $_ with named-arg %_, <B before before-context>;
    my UInt() $after  = $_ with named-arg %_, <A after after-context>;
    $before = $after  = $_ with named-arg %_, <C context>;

    my Bool() $line-number;
    my Bool() $highlight;
    my Bool() $trim;
    my Bool() $no-filename;
    my Bool() $only;

    if %_<human> // $isa-tty {
        $line-number = $highlight = True;
        $no-filename = $only      = False;
        $trim = !($before || $after);
    }

    $highlight = $_ with named-arg %_, <highlight>;
    $trim      = $_ with named-arg %_, <trim>;
    $only      = $_ with named-arg %_, <o only-matching>;
    $before = $after = 0 if $only;

    my &show-line;
    if $highlight {
        my Str() $pre = my Str() $post = named-arg(%_, <highlight-before>);
        $post = $_ with named-arg %_, <highlight-after>;
        $pre  = $only ?? " " !! BON  without $pre;
        $post = $only ?? ""  !! BOFF without $post;

        &show-line = $trim 
          ?? -> $line {
                 highlighter $line.trim, $needle, $pre, $post,
                 :$ignorecase, :$ignoremark, :$only
             }
          !! -> $line {
                 highlighter $line, $needle, $pre, $post,
                 :$ignorecase, :$ignoremark, :$only
             }
        ;
    }
    else {
        &show-line = $only
          ?? -> $line { highlighter $line, $needle, "", " ", :$only }
          !! $trim
            ?? *.trim
            !! -> $line { $line }
        ;
    }

    $line-number   = $_ with named-arg %_, <n line-number>;
    $no-filename   = $_ with named-arg %_, <h no-filename>;

    $before || $after
      ?? lines-with-context($seq, &show-line, $before, $after, %_)
      !! just-lines($seq, &show-line, %_);
}

my sub lines-with-context($seq is raw, &show-line, $before, $after, %_) {
    my int $nr-files;

    for $seq {
        say "" if $nr-files++;

        my $io := .key;
        say $io.relative;

        my @selected := add-before-after($io, .value, $before//0, $after//0);
        my $format   := '%' ~ (@selected.tail.key.chars + 1) ~ 'd:';

        say sprintf($format, .key) ~ show-line .value for @selected;
    }
}

my sub just-lines($seq is raw, &show-line, %_) {
    my int $nr-files;

    for $seq {
        say "" if $nr-files++;

        my $io := .key;
        say $io.relative;

        my $format := '%' ~ (.value.tail.key.chars + 1) ~ 'd:';
        say sprintf($format, .key) ~ show-line .value for .value;
    }
}

=begin pod

=head1 NAME

App::Rak - a CLI for searching strings in files

=head1 SYNOPSIS

=begin code :lang<bash>

$ rak foo      # look for "foo" in current directory recursively

$ rak foo bar  # look for "foo" in directory "bar" recursively

$ rak '/ << foo >> /'    # look for "foo" as word in current directory

$ raku foo --files-only  # look for "foo", only produce filenames

$ raku foo --before=2 --after=2  # also produce 2 lines before and after

=end code

=head1 DESCRIPTION

App::Rak provides a CLI called C<rak> that allows you to look for a needle
in (a selection of files) from a given directory recursively.

To a large extent, the arguments are the same as with the C<grep> utility
provided on most Unixes.

Note: this is still very much in alpha development phase.  Comments and
suggestions are more than welcome!

=head1 POSITIONAL ARGUMENTS

=head2 pattern

The pattern to search for.  This can either be a string, or a regular
expression (indicated by a string starting and ending with B</>), or a
Callable (indicated by a string starting with B<{> and ending with B<}>.  

=head2 path

Optional.  Either indicates the path of the directory (and its
sub-directories), or the file that will be searched.  By default, all
directories that do not start with a period, will be recursed into (but
this can be changed with the C<--dir> named argument).

By default, all files will be searched in the directories.  This can be
changed with the C<--file> named argument.

=head1 SUPPORTED NAMED ARGUMENTS

All named arguments are optional.  Any unexpected named arguments, will
cause an exception with the unexpected named arguments listed.

=head2 -A or --after or --after-context

Indicate the number of lines that should be shown B<after> any line that
matches.  Defaults to B<0>.  Will be overridden by a C<-C> or C<--context>
argument.

=head2 -B or --before or --before-context

Indicate the number of lines that should be shown B<before> any line that
matches.  Defaults to B<0>.  Will be overridden by a C<-C> or C<--context>
argument.

=head2 -C or --context

Indicate the number of lines that should be shown B<around> any line that
matches.  Defaults to B<0>.  Overrides any a C<-A>, C<--after>,
C<--after-context>, C<-B>, C<--before> or C<--before-context> argument.
argument.

=head2 --highlight

Indicate whether the pattern should be highlighted in the line in which
it was found.  Defaults to C<True> if C<--human> is (implicitely) set to
C<True>, else defaults to C<False>.

=head2 --highlight--after

Indicate the string that should be used at the end of the pattern found in
a line.  Only makes sense if C<--highlight> is (implicitely) set to C<True>.
Defaults to the empty string if C<-o> or C<--only-matching> is specified
with a C<True> value, or to the terminal code to end B<bold> otherwise.

=head2 --highlight--before

Indicate the string that should be used at the end of the pattern found in
a line.  Only makes sense if C<--highlight> is (implicitely) set to C<True>.
Defaults to a space if C<-o> or C<--only-matching> is specified with a
C<True> value, or to the terminal code to start B<bold> otherwise.

=head2 --human

Indicate that search results should be presented in a human readable
manner.  This means: filenames shown on a separate line, line numbers
shown, and highlighting performed.  Defaults to C<True> if C<STDOUT> is
a TTY (aka, someone is actually watching the search results), otherwise
defaults to C<False>.

=head2 -l or --files-only or --files-with-matches>

If specified with a true value, will only produce the filenames of the
files in which the pattern was found.  Defaults to C<False>.

=head2 -o or --only-matching

Indicate whether only the matched pattern should be produced, rather than
the line in which the pattern was found.  Defaults to C<False>.

=head2 --output-file

Indicate the path of the file in which the result of the search should
be placed.  Defaults to C<STDOUT>.

=head2 --trim

Indicate whether lines that have the pattern, should have any whitespace
at the start and/or end of the line removed.  Defaults to C<True> if no
context for lines was specified, else defaults to C<False>.

=head2 -V or --version

If the only argument, shows the name and version of the script, and the
system it is running on.

=head1 AUTHOR

Elizabeth Mattijsen <liz@raku.rocks>

Source can be located at: https://github.com/lizmat/App-Rak .
Comments and Pull Requests are welcome.

If you like this module, or what I’m doing more generally, committing to a
L<small sponsorship|https://github.com/sponsors/lizmat/>  would mean a great
deal to me!

=head1 COPYRIGHT AND LICENSE

Copyright 2022 Elizabeth Mattijsen

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod

# vim: expandtab shiftwidth=4
