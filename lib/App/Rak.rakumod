# The modules that we need here, with their full identities
use highlighter:ver<0.0.9>:auth<zef:lizmat>;
use Files::Containing:ver<0.0.10>:auth<zef:lizmat>;
use as-cli-arguments:ver<0.0.3>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.2>:auth<zef:lizmat>;
use JSON::Fast:ver<0.17>:auth<cpan:TIMOTIMO>;

# Defaults for highlighting on terminals
my constant BON  = "\e[1m";   # BOLD ON
my constant BOFF = "\e[22m";  # RESET

# Make sure we remember if there's a human watching (terminal connected)
my $isa-tty := $*OUT.t;

my constant @raku-extensions = <
   raku rakumod rakutest nqp t pm6 pl6
>;

# Place to keep tagged configurations
my $config-file := $*HOME.add('.rak-config.json');
my %config;
my sub load-config() {
    %config := from-json($config-file.slurp) if $config-file.e;
    %config
}

# Sane way of quitting
my sub meh($message) { exit note $message }

# Quit if unexpected named arguments hash
my sub meh-if-unexpected(%_) {
    meh "Unexpected arguments: &as-cli-arguments(%_)" if %_;
}

# Is a needle a simple Callable?
my sub is-simple-Callable($needle) {
    Callable.ACCEPTS($needle) && !Regex.ACCEPTS($needle)
}

# Process all alternate names / values into a single value and remove it
my sub named-arg(%_, *@names) {
    return %_.DELETE-KEY($_) if %_.EXISTS-KEY($_) for @names;
    Nil
}

# Process all alternate names / values into a Map and remove them
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

# Add any lines before / after in a result
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

# Make sure we can do -V --version
use CLI::Version:ver<0.0.3>:auth<zef:lizmat>
  $?DISTRIBUTION,
  my proto sub MAIN(|) is export {*}

# Processing "save" and "list-tags" requests
my multi sub MAIN(*%n) {  # *%_ causes compilation issues
    # Saving config
    if %n<save>:delete -> $tag {
        load-config;
        %n ?? (%config{$tag} := %n) !! (%config{$tag}:delete);
        $config-file.spurt: to-json %config, :!pretty, :sorted-keys;
        say (%n ?? "Saved" !! "Removed") ~ " configuration for '$tag'";
        exit;
    }
    # Show what we have
    elsif %n<list-tags>:delete {
        meh-if-unexpected(%n);

        load-config;
        my $format := '%' ~ %config.keys>>.chars.max ~ 's: ';
        say sprintf($format,.key) ~ as-cli-arguments(.value)
          for %config.sort(*.key.fc);
        exit;
    }
    meh "Must at least specify a pattern";
}

# The main processor
my multi sub MAIN($needle, *@specs, *%n) {  # *%_ causes compilation issues
    meh "Saving pattern and/or paths not supported" if %n<save>:delete;

    # Running one or more configs
    if %n<with>:delete -> $with {
        my @not-found;
        load-config;
        for $with.split(',') -> $tag {
            if %config{$tag} -> %adding {
                %n{.key} = .value for %adding;
            }
            else {
                @not-found.push: $tag;
            }
        }
        meh "Attempt to add named arguments from unknown tag(s): @not-found[]" if @not-found;
    }

    if $needle.starts-with('/') && $needle.ends-with('/') {
        $needle .= EVAL;
    }
    elsif $needle.starts-with('{') && $needle.ends-with('}') {
        $needle = ('-> $_ ' ~ $needle).EVAL;
    }
    elsif $needle.starts-with('*.') {
        $needle = $needle.EVAL;
    }

    temp $*OUT;
    with named-arg %n, <output-file> -> $path {
        $*OUT = open($path, :w) if $path ne "-";
    }

    my $root := @specs.head;
    unless $*IN.t {
        meh "Specified '$root' while reading from STDIN"
          if $root && $root ne '-';
        NYI "Under construction";
    }

    @specs.unshift(".") without $root;
    my %additional := named-args %n, :follow-symlinks<S>, :file :dir;
    my @paths = (@specs == 1
      ?? paths(@specs.head, |%additional)
      !! @specs.&hyperize(1, %n<degree>).map({ paths($_, |%additional).Slip })
    ).sort(*.fc);

    %n<edit>:delete
      ?? go-edit-files($needle, @paths, %n)
      !! is-simple-Callable($needle) && (%n<replace-files>:delete)
        ?? replace-files($needle, @paths, %n)
        !! named-arg(%n, <l files-only files-with-matches>)
          ?? files-only($needle, @paths, %n)
          !! want-lines($needle, @paths, %n);
}

my sub go-edit-files($needle, @paths, %_ --> Nil) {
    my $files-only := named-arg  %_, <l files-only files-with-matches>;
    my %ignore := named-args %_,
      :ignorecase<i ignore-case>,
      :ignoremark<m ignore-mark>,
    ;
    my %additional = |(named-args %_, :batch, :degree, :max-count), |%ignore;

    meh-if-unexpected(%_);

    edit-files $files-only
      ?? files-containing($needle, @paths, :files-only, |%additional)
      !! files-containing($needle, @paths, |%additional).map: {
             my $path := .key;
             .value.map({
                 $path => .key + 1 => columns(.value, $needle, |%ignore).head
             }).Slip
         }
}

my sub replace-files($needle, @paths, %_ --> Nil) {
    NYI "replace-files: under construction";
}

my sub files-only($needle, @paths, %_ --> Nil) {
    my %additional := named-args %_,
      :ignorecase<i ignore-case>,
      :ignoremark<m ignore-mark>,
      :invert-match<v>,
      :batch,
      :degree,
    ;
    meh-if-unexpected(%_);

    .relative.say
      for files-containing $needle, @paths, :files-only, |%additional;
}

my sub want-lines($needle, @paths, %_ --> Nil) {
    my $ignorecase := named-arg %_, <i ignorecase ignore-case>;
    my $ignoremark := named-arg %_, <m ignoremark ignore-mark>;
    my $seq := files-containing
      $needle, @paths, :$ignorecase, :$ignoremark, :offset(1),
      |named-args %_, :invert-match<v>, :max-count, :batch, :degree,
    ;

    my UInt() $before = $_ with named-arg %_, <B before before-context>;
    my UInt() $after  = $_ with named-arg %_, <A after after-context>;
    $before = $after  = $_ with named-arg %_, <C context>;
    $before = 0 without $before;
    $after  = 0 without $after;

    my Bool() $line-number;
    my Bool() $highlight;
    my Bool() $trim;
    my Bool() $no-filename;
    my Bool() $only;
    my Int()  $summary-if-larger-than;

    my $human := %_<human>:delete // $isa-tty;
    if $human {
        $highlight = !is-simple-Callable($needle);
        $no-filename = $only = False;
        $trim = !($before || $after);
        $summary-if-larger-than = 160;
    }

    $highlight  = $_ with named-arg %_, <highlight>;
    $trim       = $_ with named-arg %_, <trim>;
    $only       = $_ with named-arg %_, <o only-matching>;
    $before = $after = 0 if $only;
    $summary-if-larger-than = $_
      with named-arg %_, <sum summary-if-larger-than>;

    my &show-line;
    if $highlight {
        my Str() $pre = my Str() $post = named-arg(%_, <highlight-before>);
        $post = $_ with named-arg %_, <highlight-after>;
        $pre  = $only ?? " " !! BON  without $pre;
        $post = $only ?? ""  !! BOFF without $post;

        &show-line = $trim 
          ?? -> $line {
                 highlighter $line.trim, $needle, $pre, $post,
                 :$ignorecase, :$ignoremark, :$only,
                 :$summary-if-larger-than
             }
          !! -> $line {
                 highlighter $line, $needle, $pre, $post,
                 :$ignorecase, :$ignoremark, :$only,
                 :$summary-if-larger-than
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

    # some twisted historical logic
    $no-filename = $_ with named-arg %_, <h no-filename>;
    $no-filename = True without $no-filename;
    $line-number = $_ with named-arg %_, <n line-number>;
    without $line-number {
        $line-number = !$no-filename if $human;
    }

    meh-if-unexpected(%_);

    my int $nr-files;
    my $before-or-after := $before || $after;

    for $seq {
        say "" if $human && $nr-files++;

        my $io := .key;
        say $io.relative unless $no-filename;

        if $before-or-after {
            my @selected := add-before-after($io, .value, $before, $after);
            if $line-number {
                my $format := '%' ~ (@selected.tail.key.chars + 1) ~ 'd: ';
                say sprintf($format, .key) ~ show-line .value for @selected;
            }
            else {
                say show-line .value for @selected;
            }
        }
        else {
            if $line-number {
                my $format := '%' ~ (.value.tail.key.chars + 1) ~ 'd: ';
                say sprintf($format, .key) ~ show-line .value for .value;
            }
            else {
                say show-line .value for .value;
            }
        }
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

=head2 path(s)

Optional.  Either indicates the path of the directory (and its
sub-directories), or the file that will be searched.  By default, all
directories that do not start with a period, will be recursed into (but
this can be changed with the C<--dir> named argument).

By default, all files will be searched in the directories.  This can be
changed with the C<--file> named argument.

=head1 SUPPORTED NAMED ARGUMENTS

All named arguments are optional.  Any unexpected named arguments, will
cause an exception with the unexpected named arguments listed.

=head2 -A  --after  --after-context

Indicate the number of lines that should be shown B<after> any line that
matches.  Defaults to B<0>.  Will be overridden by a C<-C> or C<--context>
argument.

=head2 -B  --before  --before-context

Indicate the number of lines that should be shown B<before> any line that
matches.  Defaults to B<0>.  Will be overridden by a C<-C> or C<--context>
argument.

=head2 -C  --context

Indicate the number of lines that should be shown B<around> any line that
matches.  Defaults to B<0>.  Overrides any a C<-A>, C<--after>,
C<--after-context>, C<-B>, C<--before> or C<--before-context> argument.
argument.

=head2 --edit

Indicate whether the patterns found should be fed into an editor for
inspection and/or changes.  Defaults to C<False>.

=head2 -h --no-filename

Indicate whether filenames should B<not> be shown.  Defaults to C<False> if
C<--human> is (implicitely) set to C<True>, else defaults to C<True>.

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

=head2 -l  --files-only  --files-with-matches

If specified with a true value, will only produce the filenames of the
files in which the pattern was found.  Defaults to C<False>.

=head2 --list-tags

=begin code :lang<bash>

$ rak --list-tags
fs: --'follow-symlinks'
im: --ignorecase --ignoremark

=end code

If specified with a true value and as the only named argument, will list
all saved tags.

=head2 -n --line-number

Indicate whether line numbers should be shown.  Defaults to C<True> if
C<--human> is (implicitely) set to C<True> and <-h> is B<not> set to C<True>,
else defaults to C<False>.

=head2 -o  --only-matching

Indicate whether only the matched pattern should be produced, rather than
the line in which the pattern was found.  Defaults to C<False>.

=head2 --output-file

Indicate the path of the file in which the result of the search should
be placed.  Defaults to C<STDOUT>.

=head2 --replace-files

Only makes sense if the specified pattern is a C<Callable>.  Indicates
whether the output of the pattern should be applied to the file in which
it was found.  Defaults to C<False>.

=head2 --save

Save all named arguments with the given tag in the configuration file
(C<~/.rak-config.json>), and exit with a message that the named arguments
have been saved with the given tag.

This feature can used to both create shortcuts for specific (long) named
arguments, or just as a convenient way to combine often used named arguments.

=begin code :lang<bash>

$ rak --ignorecase --ignoremark --save=im
Saved configuration for 'im'

$ rak --follow-symlinks --save=fs
Saved configuration for 'fs'

$ rak --save=foo
Removed configuration for 'foo'

=end code

See C<--with> to add saved named arguments to a query.  Please note that
no validity checking on the named arguments is being performed at the
moment of saving, as validity may depend on other arguments having been
specified.

To remove a saved set of named arguments, use C<--save> as the only argument.

=head2 --sum  --summary-if-larger-than

Indicate the maximum size a line may have before it will be summarized.
Defaults to C<160> if C<STDOUT> is a TTY (aka, someone is actually watching
the search results), otherwise defaults to C<Inf> effectively (indicating
no summarization will ever occur).

=head2 -S --follow-symlinks

Indicate whether symbolic links to directories should be followed.  Defaults
to C<False>.

=head2 --trim

Indicate whether lines that have the pattern, should have any whitespace
at the start and/or end of the line removed.  Defaults to C<True> if no
context for lines was specified, else defaults to C<False>.

=head2 -V  --version

If the only argument, shows the name and version of the script, and the
system it is running on.

=head2 --with

=begin code :lang<bash>

# run search with --ignorecase --ignoremark --follow-symlinks
$ rak foo --with=im,fs

=end code

Add all named arguments previously saved with C<--save> with the given tag(s)
from the configuration file (C<~/.rak-config.json>).  Multiple tags can be
specified, separated by commas.  See C<--save> to saved named arguments with
a tag.

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
