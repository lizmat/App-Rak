# The modules that we need here, with their full identities
use highlighter:ver<0.0.12>:auth<zef:lizmat>;
use Files::Containing:ver<0.0.13>:auth<zef:lizmat>;
use as-cli-arguments:ver<0.0.3>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;
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

# Return string before marker, or string if no marker
my sub before(Str:D $string, Str:D $marker) {
    with $string.index($marker) {
        $string.substr(0,$_)
    }
    else {
        $string
    }
}

# Return named variables in order of specification on the command line
my sub original-nameds() {
    @*ARGS.map: {
        .starts-with('--/')
          ?? before(.substr(3), '=')
          !! .starts-with('--' | '-/')
            ?? before(.substr(2), '=')
            !! .starts-with('-')
              ?? before(.substr(1), '=')
              !! Empty
    }
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
role delimiter { has $.delimiter }
my sub add-before-after($io, @initially-selected, int $before, int $after) {
    my str @lines = $io.lines(:enc<utf8-c8>);
    @lines.unshift: "";   # make 1-base indexing natural
    my int $last-linenr = @lines.end;

    my int8 @seen;
    my @selected;
    for @initially-selected {
        my int $linenr = .key;
        if $before {
            for max($linenr - $before, 1) ..^ $linenr -> int $_ {
                @selected.push:
                  Pair.new($_, @lines.AT-POS($_) but delimiter('-'))
                  unless @seen.AT-POS($_)++;
            }
        }

        @selected.push: Pair.new(.key, .value but delimiter(':'))
          unless @seen.AT-POS($linenr)++;

        if $after {
            for $linenr ^.. min($linenr + $after, $last-linenr ) -> int $_ {
                @selected.push:
                  Pair.new($_, @lines.AT-POS($_) but delimiter('-'))
                  unless @seen.AT-POS($_)++;
            }
        }
    }

    @selected
}

# Set up the --help handler
use META::constants:ver<0.0.2>:auth<zef:lizmat> $?DISTRIBUTION;
my sub HELP($text, @keys, :$verbose) {
    my $SCRIPT := $*PROGRAM.basename;
    my $header := "$SCRIPT - " ~ DESCRIPTION;
    say $header;
    say "-" x $header.chars;
    say $isa-tty
      ?? $text.lines.map({
              !.starts-with(" ") && .ends-with(":") ?? BON ~ $_ ~ BOFF !! $_
         }).join("\n")
      !! $text;

    if $verbose {
        say "";
        say CREDITS;
        say "";
        say "Thank you for using $SCRIPT!";
    }
}

# Allow --no-foo as an alternative to --/foo
$_ = .subst(/^ '--' no '-' /, '--/') for @*ARGS;

# Entry point for CLI processing
my proto sub MAIN(|) is export {*}

# Make sure we can do --help and --version
use CLI::Version:ver<0.0.4>:auth<zef:lizmat>  $?DISTRIBUTION, &MAIN, 'long';
use CLI::Help:ver<0.0.3>:auth<zef:lizmat> %?RESOURCES, &MAIN, &HELP, 'long';

# Main handler
my multi sub MAIN(*@specs, *%n) {  # *%_ causes compilation issues
    my %config := $config-file.e ?? from-json($config-file.slurp) !! { }

    # Saving config
    if %n<save>:delete -> $option {
        if %n {
            if %n.grep({
                $_ eq '!' || (.starts-with('[') && .ends-with(']')) with .value
            }) -> @reps { 
                meh "Can only have one option with replacement: @reps.map({
                    '"' ~ .key ~ '"'
                }).join(", ") were given" if @reps > 1;
            }
            %config{$option} := %n;
        }
        else {
            %config{$option}:delete;
        }
        $config-file.spurt: to-json %config, :!pretty, :sorted-keys;
        say %n
          ?? "Saved option '--$option' as: " ~ as-cli-arguments(%n)
          !! "Removed option '--$option'";
        exit;
    }

    # Show what we have
    elsif %n<list-custom-options>:delete {
        meh-if-unexpected(%n);

        my $format := '%' ~ %config.keys>>.chars.max ~ 's: ';
        for %config.sort(*.key.fc) -> (:$key, :value(%args)) {
            say sprintf($format,$key) ~ as-cli-arguments(%args);
        }
        exit;
    }

    my sub is-default($value) {
        $value.starts-with('[') && $value.ends-with(']')
    }

    # Recursively translate any custom parameters
    my sub translate($option, $original-value) {
        if %config{$option} -> %adding {
            %n{$option}:delete;

            # no specific value given
            if Bool.ACCEPTS($original-value) {

                # activate option
                if $original-value {
                    for %adding -> (:$key, :$value) {
                        $value eq '!'
                          ?? meh("Must specify a value for $option for $key")
                          !! translate(
                               $key,
                               is-default($value)
                                 ?? $value.substr(1, *-1)
                                 !! $value
                             );
                    }
                }
                # de-activate option
                else {
                    %n{.key}:delete for %adding;
                }
            }

            # some specific value given
            else {
                for %adding -> (:$key, :$value) {
                    translate(
                      $key,
                      $value eq '!' || is-default($value)
                        ?? $original-value
                        !! $value
                    )
                }
            }
        }
        elsif %n{$option}:!exists {
            %n{$option} = $original-value;
        }
    }
    translate($_, %n{$_}) for original-nameds;

    my $needle = %n<pattern>:delete // @specs.shift;
    meh "Must at least specify a pattern" without $needle;

    # Return prelude from --repository and --module parameters
    my sub prelude() {
        my $prelude = "";
        if %n<I>:delete -> \libs {
            $prelude = libs.map({"use lib '$_'; "}).join;
        }
        if %n<M>:delete -> \modules {
            $prelude ~= modules.map({"use $_; "}).join;
        }
        $prelude
    }

    # Pre-process non literal string needles
    if $needle.starts-with('/') && $needle.ends-with('/') {
        $needle .= EVAL;
    }
    elsif $needle.starts-with('{') && $needle.ends-with('}') {
        $needle = (prelude() ~ 'my $ = -> $_ ' ~ $needle).EVAL;
    }
    elsif $needle.starts-with('*.') {
        $needle = (prelude() ~ $needle).EVAL;
    }
    my $is-simple-Callable := is-simple-Callable($needle);

    temp $*OUT;
    with %n<output-file>:delete -> $path {
        $*OUT = open($path, :w) if $path ne "-";
    }

    # Not reading from STDIN
    if $*IN.t {
        @specs.unshift(".") unless @specs;
        my %additional := named-args %n, :follow-symlinks, :file :dir;
        my @paths = (@specs == 1
          ?? paths(@specs.head, |%additional)
          !! @specs.&hyperize(1, %n<degree>).map({ paths($_, |%additional).Slip })
        ).sort(*.fc);

        if %n<edit>:delete -> $editor {
            go-edit-files($editor, $needle, @paths, %n);
        }
        else {
            ($is-simple-Callable && (%n<modify-files>:delete)
              ?? &modify-files
              !! $is-simple-Callable && (%n<json-per-file>:delete)
                ?? &produce-json-per-file
                !! $is-simple-Callable && (%n<json-per-line>:delete)
                  ?? &produce-json-per-line
                  !! (%n<count-only>:delete)
                    ?? &count-only
                    !! (%n<files-with-matches>:delete)
                      ?? &files-only
                      !! &want-lines
            )($needle, @paths, %n);
        }
    }

    # Reading from STDIN
    else {
        my $root := @specs.head;
        meh "Specified '$root' while reading from STDIN"
          if $root && $root ne '-';
        meh "Can not specify paths while reading from STDIN"
          if @specs > 1;
        $is-simple-Callable && (%n<json-per-line>:delete)
          ?? stdin-json-per-line($needle, %n)
          !! stdin($needle, %n)
    }
}

# Edit / Inspect some files
my sub go-edit-files($editor, $needle, @paths, %_ --> Nil) {
    CATCH { meh .message }

    my $files-with-matches := %_<files-with-matches>:delete;
    my %ignore             := named-args %_, :ignorecase :ignoremark;
    my %additional =
      |(named-args %_, :max-count, :type, :batch, :degree),
      |%ignore;
    meh-if-unexpected(%_);

    edit-files ($files-with-matches
      ?? files-containing($needle, @paths, :files-only, |%additional)
      !! files-containing($needle, @paths, |%additional).map: {
             my $path := .key;
             .value.map({
                 $path => .key + 1 => columns(.value, $needle, |%ignore).head
             }).Slip
         }),
      :editor(Bool.ACCEPTS($editor) ?? Any !! $editor)
}

my sub s($elems) { $elems == 1 ?? "" !! "s" }

# Replace contents of files
my sub modify-files($needle, @paths, %_ --> Nil) {
    my $batch   := %_<batch>:delete;
    my $degree  := %_<degree>:delete;
    my $dryrun  := %_<dryrun>:delete;
    my $verbose := %_<verbose>:delete;

    my $backup = %_<backup>:delete;
    $backup = ".bak" if $backup<> =:= True;
    $backup = ".$backup" if $backup && !$backup.starts-with('.');

    meh-if-unexpected(%_);

    my @files-changed;
    my int $nr-changed;
    my int $nr-removed;

    @paths.&hyperize($batch, $degree).map: -> $path {
        my str @lines;
        my int $lines-changed;
        my int $lines-removed;

        my $io := $path.IO;
        for $io.slurp.lines(:!chomp) {
            my $result := $needle($_);
            if $result =:= True || $result =:= Empty {
                @lines.push: $_;
            }
            elsif $result =:= False {
                ++$lines-removed;
            }
            elsif $result eq $_ {
                @lines.push: $_;
            }
            else {
                @lines.push: $result.join;
                ++$lines-changed;
            }
        }
        if $lines-changed || $lines-removed {
            unless $dryrun {
                if $backup {
                    $io.spurt(@lines.join)
                      if $io.rename($io.sibling($io.basename ~ $backup));
                }
                else {
                    $io.spurt: @lines.join;
                }
            }
            @files-changed.push: ($io, $lines-changed, $lines-removed);
            $nr-changed += $lines-changed;
            $nr-removed += $lines-removed;
        }
    }

    my $nr-files = @files-changed.elems;
    my $fb = "Processed @paths.elems() file&s(@paths.elems)";
    $fb ~= ", $nr-files file&s($nr-files) changed"     if $nr-files;
    $fb ~= ", $nr-changed line&s($nr-changed) changed" if $nr-changed;
    $fb ~= ", $nr-removed line&s($nr-removed) removed" if $nr-removed;

    if $verbose {
        $fb ~= "\n";
        for @files-changed -> ($io, $nr-changed, $nr-removed) {
            $fb ~= "$io.relative():";
            $fb ~= " $nr-changed changes" if $nr-changed;
            $fb ~= " $nr-removed removals" if $nr-removed;
            $fb ~= "\n";
        }
        $fb ~= "*** no changes where made because of --dryrun ***\n"
          if $dryrun;
        $fb .= chomp;
    }
    elsif $dryrun {
        $fb ~= "\n*** no changes where made because of --dryrun ***";
    }

    say $fb;
}

# Produce JSON per file to check
my sub produce-json-per-file(&needle, @paths, %_ --> Nil) {
    my $batch         := %_<batch>:delete;
    my $degree        := %_<degree>:delete;
    my $show-filename := %_<show-filename>:delete // True;
    meh-if-unexpected(%_);

    say $_ for @paths.&hyperize($batch, $degree).map: {
        my $io := .IO;

        if try from-json $io.slurp -> $json {
            if needle($json) -> \result {
                my $filename := $io.relative;
                result =:= True
                  ?? $filename
                  !! $show-filename
                    ?? "$filename: " ~ result
                    !! result
            }
        }
    }
}

# Produce JSON per line to check
my sub produce-json-per-line(&needle, @paths, %_ --> Nil) {
    my $batch         := %_<batch>:delete;
    my $degree        := %_<degree>:delete;
    my $show-filename := %_<show-filename>:delete // True;

    if %_<count-only>:delete {
        meh-if-unexpected(%_);
        my int $total;

        say $_ for @paths.&hyperize($batch, $degree).map: {
            my $io := .IO;
            my int $found;

            for $io.lines -> $line {
                if try from-json $line -> $json {
                    ++$found if needle($json);
                }
            }

            $total += $found;
            "$io.relative(): $found" if $show-filename;
        }
        say $total;
    }

    else {
        my $show-line-number := %_<show-line-number>:delete // True;
        meh-if-unexpected(%_);

        say $_ for @paths.&hyperize($batch, $degree).map: {
            my $io := .IO;
            my int $line-number;

            $io.lines.map(-> $line {
                ++$line-number;
                if try from-json $line -> $json {
                    if needle($json) -> \result {
                        my $filename := $io.relative;
                        my $mess     := result =:= True ?? '' !! ': ' ~ result;
                        $show-filename
                          ?? $show-line-number
                            ?? "$filename:$line-number$mess"
                            !! "$filename$mess"
                          !! $show-line-number
                            ?? "$line-number$mess"
                            !! $mess
                    }
                }
            }).Slip
        }
    }
}

# Only count matches
my sub count-only($needle, @paths, %_ --> Nil) {
    my $files-with-matches := %_<files-with-matches>:delete;
    my %additional := named-args %_,
      :ignorecase, :ignoremark, :invert-match, :type, :batch, :degree;
    meh-if-unexpected(%_);

    my int $files;
    my int $matches;
    for files-containing $needle, @paths, :count-only, |%additional {
        ++$files;
        $matches += .value;
        say .key.relative ~ ': ' ~ .value if $files-with-matches;
    }
    say "$matches matches in $files files";
}

# Only show filenames
my sub files-only($needle, @paths, %_ --> Nil) {
    my $nl := %_<file-separator-null>:delete ?? "\0" !! $*OUT.nl-out;
    my %additional := named-args %_,
      :ignorecase, :ignoremark, :invert-match, :type, :batch, :degree;
    meh-if-unexpected(%_);

    print .relative ~ $nl
      for files-containing $needle, @paths, :files-only, |%additional;
}

# Show lines with highlighting and context
my sub want-lines($needle, @paths, %_ --> Nil) {
    my $ignorecase := %_<ignorecase>:delete;
    my $ignoremark := %_<ignoremark>:delete;
    my $seq := files-containing
      $needle, @paths, :$ignorecase, :$ignoremark, :offset(1),
      |named-args %_, :invert-match, :max-count, :type, :batch, :degree,
    ;

    my UInt() $before = $_ with %_<before-context>:delete;
    my UInt() $after  = $_ with %_<after-context>:delete;
    $before = $after  = $_ with %_<context>:delete;
    $before = 0 without $before;
    $after  = 0 without $after;

    my Bool() $highlight;
    my Bool() $trim;
    my        $break;
    my Bool() $group-matches;
    my Bool() $show-filename;
    my Bool() $show-line-number;
    my Bool() $only;
    my Int()  $summary-if-larger-than;

    my $human := %_<human>:delete // $isa-tty;
    if $human {
        $highlight = !is-simple-Callable($needle);
        $break = $group-matches = $show-filename = $show-line-number = True;
        $only = False;
        $trim = !($before || $after || is-simple-Callable($needle));
        $summary-if-larger-than = 160;
    }

    $highlight = $_ with %_<highlight>:delete;
    $trim      = $_ with %_<trim>:delete;
    $only      = $_ with %_<only-matching>:delete;
    $before = $after = 0 if $only;
    $summary-if-larger-than = $_ with %_<summary-if-larger-than>:delete;

    my &show-line;
    if $highlight {
        my Str() $pre = my Str() $post = $_ with %_<highlight-before>:delete;
        $post = $_ with %_<highlight-after>:delete;
        $pre  = $only ?? " " !! BON  without $pre;
        $post = $only ?? ""  !! BOFF without $post;

        &show-line = $trim
          ?? -> $line {
                 highlighter $line.trim, $needle<>, $pre, $post,
                 :$ignorecase, :$ignoremark, :$only,
                 :$summary-if-larger-than
             }
          !! -> $line {
                 highlighter $line, $needle<>, $pre, $post,
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

    $break            = $_ with %_<break>:delete;
    $group-matches    = $_ with %_<group-matches>:delete;
    $show-filename    = $_ with %_<show-filename>:delete;
    $show-line-number = $_ with %_<show-line-number>:delete;
    meh-if-unexpected(%_);

    unless $break<> =:= False  {
        $break = "" but True
          if Bool.ACCEPTS($break) || ($break.defined && !$break);
    }
    my $before-or-after := $before || $after;

    my $show-header = $show-filename && $group-matches;
    $show-filename  = False if $show-header;
    my int $nr-files;

    for $seq -> (:key($io), :value(@matches)) {
        say $break if $break && $nr-files++;

        my str $filename = $io.relative;
        say $filename if $show-header;

        if $before-or-after {
            my @selected := add-before-after($io, @matches, $before, $after);
            my $format := '%' ~ (@selected.tail.key.chars) ~ 'd';
            if $show-line-number {
                for @selected {
                    my str $delimiter = .value.delimiter;
                    say ($show-filename ?? $filename ~ $delimiter !! '')
                      ~ sprintf($format, .key)
                      ~ $delimiter
                      ~ show-line(.value);
                }
            }
            elsif $show-filename {
                say $filename ~ .value.delimiter ~ show-line(.value)
                  for @selected;
            }
            else {
                say show-line(.value) for @selected;
            }
        }
        else {
            if $show-line-number {
                my $format := '%' ~ (@matches.tail.key.chars) ~ 'd:';
                for @matches {
                    say ($show-filename ?? $filename ~ ':' !! '')
                      ~ sprintf($format, .key)
                      ~ show-line(.value);
                }
            }
            elsif $show-filename {
                say $filename ~ ':' ~ show-line(.value) for @matches;
            }
            else {
                say show-line(.value) for @matches;
            }
        }
    }
}

my sub stdin-json-per-line(&needle, %_ --> Nil) {
    my $count-only       := %_<count-only>:delete;
    my $show-line-number := %_<show-line-number>:delete;
    meh-if-unexpected(%_);

    my int $line-number;
    my int $matches;
    for $*IN.lines -> $line {
        ++$line-number;
        if try from-json $line -> $json {
            if needle($json) -> \result {
                $count-only
                  ?? ++$matches
                  !! result =:= True
                    ?? say($line-number)
                    !! $show-line-number
                      ?? say($line-number ~ ': ' ~ result)
                      !! say(result)
            }
        }
    }
    say $matches if $count-only;
}

my sub stdin($needle, %_ --> Nil) {
    my Bool() $highlight;
    my Bool() $trim;
    my Bool() $show-line-number;
    my Bool() $only;
    my Int()  $summary-if-larger-than;

    my UInt() $before = $_ with %_<before-context>:delete;
    my UInt() $after  = $_ with %_<after-context>:delete;
    $before = $after  = $_ with %_<context>:delete;
    $before = 0 without $before;
    $after  = 0 without $after;

    my $human := %_<human>:delete // $isa-tty;
    if $human {
        $highlight = !is-simple-Callable($needle);
        $show-line-number = True;
        $only = False;
        $trim = !($before || $after || is-simple-Callable($needle));
        $summary-if-larger-than = 160;
    }

    $highlight = $_ with %_<highlight>:delete;
    $trim      = $_ with %_<trim>:delete;
    $only      = $_ with %_<only-matching>:delete;
    $before = $after = 0 if $only;
    $show-line-number       = $_ with %_<show-line-number>:delete;
    $summary-if-larger-than = $_ with %_<summary-if-larger-than>:delete;

    my $ignorecase := %_<ignorecase>:delete;
    my $ignoremark := %_<ignoremark>:delete;
    my &show-line;
    if $highlight {
        my Str() $pre = my Str() $post = $_ with %_<highlight-before>:delete;
        $post = $_ with %_<highlight-after>:delete;
        $pre  = $only ?? " " !! BON  without $pre;
        $post = $only ?? ""  !! BOFF without $post;

        &show-line = $trim
          ?? -> $line {
                 highlighter $line.trim, $needle<>, $pre, $post,
                 :$ignorecase, :$ignoremark, :$only,
                 :$summary-if-larger-than
             }
          !! -> $line {
                 highlighter $line, $needle<>, $pre, $post,
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

    my &matcher;
    if Callable.ACCEPTS($needle) {
        &matcher = Regex.ACCEPTS($needle)
          ?? { $needle.ACCEPTS($_) }
          !! $needle
    }
    else {
        my $type := %_<type>:delete // 'contains';
        &matcher  = $type eq 'words'
          ?? *.&has-word($needle, :$ignorecase, :$ignoremark)
          !! $type eq 'starts-with'
            ?? *.starts-with($needle, :$ignorecase, :$ignoremark)
            !! $type eq 'ends-with'
              ?? *.ends-with($needle, :$ignorecase, :$ignoremark)
              !! *.contains($needle, :$ignorecase, :$ignoremark);
    }
    meh-if-unexpected(%_);

    my int $line-number;
    my int $todo-after;
    my str @before;
    for $*IN.lines -> $line {
        ++$line-number;
        if matcher($line) -> \result {
            say @before.shift while @before;
            my $text := result =:= True ?? show-line($line) !! result;
            say $show-line-number ?? ($line-number ~ ':' ~ $text) !! $text;
            $todo-after = $after;
        }
        elsif $todo-after {
            say $show-line-number
              ?? $line-number ~ ':' ~ $line
              !! $line;
            --$todo-after;
        }
        elsif $before {
            @before.shift if @before.elems == $before;
            @before.push: $show-line-number
              ?? $line-number ~ ':' ~ $line
              !! $line;
        }
    }
}

=begin pod

=head1 NAME

App::Rak - a CLI for searching strings in files and more

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

The pattern to search for.  This can either be a string, or a
L<Raku regular expression|https://docs.raku.org/language/regexes>
(indicated by a string starting and ending with C</>), a
C<Callable> (indicated by a string starting with C<{> and ending with C<}>),
or a a result of L<C<Whatever> currying|https://docs.raku.org/type/Whatever>
(indicated by a string starting with C<*.>).

Can also be specified with the C<--pattern> option, in which case B<all>
the positional arguments are considered to be a path specification.

=head2 path(s)

Optional.  Either indicates the path of the directory (and its
sub-directories), or the file that will be searched.  By default, all
directories that do not start with a period, will be recursed into (but
this can be changed with the C<--dir> option).

By default, all files will be searched in the directories.  This can be
changed with the C<--file> option

=head1 SUPPORTED OPTIONS

All options are optional.  Any unexpected options, will cause an exception
to be thrown with the unexpected options listed.

=head2 --after-context=N

Indicate the number of lines that should be shown B<after> any line that
matches.  Defaults to B<0>.  Will be overridden by a C<--context> argument.

=head2 --backup[=extension]

Indicate whether backups should be made of files that are being modified.
If specified without extension, the extension C<.bak> will be used.

=head2 --before-context=N

Indicate the number of lines that should be shown B<before> any line that
matches.  Defaults to B<0>.  Will be overridden by a C<--context> argument.

=head2 --break[=string]

Indicate whether there should be a visible division between matches of
different files.  Can also be specified as a string to be used as the
divider.  Defaults to C<True> (using an empty line as a divider) if
C<--human> is (implicitly) set to C<True>, else defaults to C<False>.

=head2 --context=N

Indicate the number of lines that should be shown B<around> any line that
matches.  Defaults to B<0>.  Overrides any a C<--after-context> or
C<--before-context> arguments.

=head2 --count-only

Indicate whether just the number of lines with matches should be calculated.
When specified with a C<True> value, will show a "N matches in M files"
by default, and if the C<:files-with-matches> option is also specified with
a C<True> value, will also list the file names with their respective counts.

=head2 --dryrun

Indicate to B<not> actually make any changes to any content modification
if specified with a C<True> value.  Only makes sense in with the
C<--modify-files> option.

=head2 --edit[=editor]

Indicate whether the patterns found should be fed into an editor for
inspection and/or changes.  Defaults to C<False>.  Optionally takes the
name of the editor to be used.

=head2 --file-separator-null

Indicate to separate filenames by null bytes rather than newlines if the
C<--files-with-matches> option is specified with a C<True> value.

=head2 --group-matches

Indicate whether matches of a file should be grouped together by mentioning
the filename only once (instead of on every line).  Defaults to C<True> if
C<--human> is (implicitly) set to C<True>, else defaults to C<False>.

=head2 --highlight

Indicate whether the pattern should be highlighted in the line in which
it was found.  Defaults to C<True> if C<--human> is (implicitly) set to
C<True>, else defaults to C<False>.

=head2 --help [area-of-interest]

Show argument documentation, possibly extended by giving the area of
interest, which are:

=item pattern
=item string
=item code
=item input
=item haystack
=item result
=item listing
=item resource
=item edit
=item option
=item general
=item philosophy
=item examples

=head2 --highlight--after[=string]

Indicate the string that should be used at the end of the pattern found in
a line.  Only makes sense if C<--highlight> is (implicitly) set to C<True>.
Defaults to the empty string if C<--only-matching> is specified with a
C<True> value, or to the terminal code to end B<bold> otherwise.

=head2 --highlight--before[=string]

Indicate the string that should be used at the end of the pattern found in
a line.  Only makes sense if C<--highlight> is (implicitly) set to C<True>.
Defaults to a space if C<--only-matching> is specified with a C<True> value,
or to the terminal code to start B<bold> otherwise.

=head2 --human

Indicate that search results should be presented in a human readable
manner.  This means: filenames shown on a separate line, line numbers
shown, and highlighting performed.  Defaults to C<True> if C<STDOUT> is
a TTY (aka, someone is actually watching the search results), otherwise
defaults to C<False>.

=head2 --json-per-file

Only makes sense if the needle is a C<Callable>.  If specified with a
C<True> value, indicates that each selected file will be interpreted
as JSON, and if valid, will then be given to the needle for introspection.
If the Callable returns a true value, the filename will be shown.  If
the returned value is a string, that string will also be mentioned.
For example:

=begin code :lang<bash>

$ rak '{ $_ with .<auth> }' --json-per-file

=end code

=head2 --json-per-line

Only makes sense if the needle is a C<Callable>.  If specified with a
C<True> value, indicates that each line from the selected files will be
interpreted as JSON, and if valid, will then be given to the needle for
introspection.  If the Callable returns a true value, the filename and
line number will be shown.  If the returned value is a string, that
string will also be mentioned.  For example:

=begin code :lang<bash>

$ rak '{ $_ with .<auth> }' --json-per-line

=end code

=head2 --files-with-matches

If specified with a true value, will only produce the filenames of the
files in which the pattern was found.  Defaults to C<False>.

=head2 --list-custom-options

=begin code :lang<bash>

$ rak --list-custom-options
fs: --'follow-symlinks'
im: --ignorecase --ignoremark

=end code

If specified with a true value and as the only option, will list all
additional options previously saved with C<--save>.

=head2 --modify-files

Only makes sense if the specified pattern is a C<Callable>.  Indicates
whether the output of the pattern should be applied to the file in which
it was found.  Defaults to C<False>.

The C<Callable> will be called for each line, giving the line (B<including>
its line ending).  It is then up to the C<Callable> to return:

=head3 False

Remove this line from the file.  NOTE: this means the exact C<False> value.

=head3 True

Keep this line unchanged the file.  NOTE: this means the exact C<True> value.

=head3 Empty

Keep this line unchanged the file.  NOTE: this means the exact C<Empty> value.
This is typically returned as the result of a failed condition.  For example,
only change the string "foo" into "bar" if the line starts with "#":

=begin code :lang<bash>

$ rak '{ .subst("foo","bar") if .starts-with("#") }' --modify-files

=end code

=head3 any other value

Inserts this value in the file instead of the given line.  The value can
either be a string, or a list of strings.

=head2 --module=foo

Indicate the Raku module that should be loaded.  Only makes sense if the
pattern is executable code.

=head2 --only-matching

Indicate whether only the matched pattern should be produced, rather than
the line in which the pattern was found.  Defaults to C<False>.

=head2 --output-file=filename

Indicate the path of the file in which the result of the search should
be placed.  Defaults to C<STDOUT>.

=head2 --pattern=foo

Alternative way to specify the pattern to search for.  If (implicitly)
specified, will assume the first positional parameter specified is
actually a path specification, rather than a pattern.  This allows
the pattern to be searched for to be saved with C<--save>.

=head2 --repository=dir

Indicate the directory that should be searched for Raku module loading.
Only makes sense if the pattern is executable code.

Note that you can create a familiar shortcut for the most common arguments of
the C<--repository> option:

=begin code :lang<bash>

$ rak --repository=. --save=I.
Saved option '--I.' as: --repository='.'

$ rak --repository=lib --save=Ilib
Saved option '--Ilib' as: --repository=lib

=end code

=head2 --save=shortcut-name

Save all options with the given name in the configuration file
(C<~/.rak-config.json>), and exit with a message that these options have
been saved with the given name.

This feature can used to both create shortcuts for specific (long) options,
or just as a convenient way to combine often used options.

=begin code :lang<bash>

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

=end code

Any options can be accessed as if it is a standard option.  Please note
that no validity checking on the options is being performed at the moment
of saving, as validity may depend on other options having been specified.

One option can be marked as requiring a value to be specified (with "!")
or have a default value (with "[default-value]").

To remove a saved set of named arguments, use C<--save> as the only
named argument.

=head2 --show-filename

Indicate whether filenames should be shown.  Defaults to C<True> if
C<--human> is (implicitly) set to C<True>, else defaults to C<False>.

=head2 --show-line-number

Indicate whether line numbers should be shown.  Defaults to C<True> if
C<--human> is (implicitly) set to C<True> and <-h> is B<not> set to C<True>,
else defaults to C<False>.

=head2 --summary-if-larger-than=N

Indicate the maximum size a line may have before it will be summarized.
Defaults to C<160> if C<STDOUT> is a TTY (aka, someone is actually watching
the search results), otherwise defaults to C<Inf> effectively (indicating
no summarization will ever occur).

=item --type[=words|starts-with|ends-with|contains]

Only makes sense if the pattern is a string.  With C<words> specified,
will look for pattern as a word in a line, with C<starts-with> will
look for the pattern at the beginning of a line, with C<ends-with>
will look for the pattern at the end of a line, with C<contains> will
look for the pattern at any position in a line.

=head2 --follow-symlinks

Indicate whether symbolic links to directories should be followed.  Defaults
to C<False>.

=head2 --trim

Indicate whether lines that have the pattern, should have any whitespace
at the start and/or end of the line removed.  Defaults to C<True> if no
context for lines was specified, else defaults to C<False>.

=head2 --version

If the only argument, shows the name and version of the script, and the
system it is running on.

=head1 CREATING YOUR OWN OPTIONS

You can use the C<--save> option to save a set of options and than later
access them with the given name:

=begin code :lang<bash>

$ rak --ignorecase --ignoremark --save=im
Saved option '--im' as: --ignorecase --ignoremark

# same as --ignorecase --ignoremark
$ rak foo --im

=end code

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
