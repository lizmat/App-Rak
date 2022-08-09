# The modules that we need here, with their full identities
use highlighter:ver<0.0.12>:auth<zef:lizmat>;
use Files::Containing:ver<0.0.16>:auth<zef:lizmat>;
use as-cli-arguments:ver<0.0.4>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;
use Git::Blame::File:ver<0.0.5>:auth<zef:lizmat>;
use String::Utils:ver<0.0.8>:auth<zef:lizmat>;
use Trap:ver<0.0.1>:auth<zef:lizmat>;
use JSON::Fast:ver<0.17>:auth<cpan:TIMOTIMO>;

# Defaults for highlighting on terminals
my constant BON  = "\e[1m";   # BOLD ON
my constant BOFF = "\e[22m";  # RESET

# Make sure we remember if there's a human watching (terminal connected)
my $isa-tty := $*OUT.t;

# Set up default extension sets
my constant %exts =
  '#c'        => <c h hdl>,
  '#c++'      => <cpp cxx hpp hxx>,
  '#markdown' => <md markdown>,
  '#perl'     => ('', <pl pm t>).flat.List,
  '#python'   => <py>,
  '#raku'     => ('', <raku rakumod rakutest rakudoc nqp t pm6 pl6 pod6 t6>
                 ).flat.List,
  '#ruby'     => <rb>,
  '#text'     => ('', <txt>).flat.List,
  '#yaml'     => <yaml yml>,
  "#config"   => <ini>,
;

# Known extensions
my constant @known-extensions = %exts.values.flat.unique.sort;

# Place to keep tagged configurations
my $config-file := $*HOME.add('.rak-config.json');

# Add "s" if number is not 1, for error messages
my sub s($elems) { $elems == 1 ?? "" !! "s" }

# Sane way of quitting
my sub meh($message) is hidden-from-backtrace {
    $*REAL-MEH
      ?? exit note $message
      !! die $message
}

# Quit if unexpected named arguments hash
my sub meh-if-unexpected(%_) {
    %_{$_}:delete if %_{$_}<> =:= False for %_.keys;
    meh "Unexpected option{"s" if %_.elems != 1}: &as-cli-arguments(%_)\nUse --help for an overview of available options"
      if %_;
}

# Is a pattern a simple Callable?
my $is-simple-Callable;

# Can the pattern have phasers
my $can-have-phasers;

# Return string before marker, or string if no marker
my sub before-or-string(str $string, str $marker) {
    before($string, $marker) // $string
}

# Return named variables in order of specification on the command line
my sub original-nameds() {
    @*ARGS.map: {
        .starts-with('--/')
          ?? before-or-string(.substr(3), '=')
          !! .starts-with('--' | '-/')
            ?? before-or-string(.substr(2), '=')
            !! .starts-with('-') && $_ ne '-'
              ?? before-or-string(.substr(1), '=')
              !! Empty
    }
}

# Return extension of filename, if any
my sub extension(str $filename) {
    with rindex($filename, '.') {
        lc substr($filename, $_ + 1)
    }
    else {
        ""
    }
}

# Message for humans on STDERR
my sub human-on-stdin(--> Nil) {
    note "Reading from STDIN, please enter source and ^D when done:";
}

# Return object to call .lines on from STDIN
my sub stdin-source() {
    # handle humans
    if $*IN.t {
        human-on-stdin;
        $*IN.slurp(:enc<utf8-c8>).lines
    }
    else {
        $*IN.lines
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
    my $selected := IterationBuffer.CREATE;
    for @initially-selected {
        my int $linenr = .key;
        if $before {
            for max($linenr - $before, 1) ..^ $linenr -> int $_ {
                $selected.push:
                  Pair.new($_, @lines.AT-POS($_) but delimiter('-'))
                  unless @seen.AT-POS($_)++;
            }
        }

        $selected.push: Pair.new(.key, .value but delimiter(':'))
          unless @seen.AT-POS($linenr)++;

        if $after {
            for $linenr ^.. min($linenr + $after, $last-linenr ) -> int $_ {
                $selected.push:
                  Pair.new($_, @lines.AT-POS($_) but delimiter('-'))
                  unless @seen.AT-POS($_)++;
            }
        }
    }

    $selected.List
}
# Add any lines until any paragraph boundary
my sub add-paragraph($io, @initially-selected) {
    my str @lines = $io.lines(:enc<utf8-c8>);
    @lines.unshift: "";   # make 1-base indexing natural
    my int $last-linenr = @lines.end;

    my int8 @seen;
    my @selected is List = @initially-selected.map: {
        my int $linenr = .key;
        my int $pos = $linenr;
        my $selected := IterationBuffer.CREATE;
        while --$pos
          && !(@seen.AT-POS($pos)++)
          && @lines.AT-POS($pos) -> $line {
            $selected.unshift: Pair.new($pos, $line but delimiter('-'));
        }

        $selected.push: Pair.new(.key, .value but delimiter(':'))
          unless @seen.AT-POS($linenr)++;

        if $linenr < $last-linenr {
            $pos = $linenr;
            while ++$pos < $last-linenr
              && !(@seen.AT-POS($pos)++)
              && @lines.AT-POS($pos) -> $line {
                $selected.push:
                  Pair.new($pos, $line but delimiter('-'));
            }
            $selected.push:
              Pair.new($pos, @lines.AT-POS($pos) but delimiter('-'))
              unless @seen.AT-POS($pos)++;
        }
        $selected.Slip
    }
    @selected
}

# Return prelude from --repository and --module parameters
my sub prelude(%_) {
    my $prelude = "";
    if %_<I>:delete -> \libs {
        $prelude = libs.map({"use lib '$_'; "}).join;
    }
    if %_<M>:delete -> \modules {
        $prelude ~= modules.map({"use $_; "}).join;
    }
    $prelude
}

# Pre-process non literal string needles, return Callable if possible
my sub codify($needle, %_?) {
    Callable.ACCEPTS($needle)
      ?? $needle
      !! $needle.starts-with('/') && $needle.ends-with('/')
        ?? regexify($needle, %_)
        !! $needle.starts-with('{') && $needle.ends-with('}')
          ?? (prelude(%_) ~ 'my $ = -> $_ ' ~ $needle).EVAL
          !! $needle.starts-with('*.')
            ?? (prelude(%_) ~ 'my $ := ' ~ $needle).EVAL
            !! $needle
}

# Pre-process literal strings looking like a regex
my sub regexify($needle, %_) {
    my $i := %_<ignorecase>:delete ?? ':i' !! '';
    my $m := %_<ignoremark>:delete ?? ':m' !! '';
    "/$i$m$needle.substr(1)".EVAL
}

# Return a Seq with ~ paths substituted for actual home directory paths
my sub homify($from) {
    my $home := $*HOME.absolute ~ '/';
    ($from eq "-"
      ?? $*IN.lines
      !! $from.subst(/^ '~' '/'? /, $home).IO.lines
    ).map:
      *.subst(/^ '~' '/'? /, $home)
}

# Check the given Callable for the named phaser, and run it if there is one
my sub run-phaser(&code, str $name) {
    if Block.ACCEPTS(&code) && &code.callable_for_phaser($name) -> &phaser {
        phaser();
    }
}

# Return the NEXT phaser for the given Callable if any
my sub next-phaser(&code) {
    Block.ACCEPTS(&code) && &code.callable_for_phaser('NEXT')
}

# Drop the "location" of the warning, as it serves no useful purpose here
my sub drop-location-from-warning($warning) {
    note $warning.gist.lines.grep(!*.starts-with('  in block')).join("\n");
    $warning.resume;
}

# Change list of conditions into a Callable for :file
my sub codify-extensions(@extensions) {
    -> $_ { !is-sha1($_) && extension($_) (elem) @extensions }
}

# Return a properly pre-processed needle for various options
my sub preprocess-code-needle(&code, %_ --> Callable:D) {
    my $silently := (%_<silently>:delete)<>;
    if %_<quietly>:delete {
        # the existence of a CONTROL block appears to disallow use of ternaries
        # 2202.07
        if $silently {
            if $silently =:= True || $silently eq 'out,err' | 'err,out' {
                -> $_ {
                    CONTROL { .resume }
                    Trap(my $*OUT, my $*ERR);
                    code($_)
                }
            }
            elsif $silently eq 'out' {
                -> $_ {
                    CONTROL { .resume }
                    Trap(my $*OUT);
                    code($_)
                }
            }
            elsif $silently eq 'err' {
                -> $_ {
                    CONTROL { .resume }
                    Trap(my $*ERR);
                    code($_)
                }
            }
            else {
                meh "Unexpected value for --silently: $silently"
            }
        }
        else {
            -> $_ { CONTROL { .resume }; code($_) }
        }
    }
    elsif $silently {  # and not quietly
        $silently =:= True || $silently eq 'out,err' | 'err,out'
            ?? -> $_ { Trap(my $*OUT, my $*ERR); code($_) }
            !! $silently eq 'out'
              ?? -> $_ { Trap(my $*OUT); code($_) }
              !! $silently eq 'err'
                ?? -> $_ { Trap(my $*ERR); code($_) }
                !! meh "Unexpected value for --silently: $silently"
    }
    else {
        &code
    }
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
$_ .= subst(/^ '--' no '-' /, '--/') for @*ARGS;

# Entry point for CLI processing
my proto sub rak(|) {*}
use CLI::Version:ver<0.0.7>:auth<zef:lizmat>  $?DISTRIBUTION, &rak, 'long';
use CLI::Help:ver<0.0.4>:auth<zef:lizmat> %?RESOURCES, &rak, &HELP, 'long';

# Subroutine to actually output results
my &sayer;

# Main handler
my multi sub rak(*@specs, *%n) {  # *%_ causes compilation issues
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
          ?? "Saved '&as-cli-arguments(%n)' as: -$option"
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
    elsif %n<list-known-extensions>:delete {
        say sprintf('%9s: %s', .key, .value.map({$_ || '(none)'}).Str)
          for %exts.sort(*.key);
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

    # What did we do?
    if %n<list-expanded-options>:delete {
        say as-cli-arguments(%n);
        exit;
    }

    # Set up output file if needed
    temp $*OUT;
    with %n<output-file>:delete -> $path {
        $*OUT = open($path, :w) if $path ne "-";
    }

    # Set up pager if necessary
    my $pager;
    if %n<pager>:delete // %*ENV<RAK_PAGER> -> \pager {
        pager =:= True
          ?? meh("Must specify a specific pager to use: --pager=foo")
          !! ($*OUT = (run pager.words, :in).in);
        $pager = True;
    }

    # Start looking at actual actionable options
    my $needle = %n<pattern>:delete // @specs.shift;
    meh "Must at least specify a pattern" without $needle;

    if %n<say>:delete -> $say {
        &sayer = Callable.ACCEPTS($say) ?? $say !! codify($say);
    }
    else {
        &sayer = &say;
    }

    # Activate any uniquefier
    if %n<unique>:delete {
        my &old-sayer := &sayer<>;
        my %seen;
        &sayer = -> $_ { old-sayer($_) unless %seen{$_}++ }
    }

    # Pre-process non literal string needles
    $needle = codify($needle, %n);
    $is-simple-Callable := Callable.ACCEPTS($needle) && !Regex.ACCEPTS($needle);
    $can-have-phasers   := $is-simple-Callable && Block.ACCEPTS($needle);

    # Handle --smartcase
    %n<ignorecase> = !$needle.contains(/ <:upper> /)
      if Str.ACCEPTS($needle)
      && (%n<ignorecase>:!exists)
      && (%n<smartcase>:delete);

    # Reading from STDIN
    my $root := @specs.head;
    if ($root && $root eq '-') || !$*IN.t  {
        meh "Specified '$root' while reading from STDIN"
          if $root && $root ne '-';
        meh "Can not specify paths while reading from STDIN"
          if @specs > 1;
        ($is-simple-Callable
          ?? (%n<json-per-file>:delete)
            ?? &stdin-json-per-file
            !! (%n<json-per-line>:delete)
              ?? &stdin-json-per-line
              !! &stdin
          !! &stdin
        )($needle, %n);

        # Done
        $*OUT.close if $pager;
        exit;
    }

    # Not reading from STDIN, files are pre-specified
    my $seq := do if %n<files-from>:delete -> $from {
        meh "Cannot specify --files-from with path specification: @specs[]"
          if @specs;
        homify $from
    }

    # Need to figure out which files to check
    else {
        my %additional = named-args %n, :follow-symlinks, :file :dir;
        if %additional<file>:exists {
            ...
        }
        elsif %n<extensions>:delete -> $extensions {
            if $extensions.starts-with('#') {
                if %exts{$extensions} -> @exts {
                    %additional<file> := codify-extensions(@exts);
                }
                else {
                    meh "No extensions known for '$extensions'";
                }
            }
            else {
                %additional<file> := codify-extensions($extensions.split(','));
            }
        }
        elsif %n<known-extensions>:delete {
            %additional<file> := codify-extensions @known-extensions;
        }
        elsif %n<json-per-file> {
            %additional<file> := codify-extensions ("json",);
        }
        elsif %n<json-per-line> {
            %additional<file> := codify-extensions ("jsonl",);
        }
        elsif $isa-tty && (%n<human>:!exists || %n<human>) {
            %additional<file> := codify-extensions @known-extensions;
        }

        # Paths are pre-specified
        if %n<paths-from>:delete -> $from {
            meh "Cannot specify --paths-from with path specification: @specs[]"
              if @specs;

            homify($from).&hyperize(1,%n<degree>).map: {
                paths($_, |%additional).Slip
            }
        }

        # Paths from parameters
        elsif @specs {
            @specs.&hyperize(1,%n<degree>).map: {
                .IO.f
                  ?? $_
                  !! paths($_, |%additional).Slip  # assume shell will handle ~
            }
        }

        # no path, assume current dir
        else {
            paths ".", |%additional
        }
    }

    # Want to go edit
    if %n<edit>:delete -> $editor {
        go-edit-files($editor, $needle, $seq.sort(*.fc), %n);
    }
    
    # Just match on filenames
    elsif %n<find>:delete {
        %n<show-line-number> //= False;
        stdin($needle, %n, $seq);
    }

    # Need sorted filename list
    else {
        # Embedded in vim
        my &handle := do if %n<vimgrep>:delete {
            vimgrep($needle, %n, $seq);
        }

        # Code to run as a needle
        elsif $is-simple-Callable {
            %n<modify-files>:delete
              ?? &modify-files
              !! (%n<json-per-file>:delete)
                ?? &produce-json-per-file
                !! (%n<json-per-line>:delete)
                  ?? &produce-json-per-line
                  !! (%n<blame-per-line>:delete)
                    ?? &produce-blame-per-line
                    !! (%n<count-only>:delete)
                      ?? &count-only
                      !! (%n<files-with-matches>:delete)
                        ?? &files-only
                        !! &want-lines;
        }

        # Needle is either string or regex
        else {
            %n<count-only>:delete
              ?? &count-only
              !! (%n<files-with-matches>:delete)
                ?? &files-only
                !! &want-lines
        }
        handle($needle, $seq.sort(*.fc), %n);
    }

    $*OUT.close if $pager;
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
         }
      ),
      :editor(Bool.ACCEPTS($editor) ?? Any !! $editor)
}

# Replace contents of files using the given Callable
my sub modify-files(&needle, @paths, %_ --> Nil) {
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

    run-phaser(&needle, 'FIRST');
    my $NEXT := next-phaser(&needle);
    @paths.&hyperize($batch, $degree).map: -> $path {
        my str @lines;
        my int $lines-changed;
        my int $lines-removed;

        my $io := $path.IO;
        for $io.slurp.lines(:!chomp) {  # MUST slurp to prevent race condition
            my $*IO := $io;
            my $result := needle($_);
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
        $NEXT() if $NEXT;
    }
    run-phaser(&needle, 'LAST');

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

    sayer $fb;
}

# Produce JSON per file to check
my sub produce-json-per-file(&code, @paths, %_ --> Nil) {
    my &needle        := preprocess-code-needle(&code, %_);
    my $batch         := %_<batch>:delete;
    my $degree        := %_<degree>:delete;
    my $show-filename := %_<show-filename>:delete // True;
    meh-if-unexpected(%_);

    run-phaser(&code, 'FIRST');
    my $NEXT := next-phaser(&code);

    for @paths.&hyperize($batch, $degree).map: {
        CONTROL { drop-location-from-warning($_) }

        my $io := .IO;
        if try from-json $io.slurp -> $json {
            my $*IO := $io;
            if needle($json) -> \result {
                my $filename := $io.relative;
                result =:= True
                  ?? $filename
                  !! $show-filename
                    ?? "$filename: " ~ result
                    !! result
            }
        }
    } {
        sayer $_;
        $NEXT() if $NEXT;
    }
    run-phaser(&code, 'LAST');
}

# Produce JSON per line to check
my sub produce-json-per-line(&code, @paths, %_ --> Nil) {
    my &needle        := preprocess-code-needle(&code, %_);
    my $batch         := %_<batch>:delete;
    my $degree        := %_<degree>:delete;
    my $show-filename := %_<show-filename>:delete // True;

    run-phaser(&needle, 'FIRST');
    my $NEXT := next-phaser(&needle);
    if %_<count-only>:delete {
        meh-if-unexpected(%_);
        my int $total;

        for @paths.&hyperize($batch, $degree).map: {
            my $io := .IO;
            my int $found;

            for $io.lines -> $line {
                if try from-json $line -> $json {
                    CONTROL { drop-location-from-warning($_) }

                    my $*IO := $io;
                    ++$found if needle($json);
                }
            }

            $total += $found;
            "$io.relative(): $found" if $show-filename;
        } {
            sayer $_;
            $NEXT() if $NEXT;
        }
        sayer $total;
    }

    else {
        my $show-line-number := %_<show-line-number>:delete // True;
        meh-if-unexpected(%_);

        for @paths.&hyperize($batch, $degree).map: {
            my $io := .IO;
            my int $line-number;

            $io.lines.map(-> $line {
                ++$line-number;
                if try from-json $line -> $json {
                    CONTROL { drop-location-from-warning($_) }

                    my $*IO := $io;
                    if needle($json) -> \result {
                        my $filename := $io.relative;
                        my $mess     := result =:= True ?? '' !! ': ' ~ result;
                        $show-filename
                          ?? $show-line-number
                            ?? "$filename:$line-number$mess"
                            !! "$filename$mess"
                          !! $show-line-number
                            ?? "$line-number$mess"
                            !! $mess.substr(2)
                    }
                }
            }).List
        } -> @finds {
            sayer $_ for @finds;
            $NEXT() if $NEXT;
        }
    }
    run-phaser(&needle, 'LAST');
}

# Produce Git::Blame::Line per line to check
my sub produce-blame-per-line(&code, @paths, %_ --> Nil) {
    my &needle        := preprocess-code-needle(&code, %_);
    my $batch         := %_<batch>:delete // 1;
    my $degree        := %_<degree>:delete;
    my $show-filename := %_<show-filename>:delete // True;
    meh-if-unexpected(%_);

    run-phaser(&needle, 'FIRST');
    my $NEXT := next-phaser(&needle);
    for @paths.&hyperize($batch, $degree).map: -> $filename {
        if try Git::Blame::File.new($filename) -> $blamer {
            my $io := my $*IO := $filename.IO;
            my $finds := $blamer.lines.map(-> $_ {
                CONTROL { drop-location-from-warning($_) }
                if needle($_) -> \result {
                    result =:= True ?? .Str !! result
                }
            }).List;
            $finds.elems ?? ($io.relative, $finds) !! Empty
        }
    } -> ($filename, @finds) {
        if $show-filename {
            sayer "$filename: $_" for @finds;
        }
        else {
            sayer $_ for @finds;
        }
        $NEXT() if $NEXT;
    }
    run-phaser(&needle, 'LAST');
}

# Only count matches
my sub count-only($needle, @paths, %_ --> Nil) {
    my $verbose    := %_<verbose>:delete;
    my %additional := named-args %_,
      :ignorecase, :ignoremark, :invert-match, :type, :batch, :degree;
    meh-if-unexpected(%_);

    my int $files;
    my int $matches;
    my $NEXT := do if $can-have-phasers {
        $_() with $needle.callable_for_phaser('FIRST');
        $needle.callable_for_phaser('NEXT')
    }
    for files-containing $needle, @paths, :count-only, |%additional {
        ++$files;
        $matches += .value;
        sayer .key.relative ~ ': ' ~ .value if $verbose;
        $NEXT() if $NEXT;
    }
    run-phaser($needle, 'LAST') if $can-have-phasers;
    sayer "$matches matches in $files files";
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

    my Bool() $paragraph;
    my UInt() $before;
    my UInt() $after;

    if %_<paragraph-context>:delete {
        $paragraph := True;
    }
    elsif %_<context>:delete -> $context {
        $before = $after = $context;
    }
    else {
        $before = $_ with %_<before-context>:delete;
        $after  = $_ with %_<after-context>:delete;
    }
    $before = 0 without $before;
    $after  = 0 without $after;
    my $with-context = $paragraph || $before || $after;

    my Bool() $highlight;
    my Bool() $trim;
    my        $break;
    my Bool() $group-matches;
    my Bool() $show-filename;
    my Bool() $show-line-number;
    my Bool() $show-blame;
    my Bool() $only;
    my Int()  $summary-if-larger-than;

    my $human := %_<human>:delete // $isa-tty;
    if $human {
        $highlight = !$is-simple-Callable;
        $break = $group-matches = $show-filename = $show-line-number = True;
        $only  = $show-blame = False;
        $trim  = !$with-context;
        $summary-if-larger-than = 160;
    }

    unless $is-simple-Callable {
        $highlight := $_ with %_<highlight>:delete;
        $only      := $_ with %_<only-matching>:delete;
    }
    $trim       := $_ with %_<trim>:delete;
    $show-blame := $_ with %_<show-blame>:delete;
    $before = $after = 0 if $only;
    $summary-if-larger-than := $_ with %_<summary-if-larger-than>:delete;

    my &show-line;
    if $highlight {
        my Str() $pre = my Str() $post = $_ with %_<highlight-before>:delete;
        $post = $_ with %_<highlight-after>:delete;
        $pre  = $only ?? " " !! BON  without $pre;
        $post = $only ?? ""  !! BOFF without $post;

        &show-line = $trim && !$show-blame
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

    my $show-header = $show-filename && $group-matches;
    $show-filename  = False if $show-header;
    my int $nr-files;

    for $seq -> (:key($io), :value(@matches)) {
        sayer $break if $break && $nr-files++;

        my str $filename = $io.relative;
        my @blames;
        if $show-blame && Git::Blame::File($io) -> $blamer {
            @blames := $blamer.lines;
        }
        sayer $filename if $show-header;

        if @blames {
            sayer show-line(@blames[.key - 1].Str)
              for add-before-after($io, @matches, $before, $after);
        }
        elsif $with-context {
            my @selected := $paragraph
              ?? add-paragraph($io, @matches)
              !! add-before-after($io, @matches, $before, $after);
            my $format := '%' ~ (@selected.tail.key.chars) ~ 'd';
            if $show-line-number {
                for @selected {
                    my str $delimiter = .value.delimiter;
                    sayer ($show-filename ?? $filename ~ $delimiter !! '')
                      ~ sprintf($format, .key)
                      ~ $delimiter
                      ~ show-line(.value);
                }
            }
            elsif $show-filename {
                sayer $filename ~ .value.delimiter ~ show-line(.value)
                  for @selected;
            }
            else {
                sayer show-line(.value) for @selected;
            }
        }
        else {
            if $show-line-number {
                my $format := '%' ~ (@matches.tail.key.chars) ~ 'd:';
                for @matches {
                    sayer ($show-filename ?? $filename ~ ':' !! '')
                      ~ sprintf($format, .key)
                      ~ show-line(.value);
                }
            }
            elsif $show-filename {
                sayer $filename ~ ':' ~ show-line(.value) for @matches;
            }
            else {
                sayer show-line(.value) for @matches;
            }
        }
    }
}

# Provide output that can be used by vim to page through
my sub vimgrep($needle, @paths, %_ --> Nil) {
    my $ignorecase := %_<ignorecase>:delete;
    my $ignoremark := %_<ignoremark>:delete;
    my %additional := named-args %_, :max-count, :type, :batch, :degree;
    meh-if-unexpected(%_);

    sayer $_ for files-containing(
      $needle, @paths, :$ignorecase, :$ignoremark, :offset(1), |%additional
    ).map: {
        my $path := .key.relative;
        .value.map({
            $path
              ~ ':' ~ .key
              ~ ':' ~ columns(.value, $needle, :$ignorecase, :$ignoremark).head
              ~ ':' ~ .value
        }).Slip
    }
}

# Read from STDIN, assume JSON per line
my sub stdin-json-per-file(&code, %_ --> Nil) {
    my &needle := preprocess-code-needle(&code, %_);
    meh-if-unexpected(%_);

    human-on-stdin if $*IN.t;
    if try from-json $*IN.slurp(:enc<utf8-c8>) -> $json {
        my $*IO := $*IN;
        if needle($json) -> \result {
            sayer result;
        }
    }
}

# Read from STDIN, assume JSON per line
my sub stdin-json-per-line(&code, %_ --> Nil) {
    my &needle           := preprocess-code-needle(&code, %_);
    my $count-only       := %_<count-only>:delete;
    my $show-line-number := %_<show-line-number>:delete;
    meh-if-unexpected(%_);

    my int $line-number;
    my int $matches;
    my $*IO := $*IN;
    for stdin-source() -> $line {
        ++$line-number;
        if try from-json $line -> $json {
            if needle($json) -> \result {
                $count-only
                  ?? ++$matches
                  !! result =:= True
                    ?? sayer($line-number)
                    !! $show-line-number
                      ?? sayer($line-number ~ ': ' ~ result)
                      !! sayer(result)
            }
        }
    }
    sayer $matches if $count-only;
}

# Handle general searching on STDIN
my sub stdin($needle, %_, $source = stdin-source --> Nil) {
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
        $highlight = !$is-simple-Callable;
        $show-line-number = $only = False;
        $trim = !($before || $after || $is-simple-Callable);
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

    my &matcher := do if Callable.ACCEPTS($needle) {
        Regex.ACCEPTS($needle)
          ?? { $needle.ACCEPTS($_) }
          !! $needle
    }
    elsif %_<passthru>:delete {
        -> $ --> True { }
    }
    else {
        my $type := %_<type>:delete // 'contains';
        $type eq 'words'
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
    my $*IO := $*IN;
    for $source<> -> $line {
        ++$line-number;
        if matcher($line) -> \result {
            sayer @before.shift while @before;
            my $text := result =:= True ?? show-line($line) !! result;
            sayer $show-line-number ?? ($line-number ~ ':' ~ $text) !! $text;
            $todo-after = $after;
        }
        elsif $todo-after {
            sayer $show-line-number
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

sub EXPORT($name = '&rak') { Map.new: ($name => &rak) }

# vim: expandtab shiftwidth=4
