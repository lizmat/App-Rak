# The modules that we need here, with their full identities
use as-cli-arguments:ver<0.0.4>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;
use Git::Blame::File:ver<0.0.5>:auth<zef:lizmat>;
use has-word:ver<0.0.3>:auth<zef:lizmat>;
use highlighter:ver<0.0.12>:auth<zef:lizmat>;
use JSON::Fast:ver<0.17>:auth<cpan:TIMOTIMO>;
use rak:ver<0.0.10>:auth<zef:lizmat>;
use String::Utils:ver<0.0.8>:auth<zef:lizmat>;

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
    exit note $message;
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

# Return extension of filename, if any
my sub extension(str $filename) {
    with rindex($filename, '.') {
        lc substr($filename, $_ + 1)
    }
    else {
        ""
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

# Return prelude from --repository and --module parameters
my sub prelude(%_) {
    my $prelude = "";
    if %_<repository>:delete -> \libs {
        $prelude = libs.map({"use lib '$_'; "}).join;
    }
    if %_<module>:delete -> \modules {
        $prelude ~= modules.map({"use $_; "}).join;
    }
    $prelude
}

# Convert a string to code if possible
my sub codify(Str:D $code, %_?) {
    CATCH {
        meh "Could not compile '$code':\n$_.message()";
    }
    $code.starts-with('/') && $code.ends-with('/')
      ?? regexify($code, %_)
      !! $code.starts-with('{') && $code.ends-with('}')
        ?? (prelude(%_) ~ 'my $ := -> $_ ' ~ $code).EVAL
        !! $code.starts-with('-> $') && $code.ends-with('}')
          ?? (prelude(%_) ~ 'my $ := ' ~ $code).EVAL
          !! $code.starts-with('*.') || $code.starts-with('* ')
            ?? (prelude(%_) ~ 'my $ := ' ~ $code).EVAL
            !! $code
}

# Pre-process literal strings looking like a regex
my sub regexify($code, %_) {
    my $i := %_<ignorecase> ?? ':i' !! '';
    my $m := %_<ignoremark> ?? ':m' !! '';
    "/$i$m$code.substr(1)".EVAL
}

# Convert a string to code, fail if not possible
my sub convert-to-Callable(Str:D $code) {
    my $callable := codify($code);
    Callable.ACCEPTS($callable)
      ?? $callable
      !! meh "Does not look like a code specification: $code"
}

# Convert a string to non-Regex code, fail if not possible
my sub convert-to-simple-Callable(Str:D $code) {
    my $callable := codify($code);
    Regex.ACCEPTS($callable)
      ?? meh "Cannot use a regular expression in this context: '$code'"
      !! $callable
}

# Return Callable for a pattern that is not supposed to be code
my sub needleify($pattern, %_) {
    my $i := %_<ignorecase>;
    my $m := %_<ignoremark>;
    my $type := %_<type> //= 'contains';

    if $type eq 'words' {
        $i
          ?? $m
            ?? *.&has-word($pattern, :i, :m)
            !! *.&has-word($pattern, :i)
          !! $m
            ?? *.&has-word($pattern, :m)
            !! *.&has-word($pattern)
    }
    elsif $type eq 'contains' | 'starts-with' | 'ends-with' {
        $i
          ?? $m
            ?? *."$type"($pattern, :i, :m)
            !! *."$type"($pattern, :i)
          !! $m
            ?? *."$type"($pattern, :m)
            !! *."$type"($pattern)
    }
    else {
        die "Don't know how to handle type: $type";
    }
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

# Preprocess parameters from the given config
my sub handle-config-and-preprocessing(%n) {  # *%_ causes compilation issues
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
    translate($_, %n{$_}) for original-nameds;
}

# Subroutine to actually output results
my &sayer;
# Flag indicating a pager was used
my $pager;

# Handle the general informational and output options
my sub handle-general-options(%n) {  # *%_ causes compilation issues
    # What are the known extensions?
    if %n<list-known-extensions>:delete {
        say sprintf('%9s: %s', .key, .value.map({$_ || '(none)'}).Str)
          for %exts.sort(*.key);
        exit;
    }
    # What did we do?
    elsif %n<list-expanded-options>:delete {
        say as-cli-arguments(%n);
        exit;
    }

    # Set up pager if necessary
    if %n<pager>:delete // %*ENV<RAK_PAGER> -> \pager {
        pager =:= True
          ?? meh("Must specify a specific pager to use: --pager=foo")
          !! ($*OUT = (run pager.words, :in).in);
        $pager = True;
    }

    # Set up output file if needed
    elsif %n<output-file>:delete -> $path {
        $*OUT = open($path, :w) if $path ne "-";
    }

    # We want to say it differently?
    &sayer = do if %n<sayer>:delete -> $sayer {
        &say;  # for now
    }
    else {
        my $out := $*OUT;
        -> $_ { $out.say($_) }
    }
}

# Set up paths to search
my sub setup-sources-selection(@specs, %n, %rak) {

    # files from a file
    if %n<files-from>:delete -> $files-from {
        meh "Specified path&s(@specs) '@specs[]' with --files-from"
          if @specs;
        %rak<files-from> := $files-from;
    }

    # paths from a file
    elsif %n<paths-from> -> $paths-from {
        meh "Specified path&s(@specs) '@specs[]' with --paths-from"
          if @specs;
        %rak<paths-from> := $paths-from;
    }
    else {
        %rak<paths> := @specs.List;
    }

    # Really want to check *all* files
    if %n<all-files>:delete {
        meh "Cannot specify --dir with --find-all"  if %n<dir>:delete;
        meh "Cannot specify --file with --find-all" if %n<file>:delete;
        %rak<dir> := %rak<file> := True;
    }

    # Do not want all files
    else {

        # Explicit directory selection specification
        if %n<dir>:delete -> $dir {
            %rak<dir> := convert-to-Callable($dir);
        }
        else {
            %rak<dir> := -> $_ { !.starts-with('.') }
        }

        # Explicit file selection specification
        if %n<file>:delete -> $file {
            %rak<file> := convert-to-Callable($file);
        }

        # Explicit extensions
        elsif %n<extensions>:delete -> $extensions {
            my @unknown;
            if $extensions.split(',').map: {
                if .starts-with('#') {
                    if %exts{.substr(1)} -> @extensions {
                        @extensions.Slip
                    }
                    else {
                        @unknown.push: $_;
                    }
                }
                else {
                    $_
                }
            } -> @extensions {
                meh "No extension&s(@unknown) known for '@unknown[]'"
                  if @unknown;
                %rak<file> := codify-extensions @extensions;
            }
        }
        elsif %n<known-extensions>:delete {
            %rak<file> := codify-extensions @known-extensions;
        }

        # Implicit extensions
        elsif %n<json-per-file> {
            %rak<file> := codify-extensions ("json",);
        }
        elsif %n<json-per-line> {
            %rak<file> := codify-extensions ("jsonl",);
        }
        else {
            %rak<file> := codify-extensions @known-extensions;
        }
    }

    # Boolean flags that can also be negated
    for <recurse-symlinked-dir recurse-unmatched-dir
         is-empty is-executable is-readable is-writable is-symbolic-link
         is-group-executable is-group-readable is-group-writable
         is-owned-by-group is-owned-by-user
         is-world-executable is-world-readable is-world-writable
        > {
        %rak{$_} := %n{$_}:delete if %n{$_}:exists;
    }

    # Checking for epoch
    for <accessed created meta-modified modified> {
        # TODO
    }

    # Checking for user / group ID
    for <uid gid> {
        # TODO
    }

    # Checking for numeric value
    for <blocks device-number filesize hard-links inode mode> {
        # TODO
    }
}

# Set up the producers of information
my sub setup-producers(%n, %rak) {
    # Set up producers
    %rak<encoding> := my $enc := (%n<encoding>:delete) // 'utf8-c8';
    if %n<per-file>:delete -> $per-file {
        %rak<produce-one> := $per-file =:= True
          ?? *.slurp(:$enc)
          !! convert-to-simple-Callable($per-file)

    }
    elsif %n<per-line>:delete -> $per-line {
        %rak<produce-many> := convert-to-simple-Callable($per-line)
          unless $per-line =:= True;
    }

    # after this, these all require simple Callables

    # Match JSON data
    elsif %n<json-per-file>:delete {
        %rak<produce-one> := -> $_ { from-json .slurp(:$enc) }
        %rak<omit-item-numbers> := True;
    }
    elsif %n<json-per-line>:delete {
        %rak<produce-many> := *.lines(:$enc).map: *.&from-json
    }

    # Match git blame data
    elsif %n<blame-per-file>:delete {
        %rak<produce-one> := -> $_ { Git::Blame::File.new($_) }
        %rak<omit-item-numbers> := True;
    }
    elsif %n<blame-per-line>:delete {
        %rak<produce-many> := -> $_ { Git::Blame::File.new($_).lines }
    }
}

# Return a Callable to do highlighting
my sub make-highlighter($needle, %n, %rak) {
    my Bool() $ignorecase = %n<ignorecase>;
    my Bool() $ignoremark = %n<ignoremark>;
    my        $type       = %n<type>;
    my Bool() $highlight;
    my Bool() $trim;
    my Bool() $only;
    my Int()  $summary-if-larger-than;

    my $human := %n<human> //= $isa-tty;
    if $human {
        $highlight = True;
        $only  = False;
        $trim  = !(%n<context> || %n<before-context> || %n<after-context> ||
                   %n<paragraph-context> || %n<passthru-context>);
        $summary-if-larger-than = 160;
    }

    $highlight              := $_ with %n<highlight>:delete;
    $trim                   := $_ with %n<trim>:delete;
    $only                   := $_ with %n<only-matching>:delete;
    $summary-if-larger-than := $_ with %n<summary-if-larger-than>:delete;

    if $highlight {
        my Str() $pre = my Str() $post = $_ with %n<highlight-before>:delete;
        $post = $_ with %n<highlight-after>:delete;
        $pre  = $only ?? " " !! BON  without $pre;
        $post = $only ?? ""  !! BOFF without $post;

        $trim
          ?? -> $line {
                 highlighter $line.trim, $needle<>, $pre, $post,
                   :$ignorecase, :$ignoremark, :$type,
                   :$only, :$summary-if-larger-than
             }
          !! -> $line {
                 highlighter $line, $needle<>, $pre, $post,
                   :$ignorecase, :$ignoremark, :$type,
                   :$only, :$summary-if-larger-than
             }
    }
    else {
        $only
          ?? -> $line { highlighter $line, $needle, "", " ", :$only, :$type }
          !! $trim
            ?? *.trim
            !! -> $line { $line }
    }
}

# Entry point for CLI processing
my proto sub MAIN(|) is export {*}
use CLI::Version:ver<0.0.7>:auth<zef:lizmat>  $?DISTRIBUTION, &MAIN, 'long';
use CLI::Help:ver<0.0.4>:auth<zef:lizmat> %?RESOURCES, &MAIN, &HELP, 'long';

# Main handler
my multi sub MAIN(*@specs, *%n) {  # *%_ causes compilation issues
    handle-config-and-preprocessing(%n);
    handle-general-options(%n);

    # Start looking at actual actionable options
    my $pattern = %n<pattern>:delete // @specs.shift;
    my $needle = do if $pattern {

        # Handle smartcase
        %n<ignorecase> = !$pattern.contains(/ <:upper> /)
          if (%n<ignorecase>:!exists) && (%n<smartcase>:delete);

        # Process string pattern into a Callable
        codify($pattern, %n);
    }
    elsif %n<find> {  # no explicit pattern, but using find
        &defined
    }
    else {
        meh "Must at least specify a pattern";
    }

    # Arguments to rak
    my %rak;

    # Make sure needle is executable and create appropriate highlighter
    my &line-post-proc;
    if Regex.ACCEPTS($needle) {
        &line-post-proc = make-highlighter($needle, %n, %rak)
    }
    # non-executable, create executable needle and highlighter
    elsif !Callable.ACCEPTS($needle) {
        $needle = needleify($pattern, %n);
        &line-post-proc = make-highlighter($pattern, %n, %rak)
    }

    # Only interested in filenames
    if %n<files-with-matches>:delete {

        # Only interested in number of files
        if %n<count-only>:delete {
            %rak<max-matches-per-source> := 1;
            %rak<mapper> := -> $, @ --> Empty {
                state $seen = 0;
                LAST {
                    sayer $seen == 0
                      ?? "No files"
                      !! $seen == 1
                        ?? "One file"
                        !! "$seen files";
                }
                ++$seen;
            }
        }

        # Need to separate files with a null-byte
        elsif %n<file-separator-null>:delete {
            %rak<max-matches-per-source> := 1;
            %rak<mapper> := -> $source, @ --> Empty {
                state @files;
                LAST { sayer @files.join("\0") }
                @files.push: $source.relative;
            }
        }

        # Want to know which files
        else {
            %rak<sources-only> := True;
            &line-post-proc = *.relative;
        }
    }

    # Want to edit files
    elsif %n<edit>:delete -> $editor {
        my $ignorecase := %n<ignorecase>;
        my $ignoremark := %n<ignoremark>;
        my $type       := %n<type>;

        %rak<mapper> := -> $source, @matches --> Empty {
            state @files;
            LAST {
                edit-files
                  @files,
                  :editor(Bool.ACCEPTS($editor) ?? Any !! $editor)
            }

            my $path := $source.relative;
            @files.append: @matches.map: {
                $path => .key => columns(
                  .value, $pattern, :$ignorecase, :$ignoremark, :$type
               ).head
            }
        }
    }

    # Editor searching for files
    elsif %n<vimgrep>:delete {
        my $ignorecase := %n<ignorecase>;
        my $ignoremark := %n<ignoremark>;
        my $type       := %n<type>;

        %rak<mapper> := -> $source, @matches {
            my $path := $source.relative;
            @matches.map({
                $path
                  ~ ':' ~ .key
                  ~ ':' ~ columns(
                            .value, $pattern, :$ignorecase, :$ignoremark, :$type
                          ).head
                  ~ ':' ~ .value
            }).Slip
        }
    }

    # Modifying files
    elsif %n<modify-files>:delete {
        my $dry-run := %n<dry-run>:delete;
        my $verbose := %n<verbose>:delete;

        my $backup = %n<backup>:delete;
        $backup = ".bak" if $backup<> =:= True;
        $backup = ".$backup" if $backup && !$backup.starts-with('.');

        my constant no-changes =
          "\n*** no changes where made because of --dry-run ***";

        my @changed-files;
        my int $nr-files-seen;
        my int $nr-lines-changed;
        my int $nr-lines-removed;

        %rak<with-line-endings> := True;
        %rak<mapper> := -> $io, @matches --> Empty {
            ++$nr-files-seen;

            LAST {
                my int $nr-files-changed = @changed-files.elems;
                my $fb = "Processed $nr-files-seen file&s($nr-files-seen)";
                $fb ~= ", $nr-files-changed file&s($nr-files-changed) changed"
                  if $nr-files-changed;
                $fb ~= ", $nr-lines-changed line&s($nr-lines-changed) changed"
                  if $nr-lines-changed;
                $fb ~= ", $nr-lines-removed line&s($nr-lines-removed) removed"
                  if $nr-lines-removed;

                if $verbose {
                    $fb ~= "\n";
                    for @changed-files -> ($io, $changed, $removed) {
                        $fb ~= "$io.relative():";
                        $fb ~= " $changed change&s($changed)"  if $changed;
                        $fb ~= " $removed removal&s($removed)" if $removed;
                        $fb ~= "\n";
                    }
                    $fb ~= no-changes if $dry-run;
                    $fb .= chomp;
                }
                elsif $dry-run {
                    $fb ~= no-changes;
                }

                sayer $fb;
            }

            my int $lines-removed;
            my int $lines-changed;
            my int $index;
            for @matches {
                ++$index;
                if .key - $index -> int $missing {
                    $lines-removed += $missing;
                    $index = .key;
                }
                ++$lines-changed if .matched;
            }
            if $lines-changed || $lines-removed {
                unless $dry-run {
                    if $backup {
                        $io.spurt(@matches.map(*.value).join)
                          if $io.rename($io.sibling($io.basename ~ $backup));
                    }
                    else {
                        $io.spurt: @matches.map(*.value).join;
                    }
                }
                $nr-lines-changed += $lines-changed;
                $nr-lines-removed += $lines-removed;
                @changed-files.push: ($io, $lines-changed, $lines-removed);
            }
        }
    }

    # Various setups
    setup-sources-selection(@specs, %n, %rak);
    setup-producers(%n, %rak);

    # Set up statistics settings
    my $count-only := %n<count-only>:delete;
    %rak<stats-only> := True if %n<stats-only>:delete || $count-only;
    %rak<stats>      := True if %n<stats>:delete;
    %rak<unique>     := True if %n<unique>:delete;

    # Remove arguments that have been handled now
    %n<human ignorecase ignoremark type>:delete;

    my $show-filename := %n<show-filename>:delete // True;

    # Set up / do the work
    meh-if-unexpected(%n);
    my $rak := rak $needle, %rak;
    meh .message with $rak.exception;


    # show the results!
    for $rak.result -> $outer {
        if Pair.ACCEPTS($outer) {
            my $source := $outer.key;
            my $result := $outer.value;
            if Iterable.ACCEPTS($result) {
                if $result -> @matches {
                    sayer $source.relative if $show-filename;
                    if Pair.ACCEPTS(@matches.head) {
                        my str $format = '%' ~ @matches.tail.key.chars ~ 'd:%s';
                        for @matches {
                            sayer sprintf
                                    $format,
                                    .key,
                                    .matched && &line-post-proc
                                      ?? line-post-proc .value
                                      !! .value
                        }
                    }
                    elsif &line-post-proc {
                        sayer line-post-proc $_ for @matches;
                    }
                    else {
                        sayer $_ for @matches;
                    }
                }
            }
            else {
                sayer $source.relative if $show-filename;
                sayer &line-post-proc ?? line-post-proc($outer) !! $outer;
            }
        }

        # just show unique results
        else {
            sayer &line-post-proc ?? line-post-proc($outer) !! $outer;
        }
    }

    # Statistics to show
    if $rak.stats -> %s {
        if $count-only && !%n<verbose> {
            sayer %s<nr-matches> + %s<nr-changes>
              ~ " matches in %s<nr-sources> files";
        }
        else {
            my str @stats;
            unless $count-only {
                @stats.push: "Statistics for '" ~ BON ~ $pattern ~ BOFF ~ "':";
                my str $bar   = '-' x @stats[0].chars - BON.chars - BOFF.chars;
                @stats.unshift: $bar;
                @stats.push: $bar;
            }
            @stats.push: "    Number of files: %s<nr-sources>";

            if %s<nr-items> -> $items {
                @stats.push: "    Number of lines: $items";
            }
            if %s<nr-matches> -> $matches {
                @stats.push: "  Number of matches: $matches";
            }
            if %s<nr-passthrus> -> $passthrus {
                @stats.push: "Number of passthrus: $passthrus";
            }
            if %s<nr-changes> -> $changes {
                @stats.push: "  Number of changes: $changes";
            }

            sayer @stats.join("\n");
        }
    }

    # Done
    $*OUT.close if $pager;
    exit;
}

=finish

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

# vim: expandtab shiftwidth=4
