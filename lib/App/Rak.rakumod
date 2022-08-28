# The modules that we need here, with their full identities
use as-cli-arguments:ver<0.0.4>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;
use has-word:ver<0.0.3>:auth<zef:lizmat>;
use highlighter:ver<0.0.12>:auth<zef:lizmat>;
use JSON::Fast:ver<0.17>:auth<cpan:TIMOTIMO>;
use rak:ver<0.0.16>:auth<zef:lizmat>;
use String::Utils:ver<0.0.8>:auth<zef:lizmat>;

# Defaults for highlighting on terminals
my constant BON  = "\e[1m";   # BOLD ON
my constant BOFF = "\e[22m";  # BOLD OFF

# Make sure we remember if there's a human watching (terminal connected)
my $isa-tty := $*OUT.t;

# Set up default extension sets
my constant %exts =
  '#c'        => <c h hdl>,
  '#c++'      => <cpp cxx hpp hxx>,
  '#csv'      => ('', <csv psv tsv>).flat.List,
  '#json'     => <json>,
  '#jsonl'    => <jsonl>,
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
#    %_{$_}:delete if %_{$_}<> =:= False for %_.keys;
    meh "Unexpected option{"s" if %_.elems != 1}: &as-cli-arguments(%_)\nUse --help for an overview of available options"
      if %_;
}

# Quit if module not installed
my sub meh-not-installed($module, $param) {
    meh qq:to/MEH/;
Must have the $module module installed to do --$param.
You can do this by running 'zef install $module'.
MEH
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
          !! $code.starts-with('*')
            ?? (prelude(%_) ~ 'my $ := ' ~ $code).EVAL
            !! $code
}

# Pre-process literal strings looking like a regex
my sub regexify($code, %_) {
    my $i := %_<ignorecase> ?? ':i ' !! '';
    my $m := %_<ignoremark> ?? ':m ' !! '';
    "/$i$m$code.substr(1)".EVAL
}

# Convert a string to code, fail if not possible
my sub convert-to-matcher(Str:D $code) {
    my $callable := codify($code);
    Callable.ACCEPTS($callable)
      ?? $callable
      !! *.contains($callable)
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
    elsif %n<paths-from>:delete -> $paths-from {
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
            %rak<dir> := convert-to-matcher($dir);
        }
        else {
            %rak<dir> := -> $_ { !.starts-with('.') }
        }

        # Explicit file selection specification
        if %n<file>:delete -> $file {
            %rak<file> := convert-to-matcher($file);
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
            %rak<file> := codify-extensions %exts<#json>;
        }
        elsif %n<json-per-line> {
            %rak<file> := codify-extensions %exts<#jsonl>;
        }
        elsif %n<csv-per-line> {
            %rak<file> := codify-extensions %exts<#csv>;
        }
        else {
            %rak<file> := codify-extensions @known-extensions;
        }
    }

    # Boolean flags that can also be negated
    for <recurse-symlinked-dir recurse-unmatched-dir
         is-empty is-executable is-readable is-writable is-symbolic-link
         is-owned-by-group is-owned-by-user
         has-setuid has-setgid is-sticky
         is-owner-executable is-owner-readable is-owner-writable
         is-group-executable is-group-readable is-group-writable
         is-world-executable is-world-readable is-world-writable
        > {
        %rak{$_} := %n{$_}:delete if %n{$_}:exists;
    }

    # Checking for epoch
    for <accessed created meta-modified modified> {
        # TODO
    }

    # Checking for user ID
    if %n<user>:delete -> $code {

        # Get lookup of uid
        my (&getpwnam, &getpwuid) = do {
            CATCH { meh-not-installed 'P5getpwnam', 'user' }
            'use P5getpwnam; &getpwnam, &getpwuid'.EVAL
        }

        my sub names2uids($names) {
            $names.split(",").map: {
                getpwnam($_)[2]
                  // meh "Unknown user name '$_' with --user";
            }
        }

        # An actual condition
        my $compiled := convert-to-simple-Callable($code);
        %rak<uid> := do if Callable.ACCEPTS($compiled) {
            -> $uid { $compiled($_) with getpwuid($uid).head }
        }

        # Negation of list of user names
        elsif $compiled.starts-with('!') {
            my int @uids = names2uids($compiled.substr(1));
            my $uid := @uids.head;
            my $ := @uids == 1
              ?? * != $uid
              !! { !($_ (elem) @uids) }
        }

        # List of user names
        else {
            my int @uids = names2uids($compiled);
            my $uid := @uids.head;
            my $ := @uids == 1
              ?? * == $uid
              !! * (elem) @uids
        }
    }
    elsif %n<uid>:delete -> $code {
        my $compiled := convert-to-simple-Callable($code);
        %rak<uid> := do if Callable.ACCEPTS($compiled) {
            $compiled
        }
        elsif (try $code.Int) -> $uid {
            my $ := * == $uid
        }
        else {
            meh "Must specify a numeric uid or an expression with --uid";
        }
    }

    # Checking for group ID
    if %n<group>:delete -> $code {

        # Get lookup of gid
        my (&getgrnam, &getgrgid) = do {
            CATCH { meh-not-installed 'P5getgrnam', 'group' }
            'use P5getgrnam; &getgrnam, &getgrgid'.EVAL
        }

        my sub names2gids($names) {
            $names.split(",").map: {
                getgrnam($_)[2]
                  // meh "Unknown group name '$_' with --group";
            }
        }

        # An actual condition
        my $compiled := convert-to-simple-Callable($code);
        %rak<gid> := do if Callable.ACCEPTS($compiled) {
            -> $gid { $compiled($_) with getgrgid($gid).head }
        }

        # Negation of list of group names
        elsif $compiled.starts-with('!') {
            my int @gids = names2gids($compiled.substr(1));
            my $gid := @gids.head;
            my $ := @gids == 1
              ?? * != $gid
              !! { !($_ (elem) @gids) }
        }

        # List of group names
        else {
            my int @gids = names2gids($compiled);
            my $gid := @gids.head;
            my $ := @gids == 1
              ?? * == $gid
              !! * (elem) @gids
        }
    }
    elsif %n<gid>:delete -> $code {
        my $compiled := convert-to-simple-Callable($code);
        %rak<gid> := do if Callable.ACCEPTS($compiled) {
            $compiled
        }
        elsif (try $code.Int) -> $gid {
            my $ := * == $gid
        }
        else {
            meh "Must specify a numeric gid or an expression with --gid";
        }
    }

    # Checking for numeric value
    for <blocks device-number filesize hard-links inode mode> {
        if %n{$_}:exists {
            my $code := %n{$_}:delete;
            if Bool.ACCEPTS($code) {
                meh "Must specify a condition for '--$_'";
            }
            else {
                my $compiled := convert-to-simple-Callable($code);
                Callable.ACCEPTS($compiled)
                  ?? (%rak{$_} := $compiled)
                  !! meh "Problem compiling condition for '--$_': $code";
            }
        }
    }
}

# Set up the producers of information
my sub setup-producers(@specs, %n, %rak) {
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
        %rak<omit-item-number> = True;
    }
    elsif %n<json-per-line>:delete {
        %rak<produce-many> := *.lines(:$enc).map: *.&from-json
    }

    # Match CSV data
    elsif %n<csv-per-line>:delete {
        CATCH { meh-not-installed 'Text::CSV', 'csv-per-line' }
        require Text::CSV;

        my constant %line-endings =
          lf   => "\n",
          cr   => "\r",
          crlf => "\r\n";

        my %csv = %n<
          sep quote escape allow-whitespace allow-loose-quotes
          allow-loose-escapes keep-meta strict formula
        >:delete:p;
        %csv<auto-diag> := %n<auto-diag>:delete // True;
        %csv<eol> := %line-endings{$_} with %n<eol>:delete;

        my $csv := Text::CSV.new(|%csv);
        %rak<produce-many> := -> $io { $csv.getline-all($io.open) }
    }

    # Match git blame data
    elsif %n<blame-per-file>:delete {
        CATCH { meh-not-installed 'Git::Blame::File', 'blame-per-file' }
        require Git::Blame::File;

        %rak<produce-one> := -> $io { Git::Blame::File.new($io) }
        %rak<under-version-control> := True;
        %rak<omit-item-number>       = True;
    }
    elsif %n<blame-per-line>:delete {
        CATCH {
            meh-not-installed 'Git::Blame::File', 'blame-per-line';
        }
        require Git::Blame::File;

        %rak<produce-many> := -> $io { Git::Blame::File.new($io).lines }
        %rak<under-version-control> := True;
    }
}

# Return a Callable to do highlighting
my sub make-highlighter($needle, %n, %rak) {
    my $type      := %n<type>;
    my $trim      := %n<trim>:delete;
    my $only      := %n<matches-only>:delete;
    my Int() $summary-if-larger-than :=
      %n<summary-if-larger-than>:delete // 160;

    my $highlight;
    my $highlight-before;
    my $highlight-after;

    with %n<highlight-before>:delete {
        $highlight-before := $_;
        $highlight        := True;
    }
    with %n<highlight-after>:delete {
        $highlight-after := $_;
        $highlight       := True;
    }
    else {
        $highlight-after := $highlight-before;
    }

    $highlight := %n<highlight>:delete // $isa-tty
      without $highlight;

    if $highlight {
        my Str() $pre = my Str() $post = $_ with $highlight-before;
        $post                          = $_ with $highlight-after;
        $pre  = $only ?? " " !! BON  without $pre;
        $post = $only ?? ""  !! BOFF without $post;

        my %nameds =
          |(%n<ignorecase ignoremark>:p), :$only, :$summary-if-larger-than;
        %nameds<type> = $_ with $type;

        $trim
          ?? -> $line {
                 highlighter $line.trim, $needle<>, $pre, $post, |%nameds
             }
          !! -> $line {
                 highlighter $line, $needle<>, $pre, $post, |%nameds
             }
    }

    # No highlighting wanted, abuse highlighter logic anyway
    else {
        $only
          ?? $type
            ?? -> $line {
                   highlighter $line, $needle, "", " ", :$only, :$type
               }
            !! -> $line {
                   highlighter $line, $needle, "", " ", :$only
               }
          !! $trim ?? *.Str.trim !! *.Str
    }
}

# Handle --edit
my sub handle-edit($editor, $pattern, %n, %rak) {
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

# Handle --vimgrep
my sub handle-vimgrep($pattern, %n, %rak) {
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

# Handle --modify-files
my sub handle-modify-files($pattern, %n, %rak) {
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

    %rak<with-line-endings> := %n<with-line-endings>:delete // True;
    %rak<passthru-context>  := %n<passthru-context>:delete  // True;
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

# Handle --checkout
my sub handle-checkout($pattern, %n, %rak) {
    my $verbose := %n<verbose>:delete;

    %rak<sources>         := 'checkout';
    %rak<omit-item-number> = True;
    %rak<map-all>         := True;

    my @branches;
    %rak<produce-many> := -> $ {
        @branches = (
          run <git branch -r>, :out
        ).out.lines.map(*.&after("/"));
    }

    %rak<mapper> := -> $, @matches {
        if @matches {
            if @matches == 1 {
                run 'git', 'checkout', @matches.head;
                Empty
            }
            else {
                sayer "Found @matches.elems() branches matching '"
                  ~ BON ~ $pattern ~ BOFF ~ "':";
                @matches.Slip
            }
        }
        elsif $pattern eq 'master' {
            run <git checkout main>;
            Empty
        }
        elsif $pattern eq 'main' {
            run <git checkout master>;
            Empty
        }
        else {
            sayer "No branch found with '" ~ BON ~ $pattern ~ BOFF ~ "'.";
            if $verbose {
                sayer "@branches.elems() branches known:";
                @branches.Slip
            }
            else {
                sayer "@branches.elems() branches known, add --verbose to see them";
                Empty
            }
        }
    }
}

# Entry point for CLI processing
my proto sub MAIN(|) {*}
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
    my &line-post-proc = *.Str;
    if Regex.ACCEPTS($needle) {
        &line-post-proc = make-highlighter($needle, %n, %rak)
    }
    elsif Callable.ACCEPTS($needle) {
        $is-simple-Callable = True;
    }
    # non-executable, create executable needle and highlighter
    else {
        $needle = needleify($pattern, %n);
        &line-post-proc = make-highlighter($pattern, %n, %rak)
    }

    # Pass on any context settings
    %rak{.key} := .value for %n<
      context before-context after-context paragraph-context passthru-context
    >:delete:p;

    # Various setups
    setup-sources-selection(@specs, %n, %rak);
    setup-producers(@specs, %n, %rak);

    # Only interested in filenames
    my $filename-only;
    if %n<filename-only>:delete {
        $filename-only := True;

        # Only interested in number of files
        if %n<count-only>:delete {
            my int $seen;
            %rak<max-matches-per-source> := 1;
            %rak<mapper> := -> $, @ --> Empty {
                LAST sayer $seen == 0
                  ?? "No files"
                  !! $seen == 1
                    ?? "One file"
                    !! "$seen files";
                ++$seen;
            }
        }

        # Need to separate files with a null-byte
        elsif %n<file-separator-null>:delete {
            my @files;
            %rak<max-matches-per-source> := 1;
            %rak<mapper> := -> $source, @ --> Empty {
                LAST sayer @files.join("\0");
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
        handle-edit($editor, $pattern, %n, %rak);
    }

    # Editor searching for files
    elsif %n<vimgrep>:delete {
        handle-vimgrep($pattern, %n, %rak);
    }

    # Modifying files
    elsif %n<modify-files>:delete {
        handle-modify-files($pattern, %n, %rak);
    }

    # Perform checkout in current repo if only one match
    elsif %n<checkout>:delete {
        handle-checkout($pattern, %n, %rak);
    }

    # Just find path names
    elsif %n<find>:delete {
        %rak<find>            := True;
        %rak<find-all>        := $_ with %n<find-all>:delete;
        %rak<omit-item-number> = True;
    }

    # Set up standard flags
    %rak{.key} := .value for %n<
      quietly silently invert-match under-version-control degree batch
    >:delete:p;

    # Fetch arguments we definitely need now
    my $show-filename    := %n<show-filename>:delete // True;
    my $show-line-number := %n<show-line-number>:delete // True;
    my int $only-first = .Int with %n<only-first>:delete;
    %rak<omit-item-number> = True if $filename-only || !$show-line-number;

    # Set up statistics settings
    my $count-only := %n<count-only>:delete;
    %rak<stats-only> := True if %n<stats-only>:delete || $count-only;
    %rak<stats>      := True if %n<stats>:delete;

    # Set up frequency settings
    if %n<frequencies>:delete {
        %rak<frequencies> := True;
        %rak<omit-item-number>:delete;  # implicit
    }

    # Set up unique setting
    elsif %n<unique>:delete {
        %rak<unique> := True;
        %rak<omit-item-number>:delete;  # implicit
    }

    my $group-matches;
    my $break;
    if $show-filename {
        $group-matches := %n<group-matches>:delete // True;
        $break = $_ with %n<break>:delete // $group-matches;
        unless $break<> =:= False  {
            $break = "" but True
              if Bool.ACCEPTS($break) || ($break.defined && !$break);
        }
    }

    # Remove arguments that have been handled now
    %n<ignorecase ignoremark type>:delete;

    # Debug parameters passed to rak
    dd %rak if %n<rak>:delete;

    # Do the work, return the result
    meh-if-unexpected(%n);
    my $rak := rak $needle, %rak;
    meh .message with $rak.exception;
    note "Unexpected leftovers: %rak.raku()" if %rak;

    my &source-post-proc = {
        IO::Path.ACCEPTS($_) ?? .relative !! $_
    }

    # show the results!
    my int $seen;
    RESULT:
    for $rak.result -> $outer {
        if Pair.ACCEPTS($outer) {
            my $key   := $outer.key;
            my $value := $outer.value;

            # Just listing paths
            if $key eq '<find>' {
                for @$value.sort(*.fc) {
                    sayer IO::Path.ACCEPTS($_)
                      ?? line-post-proc .absolute
                      !! $_;
                    last RESULT if ++$seen == $only-first;
                }
            }

            # Looks like normal search result
            elsif Iterable.ACCEPTS($value) {
                if $value -> @matches {
                    my $source := $key.relative;
                    sayer $break if $break && $seen;

                    if PairContext.ACCEPTS(@matches.head) {
                        if $group-matches {
                            sayer $source if $show-filename;
                            for @matches {
                                sayer .key ~ ':' ~ (.matched
                                  ?? line-post-proc .value
                                  !! .value.Str
                                );
                                last RESULT if ++$seen == $only-first;
                            }
                        }

                        # Not grouping
                        elsif $show-filename {
                            for @matches {
                                sayer $source
                                  ~ ':' ~ .key
                                  ~ ':' ~ (.matched
                                  ?? line-post-proc .value
                                  !! .value.Str
                                );
                                last RESULT if ++$seen == $only-first;
                            }
                        }

                        # Not grouping and don't want to know the filename
                        else {
                            for @matches {
                                sayer .key
                                  ~ ':' ~ (.matched
                                  ?? line-post-proc .value
                                  !! .value.Str
                                );
                                last RESULT if ++$seen == $only-first;
                            }
                        }
                    }

                    # No line numbers expected or not showing filename
                    elsif $group-matches || !$show-filename {
                        sayer $source if $show-filename;
                        for @matches {
                            sayer line-post-proc $_;
                            last RESULT if ++$seen == $only-first;
                        }
                    }

                    # No line numbers and not grouping
                    elsif $show-filename {
                        for @matches {
                            sayer $source ~ ':' ~ line-post-proc $_;
                            last RESULT if ++$seen == $only-first;
                        }
                    }
                }
            }

            # looks like frequencies output
            else {
                sayer $outer.value ~ ':' ~ $outer.key;
                last RESULT if ++$seen == $only-first;
            }
        }

        # Only want filename, so show its relative path
        elsif $filename-only {
            sayer $outer.relative;
            last RESULT if ++$seen == $only-first;
        }

        # Probably --unique
        else {
            sayer $outer.Str;
            last RESULT if ++$seen == $only-first;
        }
    }

    # Statistics to show
    if $rak && $rak.stats -> %s {
        if $count-only && !%n<verbose> {
            sayer %s<nr-matches> + %s<nr-changes>
              ~ " matches in %s<nr-sources> files";
        }
        else {
            my str @stats;
            unless $count-only {
                @stats.push: "Statistics for '$pattern':";
                my str $bar = '-' x @stats[0].chars;
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

# Export stuff, mostly for testing
sub EXPORT() {
    Map.new: (
      '&MAIN'     => &MAIN,
      PairContext => PairContext,
      PairMatched => PairMatched,
    )
}

# vim: expandtab shiftwidth=4
