# The modules that we need here, with their full identities
use as-cli-arguments:ver<0.0.4>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;
use has-word:ver<0.0.3>:auth<zef:lizmat>;
use highlighter:ver<0.0.14>:auth<zef:lizmat>;
use JSON::Fast:ver<0.17>:auth<cpan:TIMOTIMO>;
use rak:ver<0.0.22>:auth<zef:lizmat>;
use String::Utils:ver<0.0.10>:auth<zef:lizmat> <after before between is-sha1>;

# The epoch value when process started
my $init-epoch = $*INIT-INSTANT.to-posix.head;

# Defaults for highlighting on terminals
my constant BON  = "\e[1m";   # BOLD ON
my constant BOFF = "\e[22m";  # BOLD OFF

# Options of other programs that may be false friends
my constant %falsies =
# our own
  changed          => 'meta-modified',
  run              => 'exec',

# from ack
  A                => 'after-context',
  B                => 'before-context',
  C                => 'context',
  c                => 'count-only',
  count            => 'count-only',
  dump             => 'list-expanded-options',
  f                => 'find',
  follow           => 'recurse-symlinked-dir',
  group            => 'group-matches',
  H                => 'show-filename',
  with-filename    => 'show-filename',
  h                => 'no-show-filename',
  filename         => 'show-filename',
  heading          => 'group-matches',
  help-types       => 'known-extensions',
  i                => 'ignorecase',
  ignore-case      => 'ignorecase',
  I                => 'no-ignorecase',
  ignore-dir       => 'dir',
  ignore-directory => 'dir',
  k                => 'known-extensions',
  known-types      => 'known-extensions',
  l                => 'files-with-matches',
  L                => 'files-without-matches',
  match            => 'pattern',
  m                => 'max-matches-per-file',
  max-count        => 'max-matches-per-file',
  man              => 'help',
  o                => 'matches-only',
  output           => 'pattern',
  print0           => 'file-separator-null',
  S                => 'smartcase',
  smart-case       => 'smartcase',
  t                => 'extensions',
  type             => 'extensions',
  TYPE             => 'extensions',
  v                => 'invert-match',
  x                => 'files-from',

# from ag
  a                => 'find-all',
  all-types        => 'find-all',
  after            => 'after-context',
  before           => 'before-context',
  0                => 'file-separator-null',
  null             => 'file-separator-null',
  u                => 'find-all',
  unrestricted     => 'find-all',
;

# Options that only make sense after one main option
my constant %sub-options =
  allow-loose-escapes => 'csv-per-line',
  allow-loose-quotes  => 'csv-per-line',
  allow-whitespace    => 'csv-per-line',
  auto-diag           => 'csv-per-line',
  eol                 => 'csv-per-line',
  escape              => 'csv-per-line',
  formula             => 'csv-per-line',
  quote               => 'csv-per-line',
  sep                 => 'csv-per-line',
  strict              => 'csv-per-line',
  keep-meta           => 'csv-per-line',
;

# IO subroutines convertining Instants to epoch transparently
my sub accessed($io)      { $io.accessed.to-posix.head }
my sub created($io)       { $io.created.to-posix.head  }
my sub meta-modified($io) { $io.changed.to-posix.head  }
my sub modified($io)      { $io.modified.to-posix.head }

# Multiplication values for time based filtering
my constant %mult =
  s => 1,                 # seconds
  m => 60,                # minutes
  h => 60 * 60,           # hours
  d => 24 * 60 * 60,      # days
  w => 7 * 24 * 60 * 60,  # weeks
;

# Convert a time specification into number of seconds
my sub seconds($format) {
    my int $seconds;
    for $format.split(
      / <[smhdw]> /, :v, :skip-empty,
    ) -> Int() $value, Str() $type = "d" {
        $seconds += $value * %mult{$type};
    }
    $seconds
}

# Make sure we remember if there's a human watching (terminal connected)
my $isa-tty := $*OUT.t;

# Set up default extension sets
my constant %exts =
  '#c'        => <c h hdl>,
  '#c++'      => <cpp cxx hpp hxx>,
  '#csv'      => ('', <csv psv tsv>).flat.List,
  '#cro'      => ('', 'crotmp'),
  '#html'     => <htm html>,
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

# Return "s" if number is not 1, for error messages
my sub s($elems) { $elems == 1 ?? "" !! "s" }

# Sane way of quitting
my sub meh($message) is hidden-from-backtrace {
    exit note $message;
}

# Quit if module not installed
my sub meh-not-installed($module, $param) {
    meh qq:to/MEH/.chomp;
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
my sub prelude(%n) {
    my $prelude = "";
    if %n<repository>:delete -> \libs {
        $prelude = libs.map({"use lib '$_'; "}).join;
    }
    if %n<module>:delete -> \modules {
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

# Drop the "location" of the warning, as it serves no useful purpose here
my sub drop-location-from-warning($warning) {
    note $warning.gist.lines.grep(!*.starts-with('  in block')).join("\n");
    $warning.resume;
}

# Change list of conditions into a Callable for :file
my sub codify-extensions(*@extensions) {
    if @extensions == 1 {
        my $extension := @extensions.head;
        -> $_ { !is-sha1($_) && extension($_) eq $extension }
    }
    else {
        -> $_ { !is-sha1($_) && extension($_) (elem) @extensions }
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

#-------------------------------------------------------------------------------
# Variables for grouping options given

my $verbose;      # process verbose
my $pager;        # process pager if defined
my $output-file;  # process output file if defined
my $debug-rak;    # process show rak args
my $save;         # process save options under name
my @modules;      # list of modules to -use-
my @repos;        # list of repositories to include with -use lib-

my $source-for;  # name of option providing sources
my $source;      # associated value (if any)

my $action-for;  # name of option to perform
my $action;      # associated value (if any)

my %global;      # global arguments
my %filesystem;  # filesystem selection args
my %csv;         # arguments needed for --csv-per-line
my %highlight;   # highlighting options specified
my %listing;     # listing options specified
my %modify;      # modify options specified

my $pattern;     # the pattern that was given
my $needle;      # what to actually look for
my %rak;         # arguments to be sent to rak()
my $rak;         # the result of calling rak()

# For now, the routine for outputting anything
my &sayer = do {
    my $out := $*OUT;
    -> $_ { $out.say($_) }
}

# Fetch and normalize any config, we only do List of Pairs nowadays
my %config := do {
    if $config-file.e {
        my %hash := from-json($config-file.slurp);
        for %hash.values { $_ = .pairs.List if Map.ACCEPTS($_) }
        %hash
    }
    else {
        { }
    }
}

# Run the query
my sub run-rak(:$eagerly) {
    if $debug-rak {
        note .key ~ ': ' ~ .value.raku for %rak.sort(*.key);
    }

    %rak<eager> := True if $eagerly;
    $rak := rak $needle, %rak;
    meh .message with $rak.exception;
    note "Unexpected leftovers: %rak.raku()" if %rak;
}

# Show the results
my sub rak-results() {

    my $break              := %listing<break>:delete;
    my $files-with-matches := %listing<files-with-matches>:delete;
    my $group-matches      := %listing<group-matches>:delete;
    my $has-break          := $break.defined;
    my int $only-first      = %listing<only-first>:delete // 0;
    my $show-filename      := %listing<show-filename>:delete;

    my &line-post-proc;

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
                    my $source := $key.relative if IO::Path.ACCEPTS($key);
                    sayer $break if $has-break && $seen;

                    if PairContext.ACCEPTS(@matches.head) {
                        if $group-matches {
                            sayer $source if $show-filename;
                            for @matches.map({ $_ if .value.elems }) {
                                sayer .key ~ ':' ~ (.matched
                                  ?? line-post-proc .value
                                  !! .value.Str
                                );
                                last RESULT if ++$seen == $only-first;
                            }
                        }

                        # Not grouping
                        elsif $show-filename {
                            for @matches.map({ $_ if .value.elems }) {
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
                            for @matches.map({ $_ if .value.elems }) {
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
        elsif $files-with-matches {
            sayer $outer.relative;
            last RESULT if ++$seen == $only-first;
        }

        # Probably --unique
        else {
            sayer $outer.Str;
            last RESULT if ++$seen == $only-first;
        }
    }
}

# Statistics to show
my sub rak-stats(:$count-only) {
    if $rak.stats -> %s {
        if $count-only && !$verbose {
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
}

#-------------------------------------------------------------------------------
# Helper subroutines for setting up data structures from option handling

# Indicate the action to be performed
my sub set-source(str $name, $value --> Int) {
    meh "Can only have one source at a time:\n'--$_' was specified before '--$name'"
      with $source-for;
    $source-for := $name;
    $source     := $value;
}

# Indicate the action to be performed
my sub set-action(str $name, $value --> Nil) {
    if Bool.ACCEPTS($value) {
        if $value {
            meh qq:to/MEH/ with $action-for;
Can only have one action at a time:
'--$_' was specified before '--$name'
MEH
        }
        else {
            return;  # ignore --/action
        }
    }
    $action-for := $name;
    $action     := $value;
}

# handle global boolean flag
my sub set-global-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? $value
        ?? (%global{$name} := True)
        !! Nil
      !! meh "'--$name' can only be specified as a flag";
}

# handle rak boolean flag
my sub set-rak-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? $value
        ?? (%rak{$name} := True)
        !! Nil
      !! meh "'--$name' can only be specified as a flag";
}

# handle rak integer option
my sub set-rak-Int(str $name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%rak{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}

# handle filesystem boolean flag
my sub set-filesystem-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%filesystem{$name} := $value)
      !! meh "'--$name' can only be specified as a flag";
}

# handle file attributes that return an Instant
my sub set-filesystem-Instant(str $name, $value --> Nil) {
    meh "Must specify an expression for '--$name'" if Bool.ACCEPTS($value);

    my $code := $value
      .subst( '.accessed',      '.&accessed',      :g)
      .subst( '.created',       '.&created',       :g)
      .subst( '.changed',       '.&meta-modified', :g)
      .subst( '.meta-modified', '.&meta-modified', :g)
      .subst( '.modified',      '.&modified',      :g)
      .subst(/ <["']> <[0..9smhdw]>+ <["']> \. ago /, {
        $init-epoch - seconds(.substr(1, *-5))
    });

    my $compiled := convert-to-simple-Callable($code);
    Callable.ACCEPTS($compiled)
      ?? (%filesystem{$name} := $compiled)
      !! meh "Problem compiling expression for '--$name': $value";
}

# handle file attributes that return an Int
my sub set-filesystem-Int(str $name, $value --> Nil) {
    meh "Must specify a condition for '--$name'" if Bool.ACCEPTS($value);

    my $compiled := convert-to-simple-Callable($value);
    Callable.ACCEPTS($compiled)
      ?? (%filesystem{$name} := $compiled)
      !! meh "Problem compiling condition for '--$name': $value";
}

# handle file attributes that return an id
my sub set-filesystem-id(str $name, $value --> Nil) {
    meh "Must specify a condition for '--$name'" if Bool.ACCEPTS($value);

    my $compiled := convert-to-simple-Callable($value);
    %filesystem{$name} := do if Callable.ACCEPTS($compiled) {
        $compiled
    }
    elsif (try $value.Int) -> $id {
        my $ := * == $id
    }
    else {
        meh "Must specify an integer or an expression with --$name";
    }
}

# handle file attributes that need a name
my sub set-filesystem-name(str $name, $value, $name-getter, $id-getter --> Nil) {
    meh "Must specify a condition or name for '--$name'"
      if Bool.ACCEPTS($value);

    # Get lookuppers
    my (&name-getter, &id-getter) = do {
        CATCH { meh-not-installed "P5$name-getter", $name }
        "use P5$name-getter; &$name-getter, &$id-getter".EVAL
    }

    my sub names2ids($names) {
        $names.split(',').map: {
            name-getter($_)[2]
              // meh "Unknown user name '$_' with --$name";
        }
    }

    # An actual condition
    my $compiled := convert-to-simple-Callable($value);
    %filesystem{$name} := do if Callable.ACCEPTS($compiled) {
        -> $id { $compiled($_) with id-getter($id).head }
    }

    # Negation of list of user names
    elsif $compiled.starts-with('!') {
        my int @ids = names2ids($compiled.substr(1));
        my $id := @ids.head;
        my $ := @ids == 1
          ?? * != $id
          !! { !($_ (elem) @ids) }
    }

    # List of user names
    else {
        my int @ids = names2ids($compiled);
        my $id := @ids.head;
        my $ := @ids == 1
          ?? * == $id
          !! * (elem) @ids
    }
}

# handle external execution
my sub external-execution(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh("Must specify arguments for '--$name'")
      !! (%filesystem{$name} := $value);
}

# handle additional CSV parameters
my sub set-csv-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%csv{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}

# Set up highlighting boolean option
my sub set-highlight-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%highlight{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}

# Set up highlighting integer option
my sub set-highlight-Int(str $name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%highlight{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}

# Set up highlighting string option
my sub set-highlight-Str(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--$name' must be specified with a string"
      !! (%highlight{$name} := $value);
}

# Set up listing boolean option
my sub set-listing-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%listing{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}

# Set up listing integer option
my sub set-listing-Int(str $name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%listing{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}

# Set up listing string option
my sub set-listing-Str(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--$name' must be specified with a string"
      !! (%listing{$name} := $value);
}

#-------------------------------------------------------------------------------
# One subroutine for each supported option.  Is assumed to do right thing for
# that option by setting the appropriate global hashes.  Not expected to return
# anything.  Existence of "&option-foo" means the option exists and is
# supported.  These subroutines are **only** called if the associated option
# is actually specified (after any expansion).

my sub option-accessed($value --> Nil) {
    set-filesystem-Instant('accessed', $value)
}

my sub option-after-context($value --> Nil) {
    set-listing-Int('after-context', $value);
}

my sub option-allow-loose-escapes($value --> Nil) {
    set-csv-flag('allow-loose-escapes', $value);
}

my sub option-allow-loose-quotes($value --> Nil) {
    set-csv-flag('allow-loose-quotes', $value);
}

my sub option-allow-whitespace($value --> Nil) {
    set-csv-flag('allow-whitespace', $value);
}

my sub option-auto-diag($value --> Nil) {
    set-csv-flag('auto-diag', $value);
}

my sub option-backup($value --> Nil) {
    if Bool.ACCEPTS($value) {
        %modify<backup> := "bak" if $value;
    }
    else {
        %modify<backup> := $value;
    }
}

my sub option-batch($value --> Nil) {
    set-rak-Int('batch', $value);
}

my sub option-before-context($value --> Nil) {
    set-listing-Int('before-context', $value);
}

my sub option-blame-per-file($value --> Nil) {
    CATCH { meh-not-installed 'Git::Blame::File', 'blame-per-file' }
    require Git::Blame::File;
    set-action('blame-per-file', $value);
}

my sub option-blame-per-line($value --> Nil) {
    CATCH { meh-not-installed 'Git::Blame::File', 'blame-per-line' }
    require Git::Blame::File;
    set-action('blame-per-line', $value);
}

my sub option-blocks($value --> Nil) {
    set-filesystem-Int('blocks', $value)
}

my sub option-break($value --> Nil) {
    if Bool.ACCEPTS($value) {
        set-listing-Str('break', "") if $value;
    }
    else {
        set-listing-Str('break', $value);
    }
}

my sub option-checkout($value --> Nil) {
    set-action('checkout', $value);
}

my sub option-context($value --> Nil) {
    set-listing-Int('context', $value);
}

my sub option-count-only($value --> Nil) {
    set-action('count-only', $value);
}

my sub option-created($value --> Nil) {
    set-filesystem-Instant('created', $value)
}

my sub option-csv-per-line($value --> Nil) {
    CATCH { meh-not-installed 'Text::CSV', 'csv-per-line' }
    require Text::CSV;
    set-action('csv-per-line', $value);
#
#    setup-producer 'csv-per-line', 'produce-many', {
#        %args<auto-diag> //= True;
#        my $csv := Text::CSV.new(|%csv);
#
#        %rak<file> := codify-extensions %exts<#csv>
#          unless %rak<file>;
#        -> $io { $csv.getline-all($io.open) }
#    }
}

my sub option-degree($value --> Nil) {
    set-rak-Int('degree', $value);
}

my sub option-device-number($value --> Nil) {
    set-filesystem-Int('device-number', $value)
}

my sub option-dir($value --> Nil) {
    %filesystem<dir> := Bool.ACCEPTS($value)
      ?? $value
      !! convert-to-matcher($value);
}

my sub option-dryrun($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%modify<dryrun> := $value)
      !! meh "'--dryrun' can only be specified as a flag";
}

my sub option-edit($value --> Nil) {
    set-action('edit', $value);
}

my sub option-encoding($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh("Must specify an explicit encoding with '--encoding'")
      !! (%rak<encoding> := $value);
}

my sub option-eol($value --> Nil) {
    CATCH { meh-not-installed 'Text::CSV', 'eol' }
    require Text::CSV;

    my constant %line-endings =
      lf   => "\n",
      cr   => "\r",
      crlf => "\r\n";

    Bool.ACCEPTS($value)
      ?? meh("Must specify an explicit line-ending with '--eol'")
      !! (%csv<eol> := (%line-endings{$value} // $value));
}

my sub option-escape($value --> Nil) {
    set-csv-flag('escape', $value);
}

my sub option-exec($value --> Nil) {
    external-execution('exec', $value);
}

my sub option-extensions($value --> Nil) {
    meh "'--extensions' can only be specified as a string"
      if Bool.ACCEPTS($value);

    my @unknown;
    if $value.split(',').map: {
        if .starts-with('#') {
            if %exts{.substr(1)} -> @extensions {
                @extensions.Slip
            }
            else {
                @unknown.push: $_;
                Empty
            }
        }
        else {
            $_
        }
    } -> @extensions {
        meh "No extension&s(@unknown) known for '@unknown[]'"
          if @unknown;
        %filesystem<extensions> := @extensions;
    }
}

my sub option-file($value --> Nil) {
    %filesystem<file> := Bool.ACCEPTS($value)
      ?? $value
      !! convert-to-matcher($value);
}

my sub option-file-separator-null($value --> Nil) {
    set-listing-flag('file-separator-null', $value);
}

my sub option-files-from($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--files-from' must be a file specification, not a flag"
      !! set-source('files-from', $value);
}

my sub option-files-with-matches($value --> Nil) {
    set-listing-flag('files-with-matches', $value);
}

my sub option-files-without-matches($value --> Nil) {
    set-listing-flag('files-without-matches', $value);
}

my sub option-filesize($value --> Nil) {
    set-filesystem-Int('filesize', $value)
}

my sub option-find($value --> Nil) {
    set-global-flag('find', $value);
}

my sub option-find-all($value --> Nil) {
    set-filesystem-flag('find-all', $value);
}

my sub option-formula($value --> Nil) {
    set-csv-flag('formula', $value);
}

my sub option-frequencies($value --> Nil) {
    set-listing-flag('frequencies', $value);
}

my sub option-gid($value --> Nil) {
    set-filesystem-id('gid', $value);
}

my sub option-group($value --> Nil) {
    set-filesystem-name('group', $value, 'getgrnam', 'getgrgid');
}

my sub option-group-matches($value --> Nil) {
    set-listing-flag('group-matches', $value);
}

my sub option-hard-links($value --> Nil) {
    set-filesystem-Int('hard-links', $value);
}

my sub option-has-setgid($value --> Nil) {
    set-filesystem-flag('has-setgid', $value);
}

my sub option-has-setuid($value --> Nil) {
    set-filesystem-flag('has-setuid', $value);
}

my sub option-help($value --> Nil) {
    set-action('help', $value);
}

my sub option-highlight($value --> Nil) {
    set-highlight-flag('highlight', $value);
}

my sub option-highlight-after($value --> Nil) {
    set-highlight-Str('highlight-after', $value);
}

my sub option-highlight-before($value --> Nil) {
    set-highlight-Str('highlight-before', $value);
}

my sub option-ignorecase($value --> Nil) {
    set-rak-flag('ignorecase', $value);
}

my sub option-ignoremark($value --> Nil) {
    set-rak-flag('ignoremark', $value);
}

my sub option-inode($value --> Nil) {
    set-filesystem-Int('inode', $value)
}

my sub option-invert-match($value --> Nil) {
    set-rak-flag('invert-match', $value);
}

my sub option-is-empty($value --> Nil) {
    set-filesystem-flag('is-empty', $value);
}

my sub option-is-executable($value --> Nil) {
    set-filesystem-flag('is-executable', $value);
}

my sub option-is-group-executable($value --> Nil) {
    set-filesystem-flag('is-group-executable', $value);
}

my sub option-is-group-readable($value --> Nil) {
    set-filesystem-flag('is-group-readable', $value);
}

my sub option-is-group-writable($value --> Nil) {
    set-filesystem-flag('is-group-writable', $value);
}

my sub option-is-owned-by-group($value --> Nil) {
    set-filesystem-flag('is-owned-by-group', $value);
}

my sub option-is-owned-by-user($value --> Nil) {
    set-filesystem-flag('is-owned-by-user', $value);
}

my sub option-is-owner-executable($value --> Nil) {
    set-filesystem-flag('is-owner-executable', $value);
}

my sub option-is-owner-readable($value --> Nil) {
    set-filesystem-flag('is-owner-readable', $value);
}

my sub option-is-owner-writable($value --> Nil) {
    set-filesystem-flag('is-owner-writable', $value);
}

my sub option-is-readable($value --> Nil) {
    set-filesystem-flag('is-readable', $value);
}

my sub option-is-sticky($value --> Nil) {
    set-filesystem-flag('is-sticky', $value);
}

my sub option-is-symbolic-link($value --> Nil) {
    set-filesystem-flag('is-symbolic-link', $value);
}

my sub option-is-world-executable($value --> Nil) {
    set-filesystem-flag('is-world-executable', $value);
}

my sub option-is-world-readable($value --> Nil) {
    set-filesystem-flag('is-world-readable', $value);
}

my sub option-is-world-writable($value --> Nil) {
    set-filesystem-flag('is-world-writable', $value);
}

my sub option-is-writable($value --> Nil) {
    set-filesystem-flag('is-writable', $value);
}

my sub option-json-per-file($value --> Nil) {
    set-action('json-per-file', $value);
}

my sub option-json-per-line($value --> Nil) {
    set-action('json-per-line', $value);
##    setup-producer 'json-per-line', 'produce-many', -> $_ {
#        %rak<file> := codify-extensions %exts<#json>
#          unless %rak<file>;
#        *.lines(:enc(%rak<encoding> // 'utf8-c8'),map: *.&from-json
#    }
}

my sub option-keep-meta($value --> Nil) {
    set-csv-flag('keep-meta', $value);
}

my sub option-known-extensions($value --> Nil) {
    meh "'--known-extensions' can only be specified as a flag"
      unless Bool.ACCEPTS($value);
    %filesystem<extensions> := @known-extensions if $value;
}

my sub option-list-custom-options($value --> Nil) {
    set-action('list-custom-options', $value);
}

my sub option-list-expanded-options($value --> Nil) {
    set-action('list-expanded-options', $value);
}

my sub option-list-known-extensions($value --> Nil) {
    set-action('list-known-extensions', $value);
}

my sub option-matches-only($value --> Nil) {
    set-highlight-flag('matches-only', $value);
}

my sub option-max-matches-per-file($value --> Nil) {
    set-listing-Int('max-matches-per-file', $value);
}

my sub option-meta-modified($value --> Nil) {
    set-filesystem-Instant('meta-modified', $value);
}

my sub option-mode($value --> Nil) {
    set-filesystem-Int('mode', $value);
}

my sub option-modified($value --> Nil) {
    set-filesystem-Instant('modified', $value);
}

my sub option-modify-files($value --> Nil) {
    set-action('modify-files', $value);
}

my sub option-module($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--module' expects the name of a module to load"
      !! @modules.push($value)
}

my sub option-only-first($value --> Nil) {
    set-listing-Int('only-first', $value.Int) if $value;
}

my sub option-output-file($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--output-file' expects a file specification"
      !! ($output-file := $value);
}

my sub option-pager($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'pager--' expects a program specification"
      !! ($pager := $value);
}

my sub option-paragraph-context($value --> Nil) {
    set-listing-flag('paragraph-context', $value);
}

my sub option-passthru($value --> Nil) {
    set-listing-flag('passthru', $value);
}

my sub option-passthru-context($value --> Nil) {
    set-listing-flag('pass-thru-context', $value);
}

my sub option-paths($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--paths' must be a path specification, not a flag"
      !! set-source('paths', $value);
}

my sub option-paths-from($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--paths-from' must be a file specification, not a flag"
      !! set-source('paths-from', $value);
}

my sub option-pattern($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--pattern' must be a pattern specification, not a flag"
      !! (%global<pattern> := $value);
}

my sub option-per-file($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? set-action('per-file', $value)
      !! meh "'--per-file' must be specified as a flag"
}

my sub option-per-line($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? set-action('per-line', $value)
      !! meh "'--per-line' must be specified as a flag"
}

my sub option-quietly($value --> Nil) {
    set-rak-flag('quietly', $value);
}

my sub option-quote($value --> Nil) {
    set-csv-flag('quote', $value);
}

my sub option-rak($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? ($debug-rak := $value)
      !! meh "'--rak' must be specified as a flag";
}

my sub option-recurse-symlinked-dir($value --> Nil) {
    set-filesystem-flag('recurse-symlinked-dir', $value);
}

my sub option-recurse-unmatched-dir($value --> Nil) {
    set-filesystem-flag('recurse-unmatched-dir', $value);
}

my sub option-repository($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--repository' expects the specification of a repository"
      !! @repos.push($value)
}

my sub option-save($value --> Nil) {
    Bool.ACCEPTS($value) || !$value
      ?? meh "'--save' must be contain name to save for"
      !! ($save := $value);
}

my sub option-sayer($value --> Nil) {
    NYI '--sayer'
}

my sub option-sep($value --> Nil) {
    set-csv-flag('sep', $value);
}

my sub option-shell($value --> Nil) {
    external-execution('shell', $value);
}

my sub option-show-blame($value --> Nil) {
    CATCH { meh-not-installed 'Git::Blame::File', 'show-blame' }
    require Git::Blame::File;
    set-highlight-flag('show-blame', $value);
#
#   setup-mapper 'show-blame', -> $source, @matches {
#        my @line-numbers = @matches.map: *.key;
#        with Git::Blame::File.new($source, :@line-numbers) -> $blamer {
#            $source => $blamer.lines.Slip
#        }
#        else {
#            $source => @matches.map({ .key ~ ':' ~ .value }).Slip
#        }
#    }
}

my sub option-show-filename($value --> Nil) {
    set-listing-flag('show-filename', $value);
}

my sub option-show-line-number($value --> Nil) {
    set-listing-flag('show-line-number', $value);
}

my sub option-silently($value --> Nil) {
    set-rak-flag('silently', $value);
}

my sub option-smartcase($value --> Nil) {
    set-global-flag('smartcase', $value);
}

my sub option-stats($value --> Nil) {
    set-rak-flag('stats', $value);
}

my sub option-stats-only($value --> Nil) {
    set-action('stats-only', $value);
}

my sub option-strict($value --> Nil) {
    set-csv-flag('strict', $value);
}

my sub option-summary-if-larger-than($value --> Nil) {
    set-highlight-Int('summary-if-larger-than', $value);
}

my sub option-trim($value --> Nil) {
    set-highlight-flag('trim', $value);
}

my sub option-type($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--type' must be specified with a string"
      !! $value eq 'contains' | 'words' | 'starts-with' | 'ends-with'
        ?? set-highlight-Str('type', $value)
        !! meh "'$value' is not an expected --type";
}

my sub option-uid($value --> Nil) {
    set-filesystem-id('uid', $value);
}

my sub option-under-version-control($value --> Nil) {
    set-global-flag('under-version-control', $value);
}

my sub option-unique($value --> Nil) {
    set-listing-flag('unique', $value);
}

my sub option-user($value --> Nil) {
    set-filesystem-name('user', $value, 'getpwnam', 'getpwuid');
}

my sub option-verbose($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? ($verbose := $value)
      !! meh "'--verbose' must be specified as a flag";
}

my sub option-version($value --> Nil) {
    set-action('version', $value);
}

my sub option-vimgrep($value --> Nil) {
    set-action('vimgrep', $value);
}

my sub option-with-line-endings($value --> Nil) {
    set-rak-flag('with-line-endings', $value);
}

#- start of available options --------------------------------------------------
#- Generated on 2022-09-09T12:13:40+02:00 by tools/makeOPTIONS.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE
my str @options = <accessed after-context allow-loose-escapes allow-loose-quotes allow-whitespace auto-diag backup batch before-context blame-per-file blame-per-line blocks break checkout context count-only created csv-per-line degree device-number dir dryrun edit encoding eol escape exec extensions file file-separator-null files-from files-with-matches files-without-matches filesize find find-all formula frequencies gid group group-matches hard-links has-setgid has-setuid help highlight highlight-after highlight-before ignorecase ignoremark inode invert-match is-empty is-executable is-group-executable is-group-readable is-group-writable is-owned-by-group is-owned-by-user is-owner-executable is-owner-readable is-owner-writable is-readable is-sticky is-symbolic-link is-world-executable is-world-readable is-world-writable is-writable json-per-file json-per-line keep-meta known-extensions list-custom-options list-expanded-options list-known-extensions matches-only max-matches-per-file meta-modified mode modified modify-files module only-first output-file pager paragraph-context passthru passthru-context paths paths-from pattern per-file per-line quietly quote rak recurse-symlinked-dir recurse-unmatched-dir repository save sayer sep shell show-blame show-filename show-line-number silently smartcase stats stats-only strict summary-if-larger-than trim type uid under-version-control unique user verbose version vimgrep with-line-endings>;
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of available options ----------------------------------------------------

#-------------------------------------------------------------------------------
# Subroutines checking applicability of groups of options specified

my sub meh-output-file($name --> Nil) {
    meh "Specifying --output-file is incompatible with --$name" if $output-file;
}

my sub meh-pager($name --> Nil) {
    meh "Using a pager is incompatible with --$name" if $pager;
}

my sub meh-what($name, %hash, $description --> Nil) {
    meh qq:to/MEH/ if %hash;
These $description options are incompatible with --$name:
%hash.keys.sort.map({"--$_"})
MEH
}

my sub meh-csv($name --> Nil) {
    meh-what($name, %csv, 'CSV')
}

my sub meh-listing($name --> Nil) {
    meh-what($name, %listing, 'listing')
}

my sub meh-filesystem($name --> Nil) {
    meh-what($name, %filesystem, 'filesystem');
}

my sub meh-only($name --> Nil) {
    meh "'--$name' must be the only option"
      if %filesystem
      || %global
      || %csv;
}

my sub move-filesystem-options-to-rak(--> Nil) {
    %rak{$_} := %filesystem{$_} for %filesystem.keys;
    %filesystem = ();
}

my sub move-listing-options-to-rak(--> Nil) {
    %rak{$_} := %listing{$_} for %listing.keys;
    %listing = ();
}

my sub activate-output-options() {
    if $pager {
        meh "Cannot specify a pager and an output-file" if $output-file;
        $*OUT = (run $pager.words, :in).in;
    }
    elsif $output-file {
        $*OUT = open($output-file, :w) if $output-file ne "-";
    }
}

#-------------------------------------------------------------------------------
# Actions.  These are subroutines that start with "action-" and as such handle
# the associated action arguments.  These all have access to the variables set
# up during option processing.

my sub action-blame-per-file(--> Nil) {
    meh-csv('blame-per-file');

    if %global<under-version-control>:delete {
        meh-filesystem('under-version-control');
        %rak<under-version-control> := True;
    }
    else {
        move-filesystem-options-to-rak;
    }
    %rak<omit-item-number> := True;
    %rak<batch>            := 1;
    %rak<produce-one>      := -> $io { Git::Blame::File.new($io) }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-blame-per-line(--> Nil) {
    meh-csv('blame-per-file');

    if %global<under-version-control>:delete {
        meh-filesystem('under-version-control');
        %rak<under-version-control> := True;
    }
    else {
        move-filesystem-options-to-rak;
    }
    %rak<omit-item-number> := True;
    %rak<batch>            := 1;
    %rak<produce-many>     := -> $io { Git::Blame::File.new($io).lines }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-checkout(--> Nil) {
    meh-output-file('checkout');
    meh-pager('checkout');
    meh-filesystem('checkout');
    meh-csv('checkout');

    %rak<sources>          := 'checkout';
    %rak<omit-item-number> := True;
    %rak<map-all>          := True;

    my @branches;
    %rak<produce-many> := -> $ {
        @branches = (
          run <git branch -r>, :out
        ).out.lines.map(*.&after("/"));
    }

    %rak<mapper> := -> $, @matches --> Empty {
        if @matches {
            if @matches == 1 {
                run 'git', 'checkout', @matches.head;
            }

            # one of the branches is an exact match
            elsif $pattern (elem) @matches {
                run 'git', 'checkout', $pattern;
            }

            else {
                sayer "Found @matches.elems() branches matching '"
                  ~ BON ~ $pattern ~ BOFF ~ "':";
                sayer "  $_" for @matches;
            }
        }

        # Special casing of master / main confusion
        elsif $pattern eq 'master' {
            run <git checkout main>;
        }
        elsif $pattern eq 'main' {
            run <git checkout master>;
        }
        else {
            sayer "No branch found with '" ~ BON ~ $pattern ~ BOFF ~ "'.";
            if $verbose {
                sayer "@branches.elems() branches known:";
                sayer "  $_" for @branches;
            }
            else {
                sayer "@branches.elems() branches known, add --verbose to see them";
            }
        }
    }

    run-rak(:eagerly);
    rak-stats;
}

my sub action-count-only(--> Nil) {
    meh-csv('count-only');

    %rak<count-only> := True;

    run-rak(:eagerly);
    rak-stats(:count-only);
}

my sub action-csv-per-line(--> Nil) {
    %rak<file> := %filesystem<file>:delete // codify-extensions %exts<#csv>;

    if %listing<show-line-number>:delete {
        # no action needed
    }
    elsif %listing<files-with-matches>:delete {
        %rak<files-with-matches> := True;
    }
    else {
        %rak<omit-item-number> := True;
    }

    %csv<auto-diag> := True unless %csv<auto-diag>:exists;
    my $csv := Text::CSV.new(|%csv);
    %rak<produce-many> := -> $io { $csv.getline-all($io.open) }

    run-rak;
    rak-results;
    rak-stats;
}

my sub action-edit(--> Nil) {
    meh-output-file('edit');
    meh-pager('edit');
    meh-csv('edit');

    # find filenames to edit
    if %global<find>:delete {
        %rak<find>            := True;
        %rak<omit-item-number> = True;
        %rak<mapper> := -> $, @files --> Empty {
            edit-files
              @files,
              :editor(Bool.ACCEPTS($action) ?? Any !! $action)
        }
    }

    # Look for locations in files to edit
    else {
        my @files;
        my $ignorecase := %rak<ignorecase> // False;
        my $ignoremark := %rak<ignoremark> // False;
        my $type       := %rak<type>       // 'contains';

        %rak<mapper> := -> $source, @matches --> Empty {
            LAST {
                edit-files
                  @files,
                  :editor(Bool.ACCEPTS($action) ?? Any !! $action)
            }

            my $path := $source.relative;
            @files.append: @matches.map: {
                $path => .key => columns(
                  .value, $pattern, :$ignorecase, :$ignoremark, :$type
               ).head
            }
        }
    }

    run-rak(:eagerly);
    rak-stats;
}

my sub action-help(--> Nil) {

    activate-output-options;
    my proto sub MAIN(|) {*}
    use CLI::Help:ver<0.0.4>:auth<zef:lizmat> %?RESOURCES, &MAIN, &HELP, 'long';
    MAIN(:help, :$verbose);  # XXX pattern ?  options ??
}

my sub action-json-per-file(--> Nil) {
    meh-csv('json-per-file');

    %rak<file> := %filesystem<file>:delete // codify-extensions %exts<#json>;

    if %listing<show-line-number>:delete {
        # no action needed
    }
    elsif %listing<files-with-matches>:delete {
        %rak<files-with-matches> := True;
    }
    else {
        %rak<omit-item-number> := True;
    }

    my $enc := %rak<encoding>:delete // 'utf8-c8';
    %rak<produce-one> := -> $io { from-json $io.slurp(:$enc) }

    run-rak;
    rak-results;
    rak-stats;
}

my sub action-json-per-line(--> Nil) {
    meh-csv('json-per-line');

    %rak<file> := %filesystem<file>:delete // codify-extensions %exts<#jsonl>;

    if %listing<show-line-number>:delete {
        # no action needed
    }
    elsif %listing<files-with-matches>:delete {
        %rak<files-with-matches> := True;
    }
    else {
        %rak<omit-item-number> := True;
    }

    my $enc := %rak<encoding>:delete // 'utf8-c8';
    %rak<produce-many> := *.lines(:$enc).map: *.&from-json;

    run-rak;
    rak-results;
    rak-stats;
}

my sub action-list-expanded-options(--> Nil) {
    activate-output-options;

    my %args = |%csv, |%filesystem, |%listing, |%global, |%rak,
      (output-file => $output-file if $output-file),
      (pager       => $pager       if $pager),
      (:$verbose                   if $verbose.defined),
    ;
    say as-cli-arguments(%args);
}

my sub action-list-custom-options(--> Nil) {
    meh-filesystem('list-custom-options');
    meh-csv('list-custom-options');
    meh-listing('list-custom-options');

    activate-output-options;
    my $format := '%' ~ %config.keys>>.chars.max ~ 's: %s';
    for %config.sort(*.key.fc) -> (:$key, :value(@args)) {
        printf $format, $key, as-cli-arguments(@args);
    }
}

my sub action-list-known-extensions(--> Nil) {
    meh-filesystem('list-known-extensions');
    meh-csv('list-known-extensions');
    meh-listing('list-known-extensions');

    activate-output-options;
    printf("%9s: %s\n", .key, .value.map({$_ || '(none)'}).Str)
      for %exts.sort(*.key);
}

my sub action-modify-files(--> Nil) {
    meh-output-file('modify-files');
    meh-pager('modify-files');
    meh-csv('modify-files');
    meh-listing('modify-files');

    my $dryrun := %modify<dryrun>:delete;
    my $backup  = %modify<backup>:delete;
    $backup = ".bak" if $backup<> =:= True;
    $backup = ".$backup" if $backup && !$backup.starts-with('.');

    my constant no-changes =
      "\n*** no changes where made because of --dryrun ***";

    my @changed-files;
    my int $nr-files-seen;
    my int $nr-lines-changed;
    my int $nr-lines-removed;

    %rak<passthru-context> := %listing<passthru-context>:delete  // True;
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
                $fb ~= no-changes if $dryrun;
                $fb .= chomp;
            }
            elsif $dryrun {
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
            unless $dryrun {
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

    run-rak(:eagerly);
    rak-stats;
}

my sub action-version(--> Nil) {
    meh-only('version');

    activate-output-options;
    my proto sub MAIN(|) {*}
    use CLI::Version:ver<0.0.7>:auth<zef:lizmat> $?DISTRIBUTION, &MAIN, 'long';
    MAIN(:version, :$verbose);
}

my sub action-vimgrep(--> Nil) {
    meh-csv('vimgrep');

    activate-output-options;
    move-filesystem-options-to-rak;

    my $ignorecase := %rak<ignorecase>;
    my $ignoremark := %rak<ignoremark>;
    my $type       := %rak<type>;

    %rak<mapper> := -> $source, @matches --> Empty {
        my $path := $source.relative;

        sayer $path
          ~ ':' ~ .key
          ~ ':' ~ columns(
                    .value, $pattern, :$ignorecase, :$ignoremark, :$type
                  ).head
          ~ ':' ~ .value
          for @matches;
    }

    run-rak(:eagerly);
    rak-stats;
}

#--------------------------------------------------------------------------------
# Actually set up all variables from the arguments specified and run.
# Theory of operation:
#
# 1. Loop over all of the strings in @*ARGS
#     - does it NOT start with "-"?  -> positional argument
#     - named argument: call "set-$name" with the given value
#     - add to unexpected if sub doesn't exist
# 2. See of an action name has been set, of not: assume 'per-line'
# 3. Run the "action-$name" sub
# 4. Close STDOUT if a pager was used

# Positional arguments
my @positionals;
# Pairs of unexpected arguments and their value
my @unexpected;

# Helper sub to recursively handle named arguments
my sub named($original-name, $original-value, :$recurse = True) {
    my sub is-default($value) {
        $value.starts-with('[') && $value.ends-with(']')
    }

    # Can recurse and we're allowed to
    if $recurse && %config{$original-name} -> @expanded {

        # Got a flag
        if Bool.ACCEPTS($original-value) {
            if $original-value {
                for @expanded -> (:key($name), :$value)  {
                    meh("Must specify a value for $original-name for $name")
                      if $value eq '!';

                    named(
                      $name,
                      is-default($value)
                        ?? $value.substr(1, *-1)
                        !! $value,
                      :recurse($name ne $original-name)
                    );
                }
            }
        }

        # Git a value
        else {
            for @expanded -> (:key($name), :$value)  {
                named(
                  $name,
                  $value eq '!' || is-default($value)
                    ?? $original-value
                    !! $value,
                  :recurse($name ne $original-name)
                );
            }
        }
    }

    # Known option
    elsif ::("&option-$original-name") -> &handler {
        handler($original-value);
    }
    else {
        @unexpected.push: Pair.new: $original-name, $original-value;
    }
}

# Do the actual argument parsing
for @*ARGS {

    # looks like an option
    if .starts-with('-') {

        # Allow -j2 as an alternative to --j=2, aka :numeric-suffix-as-value
        $_ = "-$_.substr(0,2)=$/" if .match: /^ '-' <.alpha> <( \d+ $/;

        if .starts-with('--/') {
            my ($before,$after) = .substr(3).split('=',2);
            named $before, $after // False;
        }
        elsif .starts-with('--no-') {
            my ($before,$after) = .substr(5).split('=',2);
            named $before, $after // False;
        }
        elsif .starts-with('--') {
            my ($before,$after) = .substr(2).split('=',2);
            named $before, $after // True;
        }
        elsif .starts-with('-/') {
            my ($before,$after) = .substr(2).split('=',2);
            if $before.chars == 1 {
                named $before, $after // False;
            }
            elsif $after.defined {
                named $_, $after for $before.comb;
            }
            else {
                named $_, False for $before.comb;
            }
        }
        else {  # .starts-with('-')
            my ($before,$after) = .substr(1).split('=',2);
            if $before.chars == 1 {
                named $before, $after // True;
            }
            elsif $after.defined {
                named $_, $after for $before.comb;
            }
            else {
                named $_, True for $before.comb;
            }
        }
    }

    # not an option
    else {
        @positionals.push: $_;
    }
}

# Find one-line description of given name
my sub description($name) {
    my $key := " --$name";
    if %?RESOURCES<help.txt>.lines.first(*.starts-with(" --$name")) -> $line {
        $line.substr(1).split(/ \s+ /, 2).tail
    }
    else {
        ""
    }
}

# Quit if unexpected named arguments hash
my sub meh-unexpected() {

    my str @text;
    for @unexpected -> (:key($option), :$value) {
        # Looks like an option from another program
        if %falsies{$option} -> $alias {
            @text.push: "Option --$option is called --$alias with 'rak'.";
        }

        # Direct match of sub-option
        elsif %sub-options{$option} -> $main-option {
            @text.push: "The --$option option only makes sense with --$main-option.";
        }

        # Direct match
        elsif description($option) -> $description {
            @text.push: qq:to/TEXT/.chomp;
The --$option option ($description)
does not make sense in this context.  If you believe this to be incorrect,
please report an issue with https://github.com/lizmat/App-Rak/issues/new .
TEXT
        }

        # There are matches
        elsif @options.map(-> $after {
            $after => StrDistance.new(:before($option), :$after).Int
        }).sort(*.value).head(5).List -> @alternatives {

            @text.push: "Regarding unexpected option --$option, did you mean:";
            my int $cutoff = $option.chars;
            for @alternatives.grep(*.value <= $cutoff) -> (
              :key($name), :value($steps)
            ) {
                if $name eq 'help' {
                }
                elsif description($name) -> $description {
                    @text.push: " --$name: $description?";
                }
                elsif %sub-options{$name} -> $main {
                    @text.push: " --$name: Must then include --$main then as well?";
                }
                else {
                    @text.push: " --$name?";
                }
            }
        }
    }

    @text.push: "Use --help for an overview of available options.";
    exit note @text.join("\n");
}

#--------------------------------------------------------------------------------
# Perform the appropriate action if possible



meh-unexpected if @unexpected;

# Done
$*OUT.close if $pager;

# vim: expandtab shiftwidth=4
