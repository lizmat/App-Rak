# The modules that we need here, with their full identities
use as-cli-arguments:ver<0.0.4>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;
use has-word:ver<0.0.3>:auth<zef:lizmat>;
use highlighter:ver<0.0.14>:auth<zef:lizmat>;
use JSON::Fast:ver<0.17>:auth<cpan:TIMOTIMO>;
use rak:ver<0.0.21>:auth<zef:lizmat>;
use String::Utils:ver<0.0.8>:auth<zef:lizmat>;

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
my sub meh-if-unexpected(%_) {
    return unless %_;

    my str @text;
    for %_.keys -> $option {
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

my str $mapper-for;  # option for which to setup mapper
my &mapper-code;     # Callable of the actual mapper

my %global;      # global arguments
my %filesystem;  # filesystem selection args
my %csv;         # arguments needed for --csv-per-line
my %highlight;   # highlighting options specified
my %listing;     # listing options specified
my %modify;      # modify options specified

my $needle;      # what to actually look for
my %rak;         # arguments to be sent to rak()
my $rak;         # the result of calling rak()

my &sayer = do {
    my $out := $*OUT;
    -> $_ { $out.say($_) }
}

# Run the query
my sub run-rak() {
    if $debug-rak {
        note .key ~ ': ' ~ .value.raku for %rak.sort(*.key);
    }

    $rak := rak $needle, %rak;
    meh .message with $rak.exception;
    note "Unexpected leftovers: %rak.raku()" if %rak;
}

# Statistics to show
my sub rak-stats() {
    if $rak.stats -> %s {
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
}

#-------------------------------------------------------------------------------
# Helper subroutines for setting up data structures from option handling

# Indicate the action to be performed
my sub set-source($name, $value --> Int) {
    meh "Can only have one source at a time:\n'--$_' was specified before '--$name'";
      with $source-for;
    $source-for := $name;
    $source     := $value;
}

# Indicate the action to be performed
my sub set-action($name, $value --> Nil) {
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
my sub set-global-flag(\variable, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? $value
        ?? %global{$name} := True
        !! Nil
      !! meh "'--$name' can only be specified as a flag";
}

# handle rak boolean flag
my sub set-rak-flag($name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? $value
        ?? (%rak{$name} := True)
        !! Nil
      !! meh "'--$name' can only be specified as a flag";
}

# handle rak integer option
my sub set-rak-Int($name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%rak{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}

# handle rak boolean flag
my sub set-rak-flag($name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%filesystem{$name} := $value)
      !! meh "'--$name' can only be specified as a flag";
}

# handle file attributes that return an Instant
my sub set-filesystem-Instant($name, $value --> Nil) {
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
my sub set-filesystem-Int($name, $value --> Nil) {
    meh "Must specify a condition for '--$name'" if Bool.ACCEPTS($value);

    my $compiled := convert-to-simple-Callable($value);
    Callable.ACCEPTS($compiled)
      ?? (%filesystem{$name} := $compiled)
      !! meh "Problem compiling condition for '--$name': $value";
}

# handle file attributes that return an id
my sub set-filesystem-id($name, $value --> Nil) {
    meh "Must specify a condition for '--$name'" if Bool.ACCEPTS($value);

    my $compiled := convert-to-simple-Callable($code);
    %filesystem{$name} := do if Callable.ACCEPTS($compiled) {
        $compiled
    }
    elsif (try $code.Int) -> $id {
        my $ := * == $id
    }
    else {
        meh "Must specify an integer or an expression with --$name";
    }
}

# handle file attributes that need a name
my sub set-filesystem-name($name, $value, $name-getter, $id-getter --> Nil) {
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
    my $compiled := convert-to-simple-Callable($valuee);
    %rak{$filesystem} := do if Callable.ACCEPTS($compiled) {
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
        my $uid := @ids.head;
        my $ := @ids == 1
          ?? * == $id
          !! * (elem) @ids
    }
}

# handle external execution
my sub external-execution($name, $value) {
    Bool.ACCEPTS($value)
      ?? meh("Must specify arguments for '--$name'")
      !! (%filesystem{$name} := $value);
}

# handle additional CSV parameters
my sub set-csv-flag($name, $value) {
    Bool.ACCEPTS($value)
      !! (%csv{$name} := $value)
      ?? meh("'--$name' can only be specified as a flag");
}

# Set up mapper for given option
my sub setup-mapper($option-name, &code --> Nil {
    meh "Can only have one mapper: --$option-name and --$producer-for both map";
      if $mapper-for;

    $mapper-for  := $option-name;
    &mapper-code := &code;
}

# Set up highlighting boolean option
my sub set-highlight-flag($name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%highlight{$name} := $value)
      ?? meh("'--$name' can only be specified as a flag");
}

# Set up highlighting integer option
my sub set-highlight-Int($name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%highlight{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}

# Set up highlighting string option
my sub set-highlight-Str($name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--$name' must be specified with a string";
      ?? (%highlight{$name} := $value)
}

# Set up listing boolean option
my sub set-listing-flag($name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%listing{$name} := $value)
      ?? meh("'--$name' can only be specified as a flag");
}

# Set up listing integer option
my sub set-listing-Int($name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%listing{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}

# Set up listing string option
my sub set-listing-Str($name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--$name' must be specified with a string";
      ?? (%listing{$name} := $value)
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

my sub option-backup($value --> Nil) { }
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

my sub option-count-only($value --> Nil) { }
    set-global-flag('count-only', $value);
}

my sub option-created($value --> Nil) { }
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
      !! %rak<encoding> := $value;
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
      !! (%csv{$name} := (%line-endings{$value} // $value));
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

my sub option-keep-meta($value --> Nil) { }
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

my sub option-modified($value --> Nil) { }
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
      !! $output-file := $value;
}

my sub option-pager($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'pager--' expects a program specification"
      !! $pager := $value;
}

my sub option-paragraph-context($value --> Nil) { }
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
      !! %global<pattern> := $value;
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

my sub option-rak($value --> Nil) { }
    Bool.ACCEPTS($value)
      ?? $debug-rak := $value
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
      !! $save := $value;
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

my sub option-show-line-number($value --> Nil) { }
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
        ?? set-highlight-Str('type', $value);
        !! meh "'$value' is not an expected --type";
}

my sub option-uid($value --> Nil) {
    set-filesystem-id('uid', $value);
}

my sub option-under-version-control($value --> Nil) {
    set-source-flag('under-version-control', $value);
}

my sub option-unique($value --> Nil) {
    set-listing-flag('unique', $value);
}

my sub option-user($value --> Nil) {
    set-filesystem-name('user', $value, 'getpwnam', 'getpwuid');
}

my sub option-verbose($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? $verbose := $value
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

my sub meh-filesystem($name --> Nil) {
    meh qq:to/MEH/ if %filesystem;
These filesystem options are incompatible with --$name:
%filesystem.keys.sort.map({"--$_"})
MEH
}

my sub meh-csv($name --> Nil) {
    meh qq:to/MEH/ if %csv;
These CSV options are incompatible with --$name:
%csv.keys.sort.map({"--$_"})
MEH
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

my sub activate-output-options() {
    if $pager {
        meh "Cannot specify a pager and an output-file" if $output-file;
        $*OUT = (run $pager.words, :in).in;
    }
    elsif $output-file {
        $*OUT = open($path, :w) if $path ne "-";
    }
}

#-------------------------------------------------------------------------------
# Actions.  These are subroutines that start with "action-" and as such handle
# the associated action arguments.  These all have access to the variables set
# up during option processing.

my sub action-blame-per-file(--> Nil) {
    meh-csv('blame-per-file');

    if %source<under-version-control>:delete {
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

    if %source<under-version-control>:delete {
        meh-filesystem('under-version-control');
        %rak<under-version-control> := True;
    }
    else {
        add-filesystem-options-to-rak;
    }
    %rak<omit-item-number> := True;
    %rak<batch>            := 1;
    %rak<produce-many>     := -> $io { Git::Blame::File.new($io).lines }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
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

    %rak<mapper> := -> $, @matches {
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

        Empty
    }

    run-rak;
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
              :editor(Bool.ACCEPTS($editor) ?? Any !! $editor)
        }
    }

    # Look for locations in files to edit
    else {
        my @files;
        %rak<mapper> := -> $source, @matches --> Empty {
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

    run-rak;
    rak-stats;
}

my sub action-help(--> Nil) {
    meh-only('version');

    activate-output-options;
    my proto sub MAIN(|) {*}
    use CLI::Help:ver<0.0.4>:auth<zef:lizmat> %?RESOURCES, &MAIN, &HELP, 'long';
    MAIN(:help, :$verbose);  # XXX pattern ?  options ??
}

my sub action-json-per-file(--> Nil) {
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

    my %args = |%csv, |%filesystem, |%global, |%rak,
      (output-file => $output-file if $output-file),
      (pager       => $pager       if $pager),
      (:$verbose                   if $verbose.defined),
    ;
    say as-cli-arguments(%args);
}

my sub action-list-custom-options(--> Nil) {
    meh-filesystems('list-custom-options');
    meh-csv('list-custom-options');

    activate-output-options;
    my $format := '%' ~ %config.keys>>.chars.max ~ 's: %s';
    for %config.sort(*.key.fc) -> (:$key, :value(%args)) {
        printf $format, $key, as-cli-arguments(%args);
    }
}

my sub action-list-known-extensions(--> Nil) {
    meh-filesystems('list-known-extensions');
    meh-csv('list-known-extensions');

    activate-output-options;
    printf("%9s: %s\n", .key, .value.map({$_ || '(none)'}).Str)
      for %exts.sort(*.key);
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
    add-filesystem-options-to-rak;

    my $ignorecase := %rak<ignorecase>;
    my $ignoremark := %rak<ignoremark>;
    my $type       := %rak<type>;

    %rak<mapper> := -> $source, @matches {
        my $path := $source.relative;

        sayer $path
          ~ ':' ~ .key
          ~ ':' ~ columns(
                    .value, $pattern, :$ignorecase, :$ignoremark, :$type
                  ).head
          ~ ':' ~ .value
          for @matches;
        
        Empty
    }

    run-rak;
    rak-stats;
}

#--------------------------------------------------------------------------------

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

    my sub meh-with-specs($name) {
        meh "Specified path&s(@specs) '@specs[]' with --$name"
    }

    # files from a file
    if %n<files-from>:delete -> $files-from {
        meh-with-specs('files-from') if @specs;
        %rak<files-from> := $files-from;
    }

    # paths from a file
    elsif %n<paths-from>:delete -> $paths-from {
        meh-with-specs('paths-from') if @specs;
        %rak<paths-from> := $paths-from;
    }

    # paths from command line
    elsif %n<paths>:delete -> $paths {
        meh-with-specs('paths') if @specs;
        if $paths eq "-" {
            %rak<paths-from> := $paths;
        }
        else {
            %rak<paths> := $paths.split(',').List;
        }
    }
    elsif @specs {
        %rak<paths> := @specs.List;
    }

    # Really want to check *all* files
    if %n<find-all>:delete {
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
}

# Set up the producers of information
my sub setup-producers(@specs, %n, %rak) {
    # Set up producers
    %rak<encoding> := my $enc := (%n<encoding>:delete) // 'utf8-c8';
    if %n<per-file>:delete -> $per-file {
        %rak<omit-item-number> := True;
        %rak<produce-one> := $per-file<> =:= True
          ?? *.slurp(:$enc)
          !! convert-to-simple-Callable($per-file)

    }
    elsif %n<per-line>:delete -> $per-line {
        %rak<produce-many> := convert-to-simple-Callable($per-line)
          unless $per-line<> =:= True;
    }

    # after this, these all require simple Callables

    # Match JSON data
    elsif %n<json-per-file>:delete {
        %rak<produce-one> := -> $_ { from-json .slurp(:$enc) }
        %rak<omit-item-number> = True unless %n<files-with-matches>;
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
        CATCH { meh-not-installed 'Git::Blame::File', 'blame-per-line' }
        require Git::Blame::File;

        %rak<produce-many> := -> $io { Git::Blame::File.new($io).lines }
        %rak<under-version-control> := True;
    }
    elsif %n<show-blame>:delete {
        CATCH { meh-not-installed 'Git::Blame::File', 'show-blame' }
        require Git::Blame::File;
        %rak<mapper> := -> $source, @matches {
            my @line-numbers = @matches.map: *.key;
            with Git::Blame::File.new($source, :@line-numbers) -> $blamer {
                $source => $blamer.lines.Slip
            }
            else {
                $source => @matches.map({ .key ~ ':' ~ .value }).Slip
            }
        }
    }
}

# Return a Callable to do highlighting
my sub make-highlighter($needle, %n, %rak) {
    my $type := %n<type>;
    my $trim := %n<trim>:delete;
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
        $pre  = BON  without $pre;
        $post = BOFF without $post;

        my %nameds =
          |(%n<ignorecase ignoremark>:p), :$summary-if-larger-than;
        %nameds<type> = $_ with $type;

        $trim
          ?? -> Str() $line {
                 highlighter $line.trim, $needle<>, $pre, $post, |%nameds
             }
          !! -> Str() $line {
                 highlighter $line, $needle<>, $pre, $post, |%nameds
             }
    }

    # No highlighting wanted, abuse highlighter logic anyway
    else {
        $trim ?? *.Str.trim !! *.Str
    }
}

# Handle --modify-files
my sub handle-modify-files($pattern, %n, %rak) {
    my $dryrun  := %n<dryrun>:delete;
    my $verbose := %n<verbose>:delete;

    my $backup = %n<backup>:delete;
    $backup = ".bak" if $backup<> =:= True;
    $backup = ".$backup" if $backup && !$backup.starts-with('.');

    my constant no-changes =
      "\n*** no changes where made because of --dryrun ***";

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
        if %n<matches-only>:delete {
            my $old-needle = $needle<>;
            $needle = *.&matches($old-needle);
        }
        else {
            &line-post-proc = make-highlighter($needle, %n, %rak)
        }
    }
    elsif Callable.ACCEPTS($needle) {
        $is-simple-Callable = True;
    }

    # non-executable
    elsif %n<matches-only>:delete {
        my %nameds = %n<ignorecase ignoremark type>:p;
        $needle = %nameds
          ?? *.&matches($pattern, |%nameds)
          !! *.&matches($pattern)
    }

    # non-executable, create executable needle and highlighter
    else {
        $needle = needleify($pattern, %n);
        &line-post-proc = make-highlighter($pattern, %n, %rak)
    }

    # Pass on any context settings
    %rak{.key} := .value for %n<
      context before-context after-context paragraph-context passthru-context
      passthru
    >:delete:p;

    # Various setups
    setup-sources-selection(@specs, %n, %rak);
    setup-producers(@specs, %n, %rak);

    my $files-with-matches;
    if %n<max-matches-per-file>:delete -> $max {
        %rak<max-matches-per-source> := $max;
    }

    # Only interested in filenames
    elsif %n<files-with-matches>:delete {

        # Only interested in number of files
        if %n<count-only>:delete {
            my int $seen;
            %rak<max-matches-per-source> := 1;
            %rak<mapper> := -> $, @ --> Empty {
                LAST sayer $seen == 0
                  ?? "No files with matches"
                  !! $seen == 1
                    ?? "One file with matches"
                    !! "$seen files with matches";
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

        # Want to know files with matches
        else {
            $files-with-matches := True;
            %rak<sources-only>  := True;
            &line-post-proc = *.relative;
        }
    }

    # Want to know files without matches
    elsif %n<files-without-matches>:delete {

        # Only interested in number of files
        if %n<count-only>:delete {
            my int $seen;
            %rak<max-matches-per-source> := 1;
            %rak<map-all> := True;
            %rak<mapper>  := -> $, @a --> Empty {
                LAST sayer $seen == 0
                  ?? "No files without matches"
                  !! $seen == 1
                    ?? "One file without matches"
                    !! "$seen files without matches";
                ++$seen if @a == 0;
            }
        }

        # Need to separate files with a null-byte
        elsif %n<file-separator-null>:delete {
            my @files;
            %rak<max-matches-per-source> := 1;
            %rak<map-all> := True;
            %rak<mapper>  := -> $source, @a --> Empty {
                LAST sayer @files.join("\0");
                @files.push: $source.relative if @a == 0;
            }
        }

        # Want to know files without matches
        else {
            $files-with-matches := True;  # same final handling
            %rak<sources-without-only>  := True;
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
    %rak<omit-item-number> = True unless $show-line-number;

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
    my $has-break;
    if $show-filename {
        $group-matches := %n<group-matches>:delete // True;
        $break = $_ with %n<break>:delete // $group-matches;
        $break = "" if $break<> =:= True;
        $has-break = !($break.defined && $break<> =:= False);
    }

    # Remove arguments that have been handled now
    %n<ignorecase ignoremark type>:delete;

    # Debug parameters passed to rak
    if %n<rak>:delete {
        note .key ~ ': ' ~ .value.raku for %rak.sort(*.key);
    }

    # Do the work, return the result
    meh-if-unexpected(%n);
    my $rak := rak $needle, %rak;
    meh .message with $rak.exception;
    note "Unexpected leftovers: %rak.raku()" if %rak;

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
