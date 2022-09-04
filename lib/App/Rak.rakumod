# The modules that we need here, with their full identities
use as-cli-arguments:ver<0.0.4>:auth<zef:lizmat>;
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;
use has-word:ver<0.0.3>:auth<zef:lizmat>;
use highlighter:ver<0.0.14>:auth<zef:lizmat>;
use JSON::Fast:ver<0.17>:auth<cpan:TIMOTIMO>;
use rak:ver<0.0.20>:auth<zef:lizmat>;
use String::Utils:ver<0.0.8>:auth<zef:lizmat>;

# Known options in App::Rak
#- start of available options --------------------------------------------------
#- Generated on 2022-09-04T13:29:26+02:00 by tools/makeOPTIONS.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE
my str @options = <@options>;
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of available options ----------------------------------------------------

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

# The epoch value when process started
my $init-epoch = $*INIT-INSTANT.to-posix.head;

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

    # Boolean flags that can also be negated
    # %n<recurse-symlinked-dir recurse-unmatched-dir is-empty is-executable is-readable is-writable is-symbolic-link is-owned-by-group is-owned-by-user has-setuid has-setgid is-sticky is-owner-executable is-owner-readable is-owner-writable is-group-executable is-group-readable is-group-writable is-world-executable is-world-readable is-world-writable> # for option parsing
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
    # %n<accessed created meta-modified modified> # for option parsing
    for <accessed created meta-modified modified> {
        if %n{$_}:delete -> $code is copy {
            $code = $code
              .subst( '.accessed',      '.&accessed',      :g)
              .subst( '.created',       '.&created',       :g)
              .subst( '.changed',       '.&meta-modified', :g)
              .subst( '.meta-modified', '.&meta-modified', :g)
              .subst( '.modified',      '.&modified',      :g)
              .subst(/ <["']> <[0..9smhdw]>+ <["']> \. ago /, {
                $init-epoch - seconds(.substr(1, *-5))
            });

            my $compiled := convert-to-simple-Callable($code);
            if Callable.ACCEPTS($compiled) {
                %rak{$_} := $compiled;
            }
            else {
                meh "Must specify an expression with --$_";
            }
        }
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
    # %n<blocks device-number filesize hard-links inode mode> # option parser
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

    # Checking with external programs
    # %n<exec shell> # option parser
    for <exec shell> {
        %rak{$_} := %n{$_}:delete if %n{$_}:exists;
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
                $blamer.lines.Slip
            }
            else {
                @matches.map({ .key ~ ':' ~ .value }).Slip
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
          ?? -> $line {
                 highlighter $line.trim, $needle<>, $pre, $post, |%nameds
             }
          !! -> $line {
                 highlighter $line, $needle<>, $pre, $post, |%nameds
             }
    }

    # No highlighting wanted, abuse highlighter logic anyway
    else {
        $trim ?? *.Str.trim !! *.Str
    }
}

# Handle --edit
my sub handle-edit($editor, $pattern, %n, %rak) {
    my $ignorecase := %n<ignorecase>;
    my $ignoremark := %n<ignoremark>;
    my $type       := %n<type>;

    if %n<find>:delete {
        %rak<find>            := True;
        %rak<omit-item-number> = True;
        %rak<mapper> := -> $, @files --> Empty {
            edit-files
              @files,
              :editor(Bool.ACCEPTS($editor) ?? Any !! $editor)
        }
    }
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

# Handle --checkout
my sub handle-checkout($pattern, %n, %rak) {
    my $verbose := %n<verbose>:delete;

    %rak<sources>         := 'checkout';
    %rak<omit-item-number> = True;
    %rak<map-all>         := True;
    %rak<dir file>:delete;  # XXX needs a better way to prevent leftovers

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

            # one of the branches is an exact match
            elsif $pattern (elem) @matches {
                run 'git', 'checkout', $pattern;
                Empty
            }
            else {
                sayer "Found @matches.elems() branches matching '"
                  ~ BON ~ $pattern ~ BOFF ~ "':";
                @matches.Slip
            }
        }

        # Special casing of master / main confusion
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
                    my $source := $key.relative;
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
