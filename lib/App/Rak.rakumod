# The modules that we need here, with their full identities
use as-cli-arguments:ver<0.0.6>:auth<zef:lizmat>;  # as-cli-arguments
use Edit::Files:ver<0.0.4>:auth<zef:lizmat>;       # edit-files
use has-word:ver<0.0.3>:auth<zef:lizmat>;          # has-word
use highlighter:ver<0.0.14>:auth<zef:lizmat>;      # columns highlighter matches
use JSON::Fast::Hyper:ver<0.0.3>:auth<zef:lizmat>; # from-json to-json
use rak:ver<0.0.29>:auth<zef:lizmat>;              # rak
use String::Utils:ver<0.0.12>:auth<zef:lizmat> <after before between is-sha1>;

# The epoch value when process started
my $init-epoch = $*INIT-INSTANT.to-posix.head;

# Defaults for highlighting on terminals
my constant BON  = "\e[1m";   # BOLD ON
my constant BOFF = "\e[22m";  # BOLD OFF

#- start of available options --------------------------------------------------
#- Generated on 2022-09-27T22:55:07+02:00 by tools/makeOPTIONS.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE
my str @options = <absolute accessed after-context allow-loose-escapes allow-loose-quotes allow-whitespace auto-diag backup batch before-context blame-per-file blame-per-line blocks break checkout context count-only created csv-per-line degree device-number dir dont-catch dryrun edit encoding eol escape exec extensions file file-separator-null files-from files-with-matches files-without-matches filesize find find-all formula frequencies gid group group-matches hard-links has-setgid has-setuid help highlight highlight-after highlight-before human ignorecase ignoremark inode invert-match is-empty is-executable is-group-executable is-group-readable is-group-writable is-owned-by-group is-owned-by-user is-owner-executable is-owner-readable is-owner-writable is-readable is-sticky is-symbolic-link is-world-executable is-world-readable is-world-writable is-writable json-per-elem json-per-file json-per-line keep-meta known-extensions list-custom-options list-expanded-options list-known-extensions matches-only max-matches-per-file meta-modified mode modified modify-files module only-first output-file pager paragraph-context passthru passthru-context paths paths-from pattern per-file per-line proximate rename-files quietly quote rak recurse-symlinked-dir recurse-unmatched-dir repository save sayer sep shell show-blame show-filename show-line-number silently smartcase stats stats-only strict summary-if-larger-than trim type uid under-version-control unicode unique user verbose version vimgrep with-line-endings>;
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of available options ----------------------------------------------------

# Options of other programs that may be false friends
my constant %falsies =
# our own
  changed          => 'meta-modified',
  run              => 'exec',
  first-only       => 'only-first',

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
  p                => 'proximate',
  P                => 'proximate',
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
my $reading-from-stdin := !$*IN.t;
my $writing-to-stdout  := $*OUT.t;

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

# Links to optional classes
my $GitBlameFile;
my $TextCSV;

# Variables for grouping options given
my $verbose;      # process verbose
my $pager;        # process pager if defined
my $output-file;  # process output file if defined
my $debug-rak;    # process show rak args

my $pattern;     # the pattern specified (if any)
my $ignorecase;  # --ignorecase
my $ignoremark;  # --ignoremark
my $type;        # --type

my @modules;  # list of modules to -use-
my @repos;    # list of repositories to include with -use lib-

my $source-for;  # name of option providing sources
my $source;      # associated value (if any)

my $action-for;  # name of option to perform
my $action;      # associated value (if any)

my %global;      # global arguments
my %filesystem;  # filesystem selection args
my %result;      # result modifier options specified
my %listing;     # listing options specified
my %csv;         # arguments needed for --csv-per-line
my %modify;      # arguments needed for --modify-files

my $needle;  # Callable needle for rak
my %rak;     # arguments to be sent to rak()
my $rak;     # the result of calling rak()

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

my @positionals; # Positional arguments
my @unexpected;  # Pairs of unexpected arguments and their value

#--------------------------------------------------------------------------------
# Actually set up all variables from the arguments specified and run.
# Theory of operation:
#
# 1. Loop over all of the strings in @ARGS
#     - does it NOT start with "-"?  -> positional argument
#     - named argument: call "set-$name" with the given value
#     - add to unexpected if sub doesn't exist
# 2. See of an action name has been set, of not: assume 'per-line'
# 3. Run the "action-$name" sub
# 4. Close STDOUT if a pager was used

my sub main(@ARGS) is export {

    # Do the actual argument parsing
    for @ARGS {

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

            # a bare - considered to be a positional
            elsif $_ eq '-' {
                @positionals.push: $_;
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

    # huh?
    meh-unexpected if @unexpected;

    # Set up the pattern
    $pattern := @positionals.shift if !$pattern.defined && @positionals;

    # Save current setting
    if %global<save>:delete -> $save {
        my @options := as-options;

        @options
          ?? (%config{$save} := @options)
          !! (%config{$save}:delete);

        $config-file.spurt: to-json %config, :!pretty, :sorted-keys;

        say @options
          ?? "Saved '&as-cli-arguments(@options)' as: -$save"
          !! "Removed custom option '--$save'";
        exit;
    }

    elsif %global<list-expanded-options>:delete {
        if $verbose {
            for as-options() {
                if description(.key) -> $description {
                    say "&as-cli-arguments($_): $description";
                }
                else {
                    say as-cli-arguments($_);
                }
            }
        }
        else {
            say as-cli-arguments as-options;
        }
        exit;
    }

    # STDIN sanity checking
    if @positionals && @positionals.head eq '-' {
        if $reading-from-stdin {
            @positionals.shift;
            meh "Cannot specify additional paths when reading from STDIN"
              if @positionals;
        }
        else {
            $reading-from-stdin := True;
        }
    }
    elsif @positionals == 1 && @positionals.head.IO.f {
        %listing<show-filename> := False if %listing<show-filename>:!exists;
    }

    # Perform the actual action
    $action-for ?? ::("&action-$action-for")() !! action-per-line();
    $*OUT.close if $pager;
}

# no mainline code from here
#-------------------------------------------------------------------------------

# Return "s" if number is not 1, for error messages
my sub s($elems) { $elems == 1 ?? "" !! "s" }

# Return '--a, --b' for one or more names
my sub mm(@names) { @names.map({"--$_"}).join(', ') }

# Sane way of quitting
my sub meh($message) is hidden-from-backtrace {
    exit note $message.ends-with('.' | '?')
      ?? $message
      !! "$message.";
}

# Quit if module not installed
my sub meh-not-installed($module, $param) is hidden-from-backtrace {
    meh qq:to/MEH/.chomp;
Must have the $module module installed to do --$param.
You can do this by running 'zef install $module'.
MEH
}

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

# Convert a string to code if possible
my sub codify(Str:D $code) {
    CATCH {
        meh "Could not compile '$code':\n$_.message()";
    }

    # Return prelude from --repository and --module parameters
    my sub prelude() {
        @repos.map({"use lib '$_'; "}).join ~ @modules.map({"use $_; "}).join
    }

    $code eq '*.defined'
      ?? &defined
      !! $code.starts-with('/') && $code.ends-with('/')
        ?? regexify($code)
        !! $code.starts-with('{') && $code.ends-with('}')
          ?? (prelude() ~ 'my $ := -> $_ ' ~ $code).EVAL
          !! $code.starts-with('-> $') && $code.ends-with('}')
            ?? (prelude() ~ 'my $ := ' ~ $code).EVAL
            !! $code.starts-with('*')
              ?? (prelude() ~ 'my $ := ' ~ $code).EVAL
              !! $code
}

# Pre-process literal strings looking like a regex
my sub regexify($code) {
    "/{ ':i ' if $ignorecase }{ ':m ' if $ignoremark }$code.substr(1)".EVAL
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
my sub needleify($pattern) {
    if !$type || $type eq 'contains' {
        $ignorecase
          ?? $ignoremark
            ?? *.contains($pattern, :i, :m)
            !! *.contains($pattern, :i)
          !! $ignoremark
            ?? *.contains($pattern, :m)
            !! *.contains($pattern)
    }
    elsif $type eq 'words' {
        $ignorecase
          ?? $ignoremark
            ?? *.&has-word($pattern, :i, :m)
            !! *.&has-word($pattern, :i)
          !! $ignoremark
            ?? *.&has-word($pattern, :m)
            !! *.&has-word($pattern)
    }
    elsif $type eq 'starts-with' {
        $ignorecase
          ?? $ignoremark
            ?? *.starts-with($pattern, :i, :m)
            !! *.starts-with($pattern, :i)
          !! $ignoremark
            ?? *.starts-with($pattern, :m)
            !! *.starts-with($pattern)
    }
    else {  # $type eq 'ends-with' {
        $ignorecase
          ?? $ignoremark
            ?? *.ends-with($pattern, :i, :m)
            !! *.ends-with($pattern, :i)
          !! $ignoremark
            ?? *.ends-with($pattern, :m)
            !! *.ends-with($pattern)
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
use META::constants:ver<0.0.3>:auth<zef:lizmat> $?DISTRIBUTION;
my sub HELP($text, @keys, :$verbose) {
    my $SCRIPT := $*PROGRAM.basename;
    my $header := "$SCRIPT - " ~ DESCRIPTION;
    say $header;
    say "-" x $header.chars;
    say $writing-to-stdout
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

# Run the query
my sub run-rak(:$eagerly) {
    if $debug-rak {
        note .key ~ ': ' ~ .value.raku for %rak.sort(*.key);
    }
    if (
      %global.keys, %result.keys, %csv.keys, %modify.keys
    ).flat -> @unhandled {
        note qq:to/TEXT/;
The &mm(@unhandled) option&s(@unhandled) {@unhandled == 1 ?? "is" !! "are"} not being handled and will be ignored.
If you believe this to be incorrect, please report an issue with
https://github.com/lizmat/App-Rak/issues/new .
TEXT
    }

    %rak<eager> := True if $eagerly;
    $rak := rak $needle, %rak;
    meh .message with $rak.exception;
    note "Unexpected leftovers: %rak.raku()" if %rak;
}

# Show the results
my sub rak-results() {

    my $human         := %listing<human>:delete // $writing-to-stdout;
    my $show-filename := %listing<show-filename>:delete // True;
    my $absolute      := %listing<absolute>:delete;
    my $break         := %listing<break>:delete;
    my $group-matches := %listing<group-matches>:delete;
    my $highlight     := %listing<highlight>:delete;
    my $trim          := %listing<trim>:delete;
    my $only-first    := %listing<only-first>:delete;
    my $proximate     := %listing<proximate>:delete;

    # Set up human defaults
    if $human {
        $break         := ""   unless $break.defined;
        $group-matches := True unless $group-matches.defined;
        $highlight     := True unless $highlight.defined;
        $trim          := True unless $trim.defined;
        $only-first    := 1000 unless $only-first.defined;
        $proximate     := 1 if !$proximate.defined && $group-matches;
    }
    my $has-break := %listing<has-break>:delete // $break.defined;
    # Switch to really large values if not specified
    my uint $skip-ok    = $proximate  || 0x7fff_ffff_ffff_ffff;
    my uint $stop-after = $only-first || 0x7fff_ffff_ffff_ffff;

    # Set up highlighting
    my $highlight-before;
    my $highlight-after;
    with %listing<highlight-before>:delete {
        $highlight-before := $_;
        $highlight        := True;
    }
    with %listing<highlight-after>:delete {
        $highlight-after := $_;
        $highlight       := True;
    }
    else {
        $highlight-after := $highlight-before;
    }

    my &line-post-proc := do if $highlight {
        my Str() $pre = my Str() $post = $_ with $highlight-before;
        $post                          = $_ with $highlight-after;
        $pre  = BON  without $pre;
        $post = BOFF without $post;

        my %nameds =
          (:$ignorecase if $ignorecase),
          (:$ignoremark if $ignoremark),
          (:$type       if $type),
          (:summary-if-larger-than($_)
            with %listing<summary-if-larger-than>:delete),
        ;

        my $target := Regex.ACCEPTS($needle) ?? $needle !! $pattern;

        $trim
          ?? -> $line {
                 highlighter $line.trim, $target, $pre, $post, |%nameds
             }
          !! -> $line {
                 highlighter $line, $target, $pre, $post, |%nameds
             }
    }

    # No highlighting wanted, abuse highlighter logic anyway
    else {
        $trim ?? *.Str.trim !! *.Str
    }

    # Set way to stringify paths
    my &stringify :=
      IO::Path.^find_method($absolute ?? "absolute" !! "relative");

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
                      ?? line-post-proc stringify($_)
                      !! line-post-proc .Str;
                    last RESULT if ++$seen == $only-first;
                }
            }

            # Looks like normal search result
            elsif Iterable.ACCEPTS($value) {
                if $value -> @matches {
                    my $source := IO::Path.ACCEPTS($key)
                      ?? stringify($key)
                      !! $key;
                    sayer $break if $has-break && $seen;

                    if PairContext.ACCEPTS(@matches.head) {
                        my uint $last-linenr = @matches.head.key - 1;

                        if $group-matches {
                            sayer $source if $show-filename;
                            for @matches.map({ $_ if .value.elems }) {
                                my uint $linenr = .key;
                                sayer "" if $linenr - $last-linenr > $skip-ok;
                                if Slip.ACCEPTS(.value) {
                                    # Can only produce a Slip from a real
                                    # Callable, which cannot have any
                                    # highlighting, so don't bother
                                    sayer "$linenr:$_" for @(.value);
                                }
                                else {
                                    sayer $linenr ~ ':' ~ (.matched
                                      ?? line-post-proc .value.Str
                                      !! .value.Str
                                    );
                                }
                                last RESULT if ++$seen == $stop-after;
                                $last-linenr = $linenr;
                            }
                        }

                        # Not grouping
                        elsif $show-filename {
                            for @matches.map({ $_ if .value.elems }) {
                                my uint $linenr = .key;
                                sayer "" if $linenr - $last-linenr > $skip-ok;
                                if Slip.ACCEPTS(.value) {
                                    # Can only produce a Slip from a real
                                    # Callable, which cannot have any
                                    # highlighting, so don't bother
                                    sayer "$source:$linenr:$_" for @(.value);
                                }
                                else {
                                    sayer $source
                                      ~ ':' ~ $linenr
                                      ~ ':' ~ (.matched
                                      ?? line-post-proc .value.Str
                                      !! .value.Str
                                    );
                                }
                                last RESULT if ++$seen == $stop-after;
                                $last-linenr = $linenr;
                            }
                        }

                        # Not grouping and don't want to know the filename
                        else {
                            for @matches.map({ $_ if .value.elems }) {
                                my uint $linenr = .key;
                                sayer "" if $linenr - $last-linenr > $skip-ok;
                                if Slip.ACCEPTS(.value) {
                                    # Can only produce a Slip from a real
                                    # Callable, which cannot have any
                                    # highlighting, so don't bother
                                    sayer "$linenr:$_" for @(.value);
                                }
                                else {
                                    sayer $linenr
                                      ~ ':' ~ (.matched
                                      ?? line-post-proc .value.Str
                                      !! .value.Str
                                    );
                                }
                                last RESULT if ++$seen == $stop-after;
                                $last-linenr = $linenr;
                            }
                        }
                    }

                    # No line numbers expected or not showing filename
                    elsif $group-matches || !$show-filename {
                        sayer $source if $show-filename;
                        for @matches {
                            if Slip.ACCEPTS($_) {
                                # Can only produce a Slip from a real
                                # Callable, which cannot have any
                                # highlighting, so don't bother
                                sayer .Str for @$_;
                            }
                            else {
                                sayer line-post-proc .Str;
                            }
                            last RESULT if ++$seen == $stop-after;
                        }
                    }

                    # No line numbers and not grouping
                    elsif $show-filename {
                        for @matches {
                            if Slip.ACCEPTS($_) {
                                # Can only produce a Slip from a real
                                # Callable, which cannot have any
                                # highlighting, so don't bother
                                sayer "$source:$_" for @$_;
                            }
                            else {
                                sayer $source ~ ':' ~ line-post-proc .Str;
                            }
                            last RESULT if ++$seen == $stop-after;
                        }
                    }
                }
            }

            # looks like frequencies output
            else {
                sayer $outer.value ~ ':' ~ $outer.key;
                last RESULT if ++$seen == $stop-after;
            }
        }

        # Only got filename, so show its path
        elsif IO::Path.ACCEPTS($outer) {
            sayer stringify($outer);
            last RESULT if ++$seen == $only-first;
        }

        # Probably --unique
        else {
            sayer $outer.Str;
            last RESULT if ++$seen == $stop-after;
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
my sub set-source(str $name, $value --> Nil) {
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

# check Text::CSV availability
my sub check-TextCSV(str $name) {
    unless $TextCSV {
        CATCH { meh-not-installed 'Text::CSV', $name }
        require Text::CSV;
        $TextCSV := Text::CSV;
    }
}

# check Git::Blame::File availability
my sub check-GitBlameFile(str $name) {
    unless $GitBlameFile {
        CATCH { meh-not-installed 'Git::Blame::File', $name }
        require Git::Blame::File;
        $GitBlameFile := Git::Blame::File;
    }
}

# handle additional CSV parameters
my sub set-csv-flag(str $name, $value --> Nil) {
    check-TextCSV($name);

    Bool.ACCEPTS($value)
      ?? (%csv{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}

# Set highlight options
my sub set-highlight-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%listing{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}
my sub set-highlight-Int(str $name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%listing{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}
my sub set-highlight-Str(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--$name' must be specified with a string"
      !! (%listing{$name} := $value);
}

# Set result options
my sub set-result-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%result{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}
my sub set-result-Int(str $name, $value --> Nil) {
    meh "'--$name' can *not* be specified as a flag"
      if Bool.ACCEPTS($value);

    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%result{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}
my sub set-result-flag-or-Int(str $name, $value --> Nil) {
    with $value.Int {
        %result{$name} := $_;
    }
    else {
        meh "'--$name' must either be an integer or a flag";
    }
}

# Set listing options
my sub set-listing-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%listing{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}
my sub set-listing-Int(str $name, $value --> Nil) {
    my $integer := $value.Int;
    Int.ACCEPTS($integer)
      ?? (%listing{$name} := $integer)
      !! meh "'--$name' can only be an integer value, not '$value'";
}
my sub set-listing-Str(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--$name' must be specified with a string"
      !! (%listing{$name} := $value);
}
my sub set-listing-flag-or-Int(str $name, $value --> Nil) {
    with $value.Int {
        %listing{$name} := $_;
    }
    else {
        meh "'--$name' must either be an integer or a flag";
    }
}

#-------------------------------------------------------------------------------
# One subroutine for each supported option.  Is assumed to do right thing for
# that option by setting the appropriate global hashes.  Not expected to return
# anything.  Existence of "&option-foo" means the option exists and is
# supported.  These subroutines are **only** called if the associated option
# is actually specified (after any expansion).

my sub option-absolute($value --> Nil) {
    set-listing-flag('absolute', $value)
}

my sub option-accessed($value --> Nil) {
    set-filesystem-Instant('accessed', $value)
}

my sub option-after-context($value --> Nil) {
    set-result-Int('after-context', $value);
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
    set-result-Int('before-context', $value);
}

my sub option-blame-per-file($value --> Nil) {
    check-GitBlameFile('blame-per-file');
    set-action('blame-per-file', $value);
}

my sub option-blame-per-line($value --> Nil) {
    check-GitBlameFile('blame-per-line');
    set-action('blame-per-line', $value);
}

my sub option-blocks($value --> Nil) {
    set-filesystem-Int('blocks', $value)
}

my sub option-break($value --> Nil) {
    if Bool.ACCEPTS($value) {
        $value
          ?? set-listing-Str('break', "")
          !! (%listing<has-break> := False);
    }
    else {
        set-listing-Str('break', $value);
    }
}

my sub option-checkout($value --> Nil) {
    set-action('checkout', $value);
}

my sub option-context($value --> Nil) {
    set-result-Int('context', $value);
}

my sub option-count-only($value --> Nil) {
    set-result-flag('count-only', $value);
}

my sub option-created($value --> Nil) {
    set-filesystem-Instant('created', $value)
}

my sub option-csv-per-line($value --> Nil) {
    check-TextCSV('csv-per-line');
    set-action('csv-per-line', $value);
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

my sub option-dont-catch($value --> Nil) {
    set-rak-flag('dont-catch', $value);
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
    check-TextCSV('eol');

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
            if %exts{$_} -> \extensions {
                extensions.Slip
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
        @unknown
          ?? meh("No extension&s(@unknown) known for '@unknown[]'")
          !! (%filesystem<extensions> := codify-extensions @extensions);
    }
}

my sub option-file($value --> Nil) {
    %filesystem<file> := Bool.ACCEPTS($value)
      ?? $value
      !! convert-to-matcher($value);
}

my sub option-file-separator-null($value --> Nil) {
    set-result-flag('file-separator-null', $value);
}

my sub option-files-from($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--files-from' must be a file specification, not a flag"
      !! set-source('files-from', $value);
}

my sub option-files-with-matches($value --> Nil) {
    set-result-flag('files-with-matches', $value);
}

my sub option-files-without-matches($value --> Nil) {
    set-result-flag('files-without-matches', $value);
}

my sub option-filesize($value --> Nil) {
    set-filesystem-Int('filesize', $value)
}

my sub option-find($value --> Nil) {
    set-result-flag('find', $value);
}

my sub option-find-all($value --> Nil) {
    set-filesystem-flag('find-all', $value);
}

my sub option-formula($value --> Nil) {
    set-csv-flag('formula', $value);
}

my sub option-frequencies($value --> Nil) {
    set-result-flag('frequencies', $value);
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
    set-listing-flag('highlight', $value);
}

my sub option-highlight-after($value --> Nil) {
    set-listing-Str('highlight-after', $value);
}

my sub option-highlight-before($value --> Nil) {
    set-listing-Str('highlight-before', $value);
}

my sub option-human($value --> Nil) {
    set-listing-flag('human', $value);
}

my sub option-ignorecase($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? ($ignorecase := $value)
      !! meh "'--ignorecase' must be specified as a flag";
}

my sub option-ignoremark($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? ($ignoremark := $value)
      !! meh "'--ignoremark' must be specified as a flag";
}

my sub option-inode($value --> Nil) {
    set-filesystem-Int('inode', $value)
}

my sub option-invert-match($value --> Nil) {
    set-result-flag('invert-match', $value);
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

my sub option-json-per-elem($value --> Nil) {
    set-action('json-per-elem', $value);
}

my sub option-json-per-file($value --> Nil) {
    set-action('json-per-file', $value);
}

my sub option-json-per-line($value --> Nil) {
    set-action('json-per-line', $value);
}

my sub option-keep-meta($value --> Nil) {
    set-csv-flag('keep-meta', $value);
}

my sub option-known-extensions($value --> Nil) {
    meh "'--known-extensions' can only be specified as a flag"
      unless Bool.ACCEPTS($value);
    %filesystem<known-extensions> := codify-extensions @known-extensions
      if $value;
}

my sub option-list-custom-options($value --> Nil) {
    set-action('list-custom-options', $value);
}

my sub option-list-expanded-options($value --> Nil) {
    set-global-flag('list-expanded-options', $value);
}

my sub option-list-known-extensions($value --> Nil) {
    set-action('list-known-extensions', $value);
}

my sub option-matches-only($value --> Nil) {
    set-result-flag('matches-only', $value);
}

my sub option-max-matches-per-file($value --> Nil) {
    set-result-flag-or-Int('max-matches-per-file', $value);
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
    set-listing-flag-or-Int('only-first', $value);
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
    set-result-flag('paragraph-context', $value);
}

my sub option-passthru($value --> Nil) {
    set-result-flag('passthru', $value);
}

my sub option-passthru-context($value --> Nil) {
    set-result-flag('passthru-context', $value);
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
      !! ($pattern := $value);
}

my sub option-per-file($value --> Nil) {
    set-action 'per-file', Bool.ACCEPTS($value)
      ?? $value
      !! convert-to-simple-Callable($value);
}

my sub option-per-line($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? set-action('per-line', $value)
      !! meh "'--per-line' must be specified as a flag";
}

my sub option-proximate($value --> Nil) {
    set-listing-flag-or-Int('proximate', $value);
}

my sub option-rename-files($value --> Nil) {
    set-action('rename-files', $value);
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
      !! (%global<save> := $value);
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
    check-GitBlameFile('show-blame');
    set-result-flag('show-blame', $value);
}

my sub option-show-filename($value --> Nil) {
    set-listing-flag('show-filename', $value);
}

my sub option-show-line-number($value --> Nil) {
    set-result-flag('show-line-number', $value);
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
    set-result-flag('stats-only', $value);
}

my sub option-strict($value --> Nil) {
    set-csv-flag('strict', $value);
}

my sub option-summary-if-larger-than($value --> Nil) {
    set-listing-Int('summary-if-larger-than', $value);
}

my sub option-trim($value --> Nil) {
    set-listing-flag('trim', $value);
}

my sub option-type($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--type' must be specified with a string"
      !! $value eq 'contains' | 'words' | 'starts-with' | 'ends-with'
        ?? ($type := $value)
        !! meh "'$value' is not an expected --type";
}

my sub option-uid($value --> Nil) {
    set-filesystem-id('uid', $value);
}

my sub option-under-version-control($value --> Nil) {
    set-filesystem-flag('under-version-control', $value);
}

my sub option-unicode($value --> Nil) {
    set-action('unicode', $value);
}

my sub option-unique($value --> Nil) {
    set-result-flag('unique', $value);
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

#-------------------------------------------------------------------------------
# Subroutines checking applicability of groups of options specified

my sub meh-output-file($name --> Nil) is hidden-from-backtrace {
    meh "Specifying --output-file is incompatible with --$name" if $output-file;
}

my sub meh-pager($name --> Nil) is hidden-from-backtrace {
    meh "Using a pager is incompatible with --$name" if $pager;
}

my sub meh-what($name, %hash, $description --> Nil) is hidden-from-backtrace {
    meh qq:to/MEH/ if %hash;
These $description options are incompatible with --$name:
%hash.keys.sort.map({"--$_"})
MEH
}

my sub meh-csv($name --> Nil) is hidden-from-backtrace {
    meh-what($name, %csv, 'CSV')
}

my sub meh-listing($name --> Nil) is hidden-from-backtrace {
    meh-what($name, %listing, 'listing')
}

my sub meh-filesystem($name --> Nil) is hidden-from-backtrace {
    meh-what($name, %filesystem, 'filesystem');
}

my sub meh-modify($name --> Nil) is hidden-from-backtrace {
    meh-what($name, %modify, 'modify')
}

my sub meh-result($name --> Nil) is hidden-from-backtrace {
    meh-what($name, %result, 'result');
}

my sub meh-only($name --> Nil) is hidden-from-backtrace {
    meh "'--$name' must be the only option"
      if %filesystem
      || %listing
      || %global
      || %csv;
}

my sub meh-for($option, *@mehs) is hidden-from-backtrace {
    ::("&meh-$_")($option) for @mehs;
}

my sub maybe-meh-together(*@keys) is hidden-from-backtrace {
    if @keys > 1 {
        meh "Cannot specify &mm(@keys.skip) with --@keys.head()";
    }
}

my sub move-filesystem-options-to-rak(--> Nil) {
    if $reading-from-stdin {
        meh "Cannot use &mm(%filesystem.keys.sort) when reading from STDIN"
          if %filesystem;
        %listing<show-filename> := False;
    }
    elsif %filesystem {
        if %filesystem<under-version-control> {
            maybe-meh-together 'under-version-control', %filesystem<
              dir file recurse-symlinked-dir recurse-unmatched-dir
            >:k;
        }
        elsif %filesystem<file>:delete -> $file {
            maybe-meh-together 'file', %filesystem<
              extensions known-extensions find-all
            >:k;
            %rak<file> := $file;
        }
        elsif %filesystem<known-extensions>:delete -> $known {
            maybe-meh-together 'known-extensions', %filesystem<
              extensions find-all
            >:k;
            %rak<file> := $known;
        }
        elsif %filesystem<find-all>:delete {
            maybe-meh-together 'find-all', %filesystem<dir extensions>:k;
            %rak<file> := True;
        }
        elsif %filesystem<extensions>:delete -> $seen {
            %rak<file> := $seen;
        }

        if %filesystem<user>:delete -> $uid {
            maybe-meh-together %filesystem<user uid>:k;
            maybe-meh-together 'user', %filesystem<uid>:k;
            %rak<uid> := $uid;
        }

        if %filesystem<group>:delete -> $gid {
            maybe-meh-together 'group', %filesystem<gid>:k;
            %rak<gid> := $gid;
        }

        %rak ,= %filesystem;
        %filesystem = ();
    }
    else {
        %rak<file> := codify-extensions @known-extensions;
    }
}

my sub move-result-options-to-rak(--> Nil) {

    if %result {
        if %result<
          context after-context before-context
          paragraph-context passthru-context
        >:k -> @contexts {
            maybe-meh-together @contexts
              unless @contexts (==) <after-context before-context>;
            %listing<trim> := False if %listing<trim>:!exists;
        }

        maybe-meh-together %result<unique frequencies>:k;
        maybe-meh-together %result<count-only stats-only>:k;

        # Seem to only want file infromation
        if %result<files-with-matches files-without-matches>:k -> @keys {
            maybe-meh-together @keys;
            maybe-meh-together @keys, %result<
              max-matches-per-file frequencies unique
            >:k;
            maybe-meh-together @keys, %result<invert-match>:k;

            my $with := do if %result<files-with-matches>:delete {
                %rak<sources-only> := True;
                'with'
            }
            elsif %result<files-without-matches>:delete {
                %rak<sources-without-only> := True;
                'without'
            }

            # Only interested in number of files
            if %result<count-only>:delete {
                my int $seen;
                %rak<eager>  := True;
                %rak<mapper> := -> $_ --> Empty {
                    LAST sayer $seen == 0
                      ?? "No files $with matches"
                      !! $seen == 1
                        ?? "One file $with matches"
                        !! "$seen files $with matches";
                    ++$seen;
                }
            }

            # Need to separate files with a null-byte
            elsif %result<file-separator-null>:delete {
                # Set way to stringify paths
                my &stringify :=
                  IO::Path.^find_method(%listing<absolute>:delete
                    ?? "absolute"
                    !! "relative"
                  );

                my str @files;
                %rak<eager>  := True;
                %rak<mapper> := -> $_ --> Empty {
                    LAST sayer @files.join("\0");
                    @files.push: stringify($_);
                }
            }
        }

        else {
            if %result<max-matches-per-file>:delete -> $max {
                %rak<max-matches-per-source> := $max;
            }

            with %result<show-line-number>:delete {
                %rak<omit-item-number> := True unless $_;
            }

            if %result<find>:delete {
                %rak<find>             := True;
                %rak<omit-item-number> := True;

                # Only interested in number of files
                if %result<count-only>:delete {
                    my int $seen;
                    %rak<eager>  := True;
                    %rak<mapper> := -> $, @files --> Empty {
                        LAST sayer $seen == 0
                          ?? "No files"
                          !! $seen == 1
                            ?? "One file"
                            !! "$seen files";
                        $seen = @files.elems;
                    }
                }
            }

            # Only interested in number of matches / files
            elsif %result<count-only>:delete {
                # Set way to stringify paths
                my &stringify :=
                  IO::Path.^find_method(%listing<absolute>:delete
                    ?? "absolute"
                    !! "relative"
                  );

                my @files;
                %rak<eager>  := True;
                %rak<mapper> := -> $io, @matches --> Empty {
                    LAST {
                        if @files == 0 {
                            sayer "No files with matches";
                        }
                        elsif $verbose {
                            sayer "$_.key() has $_.value() match{"es" if .value > 1}"
                              for @files;
                        }
                        else {
                            sayer "@files.map(*.value).sum() matches in @files.elems() files";
                        }
                    }
                    @files.push: Pair.new: stringify($io), @matches.elems;
                }
            }

            # Wanna blame
            elsif %result<show-blame>:delete {
                %rak<mapper> := -> $io, @matches {
                    my @line-numbers = @matches.map: *.key;
                    with $GitBlameFile.new($io, :@line-numbers) -> $blamer {
                        $io => $blamer.lines.Slip
                    }
                    else {
                        $io => @matches.map({ .key ~ ':' ~ .value }).Slip
                    }
                }
            }
        }

        %rak ,= %result;
        %result = ();
    }
}

my sub activate-output-options() {
    $pager := %*ENV<RAK_PAGER> unless $pager.defined;
    if $pager {
        meh "Cannot specify a pager and an output-file"
          if $output-file && $output-file ne '-';
        $*OUT = (run $pager.words, :in).in;
    }
    elsif $output-file {
        $*OUT = open($output-file, :w) if $output-file ne '-';
    }
}

#-------------------------------------------------------------------------------
# Actions.  These are subroutines that start with "action-" and as such handle
# the associated action arguments.  These all have access to the variables set
# up during option processing.

my sub action-blame-per-file(--> Nil) {
    meh-for 'blame-per-file', <csv modify>;

    prepare-needle;
    %filesystem<under-version-control> := True;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    %listing<group-matches> := False if %listing<group-matches>:!exists;
    %listing<has-break>     := False if %listing<has-break>:!exists;

    %rak<batch>        := 1;
    %rak<produce-one>  := -> $io { $GitBlameFile.new($io) }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-blame-per-line(--> Nil) {
    meh-for 'blame-per-line', <csv modify>;

    prepare-needle;
    %filesystem<under-version-control> := True;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    %rak<batch>        := 1;
    %rak<produce-many> := -> $io { $GitBlameFile.new($io).lines }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-checkout(--> Nil) {
    meh-for 'checkout', <output-file pager filesystem modify csv>;

    prepare-needle;

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

my sub action-csv-per-line(--> Nil) {
    meh-for 'csv-per-line', <modify>;

    prepare-needle;
    %filesystem<file> //= codify-extensions %exts<#csv>
      unless $reading-from-stdin;
    move-filesystem-options-to-rak;

    %csv<auto-diag> := True unless %csv<auto-diag>:exists;
    my $csv := $TextCSV.new(|%csv);
    %csv = ();
    %rak<produce-many> := -> $io { $csv.getline-all($io.open) }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-edit(--> Nil) {
    %rak<max-matches-per-source> := $_
      with %result<max-matches-per-file>:delete;

    meh-for 'edit', <output-file pager result modify csv>;

    prepare-needle;
    move-filesystem-options-to-rak;
    my $editor := Bool.ACCEPTS($action) ?? Any !! $action;

    # find filenames to edit
    if %result<find>:delete {
        my str @files;
        %rak<sources-only> := True;
        %rak<mapper>       := -> $io --> Empty {
            LAST edit-files @files, :$editor;
            @files.push: $io.relative;
        }
    }

    # Look for locations in files to edit
    else {
        my @files;
        %rak<mapper> := -> $source, @matches --> Empty {
            LAST edit-files @files, :$editor;

            my $path   := $source.relative;
            my $target := Regex.ACCEPTS($needle) ?? $needle !! $pattern;

            @files.append: @matches.map: {
                $path => .key => (columns(
                  .value, $target,
                  :$ignorecase, :$ignoremark, |(:$type if $type)
               ).head // 0)
            }
        }
    }

    run-rak(:eagerly);
    rak-stats;
}

my sub action-help(--> Nil) {

    activate-output-options;
    my proto sub MAIN(|) {*}
    use CLI::Help:ver<0.0.5>:auth<zef:lizmat> %?RESOURCES, &MAIN, &HELP, 'long';
    @positionals.unshift: $pattern if $pattern;
    MAIN(|@positionals, :help, :$verbose);  # XXX options ??
}

my sub action-json-per-file(--> Nil) {
    meh-for 'json-per-file', <csv modify>;

    prepare-needle;
    %filesystem<file> //= codify-extensions %exts<#json>
      unless $reading-from-stdin;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    my $enc := %rak<encoding>:delete // 'utf8-c8';
    %rak<produce-one> := -> $io { try from-json $io.slurp(:$enc) }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-json-per-elem(--> Nil) {
    meh-for 'json-per-elem', <csv modify>;

    prepare-needle;
    %filesystem<file> //= codify-extensions %exts<#json>
      unless $reading-from-stdin;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    if %listing<show-line-number>:delete {
        # no action needed
    }
    elsif %listing<files-with-matches>:delete {
        %rak<sources-only> := True;
    }
    elsif %result<unique>:delete {
        %rak<unique> := True;
    }
    elsif %result<frequencies>:delete {
        %rak<frequencies> := True;
    }
    else {
        %rak<omit-item-number> := True;
    }

    my $enc := %rak<encoding>:delete // 'utf8-c8';
    %rak<produce-many> := -> $io {
        with try from-json $io.slurp(:$enc) -> \data {
            Seq.new: data.iterator
        }
    }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-json-per-line(--> Nil) {
    meh-for 'json-per-line', <csv modify>;

    prepare-needle;
    %filesystem<file> //= codify-extensions %exts<#jsonl>
      unless $reading-from-stdin;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    if %listing<show-line-number>:delete {
        # no action needed
    }
    elsif %listing<files-with-matches>:delete {
        %rak<sources-only> := True;
    }
    elsif %result<unique>:delete {
        %rak<unique> := True;
    }
    elsif %result<frequencies>:delete {
        %rak<frequencies> := True;
    }
    else {
        %rak<omit-item-number> := True;
    }

    my $enc := %rak<encoding>:delete // 'utf8-c8';
    %rak<produce-many> := -> $io {
        with try $io.lines(:$enc) -> $seq {
            $seq.map: { (try from-json($_)) // Empty }
        }
    }

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-list-custom-options(--> Nil) {
    meh-for 'list-custom-options', <filesystem listing modify csv>;

    activate-output-options;
    my $format := '%' ~ %config.keys>>.chars.max ~ 's: %s';
    for %config.sort(*.key.fc) -> (:$key, :value(@args)) {
        printf $format, $key, as-cli-arguments(@args);
    }
}

my sub action-list-known-extensions(--> Nil) {
    meh-for 'list-known-extensions', <filesystem listing modify csv>;

    activate-output-options;
    printf("%9s: %s\n", .key, .value.map({$_ || '(none)'}).Str)
      for %exts.sort(*.key);
}

my sub action-modify-files(--> Nil) {
    meh-for 'modify-files', <output-file pager listing csv>;

    prepare-needle;
    move-filesystem-options-to-rak;

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

my sub action-per-file(--> Nil) {
    meh-for 'per-file', <csv modify>;

    prepare-needle;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    %rak<produce-one> := $action<> =:= True
      ?? *.slurp(:enc(%rak<encoding> // 'utf8-c8'))
      !! $action;

    run-rak;
    rak-results;
    rak-stats;
}

my sub action-per-line(--> Nil) {
    meh-for 'per-line', <csv modify>;

    prepare-needle;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    run-rak;
    rak-results;
    rak-stats;
}

my sub action-rename-files(--> Nil) {
    my $dryrun   := %modify<dryrun>:delete;
    my $absolute := %listing<absolute>:delete;
    meh-for 'rename-files', <output-file pager listing modify csv>;

    prepare-needle;
    move-filesystem-options-to-rak;

    # First try to do a "git mv", if that failed, try an ordinary rename
    my sub rename-it($from, $to) {
        my $proc := run <git mv>, $from, $to, :err;
        rename $from, $to if $proc.err.slurp(:close);
    }

    # Set way to stringify paths
    my &stringify := IO::Path.^find_method($absolute
      ?? "absolute"
      !! "relative"
    );

    # Make sure the needle returns an IO::Path object to make it easier
    # for the user should they forget to add an .IO at the end
    my &needle := $needle;
    $needle := -> $old {
        my $new := needle($old);
        Bool.ACCEPTS($new) || $new =:= Empty || $new =:= Nil
          ?? $new
          !! $new.IO
    }

    %rak<find>             := True;
    %rak<omit-item-number> := True;
    %rak<map-all>          := True;
    %rak<old-new>          := True;
    %rak<mapper> := -> $, @files --> Empty {
        my @existed;
        my @done;
        for @files {
            my $destination := .value.IO;
            if $destination.e {
                @existed.push: Pair.new: stringify(.key), stringify($destination);
            }
            else {
                rename-it .key, $destination unless $dryrun;
                @done.push: Pair.new: stringify(.key), stringify($destination);
            }
        }
        my str @fb;
        if $verbose {
            if @existed {
                @fb.push: "Refused to rename @existed.elems() file&s(@existed):";
                @fb.push: "  $_.key() -> $_.value()" for @existed;
                @fb.push: "" if @done;
            }
            if @done {
                @fb.push: "Renamed @done.elems() file&s(@done):";
                @fb.push: "  $_.key() -> $_.value()" for @done;
            }
        }
        else {
            @fb.push: "Did not rename @existed.elems() file&s(@existed) because destination already existed."
              if @existed;
            @fb.push: "Renamed @done.elems() file&s(@done)."
              if @done;
        }
        @fb.push: "No files were selected for renaming."
          unless @files;
        @fb.push: "*** no files were renamed because of --dryrun ***"
          if $dryrun and @done;

        sayer @fb.join("\n");
    }

    run-rak(:eagerly);
    rak-stats;
}

my sub action-unicode(--> Nil) {
    my $count-only := %result<count-only>:delete;
    meh-for 'unicode', <csv modify filesystem result>;

    class Unicodes {
        method lines() {
            (0..0x10FFFF).map: {
                my $uniname := .uniname;
                $uniname
                  unless $uniname.starts-with('<') && $uniname.ends-with('>')
            }
        }
    }

    %rak<sources>          := (Unicodes,);
    %rak<omit-item-number> := True;
    %rak<batch>            := 16384;

    %rak<map-all> := $count-only;
    %rak<mapper>  := $count-only
      ?? -> $, @matches --> Empty {
             sayer @matches
               ?? "Found @matches.elems() match{'es' if @matches > 1}"
               !! 'No matches found'
         }
      !! -> $source, @matches {
             '<unicode>' => @matches.map: {
                 my $chr := .uniparse;
                 "$chr.ord.fmt('%5X') $_ $chr"
             }
         }

    $ignorecase := True;
    prepare-needle;

    run-rak;

    %listing<show-filename> := False unless %listing<show-filename>:exists;
    %listing<trim>          := False unless %listing<trim>:exists;
    rak-results;
    rak-stats;
}

my sub action-version(--> Nil) {
    meh-only('version');

    activate-output-options;
    my proto sub MAIN(|) {*}
    use CLI::Version:ver<0.0.8>:auth<zef:lizmat> $?DISTRIBUTION, &MAIN, 'long';
    MAIN(:version, :$verbose);
}

my sub action-vimgrep(--> Nil) {
    %rak<max-matches-per-source> := $_
      with %result<max-matches-per-file>:delete;

    meh-for 'vimgrep', <result modify csv>;

    prepare-needle;
    move-filesystem-options-to-rak;

    %rak<mapper> := -> $source, @matches --> Empty {
        my $path   := $source.relative;
        my $target := Regex.ACCEPTS($needle) ?? $needle !! $pattern;

        sayer $path
          ~ ':' ~ .key
          ~ ':' ~ (columns(
                    .value, $target,
                    :$ignorecase, :$ignoremark, |(:$type if $type)
                  ).head // "0")
          ~ ':' ~ .value
          for @matches;
    }
    activate-output-options;

    run-rak(:eagerly);
    rak-stats;
}

#-------------------------------------------------------------------------------

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

# Find one-line description of given name
my sub description($name) {
    if $name eq 'help' | 'foo' | 'no-foo' {
        ""
    }
    elsif %?RESOURCES<help.txt>.lines.first(*.starts-with(" --$name ")) -> $line {
        $line.substr(1).split(/ \s+ /, 2).tail
    }
    else {
        ""
    }
}

# Quit if unexpected named arguments hash
my sub meh-unexpected() {

    my str @text;
    my str @no-match;
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
        else {
            my int $cutoff = ($option.chars / 2).Int;

            if @options.map(-> $after {
                my $distance := StrDistance.new(:before($option), :$after).Int;
                Pair.new($after, $distance) if $distance <= $cutoff
            }).sort(*.value).head(5).List -> @suggestions {

                @text.push: "Regarding unexpected option --$option, did you mean:";
                for @suggestions -> (
                  :key($name), :value($steps)
                ) {
                    my $score := $verbose ?? " ($steps)" !! "";
                    if $name eq 'help' {
                        # noop
                    }
                    elsif description($name) -> $description {
                        @text.push: " --$name: $description?$score";
                    }
                    elsif %sub-options{$name} -> $main {
                        @text.push: " --$name: May need to include --$main then as well?$score";
                    }
                    else {
                        @text.push: " --$name?$score";
                    }
                }
            }
            else {
                @no-match.push: $option;
            }
        }
    }

    # no falsies and no suggestions
    @text.push: "Unexpected option{
        's' if @no-match > 1
    }: @no-match.map({"--$_"})."
      if @no-match;

    @text.push: "Use --help for an overview of available options.";
    exit note @text.join("\n");
}

# Prepare the executable needle
my sub prepare-needle(:$allow-matches-only = True) {

    if $pattern {
        if %global<smartcase>:delete {
            $ignorecase.defined
              ?? meh "Cannot specify --smartcase when --ignorecase is also specified"
              !! ($ignorecase := !$pattern.contains(/ <:upper> /));
        }
    }
    elsif %result<find> {         # no explicit pattern, but using find
        $pattern := '*.defined';  # put in a basic noop
    }
    else {
        meh "Must at least specify a pattern";
    }

    # first attempt at codifying pattern
    $needle := codify($pattern);

    # already executable
    if Callable.ACCEPTS($needle) {
        if Regex.ACCEPTS($needle) {
            if $allow-matches-only && (%result<matches-only>:delete) {
                my $old-needle := $needle;
                $needle := *.&matches($old-needle)
            }
        }
    }

    # non-executable
    elsif $allow-matches-only && (%result<matches-only>:delete) {
        # Note that we if we want matches only and we didn't have
        # a regex yet, we must use the highlighter.matches method
        # to generate the matches from the string pattern.  If we
        # would first convert to a Callable, we wouldn't be able
        # to find the matches anymore, as a Callable can only say
        # whether there was a match, not where.
        $needle := *.&matches:
          $pattern, :$ignorecase, :$ignoremark, |(:$type if $type)
    }

    # convert string to Callable
    else {
        $needle := needleify($pattern)
    }

    if $source-for {
        @positionals
          ?? meh("Specified path&s(@positionals) '@positionals[]' with --$source-for")
          !! (%rak{$source-for} := $source);
    }
    elsif @positionals {
        %rak<paths> := @positionals.splice;
    }
}

# Return all options as a list of Pairs
my sub as-options() {
    my @options;
    my sub add($name, $value) { @options.push: Pair.new: $name, $value }

    add('pattern', $pattern)             if $pattern;
    add('ignorecase', $ignorecase)       if $ignorecase;
    add('ignoremark', $ignoremark)       if $ignoremark;
    add('type', $type)                   if $type;
    add('paths', @positionals.join(',')) if @positionals;
    add($action-for, $action)            if $action-for;

    @options.append: $_ for
      %global, %filesystem, %csv, %listing, %modify;

    add('verbose', $verbose) if $verbose;
    @options
}

# vim: expandtab shiftwidth=4
