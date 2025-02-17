# The modules that we need here, with their full identities
use as-cli-arguments:ver<0.0.8+>:auth<zef:lizmat>;   # as-cli-arguments
use IO::Path::AutoDecompress:ver<0.0.3+>:auth<zef:lizmat>; # IOAD
use JSON::Fast::Hyper:ver<0.0.9+>:auth<zef:lizmat>; # from-json to-json
use META::constants:ver<0.0.5+>:auth<zef:lizmat> $?DISTRIBUTION;
use rak:ver<0.0.65+>:auth<zef:lizmat>;              # rak Rak

use Backtrace::Files:ver<0.0.4+>:auth<zef:lizmat> <
  backtrace-files
>;
use highlighter:ver<0.0.21+>:auth<zef:lizmat> <
  columns highlighter
>;
use Needle::Compile:ver<0.0.9+>:auth<zef:lizmat> <
  compile-needle implicit2explicit StrType Type
>;
use String::Utils:ver<0.0.32+>:auth<zef:lizmat> <
  after before between has-marks is-sha1 non-word paragraphs regexify
>;

# Returns the names of branch in current git repo
my sub branches(|) { # Until this lives in a module somewhere
    (run <git branch -r>, :out).out.lines.map(*.&after("/"))
}

# The epoch value when process started
my $init-epoch = $*INIT-INSTANT.to-posix.head;

# Defaults for highlighting on terminals
my constant BON  = "\e[1m";   # BOLD ON
my constant BOFF = "\e[22m";  # BOLD OFF

#- start of available options --------------------------------------------------
#- Generated on 2025-02-11T22:18:16+01:00 by tools/makeOPTIONS.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE
my str @options = <absolute accept accessed ack after-context allow-loose-escapes allow-loose-quotes allow-whitespace also-first always-first and andnot auto-decompress auto-diag backtrace backup batch before-context blame-per-file blame-per-line blocks break categorize checkout classify context count-only created csv-per-line degree deny description device-number dir dont-catch dryrun eco-code eco-doc eco-meta eco-provides eco-scripts eco-tests edit encoding eol escape exec execute-raku extensions file file-separator-null files-from files-with-matches files-without-matches filesize find formula frequencies gid group group-matches hard-links has-setgid has-setuid headers help highlight highlight-after highlight-before human ignorecase ignoremark inode invert-match is-empty is-executable is-group-executable is-group-readable is-group-writable is-moarvm is-owned-by-group is-owned-by-user is-owner-executable is-owner-readable is-owner-writable is-pdf is-readable is-sticky is-symbolic-link is-text is-world-executable is-world-readable is-world-writable is-writable json-per-elem json-per-file json-per-line keep-meta list-custom-options list-expanded-options list-known-extensions matches-only max-matches-per-file mbc mbc-frames mbc-strings meta-modified mode modifications modified modify-files module not only-first or ornot output-dir output-file pager paragraph-context passthru passthru-context paths paths-from pattern patterns-from pdf-info pdf-per-file pdf-per-line per-file per-line per-paragraph progress proximate rakudo-all rakudo-c rakudo-doc rakudo-java rakudo-js rakudo-nqp rakudo-perl rakudo-raku rakudo-shell rakudo-test rakudo-yaml rename-files quietly quote rak recurse-symlinked-dir recurse-unmatched-dir repository save sayer sep shell show-blame show-filename show-item-number silently smartcase smartmark sourcery stats stats-only strict summary-if-larger-than trim type uid under-version-control unicode unique user verbose version vimgrep with-line-endings>;
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of available options ----------------------------------------------------

# Options of other programs that may be false friends
my constant %falsies =
# our own
  changed          => 'meta-modified',
  run              => 'exec',
  first-only       => 'only-first',
  known-extensions => 'extensions=*',

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
  help-types       => 'extensions=*',
  i                => 'ignorecase',
  ignore-case      => 'ignorecase',
  I                => 'no-ignorecase',
  ignore-dir       => 'dir',
  ignore-directory => 'dir',
  k                => 'extensions=*',
  known-types      => 'extensions=*',
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
  1                => 'only-first',

# from ag
  a                => 'dir',
  all-types        => 'dir',
  after            => 'after-context',
  before           => 'before-context',
  0                => 'file-separator-null',
  null             => 'file-separator-null',
  u                => 'dir',
  unrestricted     => 'dir',
;

# The JSON config to map "ack" arguments to "rak" arguments as closely as
# possible
my constant %ack-interface-config =
  "1"                => (:only-first,),
  "A"                => (:after-context<!>,),
  "B"                => (:before-context<!>,),
  "c"                => (:count-only,),
  "C"                => (:context<[2]>,),
  "count"            => (:count-only,),
  "dump"             => (:list-expanded-options,),
  "f"                => (:find,),
  "filename"         => (:show-filename,),
  "follow"           => (:recurse-symlinked-dir,),
  "group"            => (:group-matches,),
  "h"                => (:!show-filename,),
  "H"                => (:show-filename,),
  "heading"          => (:group-matches,),
  "help-types"       => (:extensions<[*]>,),
  "i"                => (:ignorecase,),
  "I"                => (:!ignorecase,),
  "ignore-case"      => (:ignorecase,),
  "ignore-dir"       => (:dir,),
  "ignore-directory" => (:dir,),
  "k"                => (:extensions<[*]>,),
  "known-types"      => (:extensions<[*]>,),
  "l"                => (:files-with-matches,),
  "L"                => (:files-without-matches,),
  "m"                => (:max-matches-per-file,),
  "n"                => (:!dir,),
  "man"              => (:help,),
  "match"            => (:pattern,),
  "max-count"        => (:max-matches-per-file,),
  "o"                => (:matches-only,),
  "output"           => (:pattern,),
  "p"                => (:proximate<!>,),
  "P"                => (:proximate<0>,),
  "print0"           => (:file-separator-null,),
  "S"                => (:smartcase,),
  "smart-case"       => (:smartcase,),
  "t"                => (:extensions<!>,),
  "type"             => (:extensions<!>,),
  "TYPE"             => (:extensions<!>,),
  "v"                => (:invert-match,),
  "w"                => (:type<words>,),
  "with-filename"    => (:show-filename,),
  "x"                => (:files-from<->,),
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
my $writing-to-stderr  := $*ERR.t;

# Set up default extension sets
my constant %exts =
  '#c'        => <c h hdl>,
  '#c++'      => <cpp cxx hpp hxx>,
  '#csv'      => ('', <csv psv tsv>).flat.List,
  '#cro'      => ('', 'crotmp'),
  '#html'     => <htm html css>,
  '#js'       => <js ts tsx>,
  '#json'     => <json>,
  '#jsonl'    => <jsonl>,
  '#markdown' => <md markdown>,
  '#perl'     => ('', <pl pm t>).flat.List,
  '#python'   => <py ipynb>,
  '#raku'     => ('', <raku rakumod rakutest rakudoc nqp t pm6 pl6 pod6 t6>
                 ).flat.List,
  # I am not sure should the binary formats be included or not:
  # https://reference.wolfram.com/language/guide/WolframLanguageFileFormats.html
  # Also #mathematica can be included as synonym of #wl .
  '#wl'     => ('', <wl wlt m mt nb>).flat.List,
  # I am including .Rmd because of .ipynb inclusion above.
  # More generally, the extensions of the project https://quarto.org should be included.
  '#r'     => ('', <r R Rmd>).flat.List,
  '#ruby'     => <rb>,
  '#text'     => ('', <txt>).flat.List,
  '#yaml'     => <yaml yml>,
  "#config"   => <ini>,
;

# Known extensions
my constant @known-extensions = %exts.values.flat.unique.sort;

# Place to keep tagged configurations
my $config-file := do if %*ENV<RAK_CONFIG> -> $rak-config {
    $rak-config.IO
}
elsif %*ENV<RAK_CONFIG>:!exists {  # want to have the default config
    $*HOME.add('.rak-config.json')
}

# Links to optional classes
my $GitBlameFile;
my $MoarVMBytecode;
my $PDFExtract;
my $TextCSV;
my &edit-files;
my &sourcery;
my &sourcery-pattern;

# Variables for grouping options given
my $verbose;      # process verbose
my $pager;        # process pager if defined
my $output-file;  # process output file if defined
my $output-dir;   # process output directory if defined
my $debug-rak;    # process show rak args

my @patterns;      # the specified patterns (if any)
my @highlights;    # the pattern used for highlighting
my $seen-initial-pattern;  # flag, --pattern/--patterns-from seen
my $matches-only;  # whether to produce matches only
my $smartcase;     # --smartcase
my $smartmark;     # --smartmark
my $ignorecase;    # --ignorecase
my $ignoremark;    # --ignoremark
my $modifications := True;  # --modifications

my $type;  # --type (implicitely) specified

my @modules;  # list of modules to -use-
my @repos;    # list of repositories to include with -use lib-

my $source-for;         # name of option providing sources
my $source-rak;         # name of rak option providing sources
my $source;             # associated value (if any)
my int $source-offset;  # start of source to be shown

my $producer-for;  # name of option providing producer

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

my $enc;      # actual encoding to be used inside rak
my $headers;  # actual headers arg for CSV
my %CSV;      # actual CSV arguments sent to rak

# For now, the routine for outputting anything
my &sayer = do {
    my $out := $*OUT;
    -> $_ { $out.say($_) }
}

# Role for marking default values in expanded parameters
my role is-default-option { method gist(--> Nil) { } }

# Fetch and normalize any config, we only do List of Pairs nowadays
my %config := do {
    if $config-file && $config-file.e {
        my %hash := from-json($config-file.slurp);

        # fix various save snafus and content changes
        for %hash.values {
            $_ = .pairs.List if Hash.ACCEPTS($_);
            if Array.ACCEPTS($_) {
                $_ = .map(*.pairs.Slip).List;
            }
        }
        if %hash<(default)> -> @defaults {
            $_ does is-default-option for @defaults.map(*.value);
        }
        %hash
    }
    else {
        { }
    }
}

my @positionals; # Positional arguments
my @unexpected;  # Pairs of unexpected arguments and their value

# Save current setting
my constant $safe-marker = '--save=';
if @*ARGS.first(*.starts-with($safe-marker)) -> $custom {

    meh "Cannot save: configuration file location explicitely disabled"
      unless $config-file;

    my @opts;
    for @*ARGS -> $arg {
        if $arg.starts-with($safe-marker) {
            # no action
        }
        elsif $arg eq '-' {
            @positionals.push: $arg;
        }
        elsif $arg.match(
          /^ '-' '-'? ('/' | 'no-')? (<-[=]>+) ('='?) (.*) /
        ) -> $/ {
            @opts.push: Pair.new: $1.Str, $2.Str ?? $3.Str !! $0.defined.not;
        }
        elsif $arg.starts-with('-') {
            meh "Improperly formatted option: '$arg')";
        }
        else {
            @positionals.push: $arg;
        }
    }
    meh-unexpected if @unexpected;

    my $name := $custom.substr($safe-marker.chars);
    if @opts {
        @opts.append: %config{$name}.grep(*.key ne 'description')
          if @opts == 1 && @opts.head.key eq 'description';
        %config{$name} := @opts;
    }
    else {
        %config{$name}:delete;
    }

    $config-file.spurt: to-json %config, :!pretty, :sorted-keys;

    say @opts
          ?? "Saved '&as-cli-arguments(@opts)' as: &o($name)"
          !! "Removed custom option '&o($name)'";
        exit;
}

# Make sure defaults are activated unless we're saving
elsif %config<(default)>:exists {
    named('(default)', True);
}

# Helper sub to add a highlight spec
my sub add-highlight($_) {

    # Make sure highlightable patterns are added
    if .key eq 'contains' | 'words' | 'starts-with' | 'ends-with' {
        @highlights.push: $_;
    }
    elsif .key eq 'regex' {
        @highlights.push: regexify .value,
          :$ignorecase, :$smartcase, :$ignoremark, :$smartmark
    }
    elsif .key eq 'split' {
        add-highlight(implicit2explicit($_)) for .value.words;
    }
    elsif .key eq 'and' {
        add-highlight(.value)
    }
}

# Helper sub to add a pattern
my sub add-pattern($pattern, :$first --> Nil) {
    $_ := $type && $type ne "auto"
      ?? Pair.new($type, $pattern)
      !! implicit2explicit($pattern);

    $first
      ?? @patterns.unshift($_)
      !! @patterns.push($_);

    # Make sure highlightable patterns are added
    add-highlight($_);
}

#-------------------------------------------------------------------------------
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

my sub main() is export {

    # Must have something to work with
    meh q:to/MEH/.chomp unless @*ARGS || @patterns;
Should at least specify a pattern.  For instance:

  rak foo

will look for "foo" in all text files in the current directory
and any sub-directories. See:

  rak --help

for (much) more information.
MEH

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
    add-pattern(@positionals.shift, :first)
      if !$seen-initial-pattern && @positionals;

    # from here on out, description is a noop
    %global<description>:delete;

    if %global<list-expanded-options>:delete {
        if as-options() -> @options {
            if $verbose {
                for @options {
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
        }
        else {
            say "No options found";
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
        %listing<show-filename> := False
          if %listing<show-filename>:!exists
          && !(%result<categorize classify>:k);
    }
    elsif $reading-from-stdin {
        %listing<show-filename> := False
          unless %result<show-filename>:exists;
        %result<show-item-number> := False
          unless %result<show-item-number>:exists;
    }

    # Perform the actual action
    $action-for ?? ::("&action-$action-for")() !! action-per-line();
    $*OUT.close if $pager;
}

# no mainline code from here
#-------------------------------------------------------------------------------

# Return "s" if number is not 1, for error messages
my sub s($elems)  { $elems == 1 ?? '' !! 's'  }
my sub es($elems) { $elems == 1 ?? '' !! 'es' }

# Properly show an option with one or two dashes
sub o($option) {
    $option eq '(default)'
      ?? $option
      !! $option.chars == 1
        ?? "-$option"
        !! "--$option"
}

# Return '--a, --b' for one or more names
my sub mm(@names) { @names.map({"--$_"}).join(', ') }

# Sane way of quitting
my sub meh($message) is hidden-from-backtrace {
    exit note $message.ends-with('.' | '?')
      ?? $message
      !! "$message.";
}

# Quit if module not installed
my sub meh-not-installed($module, $feature) is hidden-from-backtrace {
    meh qq:to/MEH/.chomp;
Must have the $module module installed to do $feature.
You can do this by running 'zef install $module'.
MEH
}

# Quit if a code compilation error occurred
my sub meh-compilation($code, $_) {
    %rak<dont-catch>
      ?? .throw
      !! meh "Could not compile '$code':\n$_.message()";
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

# Convert a string to code if possible, adhering to type
my sub codify(Str:D $code) {
    CATCH { meh-compilation($code, $_) }

    my $callable := compile-needle($code,
      :$ignorecase, :$smartcase, :$ignoremark, :$smartmark, :@repos, :@modules
    );
    $callable ~~ Failure
      ?? meh-compilation($code, $callable.exception)
      !! $callable
}

# Convert a string to non-Regex code, fail if not possible
my sub convert-to-simple-Callable(Str:D $code, str $name) {
    my $type := implicit2explicit($code).key;

    $type eq 'regex'
      ?? meh("Cannot use a regular expression for --$name: '$code'")
      !! $type eq 'contains'
        ?? Nil
        !! compile-needle($code,
             :$ignorecase, :$smartcase, :$ignoremark, :$smartmark, :@repos, :@modules
           )
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

# Return absolute path to ecosystem cache
my sub ecosystem-cache(str $ecosystem, str $dir) {
    my $io := ((%*ENV<RAKU_ECOSYSTEM_CACHE> andthen .IO)
      // ($*HOME // $*TMPDIR).add(".ecosystem").add("cache")
    ).add($ecosystem);

    $source-offset = $io.relative.chars + 3;
    $io.add($dir).absolute
}

# Return absolute path to rakudo cache
my sub rakudo-cache(str $type) {
    # This is a little brittle
    $source-offset = $*EXECUTABLE.parent(3).absolute.chars + 1;
    %listing<absolute> := True;

    ((%*ENV<RAKU_RAKUDO> andthen .IO) // ($*HOME // $*TMPDIR))
      .add(".raku").add("cache").add("rakudo-$type").absolute
}

#-------------------------------------------------------------------------------

# Run the query
my sub run-rak(:$eagerly --> Nil) {

    meh "Searching for binary data NYI, did you forget a --find?"
      if %rak<is-text>:exists && !%rak<is-text> && !%rak<find>;

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
    if %rak<sources-only>
      || %rak<sources-without-only>
      || (%rak<max-matches-per-source>:exists) {
    }
    else {
        %rak<max-matches-per-source> := %listing<only-first>:exists
          ?? %listing<only-first>
          !! (%listing<human> // $writing-to-stdout)
            ?? 1000
            !! 0;
    }

    if $debug-rak {
        note .key ~ ': ' ~ .value.raku for %rak.sort(*.key);
    }

    $rak := rak $needle, %rak;
    meh .message with $rak.exception;
    note "Unexpected leftovers: %rak.raku()" if %rak;
}

# Process the results
my sub rak-results(--> Nil) {
    $output-dir ?? output-dir-results() !! show-results();
}

# Wanna save the classification / categorization
my sub output-dir-results(--> Nil) {
    my @keys;
    for $rak.result {
        if Pair.ACCEPTS($_) {
            if .value -> @matches {
                my $file := .key;
                my $id   := ($file || '(empty)').IO.basename;
                @keys.push: $id;
                my $io := $output-dir.add($id);
                $io.spurt: ("\n\n" x $io.e)
                  ~ (IO::Path.ACCEPTS($file) ?? $file.absolute !! $id)
                  ~ "\n"
                  ~ @matches.join("\n"),
                  :append;
            }
        }
        else {
            die $_;  # huh?
        }
    }
    if $verbose {
        sayer "Created @keys.elems() file&s(@keys) in '$output-dir':\n  @keys.join("\n  ")";
    }
    else {
        sayer "Created @keys.elems() file&s(@keys) in '$output-dir'";
    }
}

# Showing results the normal way
my sub show-results(--> Nil) {
    my $human         := %listing<human>:delete // $writing-to-stdout;
    my $show-filename := %listing<show-filename>:delete // True;
    my $absolute      := %listing<absolute>:delete;
    my $break         := %listing<break>:delete;
    my $group-matches := %listing<group-matches>:delete;
    my $highlight     := %listing<highlight>:delete;
    my $trim          := %listing<trim>:delete;
    my $only-first    := %listing<only-first>:delete;
    my uint $proximate = %listing<proximate>:delete // 0;

    # Set up human defaults
    if $human {
        $break         := ""   unless $break.defined;
        $group-matches := True unless $group-matches.defined;
        $highlight     := !$matches-only unless $highlight.defined;
        $trim          := True unless $trim.defined;
        $only-first    := 1000 unless $only-first.defined;
    }
    my $has-break := %listing<has-break>:delete // $break.defined;
    # Switch to really large values if not specified
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

    my Str() $pre = my Str() $post = $_ with $highlight-before;
    $post                          = $_ with $highlight-after;
    $pre  = BON  without $pre;
    $post = BOFF without $post;

    my &line-post-proc := do if $highlight && @highlights {

        my %nameds =
          (:$smartcase  if $smartcase),
          (:$ignorecase if $ignorecase),
          (:$smartmark  if $smartmark),
          (:$ignoremark if $ignoremark),
          (:summary-if-larger-than($_)
            with %listing<summary-if-larger-than>:delete),
        ;

        $trim
          ?? -> $line {
                 highlighter $line.trim, @highlights, $pre, $post, |%nameds
             }
          !! -> $line {
                 highlighter $line, @highlights, $pre, $post, |%nameds
             }
    }

    elsif %listing<whole-line>:delete {
        $output-dir
          ?? -> $line { $line }
          !! -> $line { "$pre$line$post" }
    }

    # No highlighting wanted, abuse highlighter logic anyway
    else {
        $trim ?? *.Str.trim !! *.Str
    }

    # Set way to stringify paths
    my &path-stringify :=
      IO::Path.^find_method($absolute ?? "absolute" !! "relative");
    my sub stringify($value) {
        $value.WHAT =:= IO::Path  # only "pure" IO::Path objects
          ?? path-stringify($value)
          !! Buf.ACCEPTS($value)
            ?? $value.List.Str
            !! $value.Str
    }

    # Proximate breaker logic
    my sub breaker(uint $linenr, uint $last-linenr --> Nil) {
        sayer ""
          if $proximate
          && ($linenr <= $last-linenr  # probably a backtrace
               |! ($linenr - $last-linenr) >= $proximate);
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
                    sayer line-post-proc stringify($_);
                        last RESULT if ++$seen == $stop-after;
                }
            }

            # Looks like normal search result
            elsif Iterable.ACCEPTS($value) {
                if $value -> @matches {
                    my $source :=
                      stringify($key).substr($source-offset) || '(empty)';
                    sayer $break if $has-break && $seen;

                    if PairContext.ACCEPTS(@matches.head) {
                        my uint $last-linenr = @matches.head.key - 1;

                        if $group-matches {
                            sayer $source if $show-filename;
                            for @matches.map({ $_ if .value.elems }) {
                                my uint $linenr = .key;
                                breaker($linenr, $last-linenr);
                                if Slip.ACCEPTS(.value) {
                                    # Can only produce a Slip from a real
                                    # Callable, which cannot have any
                                    # highlighting, so don't bother
                                    sayer "$linenr:$_" for @(.value);
                                    ++$seen;
                                }
                                else {
                                    sayer $linenr ~ ':' ~ (.matched
                                      ?? line-post-proc stringify(.value)
                                      !! stringify(.value)
                                    );
                                    ++$seen if .matched;
                                }
                                last RESULT if $seen == $stop-after;
                                $last-linenr = $linenr;
                            }
                        }

                        # Not grouping
                        elsif $show-filename {
                            for @matches.map({ $_ if .value.elems }) {
                                my uint $linenr = .key;
                                breaker($linenr, $last-linenr);
                                if Slip.ACCEPTS(.value) {
                                    # Can only produce a Slip from a real
                                    # Callable, which cannot have any
                                    # highlighting, so don't bother
                                    sayer "$source:$linenr:$_" for @(.value);
                                    ++$seen;
                                }
                                else {
                                    sayer $source
                                      ~ ':' ~ $linenr
                                      ~ ':' ~ (.matched
                                      ?? line-post-proc stringify(.value)
                                      !! stringify(.value)
                                    );
                                    ++$seen if .matched;
                                }
                                last RESULT if $seen == $stop-after;
                                $last-linenr = $linenr;
                            }
                        }

                        # Not grouping and don't want to know the filename
                        else {
                            for @matches.map({ $_ if .value.elems }) {
                                my uint $linenr = .key;
                                breaker($linenr, $last-linenr);
                                if Slip.ACCEPTS(.value) {
                                    # Can only produce a Slip from a real
                                    # Callable, which cannot have any
                                    # highlighting, so don't bother
                                    sayer "$linenr:$_" for @(.value);
                                    ++$seen;
                                }
                                else {
                                    sayer $linenr
                                      ~ ':' ~ (.matched
                                      ?? line-post-proc stringify(.value)
                                      !! stringify(.value)
                                    );
                                    ++$seen if .matched;
                                }
                                last RESULT if $seen == $stop-after;
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
                                sayer line-post-proc stringify $_;
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
                                sayer $source
                                  ~ ':' ~ line-post-proc stringify $_;
                            }
                            last RESULT if ++$seen == $stop-after;
                        }
                    }

                    # in case of --always-first without matches
                    ++$seen;
                }
            }

            # looks like frequencies output
            else {
                sayer "&stringify($outer.value):$outer.key()";
                last RESULT if ++$seen == $stop-after;
            }
        }

        # anything else
        else {
            sayer (stringify $outer).substr($source-offset);
            last RESULT if ++$seen == $stop-after;
        }
    }

    if $seen > 1 && $seen == $stop-after {
        my str $what = $writing-to-stdout ?? "showing" !! "producing";
        note "** Stopped $what results after $seen matches **";
    }
}

# Statistics to show
my sub rak-stats(:$count-only --> Nil) {

    # Stop any progress reporting
    $rak.stop-progress;

    if $rak.stats {
        if $count-only && !$verbose {
            note $rak.nr-matches + $rak.nr-changes
              ~ " matches in $rak.nr-sources() files";
        }
        else {
            my str @stats;
            unless $count-only {
                @stats.push: "Statistics for '@patterns.head.value()':";
                my str $bar = '-' x @stats[0].chars;
                @stats.unshift: $bar;
                @stats.push: $bar;
            }
            @stats.push: "    Number of files: $rak.nr-sources()";

            if $rak.nr-items -> $items {
                @stats.push: "    Number of lines: $items";
            }
            if $rak.nr-matches -> $matches {
                @stats.push: "  Number of matches: $matches";
            }
            if $rak.nr-passthrus -> $passthrus {
                @stats.push: "Number of passthrus: $passthrus";
            }
            if $rak.nr-changes -> $changes {
                @stats.push: "  Number of changes: $changes";
            }

            note @stats.join("\n");
        }
    }
    elsif $rak.nr-sources == 0 {
        note "Path specification did not select any files, so there was nothing to match.";
    }
}

#-------------------------------------------------------------------------------
# Helper subroutines for setting up data structures from option handling

# Indicate the action to be performed
my sub set-source(str $name, $value, str $rak = $name --> Nil) {
    meh "Can only have one haystack at a time:\n'--$_' was specified before '--$name'"
      with $source-for;
    $source-for := $name;
    $source     := $value;
    $source-rak := $rak;
}

# Indicate the producer
my sub set-producer(str $name, $value, str $type = 'produce-one' --> Nil) {
    if Bool.ACCEPTS($value) {
        if $value {
            meh qq:to/MEH/ with $producer-for;
Can only have one producer at a time:
'--$_' was specified before '--$name'
MEH
        }
        else {
            return;  # ignore --/producer
        }
    }
    $producer-for := $name;
    %rak{$type}   := $value;
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
        !! (%global{$name}:delete)
      !! meh "'--$name' can only be specified as a flag";
}

# handle rak boolean flag
my sub set-rak-flag(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? $value
        ?? (%rak{$name} := True)
        !! (%rak{$name}:delete)
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

    with convert-to-simple-Callable($code, $name) {
        %filesystem{$name} := $_;
    }
    else {
        meh "Problem compiling expression for '--$name': $value";
    }
}

# handle file attributes that return an Int
my sub set-filesystem-Int(str $name, $value --> Nil) {
    if Bool.ACCEPTS($value) {
        meh "Must specify a condition for '--$name'";
    }
    orwith convert-to-simple-Callable($value, $name) {
        %filesystem{$name} := $_;
    }
    else {
        meh "Problem compiling condition for '--$name': $value";
    }
}

# handle file attributes that return an id
my sub set-filesystem-id(str $name, $value --> Nil) {
    meh "Must specify a condition for '--$name'" if Bool.ACCEPTS($value);

    %filesystem{$name} := do with convert-to-simple-Callable($value, $name) {
        $_
    }
    elsif (try $value.Int) -> $id {
        my $ := * == $id
    }
    else {
        meh "Must specify an integer or an expression with --$name";
    }
}

# handle file attributes that need a name
my sub set-filesystem-name(
  str $name, $value is copy, $name-getter, $id-getter
--> Nil) {
    meh "Must specify a condition or name for '--$name'"
      if Bool.ACCEPTS($value);

    # Get lookuppers
    my (&name-getter, &id-getter) = do {
        CATCH { meh-not-installed "P5$name-getter", "--$name" }
        "use P5$name-getter; &$name-getter, &$id-getter".EVAL
    }

    my sub names2ids($names) {
        $names.split(',').map: {
            name-getter($_)[2]
              // meh "Unknown user name '$_' with --$name";
        }
    }

    # See if we have a negation
    my $not := do if $value.starts-with('!') {
        $value = $value.substr(1);
        True
    }
    else {
        False
    }

    # An actual condition
    my $compiled := convert-to-simple-Callable($value, $name);
    %filesystem{$name} := do if $compiled.defined {
        $not
          ?? -> $id { with id-getter($id).head { !$compiled($_) } }
          !! -> $id { $compiled($_) with id-getter($id).head }
    }

    # List of user names (possibly negated)
    else {
        my int @ids = names2ids($value);
        my $id := @ids.head;
        my $ := @ids == 1
          ?? $not
            ?? * != $id
            !! * == $id
          !! $not
            ?? * ∉ @ids
            !! * ∈ @ids
    }
}

# handle file attributes that need a callable
my sub set-filesystem-callable(str $name, $value --> Nil) {
    if Bool.ACCEPTS($value) {
        meh "--$name cannot be specified as a flag";
    }
    orwith convert-to-simple-Callable($value, $name) {
        %filesystem{$name} := $_;
    }
    else {
        meh "Problem compiling condition for '--$name': $value";
    }
}

# handle external execution
my sub external-execution(str $name, $value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh("Must specify arguments for '--$name'")
      !! (%filesystem{$name} := $value);
}

# check sourcery availability
my sub check-sourcery(str $name) {
    unless &sourcery {
        CATCH { meh-not-installed 'sourcery', "--$name" }
        (&sourcery, &sourcery-pattern) =
          "use sourcery; &sourcery, &needle".EVAL;
    }
}

# check Edit::Files availability
my sub check-EditFiles(str $name) {
    unless &edit-files {
        CATCH { meh-not-installed 'Edit::Files', "--$name" }
        &edit-files = "use Edit::Files; &edit-files".EVAL;
    }
}

# check Git::Blame::File availability
my sub check-GitBlameFile(str $name) {
    unless $GitBlameFile {
        CATCH { meh-not-installed 'Git::Blame::File', "--$name" }
        require Git::Blame::File;
        $GitBlameFile := Git::Blame::File;
    }
}

# check MoarVM::Bytecode availability
my sub check-MoarVMBytecode(str $name) {
    unless $MoarVMBytecode {
        CATCH { meh-not-installed 'MoarVM::Bytecode', "--$name" }
        $MoarVMBytecode := "use MoarVM::Bytecode; MoarVM::Bytecode".EVAL;
    }
}

# check PDF::Extract availability
my sub check-PDFExtract(str $name) {
    unless $PDFExtract {
        CATCH { meh-not-installed 'PDF::Extract', "--$name" }
        $PDFExtract := "use PDF::Extract; Extract".EVAL;
    }
}

# check Text::CSV availability
my sub check-TextCSV(str $name) {
    unless $TextCSV {
        CATCH { meh-not-installed 'Text::CSV', "--$name" }
        require Text::CSV;
        $TextCSV := Text::CSV;
    }
}

# handle additional CSV flags
my sub set-csv-flag(str $name, $value --> Nil) {
    check-TextCSV($name);
    Bool.ACCEPTS($value)
      ?? (%csv{$name} := $value)
      !! meh("'--$name' can only be specified as a flag");
}

# handle additional CSV values
my sub set-csv-value(str $name, $value, str $type --> Nil) {
    check-TextCSV($name);
    Bool.ACCEPTS($value)
      ?? meh "'--$name' expects $type character, not a flag"
      !! (%csv{$name} := $value)
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
my sub set-result-Callable(str $name, $value --> Nil) {
    if Bool.ACCEPTS($value) {
        meh("'--$name' can *not* be specified as a flag");
    }
    orwith convert-to-simple-Callable($value, $name) {
        %result{$name} := $_;
    }
    else {
        meh "Problem compiling condition for '--$name': $value";
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

# Set pattern option
my sub set-additional-pattern(str $name, $value, str $prefix) {
    Bool.ACCEPTS($value)
      ?? meh "'--$name' must be a pattern specification, not a flag"
      !! add-pattern($prefix ~ $value)
}

# Set eco-xxx option in ecosystem cache
my sub set-eco-xxx($value, str $type --> Nil) {
    my str $parameter = "eco-$type";
    if Bool.ACCEPTS($value) {
        set-source($parameter, ecosystem-cache("rea", $type), 'files-from')
          if $value;
    }
    elsif $value ~~ Str && $value eq 'rea' | 'fez' {
        set-source($parameter, ecosystem-cache($value, $type), 'files-from');
    }
    else {
        meh "'--$parameter' must be either 'rea' or 'fez'";
    }
}

# Set rakudo-xxx option in ecosystem cache
my sub set-rakudo-xxx($value, str $type --> Nil) {
    my str $parameter = "rakudo-$type";
    if Bool.ACCEPTS($value) {
        set-source($parameter, rakudo-cache($type), 'files-from')
          if $value;
    }
    else {
        meh "'--$parameter' can only be specified as a flag"; 
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

my sub option-accept($value --> Nil) {
    set-filesystem-callable('accept', $value)
}

my sub option-accessed($value --> Nil) {
    set-filesystem-Instant('accessed', $value)
}

my sub option-ack($value --> Nil) {
    %config := %ack-interface-config;
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

my sub option-also-first($value --> Nil) {
    set-result-flag-or-Int('also-first', $value);
}

my sub option-always-first($value --> Nil) {
    set-result-flag-or-Int('always-first', $value);
}

my sub option-and($value --> Nil) {
    set-additional-pattern('and', $value, '&');
}

my sub option-andnot($value --> Nil) {
    set-additional-pattern('andnot', $value, '&!');
}

my sub option-auto-decompress($value --> Nil) {
    set-filesystem-flag('auto-decompress', $value);
}

my sub option-auto-diag($value --> Nil) {
    set-csv-flag('auto-diag', $value);
}

my sub option-backtrace($value --> Nil) {
    set-result-flag('backtrace', $value);
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
    set-producer('blame-per-file', -> $io { $GitBlameFile.new($io) });
    set-action('blame-per-file', $value);
}

my sub option-blame-per-line($value --> Nil) {
    check-GitBlameFile('blame-per-line');
    set-producer(
      'blame-per-line',
      -> $io { $GitBlameFile.new($io).lines, 'produce-many' }
    );
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

my sub option-categorize($value --> Nil) {
    set-result-Callable('categorize', $value);
}

my sub option-checkout($value --> Nil) {
    set-producer('checkout', &branches, 'produce-many');
    set-action('checkout', $value);
}

my sub option-classify($value --> Nil) {
    set-result-Callable('classify', $value);
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
    set-producer(
      'csv-per-line',
      -> $io { $TextCSV.new(|%CSV).csv: :$headers, :file($io.path) },
      'produce-many'
    );
    set-action('csv-per-line', $value);
}

my sub option-degree($value --> Nil) {
    my $code;
    $code := convert-to-simple-Callable($value, 'degree')
      unless Bool.ACCEPTS($value);
    my $integer := $code.defined
      ?? $code(Kernel.cpu-cores).Int
      !! $value.Int;
    Int.ACCEPTS($integer)
      ?? (%rak<degree> := $integer)
      !! meh "'--degree' must be an integer, or a Callable, not '$value'";
}

my sub option-deny($value --> Nil) {
    set-filesystem-callable('deny', $value)
}

my sub option-description($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--description' can only be specified as a string"
      !! (%global<description> := $value);
}

my sub option-device-number($value --> Nil) {
    set-filesystem-Int('device-number', $value)
}

my sub option-dir($value --> Nil) {
    %filesystem<dir> := Bool.ACCEPTS($value)
      ?? $value
      !! codify($value);
}

my sub option-dont-catch($value --> Nil) {
    set-rak-flag('dont-catch', $value);
}

my sub option-dryrun($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? (%modify<dryrun> := $value)
      !! meh "'--dryrun' can only be specified as a flag";
}

my sub option-eco-code($value --> Nil) {
    set-eco-xxx($value, "code");
}

my sub option-eco-doc($value --> Nil) {
    set-eco-xxx($value, "doc");
}

my sub option-eco-meta($value --> Nil) {
    set-producer(
      'eco-meta',
      -> $io {
          with try from-json $io.slurp(:$enc) -> \data {
              Seq.new: data.iterator
          }
      },
      'produce-many'
    );
    %result<eco-meta> := $value<> =:= True
      ?? ("rea",)
      !! (my @ecos is List = $value.split(',')).all (elem) <p6c cpan fez rea>
        ?? @ecos
        !! meh "Must specify one of p6c cpan fez rea with --eco-meta, not: $value";
    set-action('eco-meta', True);
}

my sub option-eco-provides($value --> Nil) {
    set-eco-xxx($value, "provides");
}

my sub option-eco-scripts($value --> Nil) {
    set-eco-xxx($value, "scripts");
}

my sub option-eco-tests($value --> Nil) {
    set-eco-xxx($value, "tests");
}

my sub option-edit($value --> Nil) {
    check-EditFiles('edit');
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
    set-csv-value('escape', $value, 'an escape');
}

my sub option-exec($value --> Nil) {
    external-execution('exec', $value);
}

my sub option-execute-raku($value --> Nil) {
    %result<execute-raku> := $value;
}

my sub option-extensions($value --> Nil) {
    meh "'--extensions' can only be specified as a string"
      if Bool.ACCEPTS($value);

    my @unknown;
    my @extensions = $value.split(',').map: {
        if .starts-with('#') {
            if %exts{$_} -> \extensions {
                extensions.Slip
            }
            else {
                @unknown.push: $_;
                Empty
            }
        }
        elsif $_ eq '*' {
            @known-extensions.Slip
        }
        else {
            $_
        }
    }

    @unknown
      ?? meh("No extensions known for group&s(@unknown) @unknown[]")
      !! (%filesystem<extensions> := codify-extensions @extensions);
}

my sub option-file($value --> Nil) {
    %filesystem<file> := Bool.ACCEPTS($value)
      ?? $value
      !! codify($value);
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

my sub option-headers($value --> Nil) {
    CATCH {
        meh "Could not compile --headers='$value':\n$_.message()";
    }
    %csv<headers> := Bool.ACCEPTS($value) || $value (elem) <skip auto lc uc>
      ?? $value
      !! List.ACCEPTS(my $compiled := $value.EVAL)
        ?? $compiled.are =:= Pair
          ?? %($compiled)  # convert list of Pairs to hash
          !! $compiled.Array
        !! $compiled;
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

my sub option-is-moarvm($value --> Nil) {
    set-filesystem-flag('is-moarvm', $value);
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

my sub option-is-pdf($value --> Nil) {
    set-filesystem-flag('is-pdf', $value);
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

my sub option-is-text($value --> Nil) {
    set-filesystem-flag('is-text', $value);
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
    set-producer(
      'json-per-elem',
      -> $io {
          with try from-json $io.slurp(:$enc) -> \data {
              Seq.new: data.iterator
          }
      },
      'produce-many'
    );
    set-action('json-per-elem', $value);
}

my sub option-json-per-file($value --> Nil) {
    set-producer('json-per-file',-> $io { try from-json $io.slurp(:$enc) });
    set-action('json-per-file', $value);
}

my sub option-json-per-line($value --> Nil) {
    set-producer(
      'json-per-line',
      -> $io {
          with try $io.lines(:$enc) -> $seq {
              $seq.map: { (try from-json($_)) // Empty }
          }
      },
      'produce-many'
    );

    set-action('json-per-line', $value);
}

my sub option-keep-meta($value --> Nil) {
    set-csv-flag('keep-meta', $value);
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
    Bool.ACCEPTS($value)
      ?? ($matches-only := $value)
      !! meh "'--matches-only' must be specified as a flag";
}

my sub option-max-matches-per-file($value --> Nil) {
    set-result-flag-or-Int('max-matches-per-file', $value);
}

my sub option-mbc($value --> Nil) {
    check-MoarVMBytecode('mbc');
    set-action('mbc', $value);
}

my sub option-mbc-frames($value --> Nil) {
    check-MoarVMBytecode('mbc-frames');
    set-action('mbc-frames', $value);
}

my sub option-mbc-strings($value --> Nil) {
    check-MoarVMBytecode('mbc-strings');
    set-action('mbc-strings', $value);
}

my sub option-meta-modified($value --> Nil) {
    set-filesystem-Instant('meta-modified', $value);
}

my sub option-mode($value --> Nil) {
    set-filesystem-Int('mode', $value);
}

my sub option-modifications($value --> Nil) {
    if Bool.ACCEPTS($value) {
        $modifications := $value;
        if $action-for -> $_ {
            meh "Not allowed to make modify files" when 'modify-files';
            meh "Not allowed to rename files"      when 'rename-files';
        }
    }
    else {
        meh "--modifications must be specified as a flag";
    }
}

my sub option-modified($value --> Nil) {
    set-filesystem-Instant('modified', $value);
}

my sub option-modify-files($value --> Nil) {
    meh "Not allowed to make modifications" unless $modifications;
    set-action('modify-files', $value);
}

my sub option-module($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--module' expects the name of a module to load"
      !! @modules.push($value)
}

my sub option-not($value --> Nil) {
    set-additional-pattern('not', $value, '!');
    $seen-initial-pattern := True;
}

my sub option-only-first($value --> Nil) {
    set-listing-flag-or-Int(
      'only-first',
      $value eq '∞' | '*' | 'Inf' ?? 0 !! $value
    );
}

my sub option-or($value --> Nil) {
    set-additional-pattern('or', $value, '');
}

my sub option-ornot($value --> Nil) {
    set-additional-pattern('ornot', $value, '!');
}

my sub option-output-dir($value --> Nil) {
    meh "'--output-dir' expects a directory specification"
      if Bool.ACCEPTS($value);

    my $io := $value.IO;
    meh "Directory '$io' with --output-dir must not exist" if $io.e;

    mkdir $io;
    $io.d
      ?? ($output-dir := $io)
      !! meh "Could not create directory '$output-dir' for --output-dir";
}

my sub option-output-file($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--output-file' expects a file specification"
      !! ($output-file := $value);
}

my sub option-pager($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--pager' expects a program specification"
      !! ($pager = $value);
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
      !! set-source('paths', $value.split(","));
}

my sub option-paths-from($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? meh "'--paths-from' must be a file specification, not a flag"
      !! set-source('paths-from', $value);
}

my sub option-pattern($value --> Nil) {
    set-additional-pattern('pattern', $value, '');
    $seen-initial-pattern := True;
}

my sub option-patterns-from($value --> Nil) {

    # helper sub for getting pattern(s) from file
    sub read-patterns($handle) {
        if $handle.lines -> @lines {
            add-pattern($_) for @lines;
            $seen-initial-pattern := True;
        }
    }

     if Bool.ACCEPTS($value) {
         meh "'--patterns-from' must be a file specification, not a flag"
     }
     elsif $value eq '-' {
         note "Reading from STDIN, please enter patterns and ^D when done:"
           unless $reading-from-stdin;
        read-patterns($*IN);
    }
    elsif $value.IO.r {
        read-patterns($value.IO);
    }
    else {
        meh "Could not read from '$value' to obtain patterns";
    }
}

my sub option-pdf-info($value --> Nil) {
    check-PDFExtract('pdf-per-file');
    set-action('pdf-info', $value);
}

my sub option-pdf-per-file($value --> Nil) {
    check-PDFExtract('pdf-per-file');
    set-action('pdf-per-file', $value);
}

my sub option-pdf-per-line($value --> Nil) {
    check-PDFExtract('pdf-per-line');
    set-action('pdf-per-line', $value);
}

my sub option-per-file($value --> Nil) {
    if Bool.ACCEPTS($value) {
        set-producer 'per-file', $value;
    }
    orwith convert-to-simple-Callable($value, 'per-file') {
        set-producer 'per-file', $_;
    }
    else {
        meh "Problem compiling code for '--per-file': $value";
    }
}

my sub option-per-line($value --> Nil) {
    if Bool.ACCEPTS($value) {
        set-producer 'per-line', $value, 'produce-many';
    }
    orwith convert-to-simple-Callable($value, 'per-file') {
        set-producer 'per-line', $_, 'produce-many';
    }
    else {
        meh "Problem compiling code for '--per-line': $value";
    }
}

my sub option-per-paragraph($value --> Nil) {
    if Bool.ACCEPTS($value) {
        if $value {
            set-producer(
              'per-line',
              -> $io { paragraphs($io.lines, 1, :Pair(PairContext)) },
              'produce-many-pairs'
            );
        }
    }
    else {
        meh "Can only specify '--per-paragraph' as a flag";
    }
}

my sub option-progress($value --> Nil) {
    my $ERR := PROCESS::<$ERR>;

    multi sub progress() { $ERR.print: (" " x 80) ~ "\r" }
    multi sub progress($p) {
        my $progress := "$p.nr-sources() / $p.nr-items() / $p.nr-matches()";
        $ERR.print: (" " x (80 - $progress.chars)) ~ $progress ~ "\r";
    }

    Bool.ACCEPTS($value)
      ?? $value
        ?? $writing-to-stderr
          ?? (%rak<progress> := &progress)
          !! Nil
        !! Nil
      !! meh "'--progress' must be a flag"
}

my sub option-proximate($value --> Nil) {
    set-listing-flag-or-Int('proximate', $value);
}

my sub option-rakudo-all($value --> Nil) {
    set-rakudo-xxx($value, "all");
}

my sub option-rakudo-c($value --> Nil) {
    set-rakudo-xxx($value, "c");
}

my sub option-rakudo-doc($value --> Nil) {
    set-rakudo-xxx($value, "doc");
}

my sub option-rakudo-java($value --> Nil) {
    set-rakudo-xxx($value, "java");
}

my sub option-rakudo-js($value --> Nil) {
    set-rakudo-xxx($value, "js");
}

my sub option-rakudo-nqp($value --> Nil) {
    set-rakudo-xxx($value, "nqp");
}

my sub option-rakudo-perl($value --> Nil) {
    set-rakudo-xxx($value, "perl");
}

my sub option-rakudo-raku($value --> Nil) {
    set-rakudo-xxx($value, "raku");
}

my sub option-rakudo-shell($value --> Nil) {
    set-rakudo-xxx($value, "shell");
}

my sub option-rakudo-test($value --> Nil) {
    set-rakudo-xxx($value, "test");
}

my sub option-rakudo-yaml($value --> Nil) {
    set-rakudo-xxx($value, "java");
}

my sub option-rename-files($value --> Nil) {
    meh "Not allowed to rename files" unless $modifications;
    set-action('rename-files', $value);
}

my sub option-quietly($value --> Nil) {
    set-rak-flag('quietly', $value);
}

my sub option-quote($value --> Nil) {
    set-csv-value('quote', $value, 'a quote');
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
    meh "--sayer option NYI";
}

my sub option-sep($value --> Nil) {
    set-csv-value('sep', $value, 'a seperator');
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

my sub option-show-item-number($value --> Nil) {
    set-result-flag('show-item-number', $value);
}

my sub option-silently($value --> Nil) {
    set-rak-flag('silently', $value);
}

my sub option-smartcase($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? ($smartcase := $value)
      !! meh "'--smartcase' must be specified as a flag";
}

my sub option-smartmark($value --> Nil) {
    Bool.ACCEPTS($value)
      ?? ($smartmark := $value)
      !! meh "'--smartmark' must be specified as a flag";
}

my sub option-sourcery($value --> Nil) {
    check-sourcery('sourcery');
    set-result-flag('sourcery', $value);
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
      ?? ($type := Any)
      !! StrType.ACCEPTS($value)
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
    set-rak-flag('sort', True);
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
    if %hash.map({ .key if .value }) -> @keys {
        meh qq:to/MEH/.chomp;
These $description options are incompatible with --$name:
@keys.sort.map({"--$_"})
MEH
    }
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
        %listing<show-filename> := False
          if $source-for && !$source-for.ends-with("-from");
    }
    elsif %filesystem {
        if %filesystem<under-version-control> {
            maybe-meh-together 'under-version-control', %filesystem<
              dir file recurse-symlinked-dir recurse-unmatched-dir
              auto-decompress
            >:k;
        }
        else {
            if %filesystem<file>:exists {
                maybe-meh-together 'file', %filesystem<
                  extensions
                >:k;
                my $file := %rak<file> := %filesystem<file>:delete;
                meh "--/file only makes sense when used together with --find"
                  if !$file && !%result<find>;
            }
            elsif %filesystem<extensions>:delete -> $seen {
                %rak<file> := $seen;
            }

            if %filesystem<auto-decompress>:delete {
                %rak<ioify> := &IOAD;

                my &old-filter :=
                  %rak<file> // codify-extensions @known-extensions;
                %rak<file> := {
                    my str $extension = extension($_);
                    $extension eq 'gz'
                      ?? old-filter(.chop(3))
                      !! $extension eq 'bz2'
                        ?? old-filter(.chop(4))
                        !! old-filter($_)
                }
            }
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

    %rak<is-text> := True
      unless $reading-from-stdin || (%rak<
        file is-text under-version-control is-pdf is-moarvm
      >:k) || %result<find>;
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

        # Seem to only want file information
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

        # Want all information
        else {
            if %result<max-matches-per-file>:delete -> $max {
                %rak<max-matches-per-source> := $max;
            }

            if %result<show-item-number>:exists {
                %rak<omit-item-number> := True
                  unless %result<show-item-number>:delete;
            }

            if %result<find>:delete {
                %rak<find>             := True;
                %rak<omit-item-number> := True
                  unless %result<frequencies classify categorize>:k;

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
                %rak<eager>  := True;

                if %result<unique>:delete {
                    %rak<sort>:delete;
                    %rak<omit-item-number> := True;

                    my int $nr-files;
                    my @uniques;

                    %rak<mapper> := -> $, @matches --> Empty {
                        LAST {
                            my int $nr-uniques = @uniques.unique.elems;
                            sayer $verbose
                              ?? "$nr-uniques unique occurrenc&es($nr-uniques) in $nr-files file&s($nr-files)"
                              !! $nr-uniques;
                        }

                        ++$nr-files;
                        @uniques.append: @matches.unique;
                    }
                }
                else {
                    # Set way to stringify paths
                    my &stringify :=
                      IO::Path.^find_method(%listing<absolute>:delete
                        ?? "absolute"
                        !! "relative"
                      );

                    my @files;
                    %rak<mapper> := -> $io, @matches --> Empty {
                        LAST {
                            if $verbose {
                                sayer "$_.key() has $_.value() match{"es" if .value > 1}"
                                  for @files;
                            }
                            my int $nr-matches = @files.map(*.value).sum;
                            my int $nr-files   = @files.elems;
                            sayer $nr-files
                              ?? "$nr-matches match&es($nr-matches) in $nr-files file&s($nr-files)"
                              !! "No files with matches";
                        }
                        @files.push: Pair.new:
                          $reading-from-stdin ?? '<STDIN>' !! stringify($io),
                          @matches.elems;
                    }
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
    $pager = %*ENV<RAK_PAGER> unless $pager.defined;
    if $pager {
        meh "Cannot specify a pager and an output-file"
          if $output-file && $output-file ne '-';
        $pager = "$pager -r" if $pager eq "less" | "more";  # ensure raw
        $*OUT = (run $pager.words, :in).in(:bin)
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

    %rak<batch> := 1;

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

    %rak<batch> := 1;

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

    %rak<mapper> := -> $, @matches --> Empty {
        my $pattern := @patterns.head;

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
            my @branches = branches();
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
    move-result-options-to-rak;

    %csv<auto-diag> := True unless %csv<auto-diag>:exists;

    # Set up args for producer
    $headers := %csv<headers>:delete // True;
    %CSV      = %csv{}:p:delete;

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-eco-meta(--> Nil) {
    meh-for 'eco-meta', <csv modify filesystem>;

    my $dir := ($*HOME // $*TMPDIR).add('.zef').add('store');
    %rak<sources> := %result<eco-meta>:delete.map: {
        $dir.add($_).add("$_.json")
    }
    %listing<show-filename> := False
      if %listing<show-filename>:!exists;

    move-result-options-to-rak;
    %rak<omit-item-number> := True
      unless %rak<unique frequencies omit-item-number>:k;

    $enc := %rak<encoding>:delete // 'utf8-c8';

    prepare-needle;
    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-edit(--> Nil) {
    %rak<progress>:delete;  # no progress indicator if editing

    my $pattern := @patterns.head.value;

    my sub go-edit-error($error --> Nil) {
        if backtrace-files($error).map: -> (:key($file), :value(@line)) {
            @line.map({ Pair.new: $file, $_}).Slip
        } -> @result {
            edit-files @result.List;
        }
#        note $error.chomp;
    }

    if %result<sourcery>:delete {
        meh-for 'edit', <output-file pager result filesystem modify csv>;
        edit-files sourcery $pattern.trim;
        return;
    }

    elsif %result<backtrace>:delete {
        meh-for 'edit', <output-file pager result filesystem modify csv>;

        $reading-from-stdin
          ?? go-edit-error($*IN.slurp(:close))
          !! @positionals
            ?? meh "Can only specify a single file with a backtrace."
            !! $pattern.IO.e
              ?? go-edit-error($pattern.IO.slurp)
              !! meh "handling backtrace from file(s) NYI";
        return;
    }

    elsif %result<execute-raku>:delete -> $raku {
        if Bool.ACCEPTS($raku) {
            meh "Must specify name of file with Raku code to execute"
              unless $pattern;

            my $io := $pattern.IO;
            meh "File '$pattern' is not found or not readable"
              unless $io.r;

            my $proc := run $*EXECUTABLE,
              ('--ll-exception' if $verbose), $io, @positionals, :out, :err;
            if $proc.err.slurp(:close) -> $backtrace {
                go-edit-error($backtrace);
                return;
            }
            elsif $proc.out.slurp(:close) -> $output {
                meh $output;
            }
            else {
                meh "No error or output produced";
            }
        }
        else {
            meh "Can only edit backtraces from actual file";
        }
    }

    %rak<max-matches-per-source> := $_
      with %result<max-matches-per-file>:delete;
    my $find                  := %result<find>:delete;
    my $files-with-matches    := %result<files-with-matches>:delete;
    my $files-without-matches := %result<files-without-matches>:delete;

    meh-for 'edit', <output-file pager result modify csv>;

    prepare-needle;
    move-filesystem-options-to-rak;
    my $editor := Bool.ACCEPTS($action) ?? Any !! $action;

    # Find filenames to edit
    if $find {
        %rak<find>             := True;
        %rak<omit-item-number> := True;
        %rak<mapper> := -> $, @files --> Empty {
            edit-files @files, :$editor;
        }
    }

    # Use filenames of (non-)matches
    elsif $files-with-matches || $files-without-matches {
        %rak<sources-only>         := True if $files-with-matches;
        %rak<sources-without-only> := True if $files-without-matches;

        my @files;
        %rak<mapper> := -> $io --> Empty {
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
                  :$ignorecase, :$smartcase, :$ignoremark, :$smartmark,
                  |(:$type if $type)
               ).head // 0)
            }
        }
    }

    run-rak(:eagerly);
    rak-stats;
}

my sub action-help(--> Nil) {
    # nothing to do
    return if Bool.ACCEPTS($action) && !$action;

    meh-for 'how', <csv modify filesystem result>;

    my constant @sections = <
      argument code content debugging examples faq filesystem general
      haystack item listing option pattern philosophy resource result
      special string
    >;

    class Hows {
        my sub paragraphize($name) {
            my @paragraphs;
            my @lines;
            for %?RESOURCES{"help/$name.txt"}.lines -> $line {
                if (($line.ends-with(':')
                     && !($line eq 'Example:' | 'Examples:'))
                ) || $line.starts-with('--')
                  || $line.starts-with('Q: ') {
                    @paragraphs.push: @lines.join("\n") if @lines;
                    @lines = $line;
                }
                else {
                    @lines.push: $line;
                }
            }
            @paragraphs.push: @lines.join("\n") if @lines;
            @paragraphs ?? @paragraphs.Slip !! Empty
        }

        method lines() {
            Bool.ACCEPTS($action)
              ?? (@sections.map: &paragraphize)
              !! $action (elem) @sections
                ?? paragraphize($action)
                !! meh "'$action' is not a recognized help subject";
        }
    }

    sub highlight-header($_, $first?) {
        $first
          || .starts-with('--')
          || .starts-with('Q: ')
          || (.ends-with(':') && !.starts-with(' '))
          ?? BON ~ $_ ~ BOFF
          !! $_
    }

    activate-output-options;

    # let's produce the necessary help and search in it / paragraph
    if @patterns.head -> $pattern {
        %rak<sources>          := (Hows,);
        %rak<omit-item-number> := True;
        %rak<degree>           := 1;

        %rak<map-all> := True;
        %rak<mapper>  := -> $, @matches {
            '<how>' => ((@matches
              ?? "Found @matches.elems() match&es(@matches) for '$pattern':"
              !! "Nothing found for '$pattern'"
            ), @matches.map({
                ('-' x 80)
                  ~ "\n"
                  ~ ($writing-to-stdout
                      ?? .lines.map({
                             my $result;
                             FIRST $result = highlight-header($_);
                             $result // $_
                         }).join("\n") ~ "\n"
                      !! $_
                    )
            }).Slip).Slip
        }

        prepare-needle;
        run-rak;

        %listing<show-filename> := False unless %listing<show-filename>:exists;
        %listing<trim>          := False unless %listing<trim>:exists;
        rak-results;
        rak-stats;
    }

    # no pattern, generic help request
    else {
        my $text := do if Bool.ACCEPTS($action) {
            %?RESOURCES<help.txt>.slurp(:close)
        }
        elsif $action {
            meh "'$action' is not a recognized help subject"
              unless $action (elem) @sections;
            %?RESOURCES{"help/$action.txt"}.slurp(:close)
        }

        if $text {
            my $SCRIPT := $*PROGRAM.basename;
            my $header := "$SCRIPT - " ~ DESCRIPTION;
            my str @parts;
            @parts.push: $header;
            @parts.push: "-" x $header.chars;
            @parts.push: $writing-to-stdout
              ?? $text.lines.map({
                     highlight-header($_, !$++)  # always first line
                 }).join("\n")
              !! $text;

            if $verbose {
                @parts.push: "";
                @parts.push: CREDITS;
                @parts.push: "";
                @parts.push: "Thank you for using $SCRIPT!";
            }

            sayer @parts.join("\n");
        }
    }
}

my sub action-json-per-elem(--> Nil) {
    meh-for 'json-per-elem', <csv modify>;

    # Normal json-per-elem handling
    %filesystem<file> //= codify-extensions %exts<#json>
      unless $reading-from-stdin;
    move-filesystem-options-to-rak;

    move-result-options-to-rak;
    %rak<omit-item-number> := True
      unless %rak<unique frequencies omit-item-number>:k;

    $enc := %rak<encoding>:delete // 'utf8-c8';

    prepare-needle;
    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-json-per-file(--> Nil) {
    meh-for 'json-per-file', <csv modify>;

    prepare-needle;
    %filesystem<file> //= codify-extensions %exts<#json>
      unless $reading-from-stdin;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    $enc := %rak<encoding>:delete // 'utf8-c8';

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
    %rak<omit-item-number> := True
      unless %rak<unique frequencies omit-item-number>:k;

    $enc := %rak<encoding>:delete // 'utf8-c8';

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-list-custom-options(--> Nil) {
    meh-for 'list-custom-options', <filesystem listing modify csv>;

    activate-output-options;
    if %config {
        my @with;
        my @without;

        # find the ones with and without description
        for %config.sort(*.key.fc) -> (:$key, :value(@args)) {
            my $description;
            my @nodesc = @args.map: {
                if .key eq 'description' {
                    $description := .value;
                    Empty
                }
                else {
                    $_
                }
            }
            $description
              ?? (@with.push: Pair.new: $key, Pair.new: $description, @nodesc)
              !! (@without.push: Pair.new: $key, @args)
        }

        if @with {
            for @with -> (:key($option), :value($_)) {
                say "&o($option): $_.key()";
                say "  &as-cli-arguments(.value)";
            }
        }
        if @without {
            say "\nOther options without description:" if @with;
            my $format := '%' ~ (@without.map(*.key.chars).max + 2) ~ "s: %s\n";
            for @without -> (:key($option), :value(@valid)) {
                printf $format, o($option), as-cli-arguments(@valid);
            }
        }
    }

    else {
        say "No custom options where found in '$config-file'";
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

    %rak<sort-sources> := *.absolute;

    # Producing haystacks per file
    if %rak<produce-one> {
        my @changed-files;
        my int $nr-files-seen;

        my &original-needle := $needle;
        $needle := -> $before {
            my $after := original-needle($before);
            Pair.new:
              $after.WHAT =:= $before.WHAT && !($after eqv $before),
              $after
        }

        %rak<mapper> := -> $io, $matches --> Empty {
            ++$nr-files-seen;

            LAST {
                my int $nr-files-changed = @changed-files.elems;
                my $fb = "Processed $nr-files-seen file&s($nr-files-seen)";
                $fb ~= ", $nr-files-changed file&s($nr-files-changed) changed"
                  if $nr-files-changed;

                if $verbose {
                    $fb ~= "\n";
                    for @changed-files -> $io {
                        $fb ~= "$io.relative()";
                    }
                    $fb ~= no-changes if $dryrun;
                    $fb .= chomp;
                }
                elsif $dryrun {
                    $fb ~= no-changes;
                }

                sayer $fb;
            }

            # Get the result
            $_ := $matches.head;

            # Changes were made
            if .key {
                unless $dryrun {
                    if $backup {
                        $io.spurt(.value)
                          if $io.rename($io.sibling($io.basename ~ $backup));
                    }
                    else {
                        $io.spurt: .value;
                    }
                }
                @changed-files.push: $io;
            }
        }
    }

    # Producing haystacks per line
    elsif %rak<produce-many-pairs> {
        my @changed-files;
        my int $nr-files-seen;
        my int $nr-paragraphs-changed;

        %rak<passthru-context> := %listing<passthru-context>:delete  // True;
        %rak<mapper> := -> $io, @matches --> Empty {
            ++$nr-files-seen;

            LAST {
                my int $nr-files-changed = @changed-files.elems;
                my $fb = "Processed $nr-files-seen file&s($nr-files-seen)";
                $fb ~= ", $nr-files-changed file&s($nr-files-changed) changed"
                  if $nr-files-changed;
                $fb ~= ", $nr-paragraphs-changed line&s($nr-paragraphs-changed) changed"
                  if $nr-paragraphs-changed;

                if $verbose {
                    $fb ~= "\n";
                    for @changed-files -> ($io, $changed) {
                        $fb ~= "$io.relative():";
                        $fb ~= " $changed change&s($changed)"  if $changed;
                        $fb ~= "\n";
                    }
                    $fb .= chomp;
                    $fb ~= no-changes if $dryrun;
                }
                elsif $dryrun {
                    $fb ~= no-changes;
                }

                sayer $fb;
            }

            my int $paragraphs-changed;
            for @matches {
                ++$paragraphs-changed if .changed;
            }
            if $paragraphs-changed {
                unless $dryrun {
                    if $backup {
                        $io.spurt(@matches.map(*.value).join("\n"))
                          if $io.rename($io.sibling($io.basename ~ $backup));
                    }
                    else {
                        $io.spurt: @matches.map(*.value).join("\n");
                    }
                }
                $nr-paragraphs-changed += $paragraphs-changed;
                @changed-files.push: ($io, $paragraphs-changed);
            }
        }
    }

    # Producing haystacks per line
    else {
        my @changed-files;
        my int $nr-files-seen;
        my int $nr-lines-changed;
        my int $nr-lines-removed;

        my $joiner := %rak<with-line-endings> ?? "" !! "\n";
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
                    $fb .= chomp;
                    $fb ~= no-changes if $dryrun;
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
                ++$lines-changed if .changed;
            }
            if $lines-changed || $lines-removed {
                unless $dryrun {
                    if $backup {
                        $io.spurt(@matches.map(*.value).join($joiner)) ~ $joiner
                          if $io.rename($io.sibling($io.basename ~ $backup));
                    }
                    else {
                        $io.spurt: @matches.map(*.value).join($joiner) ~ $joiner;
                    }
                }
                $nr-lines-changed += $lines-changed;
                $nr-lines-removed += $lines-removed;
                @changed-files.push: ($io, $lines-changed, $lines-removed);
            }
        }
    }

    my $*N = 0;
    run-rak(:eagerly);
    rak-stats;
}

my sub handle-mbc(str $what, str $producer, &handler --> Nil) {
    meh-for $what, <modify>;

    prepare-needle;
    %filesystem<is-moarvm> //= True unless $reading-from-stdin;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;
    %rak{$producer} := &handler;

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-mbc(--> Nil) {
    handle-mbc 'mbc', 'produce-one',
      -> $io { $MoarVMBytecode.new($io) }
}

my sub action-mbc-frames(--> Nil) {
    handle-mbc 'mbc-frames', 'produce-many',
      -> $io { $MoarVMBytecode.new($io).frames }
}

my sub action-mbc-strings(--> Nil) {
    handle-mbc 'mbc-strings', 'produce-many',
      -> $io { $MoarVMBytecode.new($io).strings }
}

my sub handle-pdf(str $what, str $producer, &handler --> Nil) {
    meh-for $what, <modify>;

    prepare-needle;
    %filesystem<is-pdf> //= True unless $reading-from-stdin;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;
    %rak{$producer} := &handler;

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-pdf-info(--> Nil) {
    handle-pdf 'pdf-info', 'produce-one',
      -> $file { $PDFExtract.new(:$file).info }
}

my sub action-pdf-per-file(--> Nil) {
    handle-pdf 'pdf-per-file', 'produce-one',
      -> $file { $PDFExtract.new(:$file).text }
}

my sub action-pdf-per-line(--> Nil) {
    handle-pdf 'pdf-per-line', 'produce-many',
      -> $file { $PDFExtract.new(:$file).text.lines }
}

my sub action-per-file(--> Nil) {
    meh-for 'per-file', <csv modify>;

    prepare-needle;
    move-filesystem-options-to-rak;
    move-result-options-to-rak;

    activate-output-options;
    run-rak;
    rak-results;
    rak-stats;
}

my sub action-per-line(--> Nil) {
    meh-for 'per-line', <csv modify>;

    # helper sub for --backtrace and --execute-raku
    my sub produce-result($error) {
        my $context        := %result<context>:delete        // 2;
        my $before-context := %result<before-context>:delete // $context;
        my $after-context  := %result<after-context>:delete  // $context;
        meh-for 'backtrace', <filesystem result>;

        %listing<highlight> := False;
        %listing<trim>      := False if $before-context || $after-context;
        backtrace-files($error,
          :source, :$before-context, :$after-context,
          :in-backtrace(PairMatched), :added-context(PairContext)
        )
    }

    activate-output-options;
    my $pattern := @patterns.head.value;

    if %result<sourcery>:delete {
        meh-for 'sourcery', <filesystem>;

        $pattern     := $pattern.trim;
        my $sourcery := sourcery $pattern;

        # normalize the sourcery results
        my %result;
        for $sourcery -> (:key($file), :value($linenumber)) {
            (%result{$file} // (%result{$file} := [])).push: $linenumber;
        }

        # Decide on whether it's a match by the line number for that file
        my @sources = %result.keys>>.IO;
        my %linenrs = @sources.map: * => 0;
        $needle := -> $ {
            ++%linenrs{$*SOURCE} (elem) %result{$*SOURCE}
        }
        @highlights.push: implicit2explicit sourcery-pattern $pattern;
        %rak<sources> := @sources; # only these files
    }

    elsif %result<backtrace>:delete {
        %listing<whole-line> := True;
        %listing<proximate>  := 1;

        # $pattern here is supposed to be the file with the backtrace
        $rak := Rak.new: result => $reading-from-stdin
          ?? produce-result($*IN.slurp(:close))
          !! @positionals
            ?? meh "Can only specify a single file with a backtrace."
            !! $pattern
              ?? $pattern.IO.e
                ?? produce-result($pattern.IO.slurp)
                !! meh "handling backtrace from file(s) NYI"
              !! meh "Must specify a file with a backtrace.";

        rak-results;
        return;
    }

    elsif %result<execute-raku>:delete -> $raku is copy {
        %listing<whole-line> := True;
        %listing<proximate>  := 1;

        # Executing from a script
        if Bool.ACCEPTS($raku) {
            meh "Must specify name of file with Raku code to execute"
              unless $pattern;

            my $io := $pattern.IO;
            meh "File '$pattern' is not found or not readable"
              unless $io.r;

            my $proc := run $*EXECUTABLE,
              ('--ll-exception' if $verbose), $io, @positionals, :out, :err;
            if $proc.err.slurp(:close) -> $backtrace {
                $rak := Rak.new: result => produce-result($backtrace);
            }
            elsif $proc.out.slurp(:close) -> $output {
                meh $output;
            }
            else {
                meh "No error or output produced";
            }

            rak-results;
            return
        }

        # Executing from literal code or STDIN
        else {
            CATCH {
                my $gist := .gist;
                if $gist.contains('Error while compiling') {
                    meh $gist.subst(/ 'compiling' <( \s+ \S+ /, ':');
                }

                my @result;
                for produce-result($_) {
                    if .key.starts-with('EVAL_') {
                        my int $index = .value.head.key.Int;
                        my int $linenr;
                        @result.push: Pair.new:
                          "CODE",
                          $raku.lines.map({
                            (++$linenr == $index ?? PairMatched !! PairContext)
                              .new: $linenr, $_
                          }).List;
                        last;
                    }
                    else {
                        @result.push: $_;
                    }
                }
                if @result {
                    $rak := Rak.new: :@result;
                    rak-results;
                    return;
                }
                meh $gist;  # some other weird error
            }
            if $raku eq '-' {
                note "Reading from STDIN, please enter code and ^D when done:"
                  unless $reading-from-stdin;
                $raku = $*IN.slurp(:close);
            }

            # Actually run the code
            $raku.EVAL;
            meh "Did not get an execution error, so no backtrace to work with.";
        }
    }

    else {
        prepare-needle;
        move-filesystem-options-to-rak;

        # The default in rak already does the right thing
        %rak<produce-many> := $action
          if $action.defined && !($action<> =:= True);

    }

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
    activate-output-options;
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
    %rak<sort-sources>     := *.absolute;
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

    my $*N = 0;
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
      !! -> $, @matches {
             '<unicode>' => @matches.map: {
                 my $chr := .uniparse;
                 "$chr.ord.fmt('%5X') $_ $chr"
             }
         }

    $smartcase  := $smartmark  := False;
    $ignorecase := $ignoremark := True;
    prepare-needle;

    run-rak;

    activate-output-options;
    %listing<show-filename> := False unless %listing<show-filename>:exists;
    %listing<trim>          := False unless %listing<trim>:exists;
    rak-results;
    rak-stats;
}

my sub action-version(--> Nil) {
    meh-only('version');

    activate-output-options;
    my proto sub MAIN(|) {*}
    use CLI::Version:ver<0.0.9+>:auth<zef:lizmat> $?DISTRIBUTION, &MAIN, 'long';
    MAIN(:version, :$verbose);
}

my sub action-vimgrep(--> Nil) {
    my $pattern := @patterns.head;

    # helper sub for --backtrace and _-execute-raku
    my sub vimgreppify($error --> Nil) {
        for backtrace-files($error) -> (:key($file), :value(@line)) {
            sayer "$file:$_\::" for @line;
        }
    }

    if %result<sourcery>:delete {
        meh-for 'vimgrep', <output-file pager result filesystem modify csv>;
        sayer "$_.key():$_.value()::" for sourcery $pattern.trim;
        return;
    }

    elsif %result<backtrace>:delete {
        meh-for 'edit', <output-file pager result filesystem modify csv>;

        $reading-from-stdin
          ?? vimgreppify($*IN.slurp(:close))
          !! @positionals
            ?? meh "Can only specify a single file with a backtrace."
            !! $pattern.IO.e
              ?? vimgreppify($pattern.IO.slurp)
              !! meh "handling backtrace from file(s) NYI";
        return;
    }

    elsif %result<execute-raku>:delete -> $raku {
        if Bool.ACCEPTS($raku) {
            meh "Must specify name of file with Raku code to execute"
              unless $pattern;

            my $io := $pattern.IO;
            meh "File '$pattern' is not found or not readable"
              unless $io.r;

            my $proc := run $*EXECUTABLE,
              ('--ll-exception' if $verbose), $io, @positionals, :out, :err;
            if $proc.err.slurp(:close) -> $backtrace {
                vimgreppify($backtrace);
                return;
            }
            elsif $proc.out.slurp(:close) -> $output {
                meh $output;
            }
            else {
                meh "No error or output produced";
            }
        }
        else {
            meh "Can only vimgrep backtraces from actual file";
        }
    }

    meh "Cannot use --vimgrep when reading from STDIN"
      if $reading-from-stdin;

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
                    :$ignorecase, :$smartcase, :$ignoremark, :$smartmark,
                    |(:$type if $type)
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
                    meh("Must specify a value for -$original-name to satisfy '$name'")
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

            # specified as negation
            else {
                for @expanded -> (:key($name), :$value)  {
                    # only negate Bools, ignore others because negation of
                    # other values doesn't make sense other than it not
                    # being specified
                    named($name, !$value, :recurse($name ne $original-name))
                      if Bool.ACCEPTS($value);
                }
            }
        }

        # Got a value
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
    elsif %?RESOURCES<help.txt>.lines.first(/ ' --' $name \W /) -> $line {
        $line.substr(1).split(/ \s+ /, 2).tail
    }
    elsif %config{$name} -> @args {
        @args.map({ .value if .key eq 'description' }).head // ""
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

            if (|@options,|%config.keys).map(-> $after {
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

    @text.push: "Specify --help for an overview of available options.";
    exit note @text.join("\n");
}

# Prepare the executable needle
my sub prepare-needle() {
    $needle := @patterns
      # A full blown needle
      ?? compile-needle
            @patterns,
           :$ignorecase,
           :$smartcase,
           :$ignoremark,
           :$smartmark,
           :@repos,
           :@modules,
           :matches($matches-only)
      # put in a basic noop
      !! &defined;

    if $source-rak {
        @positionals
          ?? meh("Specified path&s(@positionals) '@positionals[]' with --$source-for")
          !! (%rak{$source-rak} := $source);
    }
    elsif @positionals {
        %rak<paths> := @positionals.splice;
    }

    %rak<produce-one> := *.slurp(:enc(%rak<encoding> // 'utf8-c8'))
      if %rak<produce-one><> =:= True;
}

# Return all options as a list of Pairs
my sub as-options() {
    my @options;
    my sub add($name, $value) { @options.push: Pair.new: $name, $value }

    add('pattern', @patterns)            if @patterns;
    add('smartcase', $smartcase)         if $smartcase;
    add('ignorecase', $ignorecase)       if $ignorecase;
    add('ignoremark', $ignoremark)       if $ignoremark;
    add('type', $type)                   if $type;
    add('paths', @positionals.join(',')) if @positionals;
    add($action-for, $action)            if $action-for;

    @options.append: .pairs for
      %global, %filesystem, %csv, %listing, %modify;

    add('verbose', $verbose) if $verbose;
    @options
}

# vim: expandtab shiftwidth=4
