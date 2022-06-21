use highlighter:ver<0.0.3>:auth<zef:lizmat>;
use Files::Containing:ver<0.0.1>:auth<zef:lizmat>;

my constant BON  = "\e[1m";
my constant BOFF = "\e[22m";

my constant @raku-extensions = <
   raku rakumod rakutest nqp t pm6 pl6
>;

sub add-before-after($io, @initially-selected, int $before, int $after) {
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

my sub MAIN(
          $needle is copy,
          $dir = ".",
  Bool() :l($files-only),
         :$human  = $*OUT.t,
  UInt:D :$before = 0,       
  UInt:D :$after  = 0,       
         :$batch,
         :$degree,
) is export {

    $needle .= trim;
    if $needle.starts-with('/') && $needle.ends-with('/')
      || $needle.indices('*') == 1 {
        $needle .= EVAL;
    }
    elsif $needle.starts-with('{') && $needle.ends-with('}') {
        $needle = ('-> $_ ' ~ $needle).EVAL;
    }

    my $seq := files-containing
      $needle,
      $dir,
      :$files-only,
      :extensions(),
      :offset(1),
      :$batch,
      :$degree,
    ;

    if $files-only {
        say .relative for $seq;
    }
    elsif $human {
        if $before || $after {
            for $seq {
                my $io := .key;
                say $io.relative;

                my @selected := add-before-after($io, .value, $before, $after);
                my $width := @selected.tail.key.chars + 1;

                for @selected {
                    say sprintf('%' ~ $width ~ 'd', .key)
                      ~ ': '
                      ~ highlighter .value, $needle, BON, BOFF
                }
                say "";
            }
        }
        else {
            for $seq {
                say .key.relative;
                my $width := .value.tail.key.chars + 1;
                for .value {
                    say sprintf('%' ~ $width ~ 'd', .key)
                      ~ ': '
                      ~ highlighter .value.trim, $needle, BON, BOFF
                }
                say "";
            }
        }
    }
    else {
        if $before || $after {
            for $seq {
                my $io   := .key;
                my $file := $io.relative;
                my @selected := add-before-after($io, .value, $before, $after);
                my int $last-linenr = @selected[0].key - 1;

                for @selected {
                    my int $linenr = .key;
                    say "--" if $last-linenr != $linenr - 1;
                    say "$file: " ~ .value;
                    $last-linenr = $linenr;
                }
                say "--";
            }
        }
        else {
            for $seq {
                my $file := .key.relative;
                say "$file: " ~ .value.trim for .value;
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

Note: this is still very much in alpha development phase.  Comments and
suggestions are more than welcome!

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
