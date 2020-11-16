# Unicode character database

$| = 1;
use v5.10;
use strict;
use warnings;

use Data::Dumper;
use Unicode::UCD;

use Cwd;
my $dir = getcwd;

binmode STDOUT, ":utf8";

chdir '/usr/local/lib/perl5/5.32/unicore' or die "Set dir to perl unicore lib";

my %LOWER = Unicode::UCD::_read_table("To/Lc.pl", "use_hash");
my %UPPER = Unicode::UCD::_read_table("To/Uc.pl", "use_hash");
my %TITLE = Unicode::UCD::_read_table("To/Tc.pl", "use_hash");



# For MLton - pattern matching, and for PolyML optimised if-then-else.
my $CASEFOLD = Unicode::UCD::all_casefolds();


sub do_casefold {
	my ($fh, $match) = @_;

	print $fh $match ? "fun " : "fun fc w ws =\n";
	my $prev = 0;
	foreach my $k (sort { $a <=> $b } keys %$CASEFOLD) {
		my $v = $$CASEFOLD{$k};
		# if ($$v{mapping} =~ m/ /) {
		# 	my @mapping = split / +/, $$v{mapping};
		# 	my $mapping = join "", map { s/([0-9A-F]{4})/chr(hex($1))/eigr } @mapping;
		# 	say $k, " ", chr($k), " $mapping ", scalar @mapping;
		# }

		my $w = sprintf "0wx%x",  $k;
		my @fc = map { s/^0*//g; "0wx" . lc($_) } reverse split / +/, $$v{mapping};
		my $fc = join "::", @fc;


		if ($match) {
			print $fh "fc $w ws = ${fc}::ws\n  | ";
		} else {
			if ($prev + 10 < $k) {
				print $fh "  if w < $w then w::ws else (* ***** *)\n";
			}
			print $fh "  if w = $w then ${fc}::ws else\n";
		}
		$prev = $k;
	}
	print $fh $match ? "fc w       ws = w::ws\n\n\n" : "  w::ws\n\n\n";
}


sub do_map {
	my ($fh, $MAP, $fn_name, $match) = @_;

	print $fh $match ? "fun " : "fun $fn_name w =\n";
	my $prev = 0;
	foreach my $k (sort { $a <=> $b } keys %$MAP) {
		my $v = $$MAP{$k};
		# say "$k => $v; ", chr($k), " => ", chr($v);
		my $wk = sprintf "0wx%x", $k;
		my $wv = sprintf "0wx%x", $v;

		if ($match) {
			print $fh "$fn_name $wk = ${wv}\n  | ";
		} else {
			if ($prev + 10 < $k) {
				print $fh "  if w < $wk then w else (* ***** *)\n";
			}
			print $fh "  if w = $wk then ${wv} else\n";
		}
		$prev = $k;
	}
	print $fh $match ? "$fn_name w        = w\n\n\n" : "  w\n\n\n";
}


sub doit {
	my ($file, $match) = @_;

	open my $fh, ">", $file or die "Cannot open '$file' file: $!";

	print $fh <<'EOD';
structure Unicode :
sig
  val toCaseFolded : word list -> word list
  val toLower      : word list -> word list
  val toUpper      : word list -> word list
  val toTitle      : word list -> word list

  (* N.B. reversed lists *)
  val fc : word -> word list -> word list
  val lc : word -> word
  val uc : word -> word
  val tc : word -> word
end
=
struct

EOD

	do_casefold($fh, $match);

	do_map($fh, \%LOWER, "lc", $match);
	do_map($fh, \%UPPER, "uc", $match);
	do_map($fh, \%TITLE, "tc", $match);

	print $fh <<'EOD';
fun toCaseFolded ws =
  let
  fun loop [] r = rev r
    | loop (w::ws) r = loop ws (fc w r)
in
  loop ws []
end

fun toLower wc = map lc wc
fun toUpper ws = map uc (toCaseFolded ws)
fun toTitle ws = map tc (toCaseFolded ws)

end
EOD

	close $fh;
}


doit("$dir/unicode-if.sml", 0); # if-then-else
doit("$dir/unicode.sml", 1);    # pattern matching
