#!/usr/bin/perl

use CGI;
use CGI::Carp "fatalsToBrowser";
$I = new CGI->param("I");
@i = split /\x0D\x0A|\x0D|\x0A/, $I;

-e || mkdir $_ and chdir $_ || die for "kurzweiliges";

opendir DIR, ".";
/^\./ or -M > 1 and unlink for readdir DIR;
closedir DIR;

$pi = 4 * atan2 1, 1;
$s = 44100;

$i = @s = ();
/;/ and push @s, $_ or /^[\d.]+$/ and $s = $_ or $_ and $i[$i++] .= $_ or $i = 0 for splice @i;

$ms = $s / 1000;
$min = 60 * $s;

for (@s) {
	s/\^/**/g;
	s/\|(.*?)\|/abs($1)/g;
	s/\[(.*?)\]/int($1)/g;
	s/(\d|\))\s*([a-z]|\()/$1*$2/gi;
	s/([a-z]+)/$1 =~ \/^(sin|cos|tan|cot|sqrt|exp|log|abs|sgn|int)$\/ ? $1 : "\$$1"/egi;
	}

@S = ();
eval join "", map "
	\$x1 = $$_[1];
	\$x2 = $$_[2];
	\$n = int($$_[3]);
	\$dx = (\$x2 - \$x1) / \$n;

	push \@S, [];
	for \$x (map \$x1 + \$dx * \$_, 1..\$n) {
		push \@{\$S[-1]}, $$_[0];
		}", map [split /;\s*/, $_], @s;

$f = $f0 = $A0 = 27.5 / $s;
$d = $b = $F0 = $D0 = $B0 = 1;

$n = q/(\d*(?:\.\d+)?)/;
$N = qq/($_)?/ for join "|", 1..@S;

for (splice @i) {
	$i = 0;
	push @i, [];

	$t0 = $S = 0;
	$T = 4/120 * $min;

	while ($_) {
		s/^(\|+(?:$n:$n\|+)?([^|]+)|([- ]+)|(x?([A-G])?(\x23*)$n(\?|!)?$n(\[|\])?$n&?$N\^?$N\$?$N(,|\x60)?$n('|\x22)?$n(\(|\))?$n)(\\|\/)?$n(>|<)?$n(\{|\})?$n~*)/$4/ and length $1 or s/.// and next;

		if ($4) {
			$T = $2 / $3 * $min if $3;
			$S = $T / length $4;
			next;
			}

		$t0 += $S * length $1;
		$t0 -= ($t = int $t0);

		$i = 0 if $5 or length $6;

		$5 and push @{$i[-1]}, [$t] and next;

		$f = $7 ? ($f0 = $A0 * 2 ** (({C => -9, D => -7, E => -5, F => -4, G => -2, A => 0, B => 2}->{$7} + length $8) / 12 + $9)) : $f0 * 2 ** ($9 / 12) if length $9;
		$d = 10 ** ($11 * {"?" => -1, "!" => 1}->{$10} / 10) if $10;
		$b = 10 ** ($13 * {"[" => -1, "]" => 1}->{$12} / 10) if $12;

		$F0 = 2 ** ($18 * {"," => -1, "`" => 1}->{$17} / $s / 12) if $17;
		$D0 = 10 ** ($20 * {"'" => -1, "\"" => 1}->{$19} / $s / 10) if $19;
		$B0 = 10 ** ($22 * {"(" => -1, ")" => 1}->{$21} / $s / 10) if $21;

		$i or push @{$i[-1]}, [0, $f, $d, $b, $F0, $D0, $B0, 0, 0, 0, [0], [[0, 0]], [[0, 0]], [[0, 0]]];

		$i[-1][-1][7] = $S[$14 - 1] if $14;
		$i[-1][-1][8] = $S[$15 - 1] if $15;
		$i[-1][-1][9] = $S[$16 - 1] if $16;

		$i[-1][-1][0] += $t;
		$i[-1][-1][10][$i] = $t;

		$i[-1][-1][11][$i][0] = $24 * {"\\" => -1, "/" => 1}->{$23} if $23;
		$i[-1][-1][12][$i][0] = $26 * {">" => -1, "<" => 1}->{$25} if $25;
		$i[-1][-1][13][$i][0] = $28 * {"{" => -1, "}" => 1}->{$27} if $27;

		$i[-1][-1][$_][-1][1] += $t for 11..13;

		$i++;
		}
	}

$w = @w = (-1, 1);

for (splice @i) {
	$i = 0;
	for (@$_) {
		($T, $f) = @$_[0, 1];

		$f or $i += 2 * $T and next;

		($d, $b, $F0, $D0, $B0) = @$_[2..6];
		$w = @w = @{$$_[7]} if $$_[7];
		@a = @{$$_[8]} if $$_[8];
		@z = @{$$_[9]} if $$_[9];
		@t = @{$$_[10]};

		$f *= $w;
		$p = rand $w;
		$t = 0;

		for $k (grep $t[$_], 0..$#t) {
			$F = $F0 * 2 ** ($$_[11][$k][0] / $$_[11][$k][1] / 12) if $$_[11][$k];
			$D = $D0 * 10 ** ($$_[12][$k][0] / $$_[12][$k][1] / 10) if $$_[12][$k];
			$B = $B0 * 10 ** ($$_[13][$k][0] / $$_[13][$k][1] / 10) if $$_[13][$k];

			for (1..$t[$k]) {
				$e = $w[$p % $w] * $d * ($a[$t++] || 1) * ($z[$T--] || 1);
				$a = atan2 $b, 1;
				$i[$i++] += $e * cos $a;
				$i[$i++] += $e * sin $a;
				$p += $f;
				$f *= $F;
				$d *= $D;
				$b *= $B;
				}
			}
		}
	}

print "Content-type: text/html

<html>
	<head>
		<title>Tonbandfetzen</title>
		<meta http-equiv='content-type' content='text/html; charset=utf-8'>
		<style type='text/css'>
			body {
				margin: 1cm;
				color: #ff8;
				}
			body, textarea {
				font: 12px monospace;
				background-color: #000;
				}
			textarea {
				margin: 0;
				border: 0;
				padding: 0;
				color: #fff;
				}
			a {
				text-decoration: none;
				}
			a:link, a:visited {
				color: #8ff;
				}
			a:active, a:focus, a:hover {
				color: #f8f;
				}
		</style>
	</head>
	<body>
		K A N   N S T   # D U   #       I N #   D E N       #   B Ä U   M E N   # D I   E # T   O N B   A N D<br>
		  F     E   T   Z   E   N       #   S   E   H       E   N       ?         #         W   E       R   #<br>
		  H     A   T   #   S   I E #   D O R   T   H   I N #   G E B   R A C     H     T ? #   A L L   E   S<br>
		  #     G   E   H   Ö   R   T   #   D   I   R   ,   #   E       I         N     E       #       W   E<br>
		  L     T # A   U   S   # P A   P   I   E   R   . # V   O       N # T     O     C O T   R O N   I   C<br>
		<br>";

if (@i) {
	$e = 0;
	$e < abs and $e = abs for @i;
	$q = $e ? 0x7FFF / $e : 0;

	open B, ">$$.aif";
	print B

		"FORM",
		pack("N", 2 * @i + 46),
		"AIFF",

		"COMM",
		pack("N", 18),
		pack("n", 2),
		pack("N", @i / 2),
		pack("n", 16),
		b($s),

		"SSND",
		pack("N", 2 * @i + 8),
		pack("N", 0),
		pack("N", 0),
		pack("n*", map $_ * $q, splice @i);

	close B;

	for ($I) {
		s/&/&amp;/g;
		s/</&lt;/g;
		s/>/&gt;/g;
		}

	print "
		<a href='kurzweiliges/$$.aif'>
			K   E   I N E   # M E   I   S   T E R   W E R   K E #   M E H   R       ! # D       I   E # Z   E I T<br>
			#   I   S       T       #   L   Ä   N     G     S       T       #       S   C       H   O       N   #<br>
			R E I   F # D   A       F   Ü   R   !     #     W A S   #       W       I R #   N I E   M A L   S   #<br>
			Z   U   #       E       N   D   E   #     B     R       I       N       G   E   N   #   K       A   N<br>
			N   #   K E I   N       # M O   L   O     C     H # J   E       # V E   R   S   C H L   I N G   E   N<br>
		</a>
		<br>";
	}

print "
		<form action='' method='post' accept-charset='utf-8'>
			<textarea name='I' cols='101' rows='27'>", $I || "4410\n\nsin(x); 0; 2pi; s/4\nsin(x)^2; 0; pi/2; 75ms\n\n&1^2\$2&1^2$2|C2[30/60}60~~\\60{60~~~~~~~|", "</textarea><br>
			<input type='submit'>
		</form>
	</body>
</html>";

sub b {
	my $d = shift;
	my $b = "";
	my $e = int(log(abs($d)) / log(2));
	my $m = abs($d) / 2 ** $e;

	$e += 0x3FFF;

	for (1..15) {
		$b .= $e % 2;
		$e = int($e / 2)
		}

	$b .= $d < 0 ? 1 : 0;
	$b = reverse $b;

	for (1..64) {
		$b .= int($m);
		$m -= int($m);
		$m *= 2;
		}

	return pack "B80", $b;
	}

sub tan {
	eval { sin($_[0]) / cos($_[0]) };
	}

sub cot {
	eval { cos($_[0]) / sin($_[0]) };
	}

sub sgn {
	$_[0] <=> 0;
	}
