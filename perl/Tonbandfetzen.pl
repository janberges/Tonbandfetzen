#!/usr/bin/perl

$0 =~ /(.+)\//;
chdir $1;

$pi = 4 * atan2 1, 1;

@d = (".");
while ($d = shift @d) {
	opendir D, $d;
	-d and push @d, $_ or /(.+)\.txt$/ and !-e "$1.aif" || -M "$1.aif" > -M and push @i, $1 for map "$d/$_", grep /[^.]/, readdir D;
	closedir D;
	}

for (splice @i) {
	print "$_...\n";

	open T, "<$_.txt";
	chomp (@i = <T>);
	close T;

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
					$e = $w[$p % $w] * $d * ($a[$t++] // 1) * ($z[$T--] // 1);
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

	$e = 0;
	$e < abs and $e = abs for @i;
	$q = $e ? 0x7FFF / $e : 0;

	open B, ">$_.aif";
	print B

		"FORM",
		pack("l>", 2 * @i + 46),
		"AIFF",

		"COMM",
		pack("l>", 18),
		pack("s>", 2),
		pack("L>", @i / 2),
		pack("s>", 16),
		b($s),

		"SSND",
		pack("l>", 2 * @i + 8),
		pack("L>", 0),
		pack("L>", 0),
		pack("s>*", map $_ * $q, splice @i);

	close B;
	}

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
