#!/usr/bin/perl

$0 =~ /(.+)\//;
chdir $1;

$pi = 4 * atan2 1, 1;

$s = 44100;
$ms = $s / 1000;
$min = 60 * $s;
$Hz = 1 / $s;
$kHz = 1 / $ms;

$A4 = 440 * $Hz;
$T = $min / 120;
$a = 50 * $ms;

@y = map sin $_, in(0, 2 * $pi, $s);

sub mel {
	my $t = $T;

	my $p = 0;
	my $P = @y;
	my $r;
	my $R;

	my @i = ();
	my %i = ();

	my $F0 = $A0;
	my $Y0 = 1;
	my $X0 = 1;
	my $F = 1;
	my $Y = 1;
	my $X = 1;
	my $ft = 1;
	my $yt = 1;
	my $xt = 1;
	my $fs = 1;
	my $ys = 1;
	my $xs = 1;

	my $f;
	my $y;
	my $x;

	my $n;

	my @a = map sin($_) ** 2, in(0, $pi / 2, $a);

	for (split /\s+/, shift) {
		if (/\%|\*|[A-G]|-|\+|\?|!|\[|\]/) {
			$F = $F0;
			$Y = $Y0;
			$X = $X0;
			$i{$#i} = 1;
			}

		/([\d.]+):?([\d.]*)/;
		$n = $1 / ($2 or 1);

		   if (/\*/     ) { $F = $F0 = $n * $Hz }
		elsif (/([A-G])/) { $F = $F0 = $A4 * 2 ** (({C => -9, D => -7, E => -5, F => -4, G => -2, A => 0, B => 2}->{$1} + /\x23/) / 12 + $n - 4) }
		elsif ( /(-|\+)/) { $F  = $F0 *  2 ** ({"-"  => -1, "+" => 1}->{$1} * $n / 12) }
		elsif (/(\\|\/)/) { $ft =        2 ** ({"\\" => -1, "/" => 1}->{$1} * $n / 12) }
		elsif ( /(_|\^)/) { $fs =        2 ** ({"_"  => -1, "^" => 1}->{$1} * $n / 12) }
		elsif (/(\?|!)/ ) { $Y  = $Y0 = 10 ** ({"?"  => -1, "!" => 1}->{$1} * $n / 10) }
		elsif ( /(>|<)/ ) { $yt =       10 ** ({">"  => -1, "<" => 1}->{$1} * $n / 10) }
		elsif ( /(,|;)/ ) { $ys =       10 ** ({","  => -1, ";" => 1}->{$1} * $n / 10) }
		elsif (/(\[|\])/) { $X  = $X0 = 10 ** ({"["  => -1, "]" => 1}->{$1} * $n / 10) }
		elsif (/(\(|\))/) { $xt =       10 ** ({"("  => -1, ")" => 1}->{$1} * $n / 10) }
		elsif (/(\{|\})/) { $xs =       10 ** ({"{"  => -1, "}" => 1}->{$1} * $n / 10) }
		elsif (/~/      ) {
			$t = $n * $T || $t;
			$t = int $t + ($t > int $t);
			$f = $ft ** (1 / $t) * $fs ** (1 / $s);
			$y = $yt ** (1 / $t) * $ys ** (1 / $s);
			$x = $xt ** (1 / $t) * $xs ** (1 / $s);

			if ($F) {
				for (1..$t) {
					$r = $y[$p * $P % $P] * $Y;
					$R = atan2 $X, 1;
					push @i, $r * cos $R, $r * sin $R;
					$p += $F;
					$F *= $f;
					$Y *= $y;
					$X *= $x;
					}
				}
			else {
				push @i, 0, 0 for 1..$t;
				}
			}
		}
	for $i (keys %i) {
		for (@a < @i ? (0..$#a) : (0..$#i)) {
			$i[$i + $_ + 1] *= $a[$_];
			$i[$i - $_] *= $a[$_];
			}
		}

	@i;
	}

sub mix {
	my @m = ();
	for $m (@_) {
		$m[$_] += $$m[$_ % @$m] for 0..$#{$_[0]};
		}
	@m;
	}

sub rec {
	open D, ">", shift;

	my @i = @_;
	my $e = 0;
	$e < abs and $e = abs for @i;
	my $q = $e ? 0x7FFF / $e : 0;
	$_ = int $_ * $q for @i;
	print "1\n";
	print D
		"FORM", pack("l>", 2 * @i + 46), "AIFF",
			"COMM", pack("(lsLs)>", 18, 2, @i / 2, 16), ext($s),
			"SSND", pack("(lLLs*)>", 2 * @i + 8, 0, 0, @i);
	close D;
	}

sub imp {
	open D, shift;
	read D, my $d, -s D;
	close D;

	$d =~ /SSND(.{4}).{8}/g;
	map $_ / 0x8000, unpack "s>*", substr $d, pos $d, unpack("l>", $1) - 8;
	}

sub ext {
	my $d = shift;
	my $b = "";
	my $e = int log(abs $d) / log 2;
	my $m = abs($d) / 2 ** $e;

	$e += 0x3FFF;

	for (1..15) {
		$b .= $e % 2;
		$e = int $e / 2;
		}

	$b .= $d < 0 ? 1 : 0;
	$b = reverse $b;

	for (1..64) {
		$b .= int $m;
		$m -= int $m;
		$m *= 2;
		}

	return pack "B80", $b;
	}

sub in {
	my $a = shift;
	my $b = shift;
	my $n = int shift || $s;
	my $d = ($b - $a) / $n;
	map $a + $d * $_, 1..$n;
	}

sub tan {
	eval { sin($_[0]) / cos($_[0]) };
	}

sub cot {
	eval { cos($_[0]) / sin($_[0]) };
	}

sub arctan {
	atan2($_[0], 1);
	}

sub arccot {
	$pi / 2 - atan2($_[0], 1);
	}

sub arcsin {
	sgn($_[0]) * atan2(abs($_[0]), sqrt(1 - $_[0] ** 2));
	}

sub arccos {
	$pi / 2 - sgn($_[0]) * atan2(abs($_[0]), sqrt(1 - $_[0] ** 2));
	}

sub sgn {
	$_[0] <=> 0;
	}
