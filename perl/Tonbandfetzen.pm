#!/usr/bin/perl

# Tonbandfetzen
# Jan Berges
# August 15, 2013

$| = 1;

$s   = 44100;    # second/sampleRate
$c   = 2;        # numChannels
$b   = 16;       # sampleSize
$T   = $s / 2;   # beat duration
$A4  = 440 / $s; # standard pitch

$pi  = 4 * atan2 1, 1;

@p = samp("cub");              # single wave
@a = samp("cubfade", $s / 20); # attack envelope
@z = @a;                       # reversed release envelope

sub mel {
	print "mel";
	die "\nerror: \@p is empty\n" unless @p;

	my @i = (); # two-channel audio data
	my %i = (); # attack indices

	my $t = $T; # duration
	my $p = 0;  # phase

	my $F  = $A4; # frequency
	my $Y  = 1;   # amplitude
	my $X  = 1;   # r/l amplitudes quotient

	my $f0 = $A4; # reference

	my $F0 = $A4;
	my $Y0 = 1; # attack values
	my $X0 = 1;

	my $ft = 1;
	my $yt = 1; # duration-related growth factors
	my $xt = 1;

	my $fs = 1;
	my $ys = 1; # second-related growth factors
	my $xs = 1;

	for (map /\S+/g, @_) { # separate command sequence
		print " $_";

		if (/~|[A-G]|-|\+|\?|!|\[|\]|=|\*/) { # attack
			$F = $F0;
			$Y = $Y0;
			$X = $X0;
			$i{$#i} = 1;
			}

		/([\d.]+):?([\d.]*)/;
		my $n = $1 / ($2 or 1); # calculate fractional number

		if (/~/) { # frequency declared explicitly
			$F = $F0 = $f0 = $n / $s;
			}
		elsif (/([A-G])/) { # frequency declared via scientific pitch notation
			$F = $F0 = $f0 = $A4 * 2 ** ($n - 4 + ({
				C => -9,
				D => -7,
				E => -5,
				F => -4,
				G => -2,
				A => 0,
				B => 2,
				}->{$1} + /\x23/) / 12);
			}
		# signed commands with numerical values in half tones or dB:
		elsif ( /(-|\+)/) { $F  = $F0 = $f0 * 2 ** ({"-"  => -1, "+" => 1}->{$1} * $n / 12) }
		elsif (/(\\|\/)/) { $ft =             2 ** ({"\\" => -1, "/" => 1}->{$1} * $n / 12) }
		elsif ( /(_|\^)/) { $fs =             2 ** ({"_"  => -1, "^" => 1}->{$1} * $n / 12) }
		elsif (/(\?|!)/ ) { $Y  = $Y0 =      10 ** ({"?"  => -1, "!" => 1}->{$1} * $n / 10) }
		elsif ( /(>|<)/ ) { $yt =            10 ** ({">"  => -1, "<" => 1}->{$1} * $n / 10) }
		elsif ( /(,|;)/ ) { $ys =            10 ** ({","  => -1, ";" => 1}->{$1} * $n / 10) }
		elsif (/(\[|\])/) { $X  = $X0 =      10 ** ({"["  => -1, "]" => 1}->{$1} * $n / 10) }
		elsif (/(\(|\))/) { $xt =            10 ** ({"("  => -1, ")" => 1}->{$1} * $n / 10) }
		elsif (/(\{|\})/) { $xs =            10 ** ({"{"  => -1, "}" => 1}->{$1} * $n / 10) }
		else { # create audio data
			$t = round($n * $T) || $t;

			print " (warning: non-integer duration rounded)" if $t != $n * $T;

			if (/\*/) { # pause
				push @i, 0, 0 for 1..$t;
				}
			else {
				my $f = $ft ** (1 / $t) * $fs ** (1 / $s);
				my $y = $yt ** (1 / $t) * $ys ** (1 / $s); # sampleFrame related growth factors
				my $x = $xt ** (1 / $t) * $xs ** (1 / $s);

				for (1..$t) {
					my $r = $Y * $p[$p * @p % @p]; # displacement
					my $R = atan2 $X, 1;           # angle

					push @i, $r * cos $R, $r * sin $R; # l/r displacements

					$p += $F;
					$F *= $f;
					$Y *= $y;
					$X *= $x;
					}
				}
			}
		}

	print "\n";
	die "error: no data created\n" if !@i;

	for my $j (keys %i) { # add attack and release envelopes
		for my $c (0, 1) {
			$i[($j + 2 * $_ + $c) % @i] *= $a[$_] for 0..$#a;
			$i[($j - 2 * $_ + $c) % @i] *= $z[$_] for 0..$#z;
			}
		}

	$c = 2;

	\@i;
	}

sub stack {
	print "stack\n";

	my $t = 0;
	$t < $#$_ and $t = $#$_ for @_; # find maximum duration

	my @i = ();

	for my $i (grep @$_, @_) {
		$i[$_] += $$i[$_ % @$i] for 0..$t; # superpose and repeat
		}

	\@i;
	}

sub stick {
	print "stick\n";

	my @i = ();
	push @i, @$_ for @_; # juxtapose

	\@i;
	}

sub in {
	my $n = shift; # name
	my $i;         # data

	print "in '$n'\n";

	open D, $n or die "error: unable to read file\n";

	seek D, 12, 0; # skip header

	while (! eof D) { # chunkwise
		read D, my $C, 4; # ckID
		read D, my $S, 4; # ckSize

		$S = unpack "l>", $S;

		if ($C eq "COMM") { # common chunk
			read D, $c, 2;  # numChannels
			seek D, 4, 1;   # numSampleFrames
			read D, $b, 2;  # sampleSize
			read D, $s, 10; # sampleRate

			$c = unpack "s>", $c;
			$b = unpack "s>", $b;
			$s = rex($s);
			}
		elsif ($C eq "SSND") {  # sound data chunk
			seek D, 8, 1;       # skip offset and blockSize
			read D, $i, $S - 8; # soundData
			}
		else {
			seek D, $S, 1; # skip chunk
			}
		}
	close D;

	[unpack $b > 16 ? "l>*" : $b > 8 ? "s>*" : "c*", $i]
	}

sub out {
	my $n = shift; # name
	my $i = shift; # data

	print "out '$n'\n";

	my $j = pack $b > 16 ? "l>*" : $b > 8 ? "s>*" : "c*", @$i;

	open D, ">", $n or die "error: unable to write file";
	print D
		"FORM", pack("l>", 76 + length $j), "AIFF",
		"COMM", pack("(lsLs)>", 18, $c, @$i / $c, $b), ext($s),
		"SSND", pack("(lLL)>", 8 + length $j, 0, 0), $j;
	close D;
	}

sub fit {
	print "fit\n";

	my $i = shift; # data
	my $t = shift; # desired duration

	$t -= sgn($t) or return [map $$i[0], 1..$c];

	my @i = ();

	for ($t > 0 ? 0..$t : $t..0) {
		my $j = floor($_ / $t * $#$i, $c);
		push @i, $$i[$j++] for 1..$c;
		}

	\@i;
	}

sub fix {
	print "fix\n";

	my $i = shift;                                                    # data
	my $x = shift || ($b > 16 ? 0x7fffffff : $b > 8 ? 0x7fff : 0x7f); # desired displacement

	my $e = 0;
	$e < abs and $e = abs for @$i; # find old maximum displacement

	die "error: all data zero\n" if !$e;

	my $q = $x / $e;
	$_ *= $q for @$i;

	$i;
	}

sub cut {
	print "cut\n";

	my $i = shift; # data

	my $j = 0;
	my $k = $#$i;

	$j++ while !$$i[$j] and $j < $k;
	$k-- while !$$i[$k] and $j < $k;

	[@$i[floor($j, $c)..ceil($k, $c)]];
	}

sub samp {
	my %t = (
		har     => sub { sin(2 * $pi * shift) },
		har3    => sub { sin(2 * $pi * shift) ** 3 },
		lin     => sub { 2 / $pi * arcsin(sin(2 * $pi * shift)) },
		cub     => sub { my $x = shift; (12 * $x ** 3 - 18 * $x ** 2 + 6 * $x) * sqrt(3) },
		circ    => sub { my $x = 1 - 2 * shift; sgn($x) * sqrt(abs($x) - $x ** 2) },
		noise   => sub { 1 - rand(2) },
		linfade => sub { shift },
		sinfade => sub { sin($pi / 2 * shift) },
		harfade => sub { sin($pi / 2 * shift) ** 2 },
		cubfade => sub { my $x = shift; 3 * $x ** 2 - 2 * $x ** 3 },
		);

	my $n =       shift  || return keys %t;
	my $t = round(shift) || $s;

	my @i = map $t{$n}($_ / $t), 0..$t;

	wantarray ? @i : \@i;
	}

sub range {
	my $a = shift;
	my $b = shift;
	my $n = int shift || $s;
	my $d = ($b - $a) / $n;
	map $a + $d * $_, 1..$n;
	}

sub ext {
	my $d = shift;
	my $b = "";
	my $e = int log(abs $d or 1) / log 2;
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

sub rex {
	my @b = split //, unpack "B80", shift;

	my $m = 0;
	my $e = -0x3FFF;

	$m += 2 ** $_ * pop @b for -63..0;
	$e += 2 ** $_ * pop @b for 0..14;

	$m * 2 ** $e * (pop @b ? -1 : 1);
	}

sub ceil {
	my $n = shift || 0;
	my $f = shift || 1;
	$f * int $n / $f + ($n / $f > int $n / $f);
	}

sub round {
	my $n = shift || 0;
	my $f = shift || 1;
	$f * int $n / $f + sgn($n) / 2;
	}

sub floor {
	my $n = shift || 0;
	my $f = shift || 1;
	$f * int $n / $f - ($n / $f < int $n / $f);
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

sub hea {
	$_[0] >= 0;
	}

1;
