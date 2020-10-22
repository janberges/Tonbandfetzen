#!/usr/bin/perl

# Tonbandfetzen
# Jan Berges
# October 20, 2013

package Tonbandfetzen;

use strict;
use warnings;

BEGIN {
	require Exporter;
	our @ISA       = qw(Exporter);
	our @EXPORT    = qw($b $c $s $T $A4 $pi @p @a @z take make mel stick stack fix fit cut in);
	our @EXPORT_OK = qw(noise ext rex);
	}

our $b  = 16;       # sampleSize
our $c  = 2;        # numChannels
our $s  = 44100;    # second/sampleRate
our $T  = $s / 2;   # beat duration
our $A4 = 440 / $s; # standard pitch

our $pi = 4 * atan2 1, 1;

our @p = map { $_ ** 3 - $_ }              in(-1, 1);          # single wave
our @a = map { 3 * $_ ** 2 - 2 * $_ ** 3 } in( 0, 1, $s / 20); # attack envelope
our @z = @a;                                                   # reversed release envelope

# export:

sub take {
	status();

	my $n = shift;
	my $i;

	open D, $n or warn "unable to read file\n" and return [];

	seek D, 12, 0; # skip header

	while (! eof D) {
		read D, my $C, 4; # ckID
		read D, my $S, 4; # ckSize

		$S = unpack "l>", $S;

		if ($C eq "COMM") {
			read D, $c, 2;  # numChannels
			seek D, 4, 1;   # skip numSampleFrames
			read D, $b, 2;  # sampleSize
			read D, $s, 10; # sampleRate

			$b = unpack "s>", $b;
			$c = unpack "s>", $c;
			$s = rex($s);
			}
		elsif ($C eq "SSND") {
			seek D, 8, 1;       # skip offset, blockSize
			read D, $i, $S - 8; # soundData
			}
		else {
			seek D, $S, 1; # skip chunk
			}
		}
	close D;

	[unpack $b > 16 ? "l>*" : $b > 8 ? "s>*" : "c*", $i];
	}

sub make {
	status();

	my $n = shift;
	my $i = shift;
	my $j = pack $b > 16 ? "l>*" : $b > 8 ? "s>*" : "c*", @$i;

	open D, ">", $n or warn "unable to write file\n" and return;
	print D
		"FORM", pack("l>", 76 + length $j), "AIFF",
		"COMM", pack("(lsLs)>", 18, $c, @$i / $c, $b), ext($s),
		"SSND", pack("(lLL)>", 8 + length $j, 0, 0), $j;
	close D;
	}

sub mel {
	status();

	my $Fr = @p * $A4; # reference frequency

	my $F  = $Fr; # frequency
	my $Y  = 1;   # amplitude
	my $R  = 1;   # r/l amplitudes quotient

	my $F0 = $F;
	my $Y0 = $Y; # initial values
	my $R0 = $R;

	my $ft = 1;
	my $yt = 1; # growth factors per $t
	my $rt = 1;

	my $fT = 1;
	my $yT = 1; # growth factors per $s
	my $rT = 1;

	my %i = (); # attack indices

	my $dt = 0; # duration error
	my $p  = 0; # phase

	my @y  = (); # displacements
	my @r  = (); # r/l amplitudes quotients

	for (map /\S+/g, @_) {
		if (/~|[A-G]|-|\+|\?|!|\[|\]|=|\*/) {
			$F = $F0;
			$Y = $Y0;
			$R = $R0;

			$i{$#y} = 1;
			}

		/([\d.]+):?([\d.]*)/;
		my $n = ($1 or 0) / ($2 or 1);

		if (/~/) {
			$F = $F0 = $Fr = @p * $n / $s || 1;
			}
		elsif (/([A-G])/) {
			$F = $F0 = $Fr = @p * $A4 * 2 ** ($n - 4 + ({
				C => -9,
				D => -7,
				E => -5,
				F => -4,
				G => -2,
				A => 0,
				B => 2,
				}->{$1} + /#/) / 12);
			}
		elsif ( /(-|\+)/) { $F  = $F0 = $Fr * 2 ** ({"-"  => -1, "+" => 1}->{$1} * $n / 12) }
		elsif (/(\\|\/)/) { $ft =             2 ** ({"\\" => -1, "/" => 1}->{$1} * $n / 12) }
		elsif ( /(_|\^)/) { $fT =             2 ** ({"_"  => -1, "^" => 1}->{$1} * $n / 12) }
		elsif (/(\?|!)/ ) { $Y  = $Y0 =      10 ** ({"?"  => -1, "!" => 1}->{$1} * $n / 10) }
		elsif ( /(>|<)/ ) { $yt =            10 ** ({">"  => -1, "<" => 1}->{$1} * $n / 10) }
		elsif ( /(,|;)/ ) { $yT =            10 ** ({","  => -1, ";" => 1}->{$1} * $n / 10) }
		elsif (/(\[|\])/) { $R  = $R0 =      10 ** ({"["  => -1, "]" => 1}->{$1} * $n / 10) }
		elsif (/(\(|\))/) { $rt =            10 ** ({"("  => -1, ")" => 1}->{$1} * $n / 10) }
		elsif (/(\{|\})/) { $rT =            10 ** ({"{"  => -1, "}" => 1}->{$1} * $n / 10) }
		elsif (! /=/) {
			my $d = $n * $T + $dt; # exact duration
			my $t = int $d + .5;   # rounded duration
			  $dt = $d - $t;

			$t and $T or next;

			if (/\*/) {
				for (1..$t) {
					push @y, 0;
					push @r, $R;
					}
				}
			else {
				my $f = $ft ** (1 / $t) * $fT ** (1 / $T);
				my $y = $yt ** (1 / $t) * $yT ** (1 / $T); # growth factors per sampleFrame
				my $r = $rt ** (1 / $t) * $rT ** (1 / $T);

				$ft = $yt = $rt = 1;

				for (1..$t) {
					push @y, $Y * $p[$p % @p];
					push @r, $R;

					$p += $F;
					$F *= $f;
					$Y *= $y;
					$R *= $r;
					}
				}
			}
		}

	$dt and warn "output ", abs $dt / $s, " seconds too ", $dt > 0 ? "short" : "long", "\n";

	@y or return \@y;

	for my $i (keys %i) {
		$y[($i + $_) % @y] *= $a[$_] for 0..$#a;
		$y[($i - $_) % @y] *= $z[$_] for 0..$#z;
		}

	$c == 1 and return \@y;

	my @i = ();

	my @e = map $_ / ($c - 1), 0..$c - 1;

	for my $i (0..$#y) {
		my @c = map $r[$i] ** $_, @e;

		my $n = 0;
		   $n += $_ ** 2 for @c;

		$n and $y[$i] /= sqrt $n;

		push @i, map $y[$i] * $_, @c;
		}

	\@i;
	}

sub stick {
	status();

	my @i = ();
	push @i, @$_ for @_;

	\@i;
	}

sub stack {
	status();

	my $t = 0;
	$t < $#$_ and $t = $#$_ for @_;

	my @i = ();
	for my $i (grep @$_, @_) {
		$i[$_] += $$i[$_ % @$i] for 0..$t;
		}

	\@i;
	}

sub fix {
	status();

	my $i = shift;
	my $x = shift || ($b > 16 ? 0x7fffffff : $b > 8 ? 0x7fff : 0x7f);

	my $e = 0;
	$e < abs and $e = abs for @$i;

	$e or return $i;

	my $q = $x / $e;
	$_ *= $q for @$i;

	$i;
	}

sub fit {
	status();

	my  $i = shift;
	my  $d = shift || 0;
	my  $t = int $d + ($d <=> 0) / 2;
	my $dt = abs($d) - abs($t);

	$dt and warn "output ", abs $dt / $s, " seconds too ", $dt > 1 ? "short" : "long", "\n";

	$t or return [];

	my @i = ();
	my $q = int(@$i / $c - 1) / ($t - ($t <=> 0) or 1);

	for ($t > 0 ? 0..$t - 1 : $t + 1..0) {
		my $j = $c * int $q * $_ + .5;
		push @i, $$i[$j++] for 1..$c;
		}

	\@i;
	}

sub cut {
	status();

	my $i = shift;
	my $j = 0;
	my $k = $#$i;

	$j++ while ! $$i[$j] and $j < $k;
	$k-- while ! $$i[$k] and $j < $k;

	[@$i[$c * int($j / $c)..$c * int($k / $c + 1) - 1]];
	}

sub in {
	my $a = shift || 0;
	my $b = shift || 0;
	my $n = shift || $s || return();
	my $d = ($b - $a) / $n;
	map $a + $d * $_, 1..$n;
	}

# export ok:

sub noise {
	status();

	my $f = shift || 220;
	my $i = shift || 12;
	my $n = shift || 100;
	my $d = shift || $s;
	my $t = int $d + .5;

	$d != $t and warn "output ", abs($d - $t) / $s, " seconds too ", $d > $t ? "short" : "long", "\n";

	my @i = ();

	for (1..$n) {
		my $p = rand 2 * $pi;
		my $w = 2 * $pi * $f * 2 ** ($i * (.5 - rand) / 12) / $s;
		$i[$_] +=           $_  * sin($w *       $_  + $p)
		       +  ($t - 1 - $_) * sin($w * ($t + $_) + $p) for 0..$t - 1;
		}

	fix(\@i, 1);
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

# no export:

sub status {
	my $l = 0;
	my @f = ();

	unshift @f, (caller ++$l)[3] =~ /(\w+)$/ while caller $l + 1;

	print "line ", join (" > ", (caller $l)[2], @f, @_), "\n";

	$c and $s and @p or warn "global value missing\n" and exit;
	}

1;
