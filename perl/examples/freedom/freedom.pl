#!/usr/bin/perl

BEGIN { chdir $0 =~ s|[^/]*$||r }

use Tonbandfetzen;

$T = $s / 4;
$ms = $s / 1000;

# bass drum

@p = map { sin($_)      } in 0, 2 * $pi;
@a = map { sin($_) ** 2 } in 0, $pi / 2, $ms;
@z = @a;

$bd = mel q(
	!25
	E3 \30 ,0 1 \0 ,25 2
	B3 \30 ,0 1 \0 ,25 1
	G3 \30 ,0 1 \0 ,25 2
	);

# hihat

@p = (1);
@a = map { sin($_) ** 2 } in 0, $pi / 2, 2.5 * $ms;
@z = @a;

$hh = mel '!3 ,25 1 !8 1 !13 1';
$_ *= (1 - 2 * rand) for @$hh;

# melody

@p = map { ($_ <=> 0) * sqrt abs($_) - $_ ** 2 } in -1, 1;
@a = map { sin($_) ** 2 } in 0, $pi / 2, 25 * $ms;
@z = @a;

$A_lo = mel '!20 [00 {00 E2 1 E2 1 E2 1 E2  1 E2 1 E2 1 E2 1 E2 1 E2 1 E2  1 E2  1 E2 1 E2 1 E2  1 E2 1 E2  1 G2 1 G2  1 G2 1 G2  1 G2 1 G2  1 G2 1 G2  1 A2 1 A2 1 A2  1 A2 1 A2 1 A2 1 A2  1 A2 1';
$A_mi = mel '!20 ]20 {40 E3 1 G3 1 D3 1 F#3 1 D4 1 C4 1 B3 1 A3 1 G3 1 C4  1 F#3 1 B3 1 E3 1 G3  1 D3 1 F#3 1 G3 1 F#3 1 B3 1 F#3 1 G3 1 F#3 1 D4 1 F#3 1 A3 1 G3 1 F#3 1 G3 1 A3 1 G3 1 F#3 1 B3 1';
$A_hi = mel '!20 [20 }40 E4 1 G4 1 E4 1 B3  1 E4 1 G4 1 E4 1 B3 1 E4 1 F#4 1 E4  1 B3 1 E4 1 F#4 1 E4 1 D4  1 B3 1 C4  1 B3 1 D4  1 B3 1 C4  1 B3 1 D4  1 B3 1 C4 1 B3  1 D4 1 B3 1 C4 1 B3  1 D4 1';

$B_lo  = mel '!20 [00 {00 C3 1 C3 1 C3 1 C3 1 C3 1 C3 1 D3 1 C3 1 E3 1 E3 1 E3 1 E3  1 E3 1 E3 1 A2 1 B2 1';
$B_mi  = mel '!20 ]20 {40 E3 1 G3 1 C4 1 D4 1 C4 1 A3 1 B3 1 G3 1 E3 1 B3 1 G3 1 F#3 1 E3 1 D3 1 E3 1 G3 1';
$B_hi1 = mel '!20 [20 }40 G4 1 E4 1 D4 1 E4 1 G4 1 E4 1 D4 1 E4 1';
$B_hi2 = mel '!20 [20 }40 A4 1 E4 1 D4 1 E4 1 A4 1 E4 1 D4 1 E4 1';

$B_hi = stick $B_hi1, $B_hi1, $B_hi2, $B_hi1;

$A_lo = stack $A_lo, [map $$A_lo[$_ + $s / 40 * sin(2 * $pi * $_ / $#$A_lo) ** 2], 0..$#$A_lo];
$B_lo = stack $B_lo, [map $$B_lo[$_ + $s / 80 * sin(2 * $pi * $_ / $#$B_lo) ** 2], 0..$#$B_lo];

# chords

@p = map { $_ ** 3 - $_ } in 0, 1;
@a = map { sin($_) ** 2 } in 0, $pi / 2, 25 * $ms;
@z = @a;

$G  = stack mel('D5 (20 <25 8'), mel('G5 (00 <25 8'), mel('B5 )20 <25 8');
$Am = stack mel('C5 (20 <25 8'), mel('E5 (00 <25 8'), mel('A5 )20 <25 8');
$Em = stack mel('G5 (20 <25 8'), mel('B5 (00 <25 8'), mel('E6 )20 <25 8');

# main parts

$A1 = stack $hh, stick $G, $G, $G, $Am;
$A2 = stack $A1, $bd;
$A3 = stack $A2, $A_lo;
$A4 = stack $A3, $A_mi, $A_hi;
$B1 = stack $hh, $Em, $bd, $B_lo, $B_mi, $B_hi;
$C1 = stack $bd, stick $Em, $Em, $Em, $Am;

# vocals

for (1..4) {
	my $V = cut fix take("freedom$_.aif"), 75;
	push @V, fit $V, round(@$V / $C, $_ == 4 ? 2 * @$C1 / $C : @$A3 / $C);
	}

# putting all together

$intro = stick $A1, $A2;
$verse1 = stack $A3, $V[0];
$chorus = stick $A4, $A4, $B1;
$brigde = $C1;
$verse2 = stack $A3, $V[1];
$verse3 = stack $A3, $V[2];
$outro  = stack $C1, $V[3];

$song = stick $intro, $verse1, $chorus, $brigde, $verse2, $chorus, $brigde, $verse3, $chorus, $outro;

make "freedom.aif", fix $song;

sub round {
	my $n = shift || 0;
	my $f = shift || 1;
	$f * int $n / $f + ($n <=> 0) / 2;
	}

# 08/08/2013
