#!/usr/bin/perl

BEGIN { chdir $0 =~ s|[^/]*$||r }

use Tonbandfetzen;

$0 =~ /(.+)\//;
chdir $1;

$T = $s / 4;
$ms = $s / 1000;

sub arcsin {
	($_[0] <=> 0) * atan2(abs($_[0]), sqrt(1 - $_[0] ** 2));
	}

@p = map { 2 / $pi * arcsin(sin($_)) } in 0, 2 * $pi;

$fis = mel '!15 ,5 F#2 2 = 2 = 3 = 2 = 1';
$b   = mel '!15 ,5 B1  2 = 2 = 3 = 2 = 1';
$cis = mel '!15 ,5 C#2 2 = 2 = 3 = 2 = 1';

@p = map { sin($_) ** 3 } in 0, 2 * $pi;

$afis = stack mel('F#3 40'), mel('[10 A3 40'), mel(']10 C#4 40');
$ab   = stack mel('B3  20'), mel('[10 D4 20'), mel(']10 F#4 20');
$acis = stack mel('C#4 22'), mel('[10 E4 22'), mel(']10 G#4 22');

$break = mel("*3");
$chs = stack mel("A4 5 G#4 5"), mel("C#5 5 B4 5");

@p = map { $_ ** 3 - $_ } in 0, 1;
@a = map { sin($_) ** 2 } in 0, $pi / 2, 10 * $ms;
@z = @a;

$verse  = mel '!10 ,10 [20 G#5 1 )20 /1 0.2 1.8 ]20 C#6 2 [20 G#5 1 )20 /1 0.2 1.8 ]20 B5 2';
$bridge = mel '!10 ,10 [20 G#5 1 )20 /1 0.2 1.8 ]20 E6  2 [20 G#5 1 )20 /1 0.2 1.8 ]   B5 1 )20 /2 0.2 0.8 [20 G#5 1 /20 /1 0.2 1.8 ]20 F#6 2 [20 G#5 1 ]20 E6 2 ] G#5 0.4 )20 /1 0.2 0.4 (20 \1 0.2 0.8 [20 E5 2';

$i   = stack $fis, $verse;
$ii  = stack $b,   $verse;
$iii = stack $cis, $bridge;

$mel = stick $i, $i, $i, $i, $ii, $ii, $iii;
$chords = stick $afis, $ab, $acis;
$chords = stack $chords, stick $break, $chs, $chs, $chs, $chs, $chs, $chs, $chs, $chs;

make "water.aif", fix stack $mel, $chords;
