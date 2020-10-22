#!/usr/bin/perl

BEGIN { chdir $0 =~ s|[^/]*$||r }

use Tonbandfetzen;

$0 =~ /(.+)\//;
chdir $1;

$T = $s / 4;
$ms = $s / 1000;

@p = samp "lin";

$Fis = mel '!15 ,20 F#2 2 +0 2 +0 3 +0 2 +0 1';
$B   = mel '!15 ,20 B1  2 +0 2 +0 3 +0 2 +0 1';
$Cis = mel '!15 ,20 C#2 2 +0 2 +0 3 +0 2 +0 1';

@p = samp "har3";

$AFis = stack mel('F#3 40'), mel('[10 A3 40'), mel(']10 C#4 40');
$AB   = stack mel('B3  20'), mel('[10 D4 20'), mel(']10 F#4 20');
$ACis = stack mel('C#4 22'), mel('[10 E4 22'), mel(']10 G#4 22');

$Break = mel("*3");
$Chs = stack mel("A4 5 G#4 5"), mel("C#5 5 B4 5");

@p = samp "cub";
@a = samp "harfade", 10 * $ms;
@z = @a;

$verse  = mel '!10 ,40 [20 G#5 1 )20 /1 0.2 /0 1.8 ]20 C#6 2 [20 G#5 1 )20 /1 0.2 /0 1.8 ]20 B5 2';
$bridge = mel '!10 ,40 [20 G#5 1 )20 /1 0.2 /0 1.8 ]20 E6  2 [20 G#5 1 )20 /1 0.2 /0 1.8 ]   B5 1 )20 /2 0.2 /0 0.8 [20 G#5 1 )20 /1 0.2 /0 1.8 ]20 F#6 2 [20 G#5 1 ]20 E6 2 ] G#5 0.4 )20 /1 0.2 /0 0.4 (20 \1 0.2 \0 0.8 [20 E5 2';

$i   = stack $Fis, $verse;
$ii  = stack $B,   $verse;
$iii = stack $Cis, $bridge;

$mel = stick $i, $i, $i, $i, $ii, $ii, $iii;
$chords = stick $AFis, $AB, $ACis;
$chords = stack $chords, stick $Break, $Chs, $Chs, $Chs, $Chs, $Chs, $Chs, $Chs, $Chs;

out "water.aif", fix stack $mel, $chords;
