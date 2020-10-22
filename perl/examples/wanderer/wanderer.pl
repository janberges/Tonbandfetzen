#!/usr/bin/perl

BEGIN { chdir $0 =~ s|[^/]*$||r }

use Tonbandfetzen;

$T = $s / 2;
$ms = $s / 1000;

# bass drum

@p = map { ($_ <=> 0) * sqrt(abs($_) - $_ ** 2) } in -1, 1;
@a = map { sin($_) ** 2 } in 0, $pi / 2, $ms / 3;
@z = @a;

$bd = mel q(
	!1 F3 \30 ,0 1:2 \0 ,50 1:2
	);

$bd2 = mel q(
	!1 A3 \30 ,0 1:6 \0 ,50 1:6
	);

$bd3 = mel q(
	!1 A3 \30 ,0 1:5 \0 ,50 1:5
	);

# hihat

@p = (1);
@a = ();
@z = @a;

$hh = mel '*3 ?5 [21 }7 ,4 3 ;5 3';
$_ *= (1 - 2 * rand) for @$hh;

# melody

@p = map { ($_ <=> 0) * sqrt(abs($_) - $_ ** 2) } in -1, 1;
@a = map { sin($_) ** 2 } in 0, $pi / 2, 25 * $ms;
@z = @a;

# chords

@p = map { ($_ <=> 0) * sqrt(abs($_) - $_ ** 2) } in -1, 1;
@a = map { 3 * $_ ** 2 - 2 * $_ ** 3 } in 0, 1, 50 * $ms;
@z = map { 3 * $_ ** 2 - 2 * $_ ** 3 } in 0, 1, 100 * $ms;

#$F    = stack mel('F3  3'), mel('A3  3'), mel('C3  3');
#$Em   = stack mel('E3  3'), mel('G3  3'), mel('B3  3');
#$Am   = stack mel('A3  3'), mel('C3  3'), mel('E3  3');
#$Bmb5 = stack mel('B3  3'), mel('D3  3'), mel('F3  3');
#$Dm   = stack mel('D3  3'), mel('F3  3'), mel('A3  3');
#$G    = stack mel('G3  3'), mel('B3  3'), mel('D3  3');
#$AIS  = stack mel('A#3 3'), mel('D3  3'), mel('F3  3');
#$Emb5 = stack mel('E3  3'), mel('G3  3'), mel('A#3 3');
#$Gm   = stack mel('G3  3'), mel('A#3 3'), mel('D3  3');

#$bassA = mel 'F2 3 E2 3 A2 3 B2 3';
#$bassB = mel 'D2 3 G2 3 A2 3 E2 3';
#$bassC = mel 'D2 3 A#2 3 E2 3 D2 3 G2 3 A2 3';

#$Dm   = stack mel('D3  3'), mel('F3  3'), mel('A3  3');
#$AIS  = stack mel('A#3 3'), mel('D3  3'), mel('F3  3');
#$Emb5 = stack mel('E3  3'), mel('G3  3'), mel('A#3 3');
#$Dm   = stack mel('D3  3'), mel('F3  3'), mel('A3  3');
#$Gm   = stack mel('G3  3'), mel('A#3 3'), mel('D3  3');
#$Am   = stack mel('A3  3'), mel('C3  3'), mel('E3  3');

$tief   = mel 'D3 1.5 /8 1.5 /0 1.5 \6 1.5 \0 1.5 \2 1.5 \0 1.5 /5 1.5 /0 1.5 /2 1.5 /0 3';
$mittel = mel 'F3 1.5 \3 1.5 \0 1.5 /5 1.5 /0 1.5 \2 1.5 \0 1.5 /5 1.5 /0 1.5 /2 1.5 /0 3';
$hoch   = mel 'A3 1.5 \4 1.5 \0 1.5 /5 1.5 /0 1.5 \1 1.5 \0 1.5 \7 1.5 \0 1.5 /2 1.5 /0 3';

@p = map { sin($_) ** 3 } in 0, 2 * $pi;

#$melA = mel '!3 ,50 A3 1.5 A3 0.5 A3 1 B3 1.5 B3 0.5 B3 1';
#$melC = mel '?10 F6 1.5 E6 0.5 D6 1 G6 3 G6 1.5 F6 0.5 E6 1 F6 1.5 E6 0.5 D6 1 A#5 3 C6 1.5 A#5 0.5 A5 1';

$melC = mel 'F4 1.3 \1 0.2 \0 0.3 \2 0.2 \0 0.8 /5 0.2 /0 4.3 \2 0.2 \0 0.3 \1 0.2 \0 0.8 /1 0.2 /0 1.3 \1 0.2 \0 0.3 \2 0.2 \0 0.8 \4 0.2 \0 2.8 /2 0.2 /0 1.3 \2 0.2 \0 0.3 \1 0.2 \0 1';

#$a = stack $bassA, stick $F, $Em, $Am, $Bmb5;
#$b = stack $bassB, stick $Dm, $G, $Am, $Em;
#$c = stack $bassC, $melC, stick $Dm, $AIS, $Emb5, $Dm, $Gm, $Am;
#$verse = stick $a, $b, $b, $a, $c, $c;

#$verse = stack $verse;

$mus = stack $hh, $tief, $mittel, $hoch, $melC;
$drum = stack $hh, $bd, $bd2;
$both = stack $hh, $bd, $bd2, $tief, $mittel, $hoch;

make "wanderer.aif", fix stick $mus, $drum, $both, $both, $mus;

# 08/08/2013
