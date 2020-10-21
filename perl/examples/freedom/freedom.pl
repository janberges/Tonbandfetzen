#!/usr/bin/perl

$0 =~ /(.+)\//;
chdir $1;

require Tonbandfetzen;

$T = $s / 4;

$a = 2 * $ms;
@y = map sin $_, in(0, 2 * $pi, $s / 2);
@bd = mel('E3 !25 \30 ~ ,100 \0 ~2 B3 ,0 \30 ~1 ,100 \0 ~ G3 ,0 \30 ~ ,100 \0 ~2');

$a = 5 * $ms;
@y = (1);
@hh = mel('*1 !3 ,100 ~ !8 ~ !13 ~');
$_ *= (1 - 2 * rand) for @hh;

$a = 50 * $ms;
@y = map sgn($_) * sqrt(abs($_) - $_ ** 2), in(-1, 1, $s / 2);

@a1 = mel('!20 [00 (00 E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ E2 ~ G2 ~ G2 ~ G2 ~ G2 ~ G2 ~ G2 ~ G2 ~ G2 ~ A2 ~ A2 ~ A2 ~ A2 ~ A2 ~ A2 ~ A2 ~ A2 ~');
@a2 = mel('!20 ]20 (40 E3 ~ G3 ~ D3 ~ F#3 ~ D4 ~ C4 ~ B3 ~ A3 ~ G3 ~ C4 ~ F#3 ~ B3 ~ E3 ~ G3 ~ D3 ~ F#3 ~ G3 ~ F#3 ~ B3 ~ F#3 ~ G3 ~ F#3 ~ D4 ~ F#3 ~ A3 ~ G3 ~ F#3 ~ G3 ~ A3 ~ G3 ~ F#3 ~ B3 ~');
@a3 = mel('!20 [20 )40 E4 ~ G4 ~ E4 ~ B3 ~ E4 ~ G4 ~ E4 ~ B3 ~ E4 ~ F#4 ~ E4 ~ B3 ~ E4 ~ F#4 ~ E4 ~ D4 ~ B3 ~ C4 ~ B3 ~ D4 ~ B3 ~ C4 ~ B3 ~ D4 ~ B3 ~ C4 ~ B3 ~ D4 ~ B3 ~ C4 ~ B3 ~ D4 ~');

@b1 = mel('!20 [00 (00 C3 ~ C3 ~ C3 ~ C3 ~ C3 ~ C3 ~ D3 ~ C3 ~ E3 ~ E3 ~ E3 ~ E3 ~ E3 ~ E3 ~ A2 ~ B2 ~');
@b2 = mel('!20 ]20 (40 E3 ~ G3 ~ C4 ~ D4 ~ C4 ~ A3 ~ B3 ~ G3 ~ E3 ~ B3 ~ G3 ~ F#3 ~ E3 ~ D3 ~ E3 ~ G3 ~');
@b3 = mel('!20 [20 )40 G4 ~ E4 ~ D4 ~ E4 ~ G4 ~ E4 ~ D4 ~ E4 ~');
@b4 = mel('!20 [20 )40 A4 ~ E4 ~ D4 ~ E4 ~ A4 ~ E4 ~ D4 ~ E4 ~');

$a = 50 * $ms;
@y = map $_ ** 3 - $_, in(0, 1, $s / 2);
@G_D = mix([mel('D5 (20 <25 ~8')], [mel('G5 (00 <25 ~8')], [mel('B5 )20 <25 ~8')]);
@A_C = mix([mel('C5 (20 <25 ~8')], [mel('E5 (00 <25 ~8')], [mel('A5 )20 <25 ~8')]);
@E_G = mix([mel('G5 (20 <25 ~8')], [mel('B5 (00 <25 ~8')], [mel('E6 )20 <25 ~8')]);

@a1m = map $a1[$_ + $s / 40 * sin(2 * $pi * $_ / $#a1) ** 2], 0..$#a1;
@b1m = map $b1[$_ + $s / 80 * sin(2 * $pi * $_ / $#b1) ** 2], 0..$#b1;

for (1..4) {
	my @V = imp("freedom$_.aif");
	$_ *= 75 for @V;
	my $t = 2 * 4 * 8 * $T * int(.5 + @V / (2 * 4 * 8 * $T)) - 1;
	shift @V while !@V[0];
	pop @V while !@V[-1];
	push @v, [map $V[$#V * $_ / $t], 0..$t];
	}

@A1 = mix([@G_D, @G_D, @G_D, @A_C]);
@A2 = mix(\@A1, \@bd);
@A3 = mix(\@A2, \@a1, \@a1m);
@A4 = mix(\@A3, \@a2, \@a3);
@B = mix([@b3, @b3, @b4, @b3], \@b2, \@b1, \@b1m, \@E_G, \@bd);
@C = mix([@E_G, @E_G, @E_G, @A_C], \@bd);

rec("freedom.aif", mix([@A1, @A2, mix($v[0], \@A3), @A4, @A4, @B], \@hh), @C, mix([mix($v[1], \@A3), @A4, @A4, @B], \@hh), @C, mix([mix($v[2], \@A3), @A4, @A4, @B], \@hh), mix($v[3], \@C));
