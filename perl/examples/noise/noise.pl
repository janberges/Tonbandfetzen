#!/usr/bin/perl

BEGIN { chdir $0 =~ s|[^/]*$||r }

use Tonbandfetzen qw(:DEFAULT noise);

@p = @{noise 220, 12, 200, 5 * $s};
@a = map { sin($_) ** 2 } in 0, $pi / 2, $s / 4;
@z = @a;

$noise = mel '~ 5';

make "noise.aif", fix $noise;
