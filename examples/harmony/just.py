#!/usr/bin/env python3

import numpy as np
import sys

letters = 'FCGDAEB'
signs = ['bb', 'b', '', '#', 'x']
notes = [letter + sign for sign in signs for letter in letters]
commas = ["''", "'", 'Pyth.', ',', ',,', ',,,']

equal = np.empty(5 * 7)
fifth = np.empty_like(equal)
table = np.empty((5 * 7, 6))
cents = np.empty_like(table)

def unfold(f):
    return f / 2 ** np.floor(np.log2(f))

def error(f1, f2):
    P8 = np.log2(f2 / f1) % 1

    if P8 > 0.5:
        P8 -= 1

    return 1200 * P8

sys.stdout.write('%3s %3s %5s' % ('n', 'X', 'equal'))

for comma in commas:
    sys.stdout.write(' %5s %5s' % (comma, 'error'))

sys.stdout.write('\n')

for i, n in enumerate(range(-15, 19)):
    equal[i] = unfold(2 ** (7 * n / 12))
    fifth[i] = unfold(3 ** n)

    sys.stdout.write('%3d %3s %5.3f'
        % (n, notes[i], equal[i]))

    for j, m in enumerate(range(-2, 4)):
        table[i, j] = unfold(fifth[i] * 5 ** m / 3 ** (4 * m))
        cents[i, j] = error(equal[i], table[i, j])

        sys.stdout.write(' %5.3f %5.1f'
            % (table[i, j], cents[i, j]))

    sys.stdout.write(' %s' % commas[np.argmin(abs(cents[i]))])

    sys.stdout.write('\n')
