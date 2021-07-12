#!/usr/bin/env python3

from itertools import permutations

notes = '[6A4', '[3G4', '[0E4', ']3D4', ']6C4'

print('~major')
print('Tpyth')
print('|0.2')
print(',2.0')

for permutation in permutations(notes):
    for note in permutation:
        print("%s'5`4" % note)
