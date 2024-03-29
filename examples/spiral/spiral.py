#!/usr/bin/env python3

letters = 'FCGDAEB'
signs = ['#b'[int(n < 0)] * abs(n) for n in range(-8, 9)]
notes = [letter + sign for sign in signs for letter in letters]
notes = notes[::-1] + notes

tracks = ['[10', '[6', '[2', ']2', ']6', ']10']
sounds = [(2, 'cubic'), (3, 'major')]

print('Tpyth')
print('|%d' % len(tracks))

print('M')
for octave, wave in sounds:
    print('~%s' % wave)
    for n, track in enumerate(tracks):
        print('W "%d:%d %s' % (n, len(tracks), track))
        for note in notes[n::len(tracks)]:
            print("%s%d <20''>20'`" % (note, octave))
