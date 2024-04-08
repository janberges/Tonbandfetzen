#!/usr/bin/env python3

import re
import subprocess
import sys

job = sys.argv[1]

def speak(speed):
    subprocess.call(['espeak-ng', '-vde+f5', '-s%g' % speed,
        '-f%s.txt' % job, '-w%s.wav' % job])

    time = float(re.search(r'Duration:\s*(\S+)\s*s\b',
        str(subprocess.check_output(['inspect', '%s.wav' % job]))).group(1))

    print('%s takes %g seconds at %g words per minute' % (job, time, speed))

    return time

with open('%s.fit' % job) as lines:
    for line in lines:
        for match in re.findall(r'@1:([\d.]+)', line):
            t = float(match)

s0 = 120.0
t0 = speak(s0)
speak(s0 * t0 / t)
