#!/usr/bin/env python3

import numpy as np
import storylines
import sys

black = np.array([0x00, 0x00, 0x00])
white = np.array([0xff, 0xff, 0xff])
brown = np.array([0xbf, 0x80, 0x40])
green = np.array([0xbf, 0xff, 0x00])
olive = np.array([0xaf, 0xdf, 0x00])

N = 13

shades = (
    np.linspace(white, black, N),
    np.linspace(white, brown, N),
    np.linspace(white, green, N),
    np.linspace(white, olive, N),
    np.linspace(brown, black, N),
    np.linspace(brown, white / 4, N))

image = np.array(storylines.load(sys.argv[1]))[:, :, :3]

for y in range(image.shape[0]):
    for x in range(image.shape[1]):
        for shade in shades:
            n = shade[-1] - shade[0]
            n /= np.linalg.norm(n)

            d = image[y, x] - shade[0]

            if np.linalg.norm(d - np.dot(d, n) * n) < 1:
                image[y, x] = min(shade,
                    key=lambda c: np.linalg.norm(c - image[y, x]))
                break
        else:
            print('Original color (%d, %d, %d) is kept.' % tuple(image[y, x]))

storylines.save(sys.argv[2], image)
