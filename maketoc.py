#!/usr/bin/env python

from re import match, IGNORECASE as i

text = ''

title = 'Table of contents'
where = 0
table = []

counter = 5 * [0]
columns = 0
section = None

with open('README.txt') as readme:
    for number, line in enumerate(readme, 1):
        line = line.rstrip()

        if len(line) > columns:
            columns = len(line)

        headline = match(r'([\d.]+)  (.+)', line, i)

        if headline:
            label, section = headline.groups()

            level = label.count('.')
            counter[level] += 1

            for i in range(level + 1, len(counter)):
                counter[i] = 0

            label = '.'.join(map(str, counter[:level + 1]))

            text += label + '  ' + section + '\n'

            table.append([level, label, section, number])

        elif section != title:
            text += line + '\n'

        if title in line:
            section = title
            where = len(text)

dots = columns // 2 * '. ' + columns % 2 * '.'

with open('README.txt', 'w') as readme:
    readme.write(text[:where])
    readme.write('\n')

    for level, label, section, number in table:
        left = 4 * (level + 1) * ' ' + label + '  ' + section + ' '
        right = ' {}'.format(number)

        readme.write(left)
        readme.write(dots[len(left):-len(right)])
        readme.write(right)
        readme.write('\n')

    readme.write(2 * '\n')
    readme.write(text[where:])
