#!/usr/bin/env python

from re import match, IGNORECASE as i

def reformat(text, indent=0, indents=[], columns=80, justify=True):
    lines = []
    words = text.split()

    indents = iter(indents)

    while words:
        indent = next(indents, indent)

        column = indent - 1
        space = columns - indent

        n = 0
        for word in words:
            column += len(word) + 1

            if column > columns:
                break

            space -= len(word)
            n += 1

        n = max(1, n)

        if justify and 1 < n < len(words):
            spacing = space / (n - 1.0)

            spaces = [int(round(i * spacing)) for i in range(n)]

            for i in range(n - 1, 0, -1):
                spaces[i] -= spaces[i - 1]

            spaces[0] = indent

            lines.append(''.join(' ' * spaces[i] + words[i] for i in range(n)))
        else:
            lines.append(' ' * indent + ' '.join(words[:n]))

        del words[:n]

    return lines

class Lines(list):
    def __init__(self, **kwargs):
        list.__init__(self)
        self.kwargs = kwargs
        self.clean()

    def clean(self):
        self.buffer = ''
        self.indents = []

    def append(self, line):
        if self.buffer:
            self.flush()

        list.append(self, line)

    def suspend(self, line):
        indent = match(r' *', line).end()

        if indent % 2:
            self.append(line)
        else:
            self.buffer += ' ' + line
            self.indents.append(indent)

    def flush(self):
        self.extend(reformat(self.buffer, indents=self.indents, **self.kwargs))
        self.clean()

columns = 80
justify = True
counter = 5 * [0]
section = None

title = 'Table of contents'
where = 0
table = []
lines = Lines(columns=columns, justify=justify)

with open('README.txt') as readme:
    for line in readme:
        line = line.rstrip()

        headline = match(r'([\d.]+)  (.+)', line, i)

        if headline:
            label, section = headline.groups()

            level = label.count('.')
            counter[level] += 1

            for i in range(level + 1, len(counter)):
                counter[i] = 0

            label = '.'.join(map(str, counter[:level + 1]))

            lines.append(label + '  ' + section)

            if section == title:
                where = len(lines)

            table.append([level, label, section, len(lines)])

        elif section == title:
            pass

        elif not line:
            lines.append('')

        elif line == title:
            lines.append(title)

            section = title
            where = len(lines)

        else:
            lines.suspend(line)

lines.flush()

dots = columns // 2 * '. ' + columns % 2 * '.'

with open('README.txt', 'w') as readme:
    for i in range(where):
        readme.write(lines[i])
        readme.write('\n')

    readme.write('\n')

    for level, label, section, number in table:
        if number > where:
            number += len(table) + 3

        left = 4 * (level + 1) * ' ' + label + '  ' + section + ' '
        right = ' {}'.format(number)

        readme.write(left)
        readme.write(dots[len(left):-len(right)])
        readme.write(right)
        readme.write('\n')

    readme.write(2 * '\n')

    for i in range(where, len(lines)):
        readme.write(lines[i])
        readme.write('\n')
