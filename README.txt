T o n b a n d f e t z e n

'Tonbandfetzen' is a collection of programs which allow you to create audio data
from plain  text input, modify and  arrange it and handle  the Audio Interchange
File Format.


I n s t a l l a t i o n

Given a recent version of the GNU Fortran compiler, simply run 'make'.


B a s i c   i d e a   o f   u s a g e

Different task are spread across  separate executables. This yields a collection
of independent, specific tools which can  be applied easily via the command line
without having to pass lots of additional arguments.

Most of them  act on files, especially Audio Interchange  Files (.aif). Input is
read from existing files, output is written to new ones, both depending on given
filenames.

This favors a usage in combination  with build automation software. Any project,
such as  a musical  composition, is  then made up  from several  fragments, e.g.
audio  samples and  textual musical  notes, the  dependencies between  which are
declared in a  makefile. This has the  advantage that it is possible  to work on
any intermediate step  without having to needlessly walk through  all the others
again and again.


P r o v i d e d   p r o g r a m s

    $ harmonics <sample name>

Fourier-analyzes the provided samples.

    $ mel <text file>

interprets the content of <text file> as a melody. An audio file with the same
stem as <text file> is created.

    $ reverse <audio file>

reverses the  sound data  given in <audio  file> and saves  it under  a filename
where '_reversed' is inserted in between stem and suffix.


C o m m a n d   s e q u e n c e s

The input shall be a sequence of 'special', 'literal' and 'numeral' strings, the
characters of which belong to disjoint subsets. Whereas the first type initiates
specific procedures, the latter serve as actual arguments.

Depending on context, each item of the  sequence is expected to be of a definite
type: A procedure expects a specific list of literal and numeral arguments. Once
this is satisfied, a new procedure may be started.

The input  is processed linearly.  Unexpected characters are either  skipped or,
preferably,  terminate  a matching  string,  becoming  the new  starting  point.
Separators, such as white space, may thus be omitted if no ambiguity arises.

  numeral:  digits and colon [0-9:]  (integer or rational number)
  lexical:  lowercase letters [a-z]  (arbitrary name)
  special:  other ASCII printables   (initiation of procedure)

 /-------|-------------------|----------------------------|-----------|--------\
 |  KEY  |  QUANTITY         |  DEFAULT LEXICAL           |  DEFAULT  |  UNIT  |
 |-------|-------------------|----------------------------|-----------|--------|
 |   ~   |  single wave      |  circular                  |        1  | s      |
 |  /~   |  touch envelope   |  circular                  |   1:1000  | s      |
 |   ~\  |  decay envelope   |----------------------------|   1:1000  | s      |
 |  /~\  |  touch & decay    |  FORMULA                   |   1:1000  | s      |
 |-------|-------------------|----------------------------|-----------|--------|
 |   $   |  sample rate (*)  |  s                         |    44100  |  Hz    |
 |   *   |  channels (*)     |  -                         |        2  |  1     |
 |-------|-------------------|----------------------------|-----------|--------|
 |   .   |  discontinuity    |  * holds globally and is ignored after first '  |
 |-------|-------------------|----------------------------|-----------|--------|
 |   |   |  beat duration    |  b                         |      1:2  |  s     |
 |   @   |  concert pitch    |  A4                        |      440  |  Hz    |
 |   #   |  tones/octave     |  n                         |       12  |  1     |
 |-------|-------------------|----------------------------|-----------|--------|
 |   =   |  frequency (**)   |  f0 = f                    |       A4  |  Hz    |
 |   &   |  amplitude        |  a0 = a = sqrt(R^2 + L^2)  |        1  |  1     |
 |   %   |  -"- ratio        |  r0 = r = R:L              |        1  |  1     |
 |-------|-------------------|----------------------------|-----------|--------|
 |  ** may also be declared via A, A#, B... G# with respect to to A4  |  P8    |
 |-------|-------------------|----------------------------------------|--------|
 |   '   |  play             |  d                                     |  b     |
 |   "   |  pause            |  -             /-----------|-----------|  b     |
 |   `   |  rewind           |  -             |   PER d   |   PER b   |  b     |
 |-------|-------------------|----------------|-----------|-----------|--------|
 |  - +  |  pitch interval   |  lb(f / f0)    |    \ /    |    _ ^    |  P8/n  |
 |  ? !  |  amplitude level  |  lg(a / a0)    |    > <    |    , ;    |  dB    |
 |  [ ]  |  -"- difference   |  lg(r / r0)    |    ( )    |    { }    |  dB    |
 |-------||--------------|---|----------|-----|-----------|-----------|--------|
 |  TYPE  |  NAME        |  INTERVAL x  |  FUNCTION f(x)                       |
 |--------|--------------|--------------|--------------------------------------|
 |  wave  |  harmonic    |  (0, 2 pi]   |  sin(x)                              |
 |        |  power       |  (0, 2 pi]   |  sin(x)^3                            |
 |        |  major       |  (0, 2 pi]   |  sin(x)^5                            |
 |        |  linear      |  (0, 2 pi]   |  2 / pi arcsin(sin(x))               |
 |        |  quadratic   |  (-2, 2]     |  sgn(x)     (2 abs(x) - x^2)         |
 |        |  circular    |  (-2, 2]     |  sgn(x) sqrt(2 abs(x) - x^2)         |
 |        |  cubic       |  (-1, 1]     |  1.5 sqrt(3) (x^3 - x)               |
 |        |  random      |  -           |  white noise                         |
 |--------|--------------|--------------|--------------------------------------|
 |  fade  |  harmonic    |  (0, pi/2)   |  sin(x)                              |
 |        |  smooth      |  (0, pi/2)   |  sin(x)^2                            |
 |        |  power       |  (0, pi/2)   |  sin(x)^3                            |
 |        |  major       |  (0, pi/2)   |  sin(x)^5                            |
 |        |  linear      |  (0, 1)      |                                      |
 |        |  quadratic   |  (-1, 0)     |       1 - x^2                        |
 |        |  circular    |  (-1, 0)     |  sqrt(1 - x^2)                       |
 |        |  cubic       |  ( 0, 1)     |  3 x^2 - 2 x^2                       |
 \--------|--------------|--------------|--------------------------------------/


C o n t a c t

Any response may be addressed to Jan Berges <berges@gmx.net>.
