~ ~ ~ ~ ~ ~ ~ ~ ~
~ Tonbandfetzen ~
~ ~ ~ ~ ~ ~ ~ ~ ~


Table of contents

    1  Outline  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 12
    2  Installation . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 19


1  Outline

'Tonbandfetzen'  will be  a collection  of programs  which allow  you to  create
audio data  from plain text input,  to modify and  arrange it and to  handle the
Audio Interchange File Format.


2  Installation

Python 2.7 and the GNU Fortran compiler are required.

(1) Go to the directory which holds the source files:

        $ cd path/to/tonbandfetzen

(2) Extract their dependencies and create a makefile:

        $ python makemake.py

(3) Compile and link everything:

        $ make

(4) Optionally,  for better  accessibility you  may append  the location  of the
    programs to the environmental variable  $PATH. A permanent solution would be
    to  supplement  your  ~/.profile,  ~/.bash_profile  or  ~/.bashrc  with  the
    following command:

        export PATH=$PATH:path/to/tonbandfetzen
