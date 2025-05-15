![Tonbandfetzen logo](logo/Tonbandfetzen.svg)

# Tonbandfetzen

Tonbandfetzen is a collection of command-line tools that allow you to compose
music based on audio fragments generated from plain text input. It contains:

* `tz mel` - generate sound fragments from plain text
* `tz guitar` - read guitar tablature
* `tz stick` - concatenate audio data
* `tz stack` - superimpose audio data
* `tz stretch` - rescale audio data
* `tz repeat` - loop audio data
* `tz trim` - strip leading and trailing silence
* `tz mono` - unzip audio channels
* `tz harmonics` - show spectra of wave samples
* `tz inspect` - show information on audio file
* `tz riff2aiff` - convert .wav(e) into .aif(f) files
* `tz aiff2riff` - convert .aif(f) into .wav(e) files
* `tz tag` - add ID3v2 metadata to .wav(e) files
* `zplay` - generate and play sound fragments

## Usage

Different task are spread across separate executables, which communicate via
files in the Waveform Audio File Format (.wav). Hence, the composition process
can be controlled using build-automation software, allowing for partial updates
and parallel execution (`make -j`).

At the heart of this toolbox, the program `tz mel` converts text into audio:

    echo "T pyth M A2'8 W ,5 A2' A3' E4' A4' C#v5' E5' Gz5' A5'" | tz mel | aplay

The preprocessor `tz guitar` converts tablature into suitable `tz mel` input:

    echo "X synth |3
    E4|--------------------0~~~~~~~~~~~|~~~~0~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    B3|----0~~~~~~~~~~~~~3~~~0~~~~~0~~~|~~3~~~0~~~~~0~~~~~~~~~~~~~~~~~~~|
    G3|--2~~~0~~~~~0~~~~~~~~~~~~~2~~~0~|~~~~~~~~~~2~~~0~~~~~0~~~~~~~~~~~|
    D3|----------2~~~0~~~~~~~~~~~~~~~~~|~~~~~~~~~~~~~~~~~~2~~~0~~~~~0~~~|
    A2|3~~~~~~~~~~~~~~~----------------|--------------------------2~~~0~|
    E2|----------------3~~~~~~~~~~~~~~~|0~~~~~~~~~~~~~~~2~~~~~~~3~~~~~~~|
    " | tz guitar | tz mel | tz repeat | aplay

You can try both [here](https://janberges.de/tz.cgi).

On macOS with `zsh`, you can use `afplay =(...)` instead of `... | aplay`.

## Installation

To build all tools, you only need a recent Fortran compiler:

    cd /path/to/Tonbandfetzen
    make FC=gfortran FFLAGS=-O3

To make the tools and documentation accessible, consider adding the following
lines to your .bashrc:

    REPO=/path/to/Tonbandfetzen
    export PATH=$REPO/bin:$PATH
    export PERL5LIB=$REPO/perl/lib:$PERL5LIB
    export MANPATH=$REPO/doc:$MANPATH

You might also want to link the Vim syntax and Bash completion files to the
appropriate locations:

    ln -s /path/to/Tonbandfetzen/config/syntax/tz.vim ~/.vim/syntax/
    ln -s /path/to/Tonbandfetzen/config/completions/tz \
        ~/.local/share/bash-completion/completions/

To have Vim detect the corresponding file types, add this line to your .vimrc:

    autocmd BufRead,BufNewFile *.mel,*.gtr setlocal filetype=tz

## Documentation

Please have a look at the examples, some of which require Python or eSpeak NG:

    cd examples/freedom
    make -j 2
    aplay freedom.wav

Each program has its own manual page:

    man tz mel

You can also listen to the examples and browse the manual pages
[here](https://io.janberges.de/Tonbandfetzen).

## Hear also

The name Tonbandfetzen originates from the song Explosion by Tocotronic from
their 2007 album Kapitulation.

This project utilizes the [Functional Just System](https://misotanni.github.io)
for just intonation invented by misotanni.

Have a look at [ForSynth](https://vmagnin.github.io/forsynth) by Vincent Magnin.
