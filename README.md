# GNU Octave
A high-level language for numerical computations

## Overview

GNU Octave is a high-level interpreted language, primarily intended
for numerical computations.  It provides capabilities for the
numerical solution of linear and nonlinear problems, and for
performing other numerical experiments.  It also provides extensive
graphics capabilities for data visualization and manipulation.  GNU
Octave is normally used through its interactive interface (CLI and
GUI), but it can also be used to write non-interactive programs.
The GNU Octave language is quite similar to Matlab so that most
programs are easily portable.

## Using Octave
[Download Octave](https://www.octave.org/download)
1. Launch: Once installed, launch Octave by running the executable file or typing octave in the terminal/command prompt.
2. Command-line: In the Octave command-line, type your commands and they will be executed immediately, similar to MATLAB.
3. Scripts: Write scripts for more complex tasks or to automate operations. Scripts are executed sequentially.
4. Syntax: Syntax is similar to that of MATLAB. Arithmetic operations, variable definition, matrix creation, and function calls are all done with similar syntax to MATLAB.
5. Numerical Computations: Perform operations on scalars, vectors, matrices, and higher-dimensional arrays.
6. Data Analysis: Octave provides functions and tools for data analysis, including statistical analysis, curve fitting, interpolation, and optimization. Data can be imported from files and visualized using plotting functions.
7. Plotting Functions: Data plots possible include scatter plots, histograms, surfaces, and more. Plots can be customizes with labels, titles, legends, and annotations.
8. Help: There are built-in help commands, online manuals, and community forums to learn about Octave's features, syntax, and best practices.

### License
GNU Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the 
[`GNU General Public License`](https://github.com/gnu-octave/octave/blob/default/COPYING)
along with Octave.  If not, see
[www.gnu.org/licenses](https://www.gnu.org/licenses).

### Availability

The latest released version of Octave is always available from
[here](https://ftp.gnu.org/gnu/octave)
and many mirror sites around the world.  You may also find links to binary distributions
[here](https://www.octave.org/download.html).
The current development sources may be found under the Source Code tab on [Savannah](https://savannah.gnu.org/projects/octave/).

## Developer Installation

To compile Octave, you will need a recent version of:

- [GNU Make](https://www.gnu.org/software/make/)
- [GNU G++](https://gcc.gnu.org/) or another C++11 compiler
- [GNU Fortran](https://gcc.gnu.org/fortran/), another Fortran 77
  compiler, or [Fortran 77 to C (f2c)](http://www.netlib.org/f2c/)

Octave's Makefiles use features of GNU Make that are not present in
other versions of make.  If you use `f2c`, you will need a script
like `fort77` that works like a normal Fortran compiler by combining
`f2c` with your C compiler in a single script.

Octave requires approximately `475 MB` of disk storage to unpack and
compile from source (significantly more, 3.8 GB, if you compile with
debugging symbols).  Once installed, Octave requires approximately
`75 MB` of disk space (again, considerably more, 415 MB, if you don't
build shared libraries or the binaries and libraries include
debugging symbols).

See [Octave installation guide](https://github.com/gnu-octave/octave/blob/default/doc/interpreter/install.txi)
for more detailed installation instructions.

## Bugs and Patches

[Bugs](doc/interpreter/bugs.txi) explains the recommended
procedure for reporting bugs on the [bug tracker](https://bugs.octave.org)
or contributing patches. Online documentation is also available
[here](https://www.gnu.org/software/octave/bugs.html).

Documentation
-------------

* [Octave's manual](https://www.octave.org/doc/interpreter/) is a
  comprehensive user guide covering introductive and more advanced
  topics.
* [Octave's wiki](https://wiki.octave.org) is a user community page,
  covering various topics and answering
  [FAQ](https://wiki.octave.org/FAQ).
* [Octave's Doxygen](https://www.octave.org/doxygen/) documentation
  explains the C++ class libraries.

Partially, the up-to-dateness of the documentation is lagging a bit
behind the development of the software.  If you notice omissions or
inconsistencies, please report them at our bug tracker.  Specific
suggestions for ways to improve Octave and its documentation are
always welcome.  Reports with patches are even more welcome.

Additional Information
----------------------

Up to date information about Octave is available at
<https://www.octave.org>, or ask for help via email
<help@octave.org>.

Copyright (C) 1996-2024 The Octave Project Developers

See the file 
[`COPYRIGHT.md`](https://github.com/gnu-octave/octave/blob/default/COPYRIGHT.md)
or [`octave.org/copyright`](https://octave.org/copyright/).
