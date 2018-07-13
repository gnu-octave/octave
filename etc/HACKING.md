Hacking Octave  {#Hacking}
==============

This file attempts to describe the rules to use when hacking the
Octave sources cloned from the Savannah source code
[repository](https://hg.savannah.gnu.org/hgweb/octave/).
**DO NOT** put this file into the distribution.  These requirements
do not apply when building from a distribution tarball.

Quick start
-----------

1. Install all the required dependencies.  Precisely how to do that
   depends on what type of system you are using.  There are more
   details below.

2. Clone the Octave sources:

       hg clone https://www.octave.org/hg/octave

3. Change to the top-level directory of the Octave source tree and
   run the `bootstrap` script:

       cd octave
       ./bootstrap

4. Create a build directory, `cd` to it, then run `configure` and
   `make`, or `make -jX` (to run `X` simultaneous jobs):

       mkdir .build
       cd .build
       ../configure
       make -j2

Requirements
------------

We've opted to keep only the highest-level sources in the repository.
This eases our maintenance burden, (fewer merges, etc.), but imposes
more requirements on anyone wishing to build from the just-cloned
sources.  For example, you have to use the latest stable versions of
the maintainer tools we depend upon.  These include:

- [Autoconf](https://www.gnu.org/software/autoconf/)
- [Automake](https://www.gnu.org/software/automake/)
- [Bison](https://www.gnu.org/software/bison/)
- [Flex](https://www.gnu.org/software/flex/)
- [Gnulib](https://www.gnu.org/software/gnulib/)
- [GNU Make](https://www.gnu.org/software/make/)
- [gperf](https://www.gnu.org/software/gperf/)
- [Gzip](https://www.gnu.org/software/gzip/)
- [Libtool](https://www.gnu.org/software/libtool/)
- [Mercurial](https://www.mercurial-scm.org/)
- [Perl](https://www.cpan.org/)
- [Rsync](https://samba.anu.edu.au/rsync/)
- [Tar](https://www.gnu.org/software/tar/)

In addition to these maintainer tools, Octave makes use of many
external libraries and packages.  See `doc/interpreter/install.txi`
for the complete list of required and optional dependencies.

Only building the initial full source tree will be a bit painful.
Later, after synchronizing from the repository, a plain `make` should
be sufficient.

First clone
-----------

If you are reading these notes, you may have already managed to clone
this package from the repository.  For the record, you will find all
the relevant information on downloading sources at:

  <https://www.octave.org/download.html>

After cloning Octave, you will need to run the `bootstrap` script:

    ./bootstrap

This script will examine the source tree and generate some `Makefile`
fragments, then run autotools scripts to generate `Makefile.in` files
from `Makefile.am` files and create the `configure` script.  The
`bootstrap` script comes from gnulib, but is kept in the Octave
source archive.  It should be updated from the gnulib sources as
necessary.

If you have a copy of gnulib in some directory apart from the Octave
source tree, then pass the name of the directory containing
`gnulib-tool` to the `bootstrap` script using the option:

    --gnulib-srcdir=DIRNAME

If you have downloaded gnulib directly, `DIRNAME` will be the
directory where gnulib was unpacked.  If you have installed gnulib
using a package manager, `DIRNAME` is likely to be `/usr/bin` or
`/usr/local/bin` (where the gnulib-tool script resides).

By using an external gnulib directory, you can share a single gnulib
source tree among several projects.  Since 2011, the gnulib sources
are a Mercurial subrepository of the Octave repository, so they will
be automatically updated to the corresponding Mercurial revision if
you update the working directory to a past revision not too far in
the past.

Additional options besides `--gnulib-srcdir` can be passed to
`bootstrap` and they will be forwarded without modification to the
gnulib `bootstrap` script.

Once the `bootstrap` script completes successfully, you may configure
and build Octave.  We recommend that you build Octave in a separate
directory tree from the sources.  For example, if you have just
finished running the `bootstrap` script in the top-level source
directory, run the following commands to create a build tree,
configure, and build Octave:

    mkdir .build
    cd .build
    ../configure
    make -j2
    make check

The `-j2` after the first `make` call means, that two jobs run
simultaneously.  This number may be increased depending on your
system.  At this point, there should be no difference between your
working tree and the currently visited hg revision:

    hg diff

should output no difference.

Coding style
------------

The most important advice is to follow any conventions you detect
in the file being edited.  In addition, Octave maintainers have
written a lot on the subject.  See
<https://wiki.octave.org/Contribution_guidelines>.

Bugs and patches
----------------

See the file BUGS in this directory for more guidance on reporting bugs
and preparing patches.

Source code directory layout
----------------------------

An overview of the directory layout of Octave's source files:

- `build-aux`
     scripts which support the build process.

- `doc`
     Texinfo and Doxygen documentation for Octave.

- `etc`
     miscellaneous files, such as this `HACKING` howto.

- `examples`
     some example files (`mkoctfile` and mex samples, old class
     programming interface).

- `gnulib`
     gnulib subrepo.  This is a clone of the gnulib source tree
     maintained by the Octave project.  The default branch is
     identical to the upstream gnulib sources.  There is also an
     `octave-stable` branch that may contain changes as needed for
     the `stable` branch in the Octave archive.  We usually don't
     want to update gnulib sources when going from one stable point
     release to the next, but we occasionally need to include small
     updates.

- `libgnu`
     gnulib sources that we use.  The files here are copied here from
     the gnulib directory by the `bootstrap` script.

- `liboctave`
    C++ interfaces to the numerical libraries, Fortran numerical
    libraries, various OS facilities, and utility functions.
  - `array`
       the base `Array`, `NDArray`, `Matrix`, and `Sparse` classes.
  - `external`
       various numerical libraries (mostly Fortran).
    - `amos`        bessel functions
    - `blas-xtra`   wrappers for blas functions used in Octave
    - `daspk`       large scale differential algebraic equation solver
    - `dasrt`       differential algebraic equation solver with root finding
    - `dassl`       differential-algebraic system solver
    - `fftpack`     subroutines for fast fourier transforms
    - `lapack-xtra` wrappers for lapack functions used in Octave
    - `odepack`     ordinary differential equation solver
    - `quadpack`    subroutines for numerical integration
    - `ranlib`      random number generators
    - `slatec-err`  slatec error handling library
    - `slatec-fn`   various special function subroutines
  - `numeric`
       C++ numerical algorithms and interfaces to the Fortran
       algorithms.
  - `operators`
       code for operators that act on base classes (such as `Array`).
  - `system`
       OS-related functions.
  - `util`
       utility and miscellaneous functions.
  - `wrappers`
       C++ wrappers for gnulib functions written in C.

- `libinterp`
     the interpreter itself plus lots of infrastructure around it.
     Octave's extensive `octave_value` class hierarchy for
     polymorphically handling all Octave types is defined here.
     The built-in functions are also defined here.
  - `octave-value`
       the `octave_value` class hierarchy.  These are the container
       classes that hold various Octave data types like struct
       numerical arrays, structure arrays, and cell arrays.
  - `parse-tree`
       Classes that define the parse tree for the interpreter.
  - `corefcn`
       statically linked `DEFUN` functions (callable from the
       scripting langauge) as well as internal C++ functions used by
       the interpreter.
  - `dldfcn`
       dynamically linked `DEFUN` functions (callable from the
       scripting language).  If you see `help foo` telling you that
       `foo` is defined in `foo.oct`, then `foo.cc` will be found
       here and contain the source code.
  - `operators`
       definitions and template instantiations for operators for all
       possible Octave type combinations.
  - `template-inst`
       some C++ template instantiations.


- `libgui`
     the graphical user interface of GNU Octave.
  - `graphics`
       Qt graphics toolkit (OpenGL plotting).
  - `kb-layouts`
       various files need by the qterminal widget that has been
       derived from Konsole.
  - `languages`
       translation files and list of translators.
  - `qterminal`
       Qt terminal widget.
  - `src`
       source files for the GUI
    - `icons`
         icon files that will be compiled into the executable via a
         resource file.
    - `m-editor`
         source files for the m-file editor.

- `m4` m4 scripts used by configure during the build process.

- `scripts` functions written in the Octave language.
  - `@ftp`             ftp object class
  - `+containers`      container classes (Map)
  - `audio`            play and record sound files (system dependent)
  - `deprecated`       older deprecated functions
  - `elfun`            elementary mathematical functions
  - `general`          utility functions
  - `geometry`         geometry algorithms
  - `gui`              User-Interface (UI) functions
  - `help`             help subsystem functions
  - `image`            image processing
  - `io`               input/output functions
  - `java`             java/Octave interface
  - `linear-algebra`   linear algebra
  - `miscellaneous`    stuff that doesn't fit anywhere else
  - `ode`              Ordinary Differential Equations
  - `optimization`     zero finders and minimizers
  - `path`             functions for path manipulation
  - `pkg`              the package manager
  - `plot`             plotting functions
  - `polynomial`       polynomial manipulation
  - `prefs`            user-defined preferences
  - `profiler`         code profiler for performance
  - `set`              set manipulation
  - `signal`           signal processing
  - `sparse`           sparse matrix support
  - `specfun`          special mathematical functions
  - `special-matrix`   functions for generating special types of matrices
  - `startup`          initialization functions
  - `statistics`       statistical functions, distributions, and tests
  - `strings`          character string manipulation
  - `testfun`          unit testing
  - `time`             time and date functions

- `src`
     code for the actual executables that are created.  This includes
     `octave`, `octave-cli`, `octave-gui`, as well as `mkoctfile`.

- `test`
     tests for the interpreter.
  - `*.tst`
       fixed tests for the interpreter.
  - `fntests.m`
       script to run function tests embedded in C++ and .m files.

Release Numbering
-----------------

Since version 5, Octave uses the following rules for release numbering:

  Version Dev Phase       When

  5.0.0   (experimental)  active development of Octave 5 on default branch
  5.0.1   (pre-release)   stabilization period of Octave 5 on stable branch
  6.0.0   (experimental)  active development of Octave 6 on default branch
  5.1.0   (release)       first release of Octave 5 from stable branch
  5.1.1   (pre-release)   bug fixing on stable branch after 5.1.0 release
  5.2.0   (release)       second release of Octave 5 from stable branch
  5.2.1   (pre-release)   bug fixing on stable branch after 5.2.0 release
  ...

To summarize, the first release of Octave 5 will be Octave 5.1.0 while
development snapshots will be Octave 5.0.0 and snapshots from the
release branch Octave 5.n.1.

With this numbering scheme:

  * Any version X.0.0 means "this is an experimental development
    version".

  * Any version X.Y.1 means, "this is a pre-release version meant
    for bug fixing and testing".

  * Any version X.Y.0 with Y != 0 means "this is a released version".

Shared Library Versioning
-------------------------

Version numbers for the liboctave, liboctinterp, and liboctgui shared
libraries are set in the module.mk files in the top-level directory for
each library using the variables

  %canon_reldir%_%canon_reldir%_current
  %canon_reldir%_%canon_reldir%_revision
  %canon_reldir%_%canon_reldir%_age

The rules for updating these version numbers are:

  * Start with version information of ‘0:0:0’ for each libtool library.

  * Update the version information only immediately before a public
    release of your software.  More frequent updates are unnecessary,
    and only guarantee that the current interface number gets larger
    faster.

  * If the library source code has changed at all since the last update,
    then increment revision (‘c:r:a’ becomes ‘c:r+1:a’).

  * If any interfaces have been added, removed, or changed since the
    last update, increment current, and set revision to 0.

  * If any interfaces have been added since the last public release,
    then increment age.

  * If any interfaces have been removed or changed since the last public
    release, then set age to 0.

Never try to set the interface numbers so that they correspond to the
Octave version number.  This is an abuse that only fosters
misunderstanding of the purpose of library versions.

The following explanation may help to understand the above rules a bit
better: consider that there are three possible kinds of reactions from
users of your library to changes in a shared library:

  * Programs using the previous version may use the new version as
    drop-in replacement, and programs using the new version can also
    work with the previous one.  In other words, no recompiling nor
    relinking is needed.  In this case, bump revision only, don’t touch
    current nor age.

  * Programs using the previous version may use the new version as
    drop-in replacement, but programs using the new version may use APIs
    not present in the previous one.  In other words, a program linking
    against the new version may fail with unresolved symbols if linking
    against the old version at runtime: set revision to 0, bump current
    and age.

  * Programs may need to be changed, recompiled, and relinked in order
    to use the new version.  Bump current, set revision and age to 0.

These guidelines also appear in the automake manual.


################################################################################

Copyright (C) 2009-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.


This file was adapted for Octave from the HACKING file that is part of
GNU Bison, which contained the following Copyright notice:

  Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009
  Free Software Foundation, Inc.

  This file is part of GNU Bison.

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
