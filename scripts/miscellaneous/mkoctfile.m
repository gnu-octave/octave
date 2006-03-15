## Copyright (C) 2006 Keith Goodman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} mkoctfile ()
## The @code{mkoctfile} shell script compiles source code written in C,
## C++, or Fortran.  Depending on the command-line options used with
## @code{mkoctfile}, the compiled code can be called within Octave or
## can be used as a stand-alone application.
##
## Run @code{mkoctfile} from the shell prompt, not from the Octave prompt.
##
## See the man or info page of @code{mkoctfile} for a full description.
## @end deftypefn

function mkoctfile ()

  error ("run mkoctfile from the shell prompt, not from the Octave prompt");

endfunction
