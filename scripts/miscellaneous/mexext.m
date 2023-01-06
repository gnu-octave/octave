########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{ext} =} mexext ()
## Return the filename extension used for MEX files.
##
## Programming Note: Octave uses the extension @file{mex} for all MEX files
## regardless of the operating system (Linux, Windows, Apple) or the bit-width
## (32-bit or 64-bit) of the hardware.
## @seealso{mex}
## @end deftypefn

function ext = mexext ()
  ext = "mex";
endfunction


%!assert (mexext (), "mex")
