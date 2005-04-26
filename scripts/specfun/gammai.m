## Copyright (C) 1998 John W. Eaton
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
## @deftypefn {Function File} {} gammai (@var{a}, @var{x})
## This function is provided for compatibility with older versions of
## Octave.  New programs should use @code{gammainc} instead.
##
## @code{gammai (@var{a}, @var{x})} is the same as @code{gammainc
## (@var{x}, @var{a})}.
## @end deftypefn

## Author: jwe
## Created: 30 Jan 1998

function retval = gammai (a, x)

  if (nargin == 2)
    retval = gammainc (x, a);
  else
    usage ("gammai (a, x)");
  endif

endfunction
