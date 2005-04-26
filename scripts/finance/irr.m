## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} irr (@var{p}, @var{i})
## Return the internal rate of return of a series of payments @var{p}
## from an initial investment @var{i} (i.e., the solution of
## @code{npv (r, p) = i}.  If the second argument is omitted, a value of
## 0 is used.
## @end deftypefn
##
## @seealso{npv, pv, and rate}

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Internal rate of return of an investment

function r = irr (p, i)

  if (nargin == 1)
    i = 0;
  elseif (! (nargin == 2))
    usage ("irr (p, i)");
  endif

  if (! (isvector (p)))
    error ("irr: p must be a vector");
  else
    p_string = strcat ("[", sprintf ("%.15f, ", p), "]");
  endif

  if (! isscalar (i))
    error ("irr: i must be a scalar");
  endif

  r = fsolve (sprintf ("npv (x, %s) - %g", p_string, i), 0.01);

endfunction
