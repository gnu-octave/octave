## Copyright (C) 1995, 1996 Kurt Hornik
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} nextpow2 (@var{x})
## If @var{x} is a scalar, returns the first integer @var{n} such that
## @iftex
## @tex
##  $2^n \ge |x|$.
## @end tex
## @end iftex
## @ifinfo
##  2^n >= abs (x).
## @end ifinfo
##
## If @var{x} is a vector, return @code{nextpow2 (length (@var{x}))}.
## @end deftypefn
##
## @seealso{pow2}

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 7 October 1994
## Adapted-By: jwe

function n = nextpow2 (x)

  if (nargin != 1)
    usage ("nextpow2 (x)");
  endif

  if (! (isscalar (x) || isvector (x)))
    error ("nextpow2: x must be a scalar or a vector");
  endif

  t = length (x);
  if (t > 1)
    x = t;
  endif

  [f, n] = log2 (abs (x));
  if (f == 0.5)
    n = n - 1;
  endif

endfunction
