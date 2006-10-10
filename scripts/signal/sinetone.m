## Copyright (C) 1995, 1996, 1997  Friedrich Leisch
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
## @deftypefn {Function File} {} sinetone (@var{freq}, @var{rate}, @var{sec}, @var{ampl})
## Return a sinetone of frequency @var{freq} with length of @var{sec}
## seconds at sampling rate @var{rate} and with amplitude @var{ampl}.
## The arguments @var{freq} and @var{ampl} may be vectors of common size.
##
## Defaults are @var{rate} = 8000, @var{sec} = 1 and @var{ampl} = 64.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Compute a sine tone

function retval = sinetone (f, r, s, a)

  if (nargin == 1)
    r = 8000;
    s = 1;
    a = 64;
  elseif (nargin == 2)
    s = 1;
    a = 64;
  elseif (nargin == 3)
    a = 64;
  elseif ((nargin < 1) || (nargin > 4))
    print_usage ();
  endif

  [err, f, a] = common_size (f, a);
  if (err || ! isvector (f))
    error ("sinetone: freq and ampl must be vectors of common size");
  endif

  if (! (isscalar (r) && isscalar (s)))
    error ("sinetone: rate and sec must be scalars");
  endif

  n = length (f);

  retval = zeros (r * s, n);
  for k = 1:n
    retval (:, k) = a(k) * sin (2 * pi * (1:r*s) / r * f(k))';
  endfor

endfunction

