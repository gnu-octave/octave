## Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006,
##               2007 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} cauchy_inv (@var{x}, @var{location}, @var{scale})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the Cauchy distribution with location parameter
## @var{location} and scale parameter @var{scale}.  Default values are
## @var{location} = 0, @var{scale} = 1. 
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Cauchy distribution

function inv = cauchy_inv (x, location, scale)

  if (! (nargin == 1 || nargin == 3))
    print_usage ();
  endif

  if (nargin == 1)
    location = 0;
    scale = 1;
  endif

  if (!isscalar (location) || !isscalar (scale)) 
    [retval, x, location, scale] = common_size (x, location, scale);
    if (retval > 0)
      error ("cauchy_inv: X, LOCATION and SCALE must be of common size or scalar");
    endif
  endif

  sz = size (x);
  inv = NaN (sz);

  ok = ((location > -Inf) & (location < Inf) &
       (scale > 0) & (scale < Inf));

  k = find ((x == 0) & ok);
  if (any (k))
    inv(k) = -Inf;
  endif

  k = find ((x > 0) & (x < 1) & ok);
  if (any (k))
    if (isscalar (location) && isscalar (scale)) 
      inv(k) = location - scale .* cot (pi * x(k));
    else
      inv(k) = location(k) - scale(k) .* cot (pi * x(k));
    endif
  endif

  k = find ((x == 1) & ok);
  if (any (k))
    inv(k) = Inf;
  endif

endfunction
