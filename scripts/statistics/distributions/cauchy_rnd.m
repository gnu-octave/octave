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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} cauchy_rnd (@var{lambda}, @var{sigma}, @var{r}, @var{c})
## Return an @var{r} by @var{c} matrix of random samples from the Cauchy
## distribution with parameters @var{lambda} and @var{sigma} which must
## both be scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{lambda} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the Cauchy distribution

function rnd = cauchy_rnd (l, scale, r, c)

  if (nargin == 4)
    if (! (is_scalar (r) && (r > 0) && (r == round (r))))
      error ("cauchy_rnd: r must be a positive integer");
    endif
    if (! (is_scalar (c) && (c > 0) && (c == round (c))))
      error ("cauchy_rnd: c must be a positive integer");
    endif
    [retval, l, scale] = common_size (l, scale, zeros (r, c));
    if (retval > 0)
      error ("cauchy_rnd: lambda and sigma must be scalar or of size %d by %d",
	     r, c); 
    endif
  elseif (nargin == 2)
    [retval, l, scale] = common_size (l, scale);
    if (retval > 0)
      error ("cauchy_rnd: lambda and sigma must be of common size or scalar");
    endif
    [r, c] = size (l);
  else
    usage ("cauchy_rnd (lambda, sigma, r, c)");
  endif

  s = r * c;
  l = reshape (l, 1, s);
  scale = reshape (scale, 1, s);

  rnd = NaN * ones (1, s);

  k = find ((l > -Inf) & (l < Inf) & (scale > 0) & (scale < Inf));
  if (any (k))
    rnd(k) = l(k) - cot (pi * rand (1, length (k))) .* scale(k);
  endif

  rnd = reshape (rnd, r, c);

endfunction
