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
## @deftypefn {Function File} {} cauchy_rnd (@var{lambda}, @var{sigma}, @var{r}, @var{c})
## @deftypefnx {Function File} {} cauchy_rnd (@var{lambda}, @var{sigma}, @var{sz})
## Return an @var{r} by @var{c} or a @code{size (@var{sz})} matrix of 
## random samples from the Cauchy distribution with parameters @var{lambda} 
## and @var{sigma} which must both be scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{lambda} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the Cauchy distribution

function rnd = cauchy_rnd (l, scale, r, c)

  if (nargin > 1)
    if (!isscalar (l) || !isscalar (scale)) 
      [retval, l, scale] = common_size (l, scale);
      if (retval > 0)
	error ("cauchy_rnd: lambda and sigma must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("cauchy_rnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("cauchy_rnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (l) != 1)
	&& (length (size (l)) != length (sz) || any (size (l) != sz)))
      error ("cauchy_rnd: lambda and sigma must be scalar or of size [r, c]");
    endif
  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("cauchy_rnd: r must be a postive integer or vector");
    endif

    if (any (size (l) != 1)
	&& (length (size (l)) != length (sz) ||	any (size (l) != sz)))
      error ("cauchy_rnd: lambda and sigma must be scalar or of size sz");
    endif
  elseif (nargin == 2)
    sz = size(l);
  else
    usage ("cauchy_rnd (lambda, sigma, r, c)");
  endif

  if (isscalar (l) && isscalar (scale)) 
    if (find (!(l > -Inf) | !(l < Inf) | !(scale > 0) | !(scale < Inf)))
      rnd = NaN * ones (sz);
    else
      rnd = l - cot (pi * rand (sz)) .* scale;
    endif
  else
    rnd = NaN * ones (sz);
    k = find ((l > -Inf) & (l < Inf) & (scale > 0) & (scale < Inf));
    if (any (k))
      rnd(k) = l(k)(:) - cot (pi * rand (size (k))) .* scale(k)(:);
    endif
  endif

endfunction
