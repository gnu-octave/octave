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
## @deftypefn {Function File} {} wblrnd (@var{scale}, @var{shape}, @var{r}, @var{c})
## @deftypefnx {Function File} {} wblrnd (@var{scale}, @var{shape}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## Weibull distribution with parameters @var{scale} and @var{shape}
## which must be scalar or of size @var{r} by @var{c}. Or if @var{sz}
## is a vector return a matrix of size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{alpha} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Weibull distribution

function rnd = wblrnd (scale, shape, r, c)

  if (nargin > 1)
    if (!isscalar(shape) || !isscalar(scale)) 
      [retval, shape, scale] = common_size (shape, scale);
      if (retval > 0)
	error ("wblrnd: shape and scale must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("wblrnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("wblrnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (scale) != 1) && 
	((length (size (scale)) != length (sz)) ||
	 any (size (scale) != sz)))
      error ("wblrnd: shape and scale must be scalar or of size [r, c]");
    endif
  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("wblrnd: r must be a postive integer or vector");
    endif

    if (any (size (scale) != 1) && 
	((length (size (scale)) != length (sz)) ||
	 any (size (scale) != sz)))
      error ("wblrnd: shape and scale must be scalar or of size sz");
    endif
  elseif (nargin == 2)
    sz = size(shape);
  else
    usage ("wblrnd (alpha, sigma, r, c)");
  endif

  if (isscalar (shape) && isscalar (scale))
    if ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf))
      rnd = (scale * (- log (1 - rand (sz))) .^ (1 / shape));
    else
      rnd = NaN * ones (sz);
    endif
  else
    rnd = NaN * ones (sz);
    k = find ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));
    if (any (k))
      rnd(k) = (scale(k)
		.* (- log (1 - rand (size (k)))) .^ (1 ./ shape(k)));
    endif
  endif

endfunction

