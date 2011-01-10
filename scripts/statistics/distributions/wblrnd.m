## Copyright (C) 1995, 1996, 1997, 2006, 2007, 2009 Kurt Hornik
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
## @deftypefn  {Function File} {} wblrnd (@var{scale}, @var{shape}, @var{r}, @var{c})
## @deftypefnx {Function File} {} wblrnd (@var{scale}, @var{shape}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## Weibull distribution with parameters @var{scale} and @var{shape}
## which must be scalar or of size @var{r} by @var{c}.  Or if @var{sz}
## is a vector return a matrix of size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{alpha} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Weibull distribution

function rnd = wblrnd (scale, shape, r, c)

  if (nargin > 1)
    if (!isscalar(scale) || !isscalar(shape)) 
      [retval, scale, shape] = common_size (scale, shape);
      if (retval > 0)
        error ("wblrnd: SCALE and SHAPE must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("wblrnd: R must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("wblrnd: C must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (scale) != 1)
        && ((length (size (scale)) != length (sz))
            || any (size (scale) != sz)))
      error ("wblrnd: SCALE and SHAPE must be scalar or of size [R, C]");
    endif
  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("wblrnd: R must be a positive integer or vector");
    endif

    if (any (size (scale) != 1)
        && ((length (size (scale)) != length (sz))
            || any (size (scale) != sz)))
      error ("wblrnd: SCALE and SHAPE must be scalar or of size SZ");
    endif
  elseif (nargin == 2)
    sz = size(scale);
  else
    print_usage ();
  endif

  if (isscalar (scale) && isscalar (shape))
    if (scale > 0 && scale < Inf && shape > 0 && shape < Inf)
      rnd = scale .* rande(sz) .^ (1./shape);
    else
      rnd = NaN (sz);
    endif
  else
    rnd = scale .* rande(sz) .^ (1./shape);
    k = find ((scale <= 0) | (scale == Inf) | ((shape <= 0) & (shape == Inf)));
    if (any(k))
      rnd(k) = NaN;
    endif
  endif

endfunction

