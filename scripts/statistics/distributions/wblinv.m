## Copyright (C) 1995, 1996, 1997, 2006, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} wblinv (@var{x}, @var{scale}, @var{shape})
## Compute the quantile (the inverse of the CDF) at @var{x} of the
## Weibull distribution with shape parameter @var{scale} and scale
## parameter @var{shape}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Weibull distribution

function inv = wblinv (x, scale, shape)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin < 3)
    shape = 1;
  endif

  if (nargin < 2)
    scale = 1;
  endif

  if (!isscalar (shape) || !isscalar (scale))
    [retval, x, shape, scale] = common_size (x, shape, scale);
    if (retval > 0)
      error ("wblinv: x, scale and shape must be of common size or scalar");
    endif
  endif

  inv = NaN (size (x));

  ok = ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));

  k = find ((x == 0) & ok);
  if (any (k))
    inv(k) = -Inf;
  endif

  k = find ((x > 0) & (x < 1) & ok);
  if (any (k))
    if (isscalar (shape) && isscalar (scale))
      inv(k) = scale * (- log (1 - x(k))) .^ (1 / shape);
    else
      inv(k) = scale(k) .* (- log (1 - x(k))) .^ (1 ./ shape(k));
    endif
  endif

  k = find ((x == 1) & ok);
  if (any (k))
    inv(k) = Inf;
  endif

endfunction
