## Copyright (C) 1995, 1996, 1997  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} weibull_rnd (@var{alpha}, @var{sigma}, @var{r}, @var{c})
## Return an @var{r} by @var{c} matrix of random samples from the
## Weibull distribution with parameters @var{alpha} and @var{sigma}
## which must be scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{alpha} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the Weibull distribution

function rnd = weibull_rnd (shape, scale, r, c)

  if (nargin == 4)
    if (! (is_scalar (r) && (r > 0) && (r == round (r))))
      error ("weibull_rnd: r must be a positive integer");
    endif
    if (! (is_scalar (c) && (c > 0) && (c == round (c))))
      error ("weibull_rnd: c must be a positive integer");
    endif
    [retval, shape, scale] = common_size (shape, scale, zeros (r, c));
    if (retval > 0)
      error ("weibull_rnd: alpha and sigma must be scalar or of size %d by %d",
	     r, c);
    endif
  elseif (nargin == 2)
    [retval, shape, scale] = common_size (shape, scale);
    if (retval > 0)
      error ("weibull_rnd: alpha and sigma must be of common size or scalar");
    endif
  else
    usage ("weibull_rnd (alpha, sigma, r, c)");
  endif

  [r, c] = size (shape);
  s = r * c;
  shape = reshape (shape, 1, s);
  scale = reshape (scale, 1, s);

  rnd = NaN * ones (1, s);
  k = find ((shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf));
  if (any (k))
    rnd(k) = (scale(k)
              .* (- log (1 - rand (1, length (k)))) .^ (1 ./ shape(k)));
  endif

  rnd = reshape (rnd, r, c);

endfunction