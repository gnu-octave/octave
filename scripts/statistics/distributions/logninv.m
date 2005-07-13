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
## @deftypefn {Function File} {} logninv (@var{x}, @var{a}, @var{v})
## For each element of @var{x}, compute the quantile (the inverse of the
## CDF) at @var{x} of the lognormal distribution with parameters @var{a}
## and @var{v}.  If a random variable follows this distribution, its
## logarithm is normally distributed with mean @code{log (@var{a})} and
## variance @var{v}.
##
## Default values are @var{a} = 1, @var{v} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Quantile function of the log normal distribution

function inv = logninv (x, a, v)

  if (! ((nargin == 1) || (nargin == 3)))
    usage ("logninv (x, a, v)");
  endif

  if (nargin == 1)
    a = 1;
    v = 1;
  endif

  ## The following "straightforward" implementation unfortunately does
  ## not work (because exp (Inf) -> NaN):
  ## inv = exp (normal_inv (x, log (a), v));
  ## Hence ...

  if (!isscalar (a) || !isscalar (v))
    [retval, x, a, v] = common_size (x, a, v);
    if (retval > 0)
      error ("logninv: x, a and v must be of common size or scalars");
    endif
  endif

  inv = zeros (size (x));

  k = find (!(x >= 0) | !(x <= 1) | !(a > 0) | !(a < Inf)
	    | !(v > 0) | !(v < Inf));
  if (any (k))
    inv(k) = NaN;
  endif

  k = find ((x == 1) & (a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if (any (k))
    inv(k) = Inf;
  endif

  k = find ((x > 0) & (x < 1) & (a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if (any (k))
    if (isscalar (a) && isscalar (v))
      inv(k) = a .* exp (sqrt (v) .* stdnormal_inv (x(k)));
    else
      inv(k) = a(k) .* exp (sqrt (v(k)) .* stdnormal_inv (x(k)));
    endif
  endif

endfunction
