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
## @deftypefn {Function File} {} lognormal_pdf (@var{x}, @var{a}, @var{v})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the lognormal distribution with parameters
## @var{a} and @var{v}.  If a random variable follows this distribution,
## its logarithm is normally distributed with mean @code{log (@var{a})}
## and variance @var{v}.
##
## Default values are @var{a} = 1, @var{v} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: PDF of the log normal distribution

function pdf = lognormal_pdf (x, a, v)

  if (! ((nargin == 1) || (nargin == 3)))
    usage ("lognormal_pdf (x, a, v)");
  endif

  if (nargin == 1)
    a = 1;
    v = 1;
  endif

  ## The following "straightforward" implementation unfortunately does
  ## not work for the special cases (Inf, ...)
  ## pdf = (x > 0) ./ x .* normal_pdf (log (x), log (a), v);
  ## Hence ...

  [retval, x, a, v] = common_size (x, a, v);
  if (retval > 0)
    error ("lognormal_pdf: x, a and v must be of common size or scalars");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  a = reshape (a, 1, s);
  v = reshape (v, 1, s);
  pdf = zeros (1, s);

  k = find (isnan (x) | !(a > 0) | !(a < Inf) | !(v > 0) | !(v < Inf));
  if (any (k))
    pdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x > 0) & (x < Inf) & (a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if (any (k))
    pdf(k) = normal_pdf (log (x(k)), log (a(k)), v(k)) ./ x(k);
  endif

  pdf = reshape (pdf, r, c);

endfunction
