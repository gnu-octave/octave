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
## @deftypefn {Function File} {} kolmogorov_smirnov_cdf (@var{x}, @var{tol})
## Return the CDF at @var{x} of the Kolmogorov-Smirnov distribution,
## @iftex
## @tex
## $$ Q(x) = sum_{k=-\infty}^\infty (-1)^k exp(-2 k^2 x^2) $$
## @end tex
## @end iftex
## @ifinfo
## @example
##          Inf
## Q(x) =   SUM    (-1)^k exp(-2 k^2 x^2)
##        k = -Inf
## @end example
## @end ifinfo
##
## @noindent
## for @var{x} > 0.
##
## The optional parameter @var{tol} specifies the precision up to which
## the series should be evaluated;  the default is @var{tol} = @code{eps}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the Kolmogorov-Smirnov distribution

function cdf = kolmogorov_smirnov_cdf (x, tol)

  if (nargin < 1 || nargin > 2)
    usage ("kolmogorov_smirnov_cdf (x, tol)");
  endif

  if (nargin == 1)
    tol = eps;
  else
    if (! isscalar (tol) || ! (tol > 0))
      error ("kolmogorov_smirnov_cdf: tol has to be a positive scalar");
    endif
  endif

  [nr, nc] = size (x);
  if (min (nr, nc) == 0)
    error ("kolmogorov_smirnov_cdf: x must not be empty");
  endif

  n   = nr * nc;
  x   = reshape (x, 1, n);
  cdf = zeros (1, n);
  ind = find (x > 0);
  if (length (ind) > 0)
    y   = x(ind);
    K   = ceil (sqrt (- log (tol) / 2) / min (y));
    k   = (1:K)';
    A   = exp (- 2 * k.^2 * y.^2);
    odd = find (rem (k, 2) == 1);
    A(odd,:) = -A(odd,:);
    cdf(ind) = 1 + 2 * sum (A);
  endif

  cdf = reshape (cdf, nr, nc);

endfunction
