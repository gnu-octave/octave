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
## @deftypefn {Function File} {} gamma_pdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, return the probability density function
## (PDF) at @var{x} of the Gamma distribution with parameters @var{a}
## and @var{b}.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: PDF of the Gamma distribution

function pdf = gamma_pdf (x, a, b)

  if (nargin != 3)
    usage ("gamma_pdf (x, a, b)");
  endif

  [retval, x, a, b] = common_size (x, a, b);
  if (retval > 0)
    error ("gamma_pdf: x, a and b must be of common size or scalars");
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, s, 1);
  a   = reshape (a, s, 1);
  b   = reshape (b, s, 1);
  pdf = zeros (s, 1);

  k = find (!(a > 0) | !(b > 0) | isnan (x));
  if (any (k))
    pdf (k) = NaN * ones (length (k), 1);
  endif

  k = find ((x > 0) & (a > 0) & (b > 0));
  if (any (k))
    pdf(k) = ((b(k) .^ a(k)) .* (x(k) .^ (a(k) - 1))
	      .* exp(-b(k) .* x(k)) ./ gamma (a(k)));
  endif

  pdf = reshape (pdf, r, c);

endfunction
