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

## usage:  beta_pdf (x, a, b)
##
## For each element of x, returns the PDF at x of the beta distribution
## with parameters a and b.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  PDF of the Beta distribution

function pdf = beta_pdf (x, a, b)

  if (nargin != 3)
    usage ("beta_pdf (a, b, x)");
  endif

  [retval, x, a, b] = common_size (x, a, b);
  if (retval > 0)
    error ("beta_pdf:  x, a and b must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x   = reshape (x, s, 1);
  a   = reshape (a, s, 1);
  b   = reshape (b, s, 1);
  pdf = zeros (s, 1);

  k = find (!(a > 0) | !(b > 0) | isnan (x));
  if any (k)
    pdf (k) = NaN * ones (length (k), 1);
  endif

  k = find ((x > 0) & (x < 1) & (a > 0) & (b > 0));
  if any (k)
    pdf(k) = exp ((a(k) - 1) .* log (x(k)) ...
        + (b(k) - 1) .* log (1 - x(k))) ./ beta (a(k), b(k));
  endif

  pdf = reshape (pdf, r, c);

endfunction
