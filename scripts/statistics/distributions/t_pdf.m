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

## usage:  t_pdf (x, n)
##
## For each element of x, compute the probability density function (PDF)
## at x of the t (Student) distribution with n degrees of freedom.

## Author:  KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description:  PDF of the t distribution

function pdf = t_pdf (x, n)

  if (nargin != 2)
    usage ("t_pdf (x, n)");
  endif

  [retval, x, n] = common_size (x, n);
  if (retval > 0)
    error ("t_pdf:  x and n must be of common size or scalar");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  n = reshape (n, 1, s);
  pdf = zeros (1, s);

  k = find (isnan (x) | !(n > 0) | !(n < Inf));
  if any (k)
    pdf(k) = NaN * ones (1, length (k));
  endif

  k = find (!isinf (x) & !isnan (x) & (n > 0) & (n < Inf));
  if any (k)
    pdf(k) = exp (- (n(k) + 1) .* log (1 + x(k) .^ 2 ./ n(k)) / 2) ...
      ./ (sqrt (n(k)) .* beta (n(k) / 2, 1 / 2));
  endif

  ## should we really only allow for positive integer n?
  k = find (n != round (n));
  if any (k)
    fprintf (stderr, ...
        "WARNING:  n should be positive integer\n");
    pdf(k) = NaN * ones (1, length (k));
  endif

  pdf = reshape (pdf, r, c);

endfunction
