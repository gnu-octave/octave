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

## usage:  chisquare_pdf (x, n)
##
## For each element of x, compute the probability density function (PDF)
## at x of the chisquare distribution with k degrees of freedom.

## Author:  TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description:  PDF of the chi-sqaure distribution

function pdf = chisquare_pdf (x, n)

  if (nargin != 2)
    usage ("chisquare_pdf (x, n)");
  endif

  [retval, x, n] = common_size (x, n);
  if (retval > 0)
    error (["chisquare_pdf:  ", ...
            "x and n must be of common size or scalar"]);
  endif

  pdf = gamma_pdf (x, n / 2, 1 / 2);

  ## should we really only allow for positive integer n?
  k = find (n != round (n));
  if any (k)
    fprintf (stderr, ...
             "WARNING:  n should be positive integer\n");
    [r, c] = size (x);
    pdf = reshape (pdf, 1, r * c);
    pdf(k) = NaN * ones (1, length (k));
    pdf = reshape (pdf, r, c);
  endif

endfunction
