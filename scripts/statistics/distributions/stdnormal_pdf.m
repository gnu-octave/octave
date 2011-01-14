## Copyright (C) 1995-2011 Kurt Hornik
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
## @deftypefn {Function File} {} stdnormal_pdf (@var{x})
## For each element of @var{x}, compute the probability density function
## (PDF) of the standard normal distribution at @var{x}.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: PDF of the standard normal distribution

function pdf = stdnormal_pdf (x)

  if (nargin != 1)
    print_usage ();
  endif

  sz = size(x);
  pdf = zeros (sz);

  k = find (isnan (x));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find (!isinf (x));
  if (any (k))
    pdf (k) = (2 * pi)^(- 1/2) * exp (- x(k) .^ 2 / 2);
  endif

endfunction
