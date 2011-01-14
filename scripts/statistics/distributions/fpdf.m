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
## @deftypefn {Function File} {} fpdf (@var{x}, @var{m}, @var{n})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the F distribution with @var{m} and @var{n}
## degrees of freedom.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the F distribution

function pdf = fpdf (x, m, n)

  if (nargin != 3)
    print_usage ();
  endif

  if (!isscalar (m) || !isscalar (n))
    [retval, x, m, n] = common_size (x, m, n);
    if (retval > 0)
      error ("fpdf: X, M and N must be of common size or scalar");
    endif
  endif

  sz = size (x);
  pdf = zeros (sz);

  k = find (isnan (x) | !(m > 0) | !(n > 0));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find ((x > 0) & (x < Inf) & (m > 0) & (n > 0));
  if (any (k))
    if (isscalar (m) && isscalar (n))
      tmp = m / n * x(k);
      pdf(k) = (exp ((m / 2 - 1) .* log (tmp)
                     - ((m + n) / 2) .* log (1 + tmp))
                .* (m / n) ./ beta (m / 2, n / 2));
    else
      tmp = m(k) .* x(k) ./ n(k);
      pdf(k) = (exp ((m(k) / 2 - 1) .* log (tmp)
                     - ((m(k) + n(k)) / 2) .* log (1 + tmp))
                .* (m(k) ./ n(k)) ./ beta (m(k) / 2, n(k) / 2));
    endif
  endif

endfunction
