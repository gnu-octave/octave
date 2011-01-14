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
## @deftypefn {Function File} {} tpdf (@var{x}, @var{n})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the @var{t} (Student) distribution with @var{n}
## degrees of freedom. 
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the t distribution

function pdf = tpdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("tpdf: X and N must be of common size or scalar");
    endif
  endif

  pdf = zeros (size (x));

  k = find (isnan (x) | !(n > 0) | !(n < Inf));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find (!isinf (x) & !isnan (x) & (n > 0) & (n < Inf));
  if (any (k))
    if (isscalar (n))
      pdf(k) = (exp (- (n + 1) .* log (1 + x(k) .^ 2 ./ n)/2)
                / (sqrt (n) * beta (n/2, 1/2)));
    else
      pdf(k) = (exp (- (n(k) + 1) .* log (1 + x(k) .^ 2 ./ n(k))/2)
                ./ (sqrt (n(k)) .* beta (n(k)/2, 1/2)));
    endif
  endif

endfunction
