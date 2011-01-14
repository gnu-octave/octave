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
## @deftypefn {Function File} {} geopdf (@var{x}, @var{p})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the geometric distribution with parameter @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the geometric distribution

function pdf = geopdf (x, p)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (x) && !isscalar (p))
    [retval, x, p] = common_size (x, p);
    if (retval > 0)
      error ("geopdf: X and P must be of common size or scalar");
    endif
  endif

  pdf = zeros (size (x));

  k = find (isnan (x) | !(p >= 0) | !(p <= 1));
  if (any (k))
    pdf(k) = NaN;
  endif

  ## Just for the fun of it ...
  k = find ((x == Inf) & (p == 0));
  if (any (k))
    pdf(k) = 1;
  endif

  k = find ((x >= 0) & (x < Inf) & (x == round (x)) & (p > 0) & (p <= 1));
  if (any (k))
    if (isscalar (x))
      pdf(k) = p(k) .* ((1 - p(k)) .^ x);
    elseif (isscalar (p))
      pdf(k) = p .* ((1 - p) .^ x(k));
    else
      pdf(k) = p(k) .* ((1 - p(k)) .^ x(k));
    endif
  endif

endfunction
