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
## @deftypefn {Function File} {} unifpdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the PDF at @var{x} of the uniform
## distribution on [@var{a}, @var{b}].
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the uniform distribution

function pdf = unifpdf (x, a, b)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    a = 0;
    b = 1;
  endif

  if (!isscalar (a) || !isscalar(b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("unifpdf: X, A and B must be of common size or scalars");
    endif
  endif

  sz = size (x);
  pdf = zeros (sz);

  k = find (isnan (x) | !(a < b));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find ((x >= a) & (x <= b));
  if (any (k))
    if (isscalar (a) && isscalar(b))
      pdf(k) = 1 ./ (b - a);
    else
      pdf(k) = 1 ./ (b(k) - a(k));
    endif
  endif

endfunction
