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
## @deftypefn {Function File} {} uniform_pdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the PDF at @var{x} of the uniform
## distribution on [@var{a}, @var{b}].
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: PDF of the uniform distribution

function pdf = uniform_pdf (x, a, b)

  if (! (nargin == 1 || nargin == 3))
    usage ("uniform_pdf (x, a, b)");
  endif

  if (nargin == 1)
    a = 0;
    b = 1;
  endif

  [retval, x, a, b] = common_size (x, a, b);
  if (retval > 0)
    error ("uniform_pdf: x, a and b must be of common size or scalars");
  endif

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  a = reshape (a, 1, s);
  b = reshape (b, 1, s);
  pdf = zeros (1, s);

  k = find (isnan (x) | !(a < b));
  if (any (k))
    pdf(k) = NaN * ones (1, length (k));
  endif

  k = find ((x > a) & (x < b));
  if (any (k))
    pdf(k) = 1 ./ (b(k) - a(k));
  endif

  pdf = reshape (pdf, r, c);

endfunction
