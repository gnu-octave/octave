## Copyright (C) 1995, 1996  Kurt Hornik
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
## @deftypefn {Function File} {} vech (@var{x})
## Return the vector obtained by eliminating all supradiagonal elements of
## the square matrix @var{x} and stacking the result one column above the
## other.
## @end deftypefn

## See Magnus and Neudecker (1988), Matrix differential calculus with
## applications in statistics and econometrics.

## Author KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 8 May 1995
## Adapted-By: jwe

function v = vech (x)

  if (nargin != 1)
    usage ("vech (x)");
  endif

  if (! is_square (x))
    error ("vech: x must be square");
  endif

  ## This should be quicker than having an inner `for' loop as well.
  ## Ideally, vech should be written in C++.
  n = rows (x);
  v = zeros ((n+1)*n/2, 1);
  count = 0;
  for j = 1 : n
    i = j : n;
    v (count + i) = x (i, j);
    count = count + n - j;
  endfor

endfunction
