## Copyright (C) 2000 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} factorial (@var{n})
## Return the factorial of @var{n}. If @var{n} is scalar, this is
## equivalent to @code{prod (1:@var{n})}.  If @var{n} is an array,
## the factorial of the elements of the array are returned.
## @end deftypefn

function x = factorial (n)
  if (any (n(:) < 0))
    error ("factorial: n be be a scalar or array of positive integers");
  endif
  if (isscalar (n))
    x = prod (2:n);
  else
    n (n < 1) = 1;
    m = max (n(:));
    c = cumprod (1:m);
    x = c(floor (n));
  endif
endfunction
