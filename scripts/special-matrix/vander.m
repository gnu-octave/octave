## Copyright (C) 1996, 1997 John W. Eaton
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} vander (@var{c})
## Return the Vandermonde matrix whose next to last column is @var{c}.
##
## A Vandermonde matrix has the form:
## @iftex
## @tex
## $$
## \left[\matrix{c_1^{n-1}  & \cdots & c_1^2  & c_1    & 1      \cr
##               c_2^{n-1}  & \cdots & c_2^2  & c_2    & 1      \cr
##               \vdots     & \ddots & \vdots & \vdots & \vdots \cr
##               c_n^{n-1}  & \cdots & c_n^2  & c_n    & 1      }\right]
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## @group
## c(1)^(n-1) ... c(1)^2  c(1)  1
## c(2)^(n-1) ... c(2)^2  c(2)  1
##     .     .      .      .    .
##     .       .    .      .    .
##     .         .  .      .    .
## c(n)^(n-1) ... c(n)^2  c(n)  1
## @end group
## @end example
## @end ifinfo
## @end deftypefn
## @seealso{hankel, sylvester_matrix, hilb, invhilb, and toeplitz}

## Author: jwe

function retval = vander (c)

  if (nargin != 1)
    usage ("vander (c)");
  endif

  if (isvector (c))
    n = length (c);
    retval = zeros (n, n);
    j = 1:n;
    for i = 1:n
      retval(i,:) = c(i) .^ (n - j);
    endfor
  else
    error ("vander: argument must be a vector");
  endif

endfunction
