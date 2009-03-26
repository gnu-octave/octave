## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002,
##               2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton
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
## @ifnottex
##
## @example
## @group
## c(1)^(n-1) @dots{} c(1)^2  c(1)  1
## c(2)^(n-1) @dots{} c(2)^2  c(2)  1
##     .     .      .      .    .
##     .       .    .      .    .
##     .         .  .      .    .
## c(n)^(n-1) @dots{} c(n)^2  c(n)  1
## @end group
## @end example
## @end ifnottex
## @seealso{hankel, sylvester_matrix, hilb, invhilb, toeplitz}
## @end deftypefn

## Author: jwe

function retval = vander (c)

  if (nargin != 1)
    print_usage ();
  endif

  if (isvector (c))
    n = length (c);
    retval = (c(:) * ones (1, n)) .^ (ones (n, 1) * (n-1:-1:0));
  else
    error ("vander: argument must be a vector");
  endif

endfunction

%!test
%! c = [0,1,2,3];
%! expect = [0,0,0,1; 1,1,1,1; 8,4,2,1; 27,9,3,1];
%! result = vander(c);
%! assert(expect, result);

%!assert((vander (1) == 1 && vander ([1, 2, 3]) == vander ([1; 2; 3])
%! && vander ([1, 2, 3]) == [1, 1, 1; 4, 2, 1; 9, 3, 1]
%! && vander ([1, 2, 3]*i) == [-1, i, 1; -4, 2i, 1; -9, 3i, 1]));

%!error vander ([1, 2; 3, 4]);

%!error vander ();

%!error vander (1, 2);

