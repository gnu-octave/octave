## Copyright (C) 1996, 1997, 2002, 2004, 2005, 2006, 2007, 2008 John W. Eaton
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
## @deftypefn {Function File} {} isvector (@var{x})
## Return true if @var{x} is a vector.  A vector is a 2-D array
## where one of the dimensions is equal to 1.  As a consequence a
## 1x1 array, or scalar, is also a vector.
## @seealso{isscalar, ismatrix, size, rows, columns, length}
## @end deftypefn

## Author: jwe

function retval = isvector (x)

  retval = 0;

  if (nargin == 1)
    sz = size (x);
    retval = (ndims (x) == 2 && (sz(1) == 1 || sz(2) == 1));
  else
    print_usage ();
  endif

endfunction

%!assert(isvector (1));

%!assert(isvector ([1; 2; 3]));

%!assert(!(isvector ([])));

%!assert(!(isvector ([1, 2; 3, 4])));

%!test
%! warn_str_to_num = 0;
%! assert((isvector ("t")));

%!test
%! warn_str_to_num = 0;
%! assert((isvector ("test")));

%!assert(!(isvector (["test"; "ing"])));

%!test
%! s.a = 1;
%! assert((isvector (s)));

%!error isvector ();

%!error isvector ([1, 2], 2);

