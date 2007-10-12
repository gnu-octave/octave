## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} isvector (@var{a})
## Return 1 if @var{a} is a vector.  Otherwise, return 0.
## @seealso{size, rows, columns, length, isscalar, ismatrix}
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
