## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn {Function File} {} flipud (@var{x})
## Return a copy of @var{x} with the order of the rows reversed.  In
## other words, @var{x} is flipped upside-down about a horizontal axis.  For
## example:
##
## @example
## @group
## flipud ([1, 2; 3, 4])
##      @result{}  3  4
##          1  2
## @end group
## @end example
##
## Note that @code{flipud} only works with 2-D arrays.  To flip N-D arrays
## use @code{flipdim} instead.
## @seealso{fliplr, flipdim, rot90, rotdim}
## @end deftypefn

## Author: jwe

function y = flipud (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (ndims (x) > 2)
    error ("flipud: Only works with 2-d arrays");
  endif

  nr = rows (x);
  y = x (nr:-1:1, :);

endfunction

%!assert((flipud ([1, 2; 3, 4]) == [3, 4; 1, 2]
%! && flipud ([1, 2; 3, 4; 5, 6]) == [5, 6; 3, 4; 1, 2]
%! && flipud ([1, 2, 3; 4, 5, 6]) == [4, 5, 6; 1, 2, 3]));

%!error flipud ();

%!error flipud (1, 2);

