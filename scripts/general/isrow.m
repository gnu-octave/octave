## Copyright (C) 2012 John W. Eaton
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
## @deftypefn {Function File} {} isrow (@var{x})
## Return true if @var{x} is a row vector.
## @seealso{iscolumn, isscalar, isvector, ismatrix}
## @end deftypefn

## Author: Rik Wehbring

function retval = isrow (x)

  if (nargin != 1)
    print_usage ();
  endif

  sz = size (x);
  retval = (ndims (x) == 2 && (sz(1) == 1));

endfunction


%!assert (isrow ([1, 2, 3]))
%!assert (isrow ([1; 2; 3]), false)
%!assert (isrow (1))
%!assert (isrow ([]), false)
%!assert (isrow ([1, 2; 3, 4]), false)

%!assert (isrow ("t"))
%!assert (isrow ("test"))
%!assert (isrow (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isrow (s));

%% Test input validation
%!error isrow ()
%!error isrow ([1, 2], 2)

