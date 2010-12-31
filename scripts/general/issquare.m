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
## @deftypefn {Function File} {} issquare (@var{x})
## Return true if @var{x} is a square matrix.
## @seealso{isscalar, isvector, ismatrix, size}
## @end deftypefn

## Author: A. S. Hodel <scotte@eng.auburn.edu>
## Created: August 1993
## Adapted-By: jwe

function retval = issquare (x)

  if (nargin == 1)
    if (ndims (x) == 2)
      [r, c] = size (x);
      retval = r == c;
    else
      retval = false;
    endif
  else
    print_usage ();
  endif

endfunction

%!assert(issquare (1));

%!assert(!(issquare ([1, 2])));

%!assert(issquare ([]));

%!assert(issquare ([1, 2; 3, 4]));

%!test
%! assert(issquare ("t"));

%!assert(!(issquare ("test")));

%!test
%! assert(issquare (["test"; "ing"; "1"; "2"]));

%!test
%! s.a = 1;
%! assert(issquare (s));

%!assert(!(issquare ([1, 2; 3, 4; 5, 6])));

%!error issquare ();

%!error issquare ([1, 2; 3, 4], 2);

