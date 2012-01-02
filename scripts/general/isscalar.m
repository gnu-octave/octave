## Copyright (C) 1996-2012 John W. Eaton
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
## @deftypefn {Function File} {} isscalar (@var{x})
## Return true if @var{x} is a scalar.
## @seealso{isvector, ismatrix}
## @end deftypefn

## Author: jwe

function retval = isscalar (x)

  if (nargin != 1)
    print_usage ();
  endif

  retval = numel (x) == 1;

endfunction


%!assert (isscalar (1))
%!assert (isscalar ([1, 2]), false)
%!assert (isscalar ([]), false)
%!assert (isscalar ([1, 2; 3, 4]), false)

%!assert (isscalar ("t"))
%!assert (isscalar ("test"), false)
%!assert (isscalar (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isscalar (s));

%% Test input validation
%!error isscalar ()
%!error isscalar (1, 2)

