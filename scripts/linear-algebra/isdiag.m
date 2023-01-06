########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{tf} =} isdiag (@var{A})
## Return true if @var{A} is a diagonal matrix.
## @seealso{isbanded, istril, istriu, diag, bandwidth}
## @end deftypefn

function tf = isdiag (A)

  if (nargin < 1)
    print_usage ();
  endif

  if (strfind (typeinfo (A), "diagonal matrix"))
    tf = true;
  elseif ((isnumeric (A) || islogical (A)) && ndims (A) == 2)
    [i, j] = find (A);
    tf = all (i == j);
  else
    tf = false;
  endif

endfunction


%!assert (isdiag ("string"), false)
%!assert (isdiag (zeros (2,2,2)), false)
%!assert (isdiag (zeros (2)))
%!assert (isdiag ([]))
%!assert (isdiag (1))
%!assert (isdiag ([1, 1]), false)
%!assert (isdiag ([1; 1]), false)
%!assert (isdiag (eye (10)))
%!assert (isdiag (single (eye (10))))
%!assert (isdiag (logical (eye (10))))
%!assert (isdiag (speye (1e2)))
%!assert (isdiag (diag (1:10)))

## Test input validation
%!error <Invalid call> isdiag ()
