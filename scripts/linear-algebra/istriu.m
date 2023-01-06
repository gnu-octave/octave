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
## @deftypefn {} {@var{tf} =} istriu (@var{A})
## Return true if @var{A} is an upper triangular matrix.
##
## An upper triangular matrix has nonzero entries only on the main diagonal and
## above.
## @seealso{isdiag, isbanded, istril, triu, bandwidth}
## @end deftypefn

function tf = istriu (A)

  if (nargin < 1)
    print_usage ();
  endif

  tf = (isnumeric (A) || islogical (A)) && ndims (A) == 2;
  if (tf)
    [i, j] = find (A);
    tf = all (i <= j);
  endif

endfunction


%!assert (! istriu ("string"))
%!assert (istriu ([]))
%!assert (! istriu (zeros (2,2,2)))

%!assert (istriu (1))
%!assert (istriu ([1, 1]))
%!assert (! istriu ([1; 1]))
%!assert (istriu (eye (10)))
%!assert (istriu (speye (100)))

%!assert (istriu (triu (randn (10))))
%!assert (! istriu (randn (10)))

## Test input validation
%!error <Invalid call> istriu ()
