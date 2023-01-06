########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn {} {@var{v} =} nonzeros (@var{A})
## Return a column vector of the nonzero values of the matrix @var{A}.
## @seealso{find, nnz}
## @end deftypefn

function v = nonzeros (A)

  if (nargin < 1)
    print_usage ();
  endif

  if (issparse (A))
    [~, ~, v] = find (A);
    v = v(:);
  else
    v = A(find (A));
    v = v(:);
  endif

endfunction


%!assert (nonzeros ([1,2;3,0]), [1;3;2])
%!assert (nonzeros ([1,2,3,0]), [1;2;3])
%!assert (nonzeros (sparse ([1,2;3,0])), [1;3;2])
%!assert (nonzeros (sparse ([1,2,3,0])), [1;2;3])

## Test input validation
%!error <Invalid call> nonzeros ()
