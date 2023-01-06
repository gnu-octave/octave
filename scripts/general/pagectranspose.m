########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} pagectranspose (@var{A})
## Return the page-wise complex conjugate transpose of the N-dimensional array
## @var{A}.
##
## This is equivalent to @tcode{@var{A}(:,:, @var{k})'} for each page @var{k}.
##
## @seealso{pagetranspose, ctranspose, permute}
## @end deftypefn

function B = pagectranspose (A)

  if (nargin != 1)
    print_usage ();
  endif

  B = permute (conj (A), [2, 1, 3:ndims(A)]);

endfunction


%!test
%! A = reshape (1:8, [2, 2, 2]);
%! B = pagectranspose (A);
%! assert (B, cat (3, [1,2;3,4], [5,6;7,8]));

%!test
%! A = reshape ((1:8)*i, [2, 2, 2]);
%! B = pagectranspose (A);
%! assert (B, -i * cat (3, [1,2;3,4], [5,6;7,8]));

## Test input validation
%!error <Invalid call> pagectranspose ()
