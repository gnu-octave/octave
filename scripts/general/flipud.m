########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn {} {@var{B} =} flipud (@var{A})
## Flip array upside down.
##
## Return a copy of @var{A} with the order of the rows reversed.  In other
## words, @var{A} is flipped upside-down about a horizontal axis.  For example:
##
## @example
## @group
## flipud ([1, 2; 3, 4])
##      @result{}  3  4
##          1  2
## @end group
## @end example
##
## @seealso{fliplr, flip, rot90, rotdim}
## @end deftypefn

function B = flipud (A)

  if (nargin < 1)
    print_usage ();
  endif
  B = flip (A, 1);

endfunction


%!assert (flipud ([1, 2; 3, 4]), [3, 4; 1, 2])
%!assert (flipud ([1, 2; 3, 4; 5, 6]), [5, 6; 3, 4; 1, 2])
%!assert (flipud ([1, 2, 3; 4, 5, 6]), [4, 5, 6; 1, 2, 3])
%!assert (flipud ([1 2 3]), [1 2 3])

## Test NDArrays
%!test
%! a(:,:,1) = [ 1  2  3;  4  5  6];
%! a(:,:,2) = [ 7  8  9; 10 11 12];
%! b(:,:,1) = [ 4  5  6;  1  2  3];
%! b(:,:,2) = [10 11 12;  7  8  9];
%! assert (flipud (a), b);

## Test NDArray with singleton dimensions
%!test
%! a(:,:,:,1) = [ 1  2  3;  4  5  6];
%! a(:,:,:,2) = [ 7  8  9; 10 11 12];
%! b(:,:,:,1) = [ 4  5  6;  1  2  3];
%! b(:,:,:,2) = [10 11 12;  7  8  9];
%! assert (flipud (a), b);

## Test for 1 row, i.e., returns the same
%!test
%! a(1,:,:,1) = [ 1  2  3  4];
%! a(1,:,:,2) = [ 5  6  7  8];
%! assert (flipud (a), a);

%!error <Invalid call> flipud ()
