########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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

## Tests for SparseMatrix

## Test broadcasting of empty matrices with math operators.
## This has been fixed in MSparse.cc and Sparse-op-defs.h
%!assert (sparse (zeros (0,1)) + sparse ([1, 2, 3, 4]), sparse (zeros (0,4)))
%!error <operator \+: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) + sparse ([1, 2, 3, 4])
%!assert (sparse (zeros (0,1)) - sparse ([1, 2, 3, 4]), sparse (zeros (0,4)))
%!error <operator -: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) - sparse ([1, 2, 3, 4])
%!assert (sparse (zeros (0,1)) * sparse ([1, 2, 3, 4]), sparse (zeros (0,4)))
%!error <operator \*: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) * sparse ([1, 2, 3, 4])
%!assert (sparse (zeros (0,1)) .* sparse ([1, 2, 3, 4]), sparse (zeros (0,4)))
%!error <product: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) .* sparse ([1, 2, 3, 4])
%!assert (sparse (zeros (0,1)) ./ sparse ([1, 2, 3, 4]), sparse (zeros (0,4)))
%!error <quotient: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) ./ sparse ([1, 2, 3, 4])
%!test
%! a = sparse (zeros (0,1));
%! assert (a += sparse ([1, 2, 3, 4]), sparse (zeros (0,4)));
%! assert (a -= sparse ([1, 2, 3, 4]), sparse (zeros (0,4)));

%!assert (sparse (zeros (1,0)) + sparse ([1; 2; 3; 4]), sparse (zeros (4,0)))
%!error <operator \+: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) + sparse ([1; 2; 3; 4])
%!assert (sparse (zeros (1,0)) - sparse ([1; 2; 3; 4]), sparse (zeros (4,0)))
%!error <operator -: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) - sparse ([1; 2; 3; 4])
%!error <operator \*: nonconformant arguments \(op1 is 1x0, op2 is 4x1\)>
%! sparse (zeros (1,0)) * sparse ([1; 2; 3; 4])
%!error <operator \*: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) * sparse ([1; 2; 3; 4])
%!assert (sparse (zeros (1,0)) .* sparse ([1; 2; 3; 4]), sparse (zeros (4,0)))
%!error <product: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) .* sparse ([1; 2; 3; 4])
%!assert (sparse (zeros (1,0)) ./ sparse ([1; 2; 3; 4]), sparse (zeros (4,0)))
%!error <quotient: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) ./ sparse ([1; 2; 3; 4])
%!test
%! a = sparse (zeros (1,0));
%! assert (a += sparse ([1; 2; 3; 4]), sparse (zeros (4,0)));
%! assert (a -= sparse ([1; 2; 3; 4]), sparse (zeros (4,0)));

## Test broadcasting of sparse matrices with math operators
%!assert (full (sparse ([1, 2, 3]) + sparse ([1, 2, 3; 4, 5, 6])),
%!        [1, 2, 3] + [1, 2, 3; 4, 5, 6])
%!assert (full (sparse ([1; 2; 3]) + sparse ([1, 2; 1, 2; 1, 2])),
%!        [1; 2; 3] + [1, 2; 1, 2; 1, 2])
%!assert (full (sparse ([1, 2, 3]) + sparse ([1; 2; 3; 4])),
%!        [1, 2, 3] + [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) + sparse ([1, 2, 3, 4, 5])),
%!        [1; 2; 3] + [1, 2, 3, 4, 5])
%!assert (full (sparse ([1, 0, 3]) + sparse ([1, 0, 3; 0, 5, 6])),
%!        [1, 0, 3] + [1, 0, 3; 0, 5, 6])
%!assert (full (sparse ([1; 2; 0]) + sparse ([1, 2; 1, 0; 0, 0])),
%!        [1; 2; 0] + [1, 2; 1, 0; 0, 0])
%!assert (full (sparse ([0, 0, 3]) + sparse ([1; 2; 3; 4])),
%!        [0, 0, 3] + [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) + sparse ([1, 0, 0, 0, 5])),
%!        [1; 2; 3] + [1, 0, 0, 0, 5])
%!assert (full (sparse ([1, 2, 3; 4, 5, 6]) + sparse ([1, 2, 3])),
%!        [1, 2, 3; 4, 5, 6] + [1, 2, 3])
%!assert (full (sparse ([1, 2; 1, 2; 1, 2]) + sparse ([1; 2; 3])),
%!        [1, 2; 1, 2; 1, 2] + [1; 2; 3])
%!assert (full (sparse ([1; 2; 3; 4]) + sparse ([1, 2, 3])),
%!        [1; 2; 3; 4] + [1, 2, 3])
%!assert (full (sparse ([1, 2, 3, 4, 5]) + sparse ([1; 2; 3])),
%!        [1, 2, 3, 4, 5] + [1; 2; 3])
%!assert (full (sparse ([1, 0, 3; 0, 5, 6]) + sparse ([1, 0, 3])),
%!        [1, 0, 3; 0, 5, 6] + [1, 0, 3])
%!assert (full (sparse ([1, 2; 1, 0; 0, 0]) + sparse ([1; 2; 0])),
%!        [1, 2; 1, 0; 0, 0] + [1; 2; 0])
%!assert (full (sparse ([1; 2; 3; 4]) + sparse ([0, 0, 3])),
%!        [1; 2; 3; 4] + [0, 0, 3])
%!assert (full (sparse ([1, 0, 0, 0, 5]) + sparse ([1; 2; 3])),
%!        [1, 0, 0, 0, 5] + [1; 2; 3])

%!assert (sparse (1) + sparse ([1, 2, 3]), sparse ([2, 3, 4]))
%!assert (sparse (1) + sparse ([1, 2; 3, 4]), sparse ([2, 3; 4, 5]))
%!assert (sparse ([1, 2, 3]) + sparse (1), sparse ([2, 3, 4]))
%!assert (sparse ([1, 2; 3, 4]) + sparse (1), sparse ([2, 3; 4, 5]))
%!assert (full (sparse ([1; 2]) + sparse ([1, 0, 2, 0, 4])),
%!        [1; 2] + [1, 0, 2, 0, 4])
%!assert (full (sparse ([1, 0, 2, 0, 4]) + sparse ([1; 2])),
%!        [1, 0, 2, 0, 4] + [1; 2])
%!assert (full (sparse ([1, 2]) + sparse ([1; 0; 2; 0; 4])),
%!        [1, 2] + [1; 0; 2; 0; 4])
%!assert (full (sparse ([1; 0; 2; 0; 4]) + sparse ([1, 2])),
%!        [1; 0; 2; 0; 4] + [1, 2])
%!assert (full (sparse ([1, 0, 3]) + sparse ([1, 2, 3; 4, 0, 0])),
%!        [1, 0, 3] + [1, 2, 3; 4, 0, 0])
%!assert (full (sparse ([10, 0, 3; 4, 5, 0]) + sparse ([1, 0, 3])),
%!        [10, 0, 3; 4, 5, 0] + [1, 0, 3])
%!assert (full (sparse ([1; 2; 0]) + sparse ([0, 2; 3, 4; 5, 0])),
%!        [1; 2; 0] + [0, 2; 3, 4; 5, 0])
%!assert (full (sparse ([1, 2; 0, 0; 0, 6]) + sparse ([0; 2; 3])),
%!        [1, 2; 0, 0; 0, 6] + [0; 2; 3])

%!assert (full (sparse ([1, 2, 3]) - sparse ([1, 2, 3; 4, 5, 6])),
%!        [1, 2, 3] - [1, 2, 3; 4, 5, 6])
%!assert (full (sparse ([1; 2; 3]) - sparse ([1, 2; 1, 2; 1, 2])),
%!        [1; 2; 3] - [1, 2; 1, 2; 1, 2])
%!assert (full (sparse ([1, 2, 3]) - sparse ([1; 2; 3; 4])),
%!        [1, 2, 3] - [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) - sparse ([1, 2, 3, 4, 5])),
%!        [1; 2; 3] - [1, 2, 3, 4, 5])
%!assert (full (sparse ([1, 0, 3]) - sparse ([1, 0, 3; 0, 5, 6])),
%!        [1, 0, 3] - [1, 0, 3; 0, 5, 6])
%!assert (full (sparse ([1; 2; 0]) - sparse ([1, 2; 1, 0; 0, 0])),
%!        [1; 2; 0] - [1, 2; 1, 0; 0, 0])
%!assert (full (sparse ([0, 0, 3]) - sparse ([1; 2; 3; 4])),
%!        [0, 0, 3] - [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) - sparse ([1, 0, 0, 0, 5])),
%!        [1; 2; 3] - [1, 0, 0, 0, 5])
%!assert (full (sparse ([1, 2, 3; 4, 5, 6]) - sparse ([1, 2, 3])),
%!        [1, 2, 3; 4, 5, 6] - [1, 2, 3])
%!assert (full (sparse ([1, 2; 1, 2; 1, 2]) - sparse ([1; 2; 3])),
%!        [1, 2; 1, 2; 1, 2] - [1; 2; 3])
%!assert (full (sparse ([1; 2; 3; 4]) - sparse ([1, 2, 3])),
%!        [1; 2; 3; 4] - [1, 2, 3])
%!assert (full (sparse ([1, 2, 3, 4, 5]) - sparse ([1; 2; 3])),
%!        [1, 2, 3, 4, 5] - [1; 2; 3])
%!assert (full (sparse ([1, 0, 3; 0, 5, 6]) - sparse ([1, 0, 3])),
%!        [1, 0, 3; 0, 5, 6] - [1, 0, 3])
%!assert (full (sparse ([1, 2; 1, 0; 0, 0]) - sparse ([1; 2; 0])),
%!        [1, 2; 1, 0; 0, 0] - [1; 2; 0])
%!assert (full (sparse ([1; 2; 3; 4]) - sparse ([0, 0, 3])),
%!        [1; 2; 3; 4] - [0, 0, 3])
%!assert (full (sparse ([1, 0, 0, 0, 5]) - sparse ([1; 2; 3])),
%!        [1, 0, 0, 0, 5] - [1; 2; 3])

%!assert (sparse (1) - sparse ([1, 2, 3]), sparse ([0, -1, -2]))
%!assert (sparse (1) - sparse ([1, 2; 3, 4]), sparse ([0, -1; -2, -3]))
%!assert (sparse ([1, 2, 3]) - sparse (1), sparse ([0, 1, 2]))
%!assert (sparse ([1, 2; 3, 4]) - sparse (1), sparse ([0, 1; 2, 3]))
%!assert (full (sparse ([1; 2]) - sparse ([1, 0, 2, 0, 4])),
%!        [1; 2] - [1, 0, 2, 0, 4])
%!assert (full (sparse ([1, 0, 2, 0, 4]) - sparse ([1; 2])),
%!        [1, 0, 2, 0, 4] - [1; 2])
%!assert (full (sparse ([1, 2]) - sparse ([1; 0; 2; 0; 4])),
%!        [1, 2] - [1; 0; 2; 0; 4])
%!assert (full (sparse ([1; 0; 2; 0; 4]) - sparse ([1, 2])),
%!        [1; 0; 2; 0; 4] - [1, 2])
%!assert (full (sparse ([1, 0, 3]) - sparse ([1, 2, 3; 4, 0, 0])),
%!        [1, 0, 3] - [1, 2, 3; 4, 0, 0])
%!assert (full (sparse ([10, 0, 3; 4, 5, 0]) - sparse ([1, 0, 3])),
%!        [10, 0, 3; 4, 5, 0] - [1, 0, 3])
%!assert (full (sparse ([1; 2; 0]) - sparse ([0, 2; 3, 4; 5, 0])),
%!        [1; 2; 0] - [0, 2; 3, 4; 5, 0])
%!assert (full (sparse ([1, 2; 0, 0; 0, 6]) - sparse ([0; 2; 3])),
%!        [1, 2; 0, 0; 0, 6] - [0; 2; 3])

%!assert (full (sparse ([1, 2, 3]) .* sparse ([1, 2, 3; 4, 5, 6])),
%!        [1, 2, 3] .* [1, 2, 3; 4, 5, 6])
%!assert (full (sparse ([1; 2; 3]) .* sparse ([1, 2; 1, 2; 1, 2])),
%!        [1; 2; 3] .* [1, 2; 1, 2; 1, 2])
%!assert (full (sparse ([1, 2, 3]) .* sparse ([1; 2; 3; 4])),
%!        [1, 2, 3] .* [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) .* sparse ([1, 2, 3, 4, 5])),
%!        [1; 2; 3] .* [1, 2, 3, 4, 5])
%!assert (full (sparse ([1, 0, 3]) .* sparse ([1, 0, 3; 0, 5, 6])),
%!        [1, 0, 3] .* [1, 0, 3; 0, 5, 6])
%!assert (full (sparse ([1; 2; 0]) .* sparse ([1, 2; 1, 0; 0, 0])),
%!        [1; 2; 0] .* [1, 2; 1, 0; 0, 0])
%!assert (full (sparse ([0, 0, 3]) .* sparse ([1; 2; 3; 4])),
%!        [0, 0, 3] .* [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) .* sparse ([1, 0, 0, 0, 5])),
%!        [1; 2; 3] .* [1, 0, 0, 0, 5])
%!assert (full (sparse ([1, 2, 3; 4, 5, 6]) .* sparse ([1, 2, 3])),
%!        [1, 2, 3; 4, 5, 6] .* [1, 2, 3])
%!assert (full (sparse ([1, 2; 1, 2; 1, 2]) .* sparse ([1; 2; 3])),
%!        [1, 2; 1, 2; 1, 2] .* [1; 2; 3])
%!assert (full (sparse ([1; 2; 3; 4]) .* sparse ([1, 2, 3])),
%!        [1; 2; 3; 4] .* [1, 2, 3])
%!assert (full (sparse ([1, 2, 3, 4, 5]) .* sparse ([1; 2; 3])),
%!        [1, 2, 3, 4, 5] .* [1; 2; 3])
%!assert (full (sparse ([1, 0, 3; 0, 5, 6]) .* sparse ([1, 0, 3])),
%!        [1, 0, 3; 0, 5, 6] .* [1, 0, 3])
%!assert (full (sparse ([1, 2; 1, 0; 0, 0]) .* sparse ([1; 2; 0])),
%!        [1, 2; 1, 0; 0, 0] .* [1; 2; 0])
%!assert (full (sparse ([1; 2; 3; 4]) .* sparse ([0, 0, 3])),
%!        [1; 2; 3; 4] .* [0, 0, 3])
%!assert (full (sparse ([1, 0, 0, 0, 5]) .* sparse ([1; 2; 3])),
%!        [1, 0, 0, 0, 5] .* [1; 2; 3])

%!assert (sparse (1) .* sparse ([1, 2, 3]), sparse ([1, 2, 3]))
%!assert (sparse (1) .* sparse ([1, 2; 3, 4]), sparse ([1, 2; 3, 4]))
%!assert (sparse ([1, 2, 3]) .* sparse (1), sparse ([1, 2, 3]))
%!assert (sparse ([1, 2; 3, 4]) .* sparse (1), sparse ([1, 2; 3, 4]))
%!assert (full (sparse ([1; 2]) .* sparse ([1, 0, 2, 0, 4])),
%!        [1; 2] .* [1, 0, 2, 0, 4])
%!assert (full (sparse ([1, 0, 2, 0, 4]) .* sparse ([1; 2])),
%!        [1, 0, 2, 0, 4] .* [1; 2])
%!assert (full (sparse ([1, 2]) .* sparse ([1; 0; 2; 0; 4])),
%!        [1, 2] .* [1; 0; 2; 0; 4])
%!assert (full (sparse ([1; 0; 2; 0; 4]) .* sparse ([1, 2])),
%!        [1; 0; 2; 0; 4] .* [1, 2])
%!assert (full (sparse ([1, 0, 3]) .* sparse ([1, 2, 3; 4, 0, 0])),
%!        [1, 0, 3] .* [1, 2, 3; 4, 0, 0])
%!assert (full (sparse ([10, 0, 3; 4, 5, 0]) .* sparse ([1, 0, 3])),
%!        [10, 0, 3; 4, 5, 0] .* [1, 0, 3])
%!assert (full (sparse ([1; 2; 0]) .* sparse ([0, 2; 3, 4; 5, 0])),
%!        [1; 2; 0] .* [0, 2; 3, 4; 5, 0])
%!assert (full (sparse ([1, 2; 0, 0; 0, 6]) .* sparse ([0; 2; 3])),
%!        [1, 2; 0, 0; 0, 6] .* [0; 2; 3])

%!assert (full (sparse ([1, 2, 3]) ./ sparse ([1, 2, 3; 4, 5, 6])),
%!        [1, 2, 3] ./ [1, 2, 3; 4, 5, 6])
%!assert (full (sparse ([1; 2; 3]) ./ sparse ([1, 2; 1, 2; 1, 2])),
%!        [1; 2; 3] ./ [1, 2; 1, 2; 1, 2])
%!assert (full (sparse ([1, 2, 3]) ./ sparse ([1; 2; 3; 4])),
%!        [1, 2, 3] ./ [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) ./ sparse ([1, 2, 3, 4, 5])),
%!        [1; 2; 3] ./ [1, 2, 3, 4, 5])
%!assert (full (sparse ([1, 0, 3]) ./ sparse ([1, 0, 3; 0, 5, 6])),
%!        [1, 0, 3] ./ [1, 0, 3; 0, 5, 6])
%!assert (full (sparse ([1; 2; 0]) ./ sparse ([1, 2; 1, 0; 0, 0])),
%!        [1; 2; 0] ./ [1, 2; 1, 0; 0, 0])
%!assert (full (sparse ([0, 0, 3]) ./ sparse ([1; 2; 3; 4])),
%!        [0, 0, 3] ./ [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3]) ./ sparse ([1, 0, 0, 0, 5])),
%!        [1; 2; 3] ./ [1, 0, 0, 0, 5])
%!assert (full (sparse ([1, 2, 3; 4, 5, 6]) ./ sparse ([1, 2, 3])),
%!        [1, 2, 3; 4, 5, 6] ./ [1, 2, 3])
%!assert (full (sparse ([1, 2; 1, 2; 1, 2]) ./ sparse ([1; 2; 3])),
%!        [1, 2; 1, 2; 1, 2] ./ [1; 2; 3])
%!assert (full (sparse ([1; 2; 3; 4]) ./ sparse ([1, 2, 3])),
%!        [1; 2; 3; 4] ./ [1, 2, 3])
%!assert (full (sparse ([1, 2, 3, 4, 5]) ./ sparse ([1; 2; 3])),
%!        [1, 2, 3, 4, 5] ./ [1; 2; 3])
%!assert (full (sparse ([1, 0, 3; 0, 5, 6]) ./ sparse ([1, 0, 3])),
%!        [1, 0, 3; 0, 5, 6] ./ [1, 0, 3])
%!assert (full (sparse ([1, 2; 1, 0; 0, 0]) ./ sparse ([1; 2; 0])),
%!        [1, 2; 1, 0; 0, 0] ./ [1; 2; 0])
%!assert (full (sparse ([1; 2; 3; 4]) ./ sparse ([0, 0, 3])),
%!        [1; 2; 3; 4] ./ [0, 0, 3])
%!assert (full (sparse ([1, 0, 0, 0, 5]) ./ sparse ([1; 2; 3])),
%!        [1, 0, 0, 0, 5] ./ [1; 2; 3])

%!assert (sparse (1) ./ sparse ([1, 2, 3]), sparse ([1, 1/2, 1/3]))
%!assert (sparse (1) ./ sparse ([1, 2; 3, 4]), sparse ([1, 1/2; 1/3, 1/4]))
%!assert (sparse ([1, 2, 3]) ./ sparse (1), sparse ([1, 2, 3]))
%!assert (sparse ([1, 2; 3, 4]) ./ sparse (1), sparse ([1, 2; 3, 4]))
%!assert (full (sparse ([1; 2]) ./ sparse ([1, 0, 2, 0, 4])),
%!        [1; 2] ./ [1, 0, 2, 0, 4])
%!assert (full (sparse ([1, 0, 2, 0, 4]) ./ sparse ([1; 2])),
%!        [1, 0, 2, 0, 4] ./ [1; 2])
%!assert (full (sparse ([1, 2]) ./ sparse ([1; 0; 2; 0; 4])),
%!        [1, 2] ./ [1; 0; 2; 0; 4])
%!assert (full (sparse ([1; 0; 2; 0; 4]) ./ sparse ([1, 2])),
%!        [1; 0; 2; 0; 4] ./ [1, 2])
%!assert (full (sparse ([1, 0, 3]) ./ sparse ([1, 2, 3; 4, 0, 0])),
%!        [1, 0, 3] ./ [1, 2, 3; 4, 0, 0])
%!assert (full (sparse ([10, 0, 3; 4, 5, 0]) ./ sparse ([1, 0, 3])),
%!        [10, 0, 3; 4, 5, 0] ./ [1, 0, 3])
%!assert (full (sparse ([1; 2; 0]) ./ sparse ([0, 2; 3, 4; 5, 0])),
%!        [1; 2; 0] ./ [0, 2; 3, 4; 5, 0])
%!assert (full (sparse ([1, 2; 0, 0; 0, 6]) ./ sparse ([0; 2; 3])),
%!        [1, 2; 0, 0; 0, 6] ./ [0; 2; 3])

## Test broadcasting for math operations with scalar and sparse matrix
%!assert (sparse ([1, 2]) + 1, [2, 3])
%!assert (1 + sparse ([1, 2]), [2, 3])
%!assert (sparse ([1, 2]) - 1, [0, 1])
%!assert (1 - sparse ([1, 2]), [0, -1])
%!assert (sparse ([1, 2]) .* 1, sparse ([1, 2]))
%!assert (1 .* sparse ([1, 2]), sparse ([1, 2]))
%!assert (sparse ([1, 2]) ./ 1, sparse ([1, 2]))
%!assert (1 ./ sparse ([1, 2]), [1, 0.5])
%!error <operator /: nonconformant arguments \(op1 is 1x1, op2 is 1x2\)>
%! 1 / sparse ([1, 2])

## Test broadcasting for math operations with matrix and sparse matrix
%!assert (sparse ([1, 2]) + [1, 1], [2, 3])
%!assert ([1, 1] + sparse ([1, 2]), [2, 3])
%!assert (sparse ([1; 2]) + [1; 1], [2; 3])
%!assert ([1; 1] + sparse ([1; 2]), [2; 3])
%!assert (sparse ([1, 2, 3]) + ones (3), repmat ([2:4], 3, 1))
%!assert (sparse ([1; 2; 3]) + ones (3), repmat ([2:4]', 1, 3))
%!assert (ones (3) + sparse ([1, 2, 3]), repmat ([2:4], 3, 1))
%!assert (ones (3) + sparse ([1; 2; 3]), repmat ([2:4]', 1, 3))
%!assert (sparse ([1, 2]) + [1; 2], [2, 3; 3, 4])
%!assert ([1; 2] + sparse ([1, 2]), [2, 3; 3, 4])
%!assert (sparse ([1; 2]) + [1, 2], [2, 3; 3, 4])
%!assert ([1, 2] + sparse ([1; 2]), [2, 3; 3, 4])
%!error <operator \+: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2]) + [1, 1, 1]
%!error <operator \+: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3]) + ones (4)
%!error <operator \+: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3]) + ones (4, 3)

%!assert (sparse ([1, 2]) - [1, 1], [0, 1])
%!assert ([1, 1] - sparse ([1, 2]), [0, -1])
%!assert (sparse ([1; 2]) - [1; 1], [0; 1])
%!assert ([1; 1] - sparse ([1; 2]), [0; -1])
%!assert (sparse ([1, 2, 3]) - ones (3), repmat ([0:2], 3, 1))
%!assert (sparse ([1; 2; 3]) - ones (3), repmat ([0:2]', 1, 3))
%!assert (ones (3) - sparse ([1, 2, 3]), repmat ([0:-1:-2], 3, 1))
%!assert (ones (3) - sparse ([1; 2; 3]), repmat ([0:-1:-2]', 1, 3))
%!assert (sparse ([1, 2]) - [1; 2], [0, 1; -1, 0])
%!assert ([1; 2] - sparse ([1, 2]), [0, -1; 1, 0])
%!assert (sparse ([1; 2]) - [1, 2], [0, -1; 1, 0])
%!assert ([1, 2] - sparse ([1; 2]), [0, 1; -1, 0])
%!error <operator -: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2]) - [1, 1, 1]
%!error <operator -: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3]) - ones (4)
%!error <operator -: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3]) - ones (4, 3)

%!assert (sparse ([1, 2]) .* [1, 1], sparse ([1, 2]))
%!assert ([1, 1] .* sparse ([1, 2]), sparse ([1, 2]))
%!assert (sparse ([1; 2]) .* [1; 1], sparse ([1; 2]))
%!assert ([1; 1] .* sparse ([1; 2]), sparse ([1; 2]))
%!assert (sparse ([1, 2, 3]) .* ones (3), sparse (repmat ([1:3], 3, 1)))
%!assert (sparse ([1; 2; 3]) .* ones (3), sparse (repmat ([1:3]', 1, 3)))
%!assert (ones (3) .* sparse ([1, 2, 3]), sparse (repmat ([1:3], 3, 1)))
%!assert (ones (3) .* sparse ([1; 2; 3]), sparse (repmat ([1:3]', 1, 3)))
%!assert (sparse ([1, 2]) .* [1; 2], sparse ([1, 2; 2, 4]))
%!assert ([1; 2] .* sparse ([1, 2]), sparse ([1, 2; 2, 4]))
%!assert (sparse ([1; 2]) .* [1, 2], sparse ([1, 2; 2, 4]))
%!assert ([1, 2] .* sparse ([1; 2]), sparse ([1, 2; 2, 4]))
%!error <product: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2]) .* [1, 1, 1]
%!error <product: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3]) .* ones (4)
%!error <product: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3]) .* ones (4, 3)
%!error <operator \*: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2]) * [1, 1, 1]
%!error <operator \*: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3]) * ones (4)
%!error <operator \*: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3]) * ones (4, 3)

%!assert (sparse ([1, 2]) ./ [1, 1], sparse ([1, 2]))
%!assert ([1, 1] ./ sparse ([1, 2]), [1, 0.5])
%!assert (sparse ([1; 2]) ./ [1; 1], sparse ([1; 2]))
%!assert ([1; 1] ./ sparse ([1; 2]), [1; 0.5])
%!assert (sparse ([1, 2, 3]) ./ ones (3), sparse (repmat ([1:3], 3, 1)))
%!assert (sparse ([1; 2; 3]) ./ ones (3), sparse (repmat ([1:3]', 1, 3)))
%!assert (ones (3) ./ sparse ([1, 2, 3]), repmat ([1, 0.5, 1/3], 3, 1))
%!assert (ones (3) ./ sparse ([1; 2; 3]), repmat ([1, 0.5, 1/3]', 1, 3))
%!assert (sparse ([1, 2]) ./ [1; 2], sparse ([1, 2; 0.5, 1]))
%!assert ([1; 2] ./ sparse ([1, 2]), [1, 0.5; 2, 1])
%!assert (sparse ([1; 2]) ./ [1, 2], sparse ([1, 0.5; 2, 1]))
%!assert ([1, 2] ./ sparse ([1; 2]), [1, 2; 0.5, 1])
%!error <quotient: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2]) ./ [1, 1, 1]
%!error <quotient: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3]) ./ ones (4)
%!error <quotient: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3]) ./ ones (4, 3)
%!error <operator /: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2]) / [1, 1, 1]
%!error <operator /: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3]) / ones (4)
%!error <operator /: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3]) / ones (4, 3)

## Test broadcasting for comparison and boolean operators
## for sparse matrix to matrix and matrix to sparse matrix
%!assert (sparse ([1, 1, 0]) == [0, 1, 1], sparse (logical ([0, 1, 0])))
%!assert (sparse ([1; 1; 0]) == [0, 1, 1], sparse ([1; 1; 0] == [0, 1, 1]))
%!assert (sparse ([1, 1, 0]) == [0; 1; 1; 0], ...
%!        sparse ([1, 1, 0] == [0; 1; 1; 0]))
%!assert (sparse ([1, 1, 0]) == [0, 1, 1; 0, 1, 1], ...
%!        sparse (logical ([0, 1, 0; 0, 1, 0])))
%!assert (sparse ([1; 1; 0]) == [0, 1; 1, 1; 0, 1], ...
%!        sparse (logical ([0, 1; 1, 1; 1, 0])))
%!assert ([1, 0, 1] == sparse ([1, 1, 0]), sparse (logical ([1, 0, 0])))
%!assert ([0, 1, 1] == sparse ([1; 1; 0]), sparse ([0, 1, 1] == [1; 1; 0]))
%!assert ([0; 1; 1; 0] == sparse ([1, 1, 0]), ...
%!        sparse ([0; 1; 1; 0] == [1, 1, 0]))
%!assert ([0, 1, 1; 0, 1, 1] == sparse ([1, 1, 0]), ...
%!        sparse (logical ([0, 1, 0; 0, 1, 0])))
%!assert ([0, 1; 1, 1; 0, 1] == sparse ([1; 1; 0]), ...
%!        sparse (logical ([0, 1; 1, 1; 1, 0])))

%!assert (sparse ([1, 1, 0]) != [0, 1, 1], sparse (logical ([1, 0, 1])))
%!assert (sparse ([1; 1; 0]) != [0, 1, 1], sparse ([1; 1; 0] != [0, 1, 1]))
%!assert (sparse ([1, 1, 0]) != [0; 1; 1; 0], ...
%!        sparse ([1, 1, 0] != [0; 1; 1; 0]))
%!assert (sparse ([1, 1, 0]) != [0, 1, 1; 0, 1, 1], ...
%!        sparse (logical ([1, 0, 1; 1, 0, 1])))
%!assert (sparse ([1; 1; 0]) != [0, 1; 1, 1; 0, 1], ...
%!        sparse (logical ([1, 0; 0, 0; 0, 1])))
%!assert ([1, 0, 1] != sparse ([1, 1, 0]), sparse (logical ([0, 1, 1])))
%!assert ([0, 1, 1] != sparse ([1; 1; 0]), sparse ([0, 1, 1] != [1; 1; 0]))
%!assert ([0; 1; 1; 0] != sparse ([1, 1, 0]), ...
%!        sparse ([0; 1; 1; 0] != [1, 1, 0]))
%!assert ([0, 1, 1; 0, 1, 1] != sparse ([1, 1, 0]), ...
%!        sparse (logical ([1, 0, 1; 1, 0, 1])))
%!assert ([0, 1; 1, 1; 0, 1] != sparse ([1; 1; 0]), ...
%!        sparse (logical ([1, 0; 0, 0; 0, 1])))

%!assert (sparse ([1, 1, 0]) <= [0, 1, 1], sparse (logical ([0, 1, 1])))
%!assert (sparse ([1; 1; 0]) <= [0, 1, 1], sparse ([1; 1; 0] <= [0, 1, 1]))
%!assert (sparse ([1, 1, 0]) <= [0; 1; 1; 0], ...
%!        sparse ([1, 1, 0] <= [0; 1; 1; 0]))
%!assert (sparse ([1, 1, 0]) <= [0, 1, 1; 0, 1, 1], ...
%!        sparse (logical ([0, 1, 1; 0, 1, 1])))
%!assert (sparse ([1; 1; 0]) <= [0, 1; 1, 1; 0, 1], ...
%!        sparse (logical ([0, 1; 1, 1; 1, 1])))
%!assert ([1, 0, 1] <= sparse ([1, 1, 0]), sparse (logical ([1, 1, 0])))
%!assert ([0, 1, 1] <= sparse ([1; 1; 0]), sparse ([0, 1, 1] <= [1; 1; 0]))
%!assert ([0; 1; 1; 0] <= sparse ([1, 1, 0]), ...
%!        sparse ([0; 1; 1; 0] <= [1, 1, 0]))
%!assert ([0, 1, 1; 0, 1, 1] <= sparse ([1, 1, 0]), ...
%!        sparse (logical ([1, 1, 0; 1, 1, 0])))
%!assert ([0, 1; 1, 1; 0, 1] <= sparse ([1; 1; 0]), ...
%!        sparse (logical ([1, 1; 1, 1; 1, 0])))

%!assert (sparse ([1, 1, 0]) >= [0, 1, 1], sparse (logical ([1, 1, 0])))
%!assert (sparse ([1; 1; 0]) >= [0, 1, 1], sparse ([1; 1; 0] >= [0, 1, 1]))
%!assert (sparse ([1, 1, 0]) >= [0; 1; 1; 0], ...
%!        sparse ([1, 1, 0] >= [0; 1; 1; 0]))
%!assert (sparse ([1, 1, 0]) >= [0, 1, 1; 0, 1, 1], ...
%!        sparse (logical ([1, 1, 0; 1, 1, 0])))
%!assert (sparse ([1; 1; 0]) >= [0, 1; 1, 1; 0, 1], ...
%!        sparse (logical ([1, 1; 1, 1; 1, 0])))
%!assert ([1, 0, 1] >= sparse ([1, 1, 0]), sparse (logical ([1, 0, 1])))
%!assert ([0, 1, 1] >= sparse ([1; 1; 0]), sparse ([0, 1, 1] >= [1; 1; 0]))
%!assert ([0; 1; 1; 0] >= sparse ([1, 1, 0]), ...
%!        sparse ([0; 1; 1; 0] >= [1, 1, 0]))
%!assert ([0, 1, 1; 0, 1, 1] >= sparse ([1, 1, 0]), ...
%!        sparse (logical ([0, 1, 1; 0, 1, 1])))
%!assert ([0, 1; 1, 1; 0, 1] >= sparse ([1; 1; 0]), ...
%!        sparse (logical ([0, 1; 1, 1; 1, 1])))

%!assert (sparse ([1, 1, 0]) < [0, 1, 1], sparse (logical ([0, 0, 1])))
%!assert (sparse ([1; 1; 0]) < [0, 1, 1], sparse ([1; 1; 0] < [0, 1, 1]))
%!assert (sparse ([1, 1, 0]) < [0; 1; 1; 0], sparse ([1, 1, 0] < [0; 1; 1; 0]))
%!assert (sparse ([1, 1, 0]) < [0, 1, 1; 0, 1, 1], ...
%!        sparse (logical ([0, 0, 1; 0, 0, 1])))
%!assert (sparse ([1; 1; 0]) < [0, 1; 1, 1; 0, 1], ...
%!        sparse (logical ([0, 0; 0, 0; 0, 1])))
%!assert ([1, 0, 1] < sparse ([1, 1, 0]), sparse (logical ([0, 1, 0])))
%!assert ([0, 1, 1] < sparse ([1; 1; 0]), sparse ([0, 1, 1] < [1; 1; 0]))
%!assert ([0; 1; 1; 0] < sparse ([1, 1, 0]), sparse ([0; 1; 1; 0] < [1, 1, 0]))
%!assert ([0, 1, 1; 0, 1, 1] < sparse ([1, 1, 0]), ...
%!        sparse (logical ([1, 0, 0; 1, 0, 0])))
%!assert ([0, 1; 1, 1; 0, 1] < sparse ([1; 1; 0]), ...
%!        sparse (logical ([1, 0; 0, 0; 0, 0])))

%!assert (sparse ([1, 1, 0]) > [0, 1, 1], sparse (logical ([1, 0, 0])))
%!assert (sparse ([1; 1; 0]) > [0, 1, 1], sparse ([1; 1; 0] > [0, 1, 1]))
%!assert (sparse ([1, 1, 0]) > [0; 1; 1; 0], sparse ([1, 1, 0] > [0; 1; 1; 0]))
%!assert (sparse ([1, 1, 0]) > [0, 1, 1; 0, 1, 1], ...
%!        sparse (logical ([1, 0, 0; 1, 0, 0])))
%!assert (sparse ([1; 1; 0]) > [0, 1; 1, 1; 0, 1], ...
%!        sparse (logical ([1, 0; 0, 0; 0, 0])))
%!assert ([1, 0, 1] > sparse ([1, 1, 0]), sparse (logical ([0, 0, 1])))
%!assert ([0, 1, 1] > sparse ([1; 1; 0]), sparse ([0, 1, 1] > [1; 1; 0]))
%!assert ([0; 1; 1; 0] > sparse ([1, 1, 0]), sparse ([0; 1; 1; 0] > [1, 1, 0]))
%!assert ([0, 1, 1; 0, 1, 1] > sparse ([1, 1, 0]), ...
%!        sparse (logical ([0, 0, 1; 0, 0, 1])))
%!assert ([0, 1; 1, 1; 0, 1] > sparse ([1; 1; 0]), ...
%!        sparse (logical ([0, 0; 0, 0; 0, 1])))

%!assert (sparse ([1, 1, 0]) & [0, 1, 1], sparse (logical ([0, 1, 0])))
%!assert (sparse ([1; 1; 0]) & [0, 1, 1], sparse ([1; 1; 0] & [0, 1, 1]))
%!assert (sparse ([1, 1, 0]) & [0; 1; 1; 0], sparse ([1, 1, 0] & [0; 1; 1; 0]))
%!assert (sparse ([1, 1, 0]) & [0, 1, 1; 0, 1, 1], ...
%!        sparse (logical ([0, 1, 0; 0, 1, 0])))
%!assert (sparse ([1; 1; 0]) & [0, 1; 1, 1; 0, 1], ...
%!        sparse (logical ([0, 1; 1, 1; 0, 0])))
%!assert ([1, 0, 1] & sparse ([1, 1, 0]), sparse (logical ([1, 0, 0])))
%!assert ([0, 1, 1] & sparse ([1; 1; 0]), sparse ([0, 1, 1] & [1; 1; 0]))
%!assert ([0; 1; 1; 0] & sparse ([1, 1, 0]), sparse ([0; 1; 1; 0] & [1, 1, 0]))
%!assert ([0, 1, 1; 0, 1, 1] & sparse ([1, 1, 0]), ...
%!        sparse (logical ([0, 1, 0; 0, 1, 0])))
%!assert ([0, 1; 1, 1; 0, 1] & sparse ([1; 1; 0]), ...
%!        sparse (logical ([0, 1; 1, 1; 0, 0])))

## Test boolean OR operations should return dense logical arrays
%!assert (sparse ([1, 1, 0]) | [0, 1, 1], logical ([1, 1, 1]))
%!assert (sparse ([1; 1; 0]) | [0, 1, 1], [1; 1; 0] | [0, 1, 1])
%!assert (sparse ([1, 1, 0]) | [0; 1; 1; 0], [1, 1, 0] | [0; 1; 1; 0])
%!assert (sparse ([1, 1, 0]) | [0, 1, 1; 0, 1, 1], logical ([1, 1, 1; 1, 1, 1]))
%!assert (sparse ([1; 1; 0]) | [0, 1; 1, 1; 0, 1], logical ([1, 1; 1, 1; 0, 1]))
%!assert ([1, 0, 1] | sparse ([1, 1, 0]), logical ([1, 1, 1]))
%!assert ([0, 1, 1] | sparse ([1; 1; 0]), [0, 1, 1] | [1; 1; 0])
%!assert ([0; 1; 1; 0] | sparse ([1, 1, 0]), [0; 1; 1; 0] | [1, 1, 0])
%!assert ([0, 1, 1; 0, 1, 1] | sparse ([1, 1, 0]), logical ([1, 1, 1; 1, 1, 1]))
%!assert ([0, 1; 1, 1; 0, 1] | sparse ([1; 1; 0]), logical ([1, 1; 1, 1; 0, 1]))

## Test broadcasting for comparison and boolean operators
## for sparse matrix to scalar and scalar to sparse matrix
%!assert (sparse ([1, 1, 0]) == 1, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]) != 1, sparse (logical ([0, 0, 1])))
%!assert (sparse ([1, 1, 0]) <= 1, sparse (logical ([1, 1, 1])))
%!assert (sparse ([1, 1, 0]) >= 1, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]) < 1, sparse (logical ([0, 0, 1])))
%!assert (sparse ([1, 1, 0]) > 0, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]) & 1, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]) | 1, logical ([1, 1, 1]))
%!assert (sparse ([1, 1, 0]) == 1, 1 == sparse ([1, 1, 0]))
%!assert (sparse ([1, 1, 0]) != 1, 1 != sparse ([1, 1, 0]))
%!assert (sparse ([1, 1, 0]) <= 1, 1 >= sparse ([1, 1, 0]))
%!assert (sparse ([1, 1, 0]) >= 1, 1 <= sparse ([1, 1, 0]))
%!assert (sparse ([1, 1, 0]) < 1, 1 > sparse ([1, 1, 0]))
%!assert (sparse ([1, 1, 0]) > 0, 0 < sparse ([1, 1, 0]))
%!assert (sparse ([1, 1, 0]) & 1, 1 & sparse ([1, 1, 0]))
%!assert (sparse ([1, 1, 0]) | 1, 1 | sparse ([1, 1, 0]))

## Test some empty edge cases
%!assert (sparse (1) | sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1 | sparse (ones(0,1)), logical (ones (0, 1)))
%!assert (sparse (1) & sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1 & sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (sparse (1) | sparse (ones(2,0)), sparse (logical (ones (2, 0))))
%!assert (1 | sparse (ones(2, 0)), logical (ones (2, 0)))
%!assert (sparse (1) & sparse (ones(2,0)), sparse (logical (ones (2, 0))))
%!assert (1 & sparse (ones(2, 0)), sparse (logical (ones (2, 0))))
%!assert (sparse (1) == sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1 != sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (sparse (1) <= sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1 >= sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (sparse (1) < sparse (ones(2,0)), sparse (logical (ones (2, 0))))
%!assert (1 > sparse (ones(2, 0)), sparse (logical (ones (2, 0))))

## Test errors
%!error <mx_el_eq: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) == ones (2,0)
%!error <mx_el_ne: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) != ones (2,0)
%!error <mx_el_le: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) <= ones (2,0)
%!error <mx_el_ge: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) >= ones (2,0)
%!error <mx_el_lt: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) < ones (2,0)
%!error <mx_el_gt: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) > ones (2,0)
%!error <mx_el_and: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) & ones (2,0)
%!error <mx_el_or: nonconformant arguments \(op1 is 0x2, op2 is 2x0\)>
%! sparse (ones (0,2)) | ones (2,0)

## Test broadcasting for comparison and boolean operators
## for sparse matrix to sparse matrix
%!assert (sparse([1, 1, 0]) == sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] == [0, 1, 1]))
%!assert (sparse([1; 1; 0]) == sparse([0, 1, 1]),...
%!        sparse ([1; 1; 0] == [0, 1, 1]))
%!assert (sparse([1, 1, 0]) == sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] == [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) == sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] == [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) == sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] == [0, 1; 1, 1; 0, 1]))
%!assert (sparse([1, 1, 0]) != sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] != [0, 1, 1]))
%!assert (sparse([1; 1; 0]) != sparse([0, 1, 1]), ...
%!        sparse ([1; 1; 0] != [0, 1, 1]))
%!assert (sparse([1, 1, 0]) != sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] != [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) != sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] != [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) != sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] != [0, 1; 1, 1; 0, 1]))
%!assert (sparse([1, 1, 0]) <= sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] <= [0, 1, 1]))
%!assert (sparse([1; 1; 0]) <= sparse([0, 1, 1]), ...
%!        sparse ([1; 1; 0] <= [0, 1, 1]))
%!assert (sparse([1, 1, 0]) <= sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] <= [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) <= sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] <= [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) <= sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] <= [0, 1; 1, 1; 0, 1]))
%!assert (sparse([1, 1, 0]) >= sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] >= [0, 1, 1]))
%!assert (sparse([1; 1; 0]) >= sparse([0, 1, 1]), ...
%!        sparse ([1; 1; 0] >= [0, 1, 1]))
%!assert (sparse([1, 1, 0]) >= sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] >= [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) >= sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] >= [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) >= sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] >= [0, 1; 1, 1; 0, 1]))
%!assert (sparse([1, 1, 0]) < sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] < [0, 1, 1]))
%!assert (sparse([1; 1; 0]) < sparse([0, 1, 1]), ...
%!        sparse ([1; 1; 0] < [0, 1, 1]))
%!assert (sparse([1, 1, 0]) < sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] < [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) < sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] < [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) < sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] < [0, 1; 1, 1; 0, 1]))
%!assert (sparse([1, 1, 0]) > sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] > [0, 1, 1]))
%!assert (sparse([1; 1; 0]) > sparse([0, 1, 1]), ...
%!        sparse ([1; 1; 0] > [0, 1, 1]))
%!assert (sparse([1, 1, 0]) > sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] > [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) > sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] > [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) > sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] > [0, 1; 1, 1; 0, 1]))
%!assert (sparse([1, 1, 0]) & sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] & [0, 1, 1]))
%!assert (sparse([1; 1; 0]) & sparse([0, 1, 1]), ...
%!        sparse ([1; 1; 0] & [0, 1, 1]))
%!assert (sparse([1, 1, 0]) & sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] & [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) & sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] & [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) & sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] & [0, 1; 1, 1; 0, 1]))
%!assert (sparse([1, 1, 0]) | sparse([0, 1, 1]), ...
%!        sparse ([1, 1, 0] | [0, 1, 1]))
%!assert (sparse([1; 1; 0]) | sparse([0, 1, 1]), ...
%!        sparse ([1; 1; 0] | [0, 1, 1]))
%!assert (sparse([1, 1, 0]) | sparse([0; 1; 1; 0]), ...
%!        sparse ([1, 1, 0] | [0; 1; 1; 0]))
%!assert (sparse([1, 1, 0]) | sparse([0, 1, 1; 0, 1, 1]), ...
%!        sparse ([1, 1, 0] | [0, 1, 1; 0, 1, 1]))
%!assert (sparse([1; 1; 0]) | sparse([0, 1; 1, 1; 0, 1]), ...
%!        sparse ([1; 1; 0] | [0, 1; 1, 1; 0, 1]))

## Tests for SparseComplexMatrix

## Test broadcasting of empty matrices with math operators.
## This has been fixed in MSparse.cc and Sparse-op-defs.h
%!assert (sparse (zeros (0,1)) + sparse ([1, 2, 3, 4i]), sparse (zeros (0,4)))
%!error <operator \+: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) + sparse ([1, 2, 3, 4i])
%!assert (sparse (zeros (0,1)) - sparse ([1, 2, 3, 4i]), sparse (zeros (0,4)))
%!error <operator -: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) - sparse ([1, 2, 3, 4i])
%!assert (sparse (zeros (0,1)) * sparse ([1, 2, 3, 4i]), sparse (zeros (0,4)))
%!error <operator \*: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) * sparse ([1, 2, 3, 4i])
%!assert (sparse (zeros (0,1)) .* sparse ([1, 2, 3, 4i]), sparse (zeros (0,4)))
%!error <product: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) .* sparse ([1, 2, 3, 4i])
%!assert (sparse (zeros (0,1)) ./ sparse ([1, 2, 3, 4i]), sparse (zeros (0,4)))
%!error <quotient: nonconformant arguments \(op1 is 0x2, op2 is 1x4\)>
%! sparse (zeros (0,2)) ./ sparse ([1, 2, 3, 4i])
%!test
%! a = sparse (zeros (0,1));
%! assert (a += sparse ([1, 2, 3, 4i]), sparse (zeros (0,4)));
%! assert (a -= sparse ([1, 2, 3, 4i]), sparse (zeros (0,4)));

%!assert (sparse (zeros (1,0)) + sparse ([1; 2; 3; 4i]), sparse (zeros (4,0)))
%!error <operator \+: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) + sparse ([1; 2; 3; 4i])
%!assert (sparse (zeros (1,0)) - sparse ([1; 2; 3; 4i]), sparse (zeros (4,0)))
%!error <operator -: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) - sparse ([1; 2; 3; 4i])
%!error <operator \*: nonconformant arguments \(op1 is 1x0, op2 is 4x1\)>
%!       sparse (zeros (1,0)) * sparse ([1; 2; 3; 4i])
%!error <operator \*: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) * sparse ([1; 2; 3; 4i])
%!assert (sparse (zeros (1,0)) .* sparse ([1; 2; 3; 4i]), sparse (zeros (4,0)))
%!error <product: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) .* sparse ([1; 2; 3; 4i])
%!assert (sparse (zeros (1,0)) ./ sparse ([1; 2; 3; 4i]), sparse (zeros (4,0)))
%!error <quotient: nonconformant arguments \(op1 is 2x0, op2 is 4x1\)>
%! sparse (zeros (2,0)) ./ sparse ([1; 2; 3; 4i])
%!test
%! a = sparse (zeros (1,0));
%! assert (a += sparse ([1; 2; 3; 4i]), sparse (zeros (4,0)));
%! assert (a -= sparse ([1; 2; 3; 4i]), sparse (zeros (4,0)));

## Test broadcasting of sparse matrices with math operators
%!assert (full (sparse ([1, 2i, 3]) + sparse ([1, 2i, 3; 4i, 5, 6])),
%!        [1, 2i, 3] + [1, 2i, 3; 4i, 5, 6])
%!assert (full (sparse ([1; 2; 3i]) + sparse ([1, 2; 1, 2i; 1, 2i])),
%!        [1; 2; 3i] + [1, 2; 1, 2i; 1, 2i])
%!assert (full (sparse ([1i, 2, 3]) + sparse ([1i; 2; 3; 4])),
%!        [1i, 2, 3] + [1i; 2; 3; 4])
%!assert (full (sparse ([1; 2i; 3]) + sparse ([1i, 2i, 3, 4, 5])),
%!        [1; 2i; 3] + [1i, 2i, 3, 4, 5])
%!assert (full (sparse ([1, 0, 3i]) + sparse ([1, 0, 3i; 0i, 5, 6])),
%!        [1, 0, 3i] + [1, 0, 3i; 0i, 5, 6])
%!assert (full (sparse ([1; 2i; 0]) + sparse ([1i, 2i; 1, 0; 0, 0])),
%!        [1; 2i; 0] + [1i, 2i; 1, 0; 0, 0])
%!assert (full (sparse ([0, 0, 3i]) + sparse ([1; 2i; 3i; 4])),
%!        [0, 0, 3i] + [1; 2i; 3i; 4])
%!assert (full (sparse ([1; 2; 3i]) + sparse ([1, 0, 0, 0i, 5i])),
%!        [1; 2; 3i] + [1, 0, 0, 0i, 5i])
%!assert (full (sparse ([1, 2i, 3; 4, 5, 6]) + sparse ([1, 2, 3])),
%!        [1, 2i, 3; 4, 5, 6] + [1, 2, 3])
%!assert (full (sparse ([1, 2; 1, 2i; 1, 2]) + sparse ([1; 2i; 3])),
%!        [1, 2; 1, 2i; 1, 2] + [1; 2i; 3])
%!assert (full (sparse ([1; 2; 3; 4]) + sparse ([1, 2, 3i])),
%!        [1; 2; 3; 4] + [1, 2, 3i])
%!assert (full (sparse ([1, 2, 3, 4, 5i]) + sparse ([1; 2; 3])),
%!        [1, 2, 3, 4, 5i] + [1; 2; 3])
%!assert (full (sparse ([1i, 0, 3; 0, 5, 6]) + sparse ([1, 0, 3])),
%!        [1i, 0, 3; 0, 5, 6] + [1, 0, 3])
%!assert (full (sparse ([1i, 2; 1, 0; 0, 0]) + sparse ([1; 2; 0])),
%!        [1i, 2; 1, 0; 0, 0] + [1; 2; 0])
%!assert (full (sparse ([1; 2i; 3i; 4]) + sparse ([0, 0, 3])),
%!        [1; 2i; 3i; 4] + [0, 0, 3])
%!assert (full (sparse ([1, 0, 0, 0, 5i]) + sparse ([1; 2; 3i])),
%!        [1, 0, 0, 0, 5i] + [1; 2; 3i])

%!assert (sparse (1) + sparse ([1, 2i, 3]), sparse ([2, 1+2i, 4]))
%!assert (sparse (1) + sparse ([1, 2i; 3, 4]), sparse ([2, 1+2i; 4, 5]))
%!assert (sparse ([1, 2, 3i]) + sparse (1), sparse ([2, 3, 1+3i]))
%!assert (sparse ([1, 2; 3, 4]) + sparse (1i), sparse ([1+i, 2+i; 3+i, 4+i]))
%!assert (full (sparse ([1i; 2]) + sparse ([1, 0, 2, 0, 4])),
%!        [1i; 2] + [1, 0, 2, 0, 4])
%!assert (full (sparse ([1i, 0, 2, 0, 4]) + sparse ([1i; 2])),
%!        [1i, 0, 2, 0, 4] + [1i; 2])
%!assert (full (sparse ([1, 2i]) + sparse ([1; 0; 2; 0; 4])),
%!        [1, 2i] + [1; 0; 2; 0; 4])
%!assert (full (sparse ([1; 0; 2; 0i; 4i]) + sparse ([1, 2i])),
%!        [1; 0; 2; 0i; 4i] + [1, 2i])
%!assert (full (sparse ([1, 0, 3i]) + sparse ([1, 2, 3; 4, 0, 0i])),
%!        [1, 0, 3i] + [1, 2, 3; 4, 0, 0i])
%!assert (full (sparse ([10i, 0, 3; 4, 5, 0]) + sparse ([1, 0, 3])),
%!        [10i, 0, 3; 4, 5, 0] + [1, 0, 3])
%!assert (full (sparse ([1; 2i; 0]) + sparse ([0, 2; 3, 4; 5i, 0])),
%!        [1; 2i; 0] + [0, 2; 3, 4; 5i, 0])
%!assert (full (sparse ([1i, 2; 0, 0; 0, 6]) + sparse ([0; 2i; 3])),
%!        [1i, 2; 0, 0; 0, 6] + [0; 2i; 3])

%!assert (full (sparse ([1, 2, 3i]) - sparse ([1, 2, 3; 4i, 5, 6])),
%!        [1, 2, 3i] - [1, 2, 3; 4i, 5, 6])
%!assert (full (sparse ([1; 2; 3i]) - sparse ([1, 2; 1i, 2; 1, 2])),
%!        [1; 2; 3i] - [1, 2; 1i, 2; 1, 2])
%!assert (full (sparse ([1, 2, 3i]) - sparse ([1; 2; 3; 4])),
%!        [1, 2, 3i] - [1; 2; 3; 4])
%!assert (full (sparse ([1i; 2; 3]) - sparse ([1i, 2, 3i, 4, 5])),
%!        [1i; 2; 3] - [1i, 2, 3i, 4, 5])
%!assert (full (sparse ([1, 0, 3i]) - sparse ([1, 0i, 3; 0, 5, 6])),
%!        [1, 0, 3i] - [1, 0i, 3; 0, 5, 6])
%!assert (full (sparse ([1i; 2; 0]) - sparse ([1, 2; 1, 0; 0, 0])),
%!        [1i; 2; 0] - [1, 2; 1, 0; 0, 0])
%!assert (full (sparse ([0, 0, 3]) - sparse ([1; 2i; 3i; 4])),
%!        [0, 0, 3] - [1; 2i; 3i; 4])
%!assert (full (sparse ([1; 2; 3]) - sparse ([1, 0, 0, 0, 5i])),
%!        [1; 2; 3] - [1, 0, 0, 0, 5i])
%!assert (full (sparse ([1, 2, 3; 4, 5i, 6i]) - sparse ([1i, 2, 3])),
%!        [1, 2, 3; 4, 5i, 6i] - [1i, 2, 3])
%!assert (full (sparse ([1i, 2; 1, 2; 1, 2]) - sparse ([1; 2; 3])),
%!        [1i, 2; 1, 2; 1, 2] - [1; 2; 3])
%!assert (full (sparse ([1; 2; 3i; 4]) - sparse ([1, 2, 3i])),
%!        [1; 2; 3i; 4] - [1, 2, 3i])
%!assert (full (sparse ([1, 2, 3, 4, 5]) - sparse ([1; 2; 3i])),
%!        [1, 2, 3, 4, 5] - [1; 2; 3i])
%!assert (full (sparse ([1, 0, 3; 0, 5, 6i]) - sparse ([1, 0, 3])),
%!        [1, 0, 3; 0, 5, 6i] - [1, 0, 3])
%!assert (full (sparse ([1i, 2; 1, 0; 0, 0]) - sparse ([1i; 2; 0])),
%!        [1i, 2; 1, 0; 0, 0] - [1i; 2; 0])
%!assert (full (sparse ([1; 2i; 3; 4]) - sparse ([0, 0i, 3])),
%!        [1; 2i; 3; 4] - [0, 0i, 3])
%!assert (full (sparse ([1, 0, 0, 0, 5i]) - sparse ([1i; 2; 3])),
%!        [1, 0, 0, 0, 5i] - [1i; 2; 3])

%!assert (sparse (1) - sparse ([1, 2, 3i]), sparse ([0, -1, 1-3i]))
%!assert (sparse (1) - sparse ([1i, 2; 3, 4]), sparse ([1-1i, -1; -2, -3]))
%!assert (sparse ([1, 2i, 3]) - sparse (1), sparse ([0, -1+2i, 2]))
%!assert (sparse ([1, 2; 3, 4i]) - sparse (1), sparse ([0, 1; 2, -1+4i]))
%!assert (full (sparse ([1; 2i]) - sparse ([1, 0, 2, 0, 4])),
%!        [1; 2i] - [1, 0, 2, 0, 4])
%!assert (full (sparse ([1, 0, 2i, 0, 4]) - sparse ([1i; 2])),
%!        [1, 0, 2i, 0, 4] - [1i; 2])
%!assert (full (sparse ([1, 2i]) - sparse ([1; 0; 2; 0i; 4i])),
%!        [1, 2i] - [1; 0; 2; 0i; 4i])
%!assert (full (sparse ([1; 0; 2; 0; 4]) - sparse ([1, 2i])),
%!        [1; 0; 2; 0; 4] - [1, 2i])
%!assert (full (sparse ([1i, 0, 3]) - sparse ([1, 2, 3; 4, 0, 0])),
%!        [1i, 0, 3] - [1, 2, 3; 4, 0, 0])
%!assert (full (sparse ([10, 0, 3; 4i, 5i, 0]) - sparse ([1, 0, 3i])),
%!        [10, 0, 3; 4i, 5i, 0] - [1, 0, 3i])
%!assert (full (sparse ([1; 2i; 0]) - sparse ([0, 2; 3i, 4i; 5, 0])),
%!        [1; 2i; 0] - [0, 2; 3i, 4i; 5, 0])
%!assert (full (sparse ([1, 2; 0, 0; 0, 6i]) - sparse ([0; 2; 3i])),
%!        [1, 2; 0, 0; 0, 6i] - [0; 2; 3i])

%!assert (full (sparse ([1, 2i, 3]) .* sparse ([1, 2, 3; 4, 5, 6])),
%!        [1, 2i, 3] .* [1, 2, 3; 4, 5, 6])
%!assert (full (sparse ([1; 2; 3i]) .* sparse ([1, 2; 1i, 2; 1, 2])),
%!        [1; 2; 3i] .* [1, 2; 1i, 2; 1, 2])
%!assert (full (sparse ([1, 2, 3]) .* sparse ([1; 2; 3; 4i])),
%!        [1, 2, 3] .* [1; 2; 3; 4i])
%!assert (full (sparse ([1; 2i; 3i]) .* sparse ([1, 2, 3, 4, 5])),
%!        [1; 2i; 3i] .* [1, 2, 3, 4, 5])
%!assert (full (sparse ([1, 0, 3i]) .* sparse ([1i, 0, 3; 0, 5, 6])),
%!        [1, 0, 3i] .* [1i, 0, 3; 0, 5, 6])
%!assert (full (sparse ([1; 2i; 0]) .* sparse ([1, 2i; 1i, 0; 0, 0])),
%!        [1; 2i; 0] .* [1, 2i; 1i, 0; 0, 0])
%!assert (full (sparse ([0, 0i, 3i]) .* sparse ([1; 2; 3; 4])),
%!        [0, 0i, 3i] .* [1; 2; 3; 4])
%!assert (full (sparse ([1; 2; 3i]) .* sparse ([1, 0, 0i, 0, 5])),
%!        [1; 2; 3i] .* [1, 0, 0i, 0, 5])
%!assert (full (sparse ([1, 2, 3i; 4, 5, 6]) .* sparse ([1, 2, 3])),
%!        [1, 2, 3i; 4, 5, 6] .* [1, 2, 3])
%!assert (full (sparse ([1, 2; 1i, 2; 1, 2]) .* sparse ([1i; 2; 3])),
%!        [1, 2; 1i, 2; 1, 2] .* [1i; 2; 3])
%!assert (full (sparse ([1; 2; 3; 4]) .* sparse ([1, 2i, 3])),
%!        [1; 2; 3; 4] .* [1, 2i, 3])
%!assert (full (sparse ([1, 2i, 3, 4, 5]) .* sparse ([1i; 2; 3])),
%!        [1, 2i, 3, 4, 5] .* [1i; 2; 3])
%!assert (full (sparse ([1, 0, 3; 0, 5, 6]) .* sparse ([1, 0, 3i])),
%!        [1, 0, 3; 0, 5, 6] .* [1, 0, 3i])
%!assert (full (sparse ([1, 2i; 1, 0; 0i, 0]) .* sparse ([1; 2; 0])),
%!        [1, 2i; 1, 0; 0i, 0] .* [1; 2; 0])
%!assert (full (sparse ([1; 2i; 3; 4]) .* sparse ([0, 0, 3])),
%!        [1; 2i; 3; 4] .* [0, 0, 3])
%!assert (full (sparse ([1, 0, 0, 0, 5i]) .* sparse ([1; 2i; 3i])),
%!        [1, 0, 0, 0, 5i] .* [1; 2i; 3i])

%!assert (sparse (1) .* sparse ([1, 2i, 3]), sparse ([1, 2i, 3]))
%!assert (sparse (1) .* sparse ([1, 2; 3i, 4]), sparse ([1, 2; 3i, 4]))
%!assert (sparse ([1 2 3]) .* sparse (1i), sparse ([1i, 2i, 3i]))
%!assert (sparse ([1, 2; 3i, 4i]) .* sparse (1), sparse ([1, 2; 3i, 4i]))
%!assert (full (sparse ([1; 2]) .* sparse ([1, 0, 2i, 0, 4])),
%!        [1; 2] .* [1, 0, 2i, 0, 4])
%!assert (full (sparse ([1i, 0, 2, 0, 4]) .* sparse ([1; 2])),
%!        [1i, 0, 2, 0, 4] .* [1; 2])
%!assert (full (sparse ([1, 2i]) .* sparse ([1; 0; 2i; 0i; 4])),
%!        [1, 2i] .* [1; 0; 2i; 0i; 4])
%!assert (full (sparse ([1i; 0; 2; 0; 4]) .* sparse ([1, 2])),
%!        [1i; 0; 2; 0; 4] .* [1, 2])
%!assert (full (sparse ([1, 0, 3i]) .* sparse ([1i, 2i, 3; 4, 0, 0])),
%!        [1, 0, 3i] .* [1i, 2i, 3; 4, 0, 0])
%!assert (full (sparse ([10, 0, 3; 4, 5, 0]) .* sparse ([1i, 0, 3])),
%!        [10, 0, 3; 4, 5, 0] .* [1i, 0, 3])
%!assert (full (sparse ([1; 2i; 0]) .* sparse ([0, 2i; 3i, 4; 5, 0])),
%!        [1; 2i; 0] .* [0, 2i; 3i, 4; 5, 0])
%!assert (full (sparse ([1, 2i; 0, 0; 0, 6]) .* sparse ([0; 2; 3])),
%!        [1, 2i; 0, 0; 0, 6] .* [0; 2; 3])

%!assert (full (sparse ([1, 2, 3i]) ./ sparse ([1, 2, 3; 4, 5, 6])),
%!        [1, 2, 3i] ./ [1, 2, 3; 4, 5, 6])
%!assert (full (sparse ([1; 2; 3i]) ./ sparse ([1, 2; 1i, 2; 1, 2])),
%!        [1; 2; 3i] ./ [1, 2; 1i, 2; 1, 2])
%!assert (full (sparse ([1, 2, 3]) ./ sparse ([1; 2i; 3i; 4])),
%!        [1, 2, 3] ./ [1; 2i; 3i; 4])
%!assert (full (sparse ([1; 2i; 3]) ./ sparse ([1, 2, 3, 4, 5])),
%!        [1; 2i; 3] ./ [1, 2, 3, 4, 5])
%!assert (full (sparse ([1, 0, 3i]) ./ sparse ([1, 0, 3i; 0, 5, 6])),
%!        [1, 0, 3i] ./ [1, 0, 3i; 0, 5, 6])
%!assert (full (sparse ([1; 2; 0]) ./ sparse ([1, 2; 1i, 0; 0i, 0i])),
%!        [1; 2; 0] ./ [1, 2; 1i, 0; 0i, 0i])
%!assert (full (sparse ([0, 0, 3i]) ./ sparse ([1; 2; 3; 4])),
%!        [0, 0, 3i] ./ [1; 2; 3; 4])
%!assert (full (sparse ([1i; 2; 3]) ./ sparse ([1i, 0, 0, 0, 5])),
%!        [1i; 2; 3] ./ [1i, 0, 0, 0, 5])
%!assert (full (sparse ([1, 2, 3i; 4i, 5i, 6]) ./ sparse ([1, 2, 3])),
%!        [1, 2, 3i; 4i, 5i, 6] ./ [1, 2, 3])
%!assert (full (sparse ([1i, 2; 1, 2; 1, 2]) ./ sparse ([1i; 2; 3])),
%!        [1i, 2; 1, 2; 1, 2] ./ [1i; 2; 3])
%!assert (full (sparse ([1; 2; 3; 4]) ./ sparse ([1, 2i, 3])),
%!        [1; 2; 3; 4] ./ [1, 2i, 3])
%!assert (full (sparse ([1, 2i, 3, 4, 5]) ./ sparse ([1; 2; 3])),
%!        [1, 2i, 3, 4, 5] ./ [1; 2; 3])
%!assert (full (sparse ([1, 0, 3i; 0, 5i, 6]) ./ sparse ([1, 0, 3i])),
%!        [1, 0, 3i; 0, 5i, 6] ./ [1, 0, 3i])
%!assert (full (sparse ([1, 2; 1, 0; 0, 0]) ./ sparse ([1i; 2; 0])),
%!        [1, 2; 1, 0; 0, 0] ./ [1i; 2; 0])
%!assert (full (sparse ([1; 2i; 3; 4]) ./ sparse ([0, 0, 3])),
%!        [1; 2i; 3; 4] ./ [0, 0, 3])
%!assert (full (sparse ([1i, 0, 0, 0, 5i]) ./ sparse ([1; 2i; 3])),
%!        [1i, 0, 0, 0, 5i] ./ [1; 2i; 3])

%!assert (sparse (1) ./ sparse ([1, 2i, 3]), sparse ([1, -0.5i, 1/3]))
%!assert (sparse (1) ./ sparse ([1i, 2; 3, 4]), sparse ([-1i, 0.5; 1/3, 0.25]))
%!assert (sparse ([1 2 3i]) ./ sparse (1), sparse ([1, 2, 3i]))
%!assert (sparse ([1, 2; 3i, 4]) ./ sparse (1), sparse ([1, 2; 3i, 4]))
%!assert (full (sparse ([1; 2i]) ./ sparse ([1, 0, 2i, 0, 4])),
%!        [1; 2i] ./ [1, 0, 2i, 0, 4])
%!assert (full (sparse ([1, 0, 2, 0, 4i]) ./ sparse ([1; 2])),
%!        [1, 0, 2, 0, 4i] ./ [1; 2])
%!assert (full (sparse ([1i, 2]) ./ sparse ([1; 0; 2; 0i; 4i])),
%!        [1i, 2] ./ [1; 0; 2; 0i; 4i])
%!assert (full (sparse ([1i; 0; 2; 0; 4]) ./ sparse ([1, 2])),
%!        [1i; 0; 2; 0; 4] ./ [1, 2])
%!assert (full (sparse ([1, 0, 3i]) ./ sparse ([1, 2, 3; 4, 0, 0])),
%!        [1, 0, 3i] ./ [1, 2, 3; 4, 0, 0])
%!assert (full (sparse ([10i, 0, 3; 4, 5, 0]) ./ sparse ([1, 0, 3])),
%!        [10i, 0, 3; 4, 5, 0] ./ [1, 0, 3])
%!assert (full (sparse ([1; 2i; 0]) ./ sparse ([0, 2; 3, 4; 5, 0])),
%!        [1; 2i; 0] ./ [0, 2; 3, 4; 5, 0])
%!assert (full (sparse ([1i, 2i; 0, 0; 0, 6]) ./ sparse ([0; 2; 3])),
%!        [1i, 2i; 0, 0; 0, 6] ./ [0; 2; 3])

## Test broadcasting for math operations with scalar and sparse matrix
%!assert (sparse ([1, 2i]) + 1, [2, 1+2i])
%!assert (1 + sparse ([1, 2i]), [2, 1+2i])
%!assert (sparse ([1, 2]) - 1i, [1-1i, 2-1i])
%!assert (1i - sparse ([1, 2]), [-1+1i, -2+1i])
%!assert (sparse ([1, 2]) .* 1i, sparse ([1i, 2i]))
%!assert (1 .* sparse ([1i, 2]), sparse ([1i, 2]))
%!assert (sparse ([1, 2i]) ./ 1, sparse ([1, 2i]))
%!assert (1 ./ sparse ([1i, 2]), [-1i, 0.5])
%!error <operator /: nonconformant arguments \(op1 is 1x1, op2 is 1x2\)>
%! 1 / sparse ([1i, 2])

## Test broadcasting for math operations with matrix and sparse matrix
%!assert (sparse ([1, 2]) + [1, 1i], [2, 2+1i])
%!assert ([1, 1] + sparse ([1i, 2]), [1+1i, 3])
%!assert (sparse ([1; 2i]) + [1; 1], [2; 1+2i])
%!assert ([1; 1i] + sparse ([1; 2]), [2; 2+1i])
%!assert (sparse ([1, 2, 3i]) + ones (3), repmat ([2, 3, 1+3i], 3, 1))
%!assert (sparse ([1; 2; 3]) + ones (3)*i, repmat ([1; 2; 3]+i, 1, 3))
%!assert (ones (3)*i + sparse ([1, 2, 3]), repmat ([1, 2, 3]+i, 3, 1))
%!assert (ones (3) + sparse ([1; 2i; 3]), repmat ([2; 1+2i; 4], 1, 3))
%!assert (sparse ([1, 2]) + [1; 2i], [2, 3; 1+2i, 2+2i])
%!assert ([1; 2] + sparse ([1, 2i]), [2, 1+2i; 3, 2+2i])
%!assert (sparse ([1; 2]) + [1, 2i], [2, 1+2i; 3, 2+2i])
%!assert ([1, 2i] + sparse ([1; 2i]), [2, 1+2i; 1+2i, 4i])
%!error <operator \+: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2i]) + [1, 1i, 1]
%!error <operator \+: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2i, 3]) + ones (4)
%!error <operator \+: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3]) + ones (4, 3)*i

%!assert (sparse ([1, 2i]) - [1, 1], [0, -1+2i])
%!assert ([1, 1] - sparse ([1, 2i]), [0, 1-2i])
%!assert (sparse ([1; 2]) - [1; 1i], [0; 2-1i])
%!assert ([1; 1i] - sparse ([1; 2]), [0; -2+1i])
%!assert (sparse ([1, 2, 3]) - ones (3)*i, repmat ([1:3]-i, 3, 1))
%!assert (sparse ([1; 2; 3i]) - ones (3), repmat ([0; 1; -1+3i], 1, 3))
%!assert (ones (3) - sparse ([1i, 2, 3]), repmat ([1-1i, -1, -2], 3, 1))
%!assert (ones (3)*i - sparse ([1; 2; 3]), repmat ([-1; -2; -3]+i, 1, 3))
%!assert (sparse ([1, 2]) - [1; 2i], [1, 2] - [1; 2i])
%!assert ([1; 2i] - sparse ([1i, 2]), [1; 2i] - [1i, 2])
%!assert (sparse ([1; 2i]) - [1, 2], [1; 2i] - [1, 2])
%!assert ([1i, 2] - sparse ([1; 2i]), [1i, 2] - [1; 2i])
%!error <operator -: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2i]) - [1, 1i, 1]
%!error <operator -: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3i]) - ones (4)
%!error <operator -: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3i]) - ones (4, 3)

%!assert (sparse ([1, 2i]) .* [1, 1], sparse ([1, 2i]))
%!assert ([1, 1i] .* sparse ([1, 2]), sparse ([1, 2i]))
%!assert (sparse ([1i; 2]) .* [1; 1], sparse ([1i; 2]))
%!assert ([1i; 1] .* sparse ([1; 2i]), sparse ([1i; 2i]))
%!assert (sparse ([1, 2, 3i]) .* ones (3), sparse (repmat ([1, 2, 3i], 3, 1)))
%!assert (sparse ([1; 2; 3]) .* ones (3)*i, sparse (repmat ([1:3]'*i, 1, 3)))
%!assert (ones (3) .* sparse ([1, 2, 3i]), sparse (repmat ([1, 2, 3i], 3, 1)))
%!assert (ones (3) .* sparse ([1i; 2i; 3i]), sparse (repmat ([1:3]'*i, 1, 3)))
%!assert (sparse ([1i, 2]) .* [1; 2], sparse ([1i, 2] .* [1; 2]))
%!assert ([1; 2i] .* sparse ([1, 2]), sparse ([1; 2i] .* [1, 2]))
%!assert (sparse ([1; 2i]) .* [1, 2i], sparse ([1; 2i] .* [1, 2i]))
%!assert ([1i, 2i] .* sparse ([1i; 2]), sparse ([1i, 2i] .* [1i; 2]))
%!error <product: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2i]) .* [1, 1, 1]
%!error <product: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3i]) .* ones (4)
%!error <product: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2i; 3]) .* ones (4, 3)
%!error <operator \*: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2]) * [1, 1i, 1]
%!error <operator \*: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3i]) * ones (4)
%!error <operator \*: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2; 3i]) * ones (4, 3)

%!assert (sparse ([1, 2i]) ./ [1, 1], sparse ([1, 2i]))
%!assert ([1, 1i] ./ sparse ([1, 2]), [1, 0.5i])
%!assert (sparse ([1; 2]) ./ [1; 1i], sparse ([1; -2i]))
%!assert ([1i; 1] ./ sparse ([1; 2i]), [1i; -0.5i])
%!assert (sparse ([1, 2i, 3]) ./ ones (3), sparse (repmat ([1, 2i, 3], 3, 1)))
%!assert (sparse ([1; 2; 3]) ./ (ones (3)*i), ...
%!        sparse (repmat (-[1i; 2i; 3i], 1, 3)))
%!assert (ones (3) ./ sparse ([1, 2i, 3]), repmat ([1, -0.5i, 1/3], 3, 1))
%!assert ((ones (3)*(-i)) ./ sparse ([1; 2; 3]),
%!        repmat (-[1; 0.5; 1/3]*i, 1, 3))
%!assert (sparse ([1, 2i]) ./ [1; 2], sparse ([1, 2i] ./ [1; 2]))
%!assert ([1i; 2] ./ sparse ([1i, 2i]), [1i; 2] ./ [1i, 2i])
%!assert (sparse ([1; 2i]) ./ [1, 2], sparse ([1; 2i] ./ [1, 2]))
%!assert ([1i, 2i] ./ sparse ([1; 2i]), [1i, 2i] ./ [1; 2i])
%!error <quotient: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2i]) ./ [1, 1, 1]
%!error <quotient: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3i]) ./ ones (4)
%!error <quotient: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1; 2i; 3]) ./ ones (4, 3)
%!error <operator /: nonconformant arguments \(op1 is 1x2, op2 is 1x3\)>
%! sparse ([1, 2i]) / [1, 1i, 1]
%!error <operator /: nonconformant arguments \(op1 is 1x3, op2 is 4x4\)>
%! sparse ([1, 2, 3i]) / ones (4)
%!error <operator /: nonconformant arguments \(op1 is 3x1, op2 is 4x3\)>
%! sparse ([1i; 2i; 3i]) / ones (4, 3)

## Test broadcasting for comparison and boolean operators
## for sparse matrix to matrix and matrix to sparse matrix
%!assert (sparse ([1, 1i, 0]) == [0, 1, 1]*i, sparse (logical ([0, 1, 0])))
%!assert (sparse ([1; 1i; 0]) == [0, 1, 1]*i, ...
%!        sparse ([1; 1i; 0] == [0, 1, 1]*i))
%!assert (sparse ([1, 1i, 0]) == [0; 1; 1; 0]*i, ...
%!        sparse ([1, 1i, 0] == [0; 1; 1; 0]*i))
%!assert (sparse ([1, 1i, 0]) == [0, 1, 1; 0, 1, 1]*i, ...
%!        sparse (logical ([0, 1, 0; 0, 1, 0])))
%!assert (sparse ([1; 1i; 0]) == [0, 1; 1, 1; 0, 1]*i, ...
%!        sparse (logical ([0, 0; 1, 1; 1, 0])))
%!assert ([1i, 0, 1] == sparse ([1, 1, 0]*i), sparse (logical ([1, 0, 0])))
%!assert ([0, 1, 1i] == sparse ([1; 1; 0]*i), ...
%!        sparse ([0, 1, 1i] == [1; 1; 0]*i))
%!assert ([0; 1; 1i; 0] == sparse ([1, 1, 0]*i), ...
%!        sparse ([0; 1; 1i; 0] == [1, 1, 0]*i))
%!assert ([0, 1, 1i; 0+i, 1, 1] == sparse ([1, 1, 0]+i), ...
%!        sparse (logical ([0, 0, 1; 0, 0, 0])))
%!assert ([0, 1; 1, 1; 0, 1]+i == sparse ([1; 1; 0]+i), ...
%!        sparse (logical ([0, 1; 1, 1; 1, 0])))

%!assert (sparse ([1, 1i, 0]) != [0, 1, 1]*i, sparse (logical ([1, 0, 1])))
%!assert (sparse ([1; 1i; 0]) != [0, 1, 1]*i, ...
%!        sparse ([1; 1i; 0] != [0, 1, 1]*i))
%!assert (sparse ([1, 1i, 0]) != [0; 1; 1; 0]*i, ...
%!        sparse ([1, 1i, 0] != [0; 1; 1; 0]*i))
%!assert (sparse ([1, 1i, 0]) != [0, 1, 1; 0, 1, 1]*i, ...
%!        sparse (logical ([1, 0, 1; 1, 0, 1])))
%!assert (sparse ([1; 1i; 0]) != [0, 1; 1, 1; 0, 1]*i, ...
%!        sparse (logical ([1, 1; 0, 0; 0, 1])))
%!assert ([1i, 0, 1] != sparse ([1, 1, 0]*i), sparse (logical ([0, 1, 1])))
%!assert ([0, 1, 1i] != sparse ([1; 1; 0]*i), ...
%!        sparse ([0, 1, 1i] != [1; 1; 0]*i))
%!assert ([0; 1; 1i; 0] != sparse ([1, 1, 0]*i), ...
%!        sparse ([0; 1; 1i; 0] != [1, 1, 0]*i))
%!assert ([0, 1, 1i; 0+i, 1, 1] != sparse ([1, 1, 0]+i), ...
%!        sparse (logical ([1, 1, 0; 1, 1, 1])))
%!assert ([0, 1; 1, 1; 0, 1]+i != sparse ([1; 1; 0]+i), ...
%!        sparse (logical ([1, 0; 0, 0; 0, 1])))

%!assert (sparse ([1, 1i, 0]) <= [0, 1, 1]*i, sparse (logical ([0, 1, 1])))
%!assert (sparse ([1; 1i; 0]) <= [0, 1, 1]*i, sparse ([1; 1; 0] <= [0, 1, 1]))
%!assert (sparse ([1, 1i, 0]) <= [0; 1; 1; 0]*i, ...
%!        sparse ([1, 1i, 0] <= [0; 1; 1; 0]*i))
%!assert (sparse ([1, 1i, 0]) <= [0, 1, 1; 0, 1, 1]*i, ...
%!        sparse (logical ([0, 1, 1; 0, 1, 1])))
%!assert (sparse ([1; 1i; 0]) <= [0, 1; 1, 1; 0, 1]*i, ...
%!        sparse (logical ([0, 1; 1, 1; 1, 1])))
%!assert ([1i, 0, 1] <= sparse ([1, 1, 0]*i), sparse (logical ([1, 1, 0])))
%!assert ([0, 1, 1i] <= sparse ([1; 1; 0]*i), ...
%!        sparse ([0, 1i, 1] <= [1; 1; 0]*i))
%!assert ([0; 1; 1i; 0] <= sparse ([1, 1, 0]*i), ...
%!        sparse ([0; 1; 1i; 0] <= [1, 1, 0]*i))
%!assert ([0, 1, 1i; 0+i, 1, 1] <= sparse ([1, 1, 0]+i), ...
%!        sparse (logical ([1, 1, 1; 1, 1, 1])))
%!assert ([0, 1; 1, 1; 0, 1]+i <= sparse ([1; 1; 0]+i), ...
%!        sparse (logical ([1, 1; 1, 1; 1, 0])))

%!assert (sparse ([1, 1i, 0]) >= [0, 1, 1]*i, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1; 1i; 0]) >= [0, 1, 1]*i, ...
%!        sparse ([1; 1i; 0] >= [0, 1, 1]*i))
%!assert (sparse ([1, 1i, 0]) >= [0; 1; 1; 0]*i, ...
%!        sparse ([1, 1i, 0] >= [0; 1; 1; 0]*i))
%!assert (sparse ([1, 1i, 0]) >= [0, 1, 1; 0, 1, 1]*i, ...
%!        sparse (logical ([1, 1, 0; 1, 1, 0])))
%!assert (sparse ([1; 1i; 0]) >= [0, 1; 1, 1; 0, 1]*i, ...
%!        sparse (logical ([1, 0; 1, 1; 1, 0])))
%!assert ([1i, 0, 1] >= sparse ([1, 1, 0]*i), sparse (logical ([1, 0, 1])))
%!assert ([0, 1, 1i] >= sparse ([1; 1; 0]*i), ...
%!        sparse ([0, 1, 1i] >= [1; 1; 0]*i))
%!assert ([0; 1; 1i; 0] >= sparse ([1, 1, 0]*i), ...
%!        sparse ([0; 1; 1i; 0] >= [1, 1, 0]*i))
%!assert ([0, 1, 1i; 0+i, 1, 1] >= sparse ([1, 1, 0]+i), ...
%!        sparse (logical ([0, 0, 1; 0, 0, 0])))
%!assert ([0, 1; 1, 1; 0, 1]+i >= sparse ([1; 1; 0]+i), ...
%!        sparse (logical ([0, 1; 1, 1; 1, 1])))

%!assert (sparse ([1, 1i, 0]) < [0, 1, 1]*i, sparse (logical ([0, 0, 1])))
%!assert (sparse ([1; 1i; 0]) < [0, 1, 1]*i, sparse ([1; 1i; 0] < [0, 1, 1]*i))
%!assert (sparse ([1, 1i, 0]) < [0; 1; 1; 0]*i, ...
%!        sparse ([1, 1i, 0] < [0; 1; 1; 0]*i))
%!assert (sparse ([1, 1i, 0]) < [0, 1, 1; 0, 1, 1]*i, ...
%!        sparse (logical ([0, 0, 1; 0, 0, 1])))
%!assert (sparse ([1; 1i; 0]) < [0, 1; 1, 1; 0, 1]*i, ...
%!        sparse (logical ([0, 1; 0, 0; 0, 1])))
%!assert ([1i, 0, 1] < sparse ([1, 1, 0]*i), sparse (logical ([0, 1, 0])))
%!assert ([0, 1, 1i] < sparse ([1; 1; 0]*i), sparse ([0, 1, 1i] < [1; 1; 0]*i))
%!assert ([0; 1; 1i; 0] < sparse ([1, 1, 0]*i), ...
%!        sparse ([0; 1; 1i; 0] < [1, 1, 0]*i))
%!assert ([0, 1, 1i; 0+i, 1, 1] < sparse ([1, 1, 0]+i), ...
%!        sparse (logical ([1, 1, 0; 1, 1, 1])))
%!assert ([0, 1; 1, 1; 0, 1]+i < sparse ([1; 1; 0]+i), ...
%!        sparse (logical ([1, 0; 0, 0; 0, 0])))

%!assert (sparse ([1, 1i, 0]) > [0, 1, 1]*i, sparse (logical ([1, 0, 0])))
%!assert (sparse ([1; 1i; 0]) > [0, 1, 1]*i, sparse ([1; 1i; 0] > [0, 1, 1]*i))
%!assert (sparse ([1, 1i, 0]) > [0; 1; 1; 0]*i, ...
%!        sparse ([1, 1i, 0] > [0; 1; 1; 0]*i))
%!assert (sparse ([1, 1i, 0]) > [0, 1, 1; 0, 1, 1]*i, ...
%!        sparse (logical ([1, 0, 0; 1, 0, 0])))
%!assert (sparse ([1; 1i; 0]) > [0, 1; 1, 1; 0, 1]*i, ...
%!        sparse (logical ([1, 0; 0, 0; 0, 0])))
%!assert ([1i, 0, 1] > sparse ([1, 1, 0]*i), sparse (logical ([0, 0, 1])))
%!assert ([0, 1, 1i] > sparse ([1; 1; 0]*i), sparse ([0, 1, 1i] > [1; 1; 0]*i))
%!assert ([0; 1; 1i; 0] > sparse ([1, 1, 0]*i), ...
%!        sparse ([0; 1; 1i; 0] > [1, 1, 0]*i))
%!assert ([0, 1, 1i; 0+i, 1, 1] > sparse ([1, 1, 0]+i), ...
%!        sparse (logical ([0, 0, 0; 0, 0, 0])))
%!assert ([0, 1; 1, 1; 0, 1]+i > sparse ([1; 1; 0]+i), ...
%!        sparse (logical ([0, 0; 0, 0; 0, 1])))

%!assert (sparse ([1, 1i, 0]) & [0, 1, 1]*i, sparse (logical ([0, 1, 0])))
%!assert (sparse ([1; 1i; 0]) & [0, 1, 1]*i, sparse ([1; 1i; 0] & [0, 1, 1]*i))
%!assert (sparse ([1, 1i, 0]) & [0; 1; 1; 0]*i, ...
%!        sparse ([1, 1i, 0] & [0; 1; 1; 0]*i))
%!assert (sparse ([1, 1i, 0]) & [0, 1, 1; 0, 1, 1]*i, ...
%!        sparse (logical ([0, 1, 0; 0, 1, 0])))
%!assert (sparse ([1; 1i; 0]) & [0, 1; 1, 1; 0, 1]*i, ...
%!        sparse (logical ([0, 1; 1, 1; 0, 0])))
%!assert ([1i, 0, 1] & sparse ([1, 1, 0]*i), sparse (logical ([1, 0, 0])))
%!assert ([0, 1, 1i] & sparse ([1; 1; 0]*i), sparse ([0, 1, 1i] & [1; 1; 0]*i))
%!assert ([0; 1; 1i; 0] & sparse ([1, 1, 0]*i), ...
%!        sparse ([0; 1; 1i; 0] & [1, 1, 0]*i))
%!assert ([0, 1, 1i; 0+i, 1, 1] & sparse ([1, 1, 0]+i), ...
%!        sparse (logical ([0, 1, 1; 1, 1, 1])))
%!assert ([0, 1; 1, 1; 0, 1]+i & sparse ([1; 1; 0]+i), ...
%!        sparse (logical ([1, 1; 1, 1; 1, 1])))

## Test boolean OR operations should return dense logical arrays
%!assert (sparse ([1, 1i, 0]) | [0, 1, 1]*i, logical ([1, 1, 1]))
%!assert (sparse ([1; 1i; 0]) | [0, 1, 1]*i, [1; 1i; 0] | [0, 1, 1]*i)
%!assert (sparse ([1, 1i, 0]) | [0; 1; 1; 0]*i, [1, 1i, 0] | [0; 1; 1; 0]*i)
%!assert (sparse ([1, 1i, 0]) | [0, 1, 1; 0, 1, 1]*i,
%!        logical ([1, 1, 1; 1, 1, 1]))
%!assert (sparse ([1; 1i; 0]) | [0, 1; 1, 1; 0, 1]*i, 
%!        logical ([1, 1; 1, 1; 0, 1]))
%!assert ([1i, 0, 1] | sparse ([1, 1, 0]*i), logical ([1, 1, 1]))
%!assert ([0, 1, 1i] | sparse ([1; 1; 0]*i), [0, 1, 1i] | [1; 1; 0]*i)
%!assert ([0; 1; 1i; 0] | sparse ([1, 1, 0]*i), [0; 1; 1i; 0] | [1, 1, 0]*i)
%!assert ([0, 1, 1i; 0+i, 1, 1] | sparse ([1, 1, 0]+i), 
%!        logical ([1, 1, 1; 1, 1, 1]))
%!assert ([0, 1; 1, 1; 0, 1]+i | sparse ([1; 1; 0]+i), 
%!        logical ([1, 1; 1, 1; 1, 1]))

## Test broadcasting for comparison and boolean operators
## for sparse matrix to scalar and scalar to sparse matrix
%!assert (sparse ([1, 1, 0]*i) == 1i, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]*i) != 1i, sparse (logical ([0, 0, 1])))
%!assert (sparse ([1, 1, 0]*i) <= 1i, sparse (logical ([1, 1, 1])))
%!assert (sparse ([1, 1, 0]*i) >= 1i, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]*i) < 1i, sparse (logical ([0, 0, 1])))
%!assert (sparse ([1, 1, 0]*i) > 0i, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]*i) & 1i, sparse (logical ([1, 1, 0])))
%!assert (sparse ([1, 1, 0]*i) | 1i, logical ([1, 1, 1]))
%!assert (sparse ([1, 1, 0]+i) == 1+i, 1+i == sparse ([1, 1, 0]+i))
%!assert (sparse ([1, 1, 0]+i) != 1+i, 1+i != sparse ([1, 1, 0]+i))
%!assert (sparse ([1, 1, 0]+i) <= 1+i, 1+i >= sparse ([1, 1, 0]+i))
%!assert (sparse ([1, 1, 0]+i) >= 1+i, 1+i <= sparse ([1, 1, 0]+i))
%!assert (sparse ([1, 1, 0]+i) < 1+i, 1+i > sparse ([1, 1, 0]+i))
%!assert (sparse ([1, 1, 0]+i) > 0+i, 0+i < sparse ([1, 1, 0]+i))
%!assert (sparse ([1, 1, 0]+i) & 1+i, 1+i & sparse ([1, 1, 0]+i))
%!assert (sparse ([1, 1, 0]+i) | 1+i, 1+i | sparse ([1, 1, 0]+i))

## Test some empty edge cases
%!assert (sparse (1i) | sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1i | sparse (ones(0,1)), logical (ones (0, 1)))
%!assert (sparse (1i) & sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1i & sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (sparse (1i) | sparse (ones(2,0)), sparse (logical (ones (2, 0))))
%!assert (1i | sparse (ones(2, 0)), logical (ones (2, 0)))
%!assert (sparse (1i) & sparse (ones(2,0)), sparse (logical (ones (2, 0))))
%!assert (1i & sparse (ones(2, 0)), sparse (logical (ones (2, 0))))
%!assert (sparse (1i) == sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1i != sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (sparse (1i) <= sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (1i >= sparse (ones(0,1)), sparse (logical (ones (0, 1))))
%!assert (sparse (1i) < sparse (ones(2,0)), sparse (logical (ones (2, 0))))
%!assert (1i > sparse (ones(2, 0)), sparse (logical (ones (2, 0))))

## Test errors
%!error <mx_el_eq: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) == ones (2,2)*i
%!error <mx_el_ne: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) != ones (2,2)*i
%!error <mx_el_le: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) <= ones (2,2)*i
%!error <mx_el_ge: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) >= ones (2,2)*i
%!error <mx_el_lt: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) < ones (2,2)*i
%!error <mx_el_gt: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) > ones (2,2)*i
%!error <mx_el_and: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) & ones (2,2)*i
%!error <mx_el_or: nonconformant arguments \(op1 is 0x2, op2 is 2x2\)>
%! sparse (ones (0,2)) | ones (2,2)*i

## Test broadcasting for comparison and boolean operators
## for sparse matrix to sparse matrix
%!assert (sparse([1i, 1, 0]) == sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] == [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) == sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] == [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) == sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] == [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) == sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] == [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) == sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] == [0, 1; 1, 1; 0, 1]+i))
%!assert (sparse([1i, 1, 0]) != sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] != [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) != sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] != [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) != sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] != [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) != sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] != [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) != sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] != [0, 1; 1, 1; 0, 1]+i))
%!assert (sparse([1i, 1, 0]) <= sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] <= [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) <= sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] <= [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) <= sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] <= [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) <= sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] <= [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) <= sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] <= [0, 1; 1, 1; 0, 1]+i))
%!assert (sparse([1i, 1, 0]) >= sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] >= [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) >= sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] >= [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) >= sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] >= [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) >= sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] >= [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) >= sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] >= [0, 1; 1, 1; 0, 1]+i))
%!assert (sparse([1i, 1, 0]) < sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] < [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) < sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] < [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) < sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] < [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) < sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] < [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) < sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] < [0, 1; 1, 1; 0, 1]+i))
%!assert (sparse([1i, 1, 0]) > sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] > [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) > sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] > [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) > sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] > [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) > sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] > [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) > sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] > [0, 1; 1, 1; 0, 1]+i))
%!assert (sparse([1i, 1, 0]) & sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] & [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) & sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] & [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) & sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] & [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) & sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] & [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) & sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] & [0, 1; 1, 1; 0, 1]+i))
%!assert (sparse([1i, 1, 0]) | sparse([0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] | [0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) | sparse([0, 1, 1]+i), ...
%!        sparse ([1i; 1; 0] | [0, 1, 1]+i))
%!assert (sparse([1i, 1, 0]) | sparse([0; 1; 1; 0]+i), ...
%!        sparse ([1i, 1, 0] | [0; 1; 1; 0]+i))
%!assert (sparse([1i, 1, 0]) | sparse([0, 1, 1; 0, 1, 1]+i), ...
%!        sparse ([1i, 1, 0] | [0, 1, 1; 0, 1, 1]+i))
%!assert (sparse([1i; 1; 0]) | sparse([0, 1; 1, 1; 0, 1]+i), ...
%!        sparse ([1i; 1; 0] | [0, 1; 1, 1; 0, 1]+i))
