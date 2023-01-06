########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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

## Test values of range

%!assert (full (1:9), [ 1 2 3 4 5 6 7 8 9 ])
%!assert (full (1:0.4:3), [ 1.0 1.4 1.8 2.2 2.6 3.0 ])
%!assert (full (9:1), zeros (1,0))
%!assert (full (9:-1:1), [ 9 8 7 6 5 4 3 2 1 ])
%!assert (full (1:-1:9), zeros (1,0))
%!assert (full (1:1:1), 1)
%!assert (full (i:2i:10i), zeros (1,0))

## Test mixing integer range with other types

%!shared expect, r, z
%! expect = [ 1 2 3 4 5 6 7 8 9
%!            0 0 0 0 0 0 0 0 0 ];
%! z = zeros (1,9);
%! r = 1:9;

%!assert ([ r ; z                  ], expect)
%!assert ([ r ; single(z)          ], single (expect))
%!assert ([ r ; logical(z)         ], expect)
%!assert ([ r ; sparse(z)          ], sparse (expect))
%!assert ([ r ; sparse(logical(z)) ], sparse (expect))

%!assert ([ r ; int8(z)            ], int8 (expect))
%!assert ([ r ; int16(z)           ], int16 (expect))
%!assert ([ r ; int32(z)           ], int32 (expect))
%!assert ([ r ; int64(z)           ], int64 (expect))
%!assert ([ r ; uint8(z)           ], uint8 (expect))
%!assert ([ r ; uint16(z)          ], uint16 (expect))
%!assert ([ r ; uint32(z)          ], uint32 (expect))
%!assert ([ r ; uint64(z)          ], uint64 (expect))

## Test mixing non-integer range with other types

%!shared expect, r, z
%! expect = [ 1.0 1.4 1.8 2.2 2.6 3.0
%!            0   0   0   0   0   0   ];
%! z = zeros (1,6);
%! r = 1:0.4:3;

%!assert ([ r ; z                  ], expect)
%!assert ([ r ; single(z)          ], single (expect))
%!assert ([ r ; logical(z)         ], expect)
%!assert ([ r ; sparse(z)          ], sparse (expect))
%!assert ([ r ; sparse(logical(z)) ], sparse (expect))

%!assert ([ r ; int8(z)            ], int8 (expect))
%!assert ([ r ; int16(z)           ], int16 (expect))
%!assert ([ r ; int32(z)           ], int32 (expect))
%!assert ([ r ; int64(z)           ], int64 (expect))
%!assert ([ r ; uint8(z)           ], uint8 (expect))
%!assert ([ r ; uint16(z)          ], uint16 (expect))
%!assert ([ r ; uint32(z)          ], uint32 (expect))
%!assert ([ r ; uint64(z)          ], uint64 (expect))

## Test corner cases of ranges (base and limit)

%!shared r, rrev, rneg
%! r = -0:3;
%! rrev = 3:-1:-0;
%! rneg = -3:-0;

%!assert (full (r), [-0 1 2 3])
%!assert (signbit (full (r)), logical ([1 0 0 0]))
%!assert (r(1), -0)
%!assert (signbit (r(1)), true)
%!assert (signbit (r(1:2)), logical ([1 0]))
%!assert (signbit (r(2:-1:1)), logical ([0 1]))
%!assert (signbit (r([2 1 1 3])), logical ([0 1 1 0]))

%!assert (full (rrev), [3 2 1 -0])
%!assert (signbit (full (rrev)), logical ([0 0 0 1]))
%!assert (rrev(4), -0)
%!assert (signbit (rrev(4)), true)
%!assert (signbit (rrev(3:4)), logical ([0 1]))
%!assert (signbit (rrev(4:-1:3)), logical ([1 0]))
%!assert (signbit (rrev([1 4 4 2])), logical ([0 1 1 0]))

%!assert (min (r), -0)
%!assert (signbit (min (r)), true)
%!assert (min (rrev), -0)
%!assert (signbit (min (rrev)), true)

%!assert (max (rneg), -0)
%!assert (signbit (max (rneg)), true)

%!assert (sort (r, "descend"), [3 2 1 -0])
%!assert (signbit (sort (r, "descend")), logical ([0 0 0 1]))
%!assert (signbit (sort (rrev, "ascend")), logical ([1 0 0 0]))

## Test mathematical operations (also, non-finite values and 0)
%!shared r
%! r = 1:5;

%!assert (-r, -1:-1:-5)
%!assert (1 + r, 2:6)
%!assert (Inf + r, Inf (1,5))
%!assert (NaN + r, NaN (1,5))
%!assert (r + 1, 2:6)
%!assert (r + Inf, Inf (1,5))
%!assert (r + NaN, NaN (1,5))
%!assert (1 - r, 0:-1:-4)
%!assert (Inf - r, Inf (1,5))
%!assert (NaN - r, NaN (1,5))
%!assert (r - 1, 0:4)
%!assert (r - Inf, -Inf (1,5))
%!assert (r - NaN, NaN (1,5))
%!assert (2 * r, 2:2:10)
%!assert (0 * r, zeros (1,5))
%!assert (Inf * r, Inf (1,5))
%!assert (NaN * r, NaN (1,5))
%!assert (r * 2, 2:2:10)
%!assert (r * 0, zeros (1,5))
%!assert (r * Inf, Inf (1,5))
%!assert (r * NaN, NaN (1,5))

## Test sorting of ranges (bug #45739)
%!shared r, rrev
%! r = 1:2:10;
%! rrev = 10:-2:1;

%!assert <*45739> (sort (r, "descend"), [9 7 5 3 1])
%!assert <*45739> (sort (rrev, "ascend"), [2 4 6 8 10])

## Test final value within eps of an integer (bug #46859)
%!test <*46859>
%! rng = 1 : (1001/250)/(1/250);
%! assert (rng(end), 1001);

%!test <*46859>
%! rng = 2000: -1 : (1001/250)/(1/250);
%! assert (rng(end), 1001);

## This is not Matlab compatible (stops at 1000 with 999 elements)
## Octave prefers the more intuitive "pure math" approach where
## (1001/250) / (1/250) => (1001/250)*(250/1) => 1001.
%!test <*46859>
%! rng = 1 : (1001/250)/(1/250);
%! assert (numel (1000));

## Combinations of exceptional values and a few normal ones.

%!shared mt_row, inf, nan, zero, pt3, pt6, pt9, one, epsilon
%! mt_row = zeros (1, 0, 'double');
%! inf = Inf ('double');
%! nan = NaN ('double');
%! zero = double (0.0);
%! pt3 = double (0.3);
%! pt6 = double (0.6);
%! pt9 = double (0.9);
%! one = double (1.0);
%! epsilon = eps ('double');
%!assert <*59229> (nan:-pt3:-one, nan)
%!assert <*59229> (nan:-pt3:-inf, nan)
%!assert <*59229> (nan:-pt3:zero, nan)
%!assert <*59229> (nan:-pt3:one, nan)
%!assert <*59229> (nan:-pt3:inf, nan)
%!assert <*59229> (nan:-pt3:nan, nan)
%!assert <*59229> (nan:-inf:-one, nan)
%!assert <*59229> (nan:-inf:-inf, nan)
%!assert <*59229> (nan:-inf:zero, nan)
%!assert <*59229> (nan:-inf:one, nan)
%!assert <*59229> (nan:-inf:inf, nan)
%!assert <*59229> (nan:-inf:nan, nan)
%!assert <*59229> (nan:zero:-one, nan)
%!assert <*59229> (nan:zero:-inf, nan)
%!assert <*59229> (nan:zero:zero, nan)
%!assert <*59229> (nan:zero:one, nan)
%!assert <*59229> (nan:zero:inf, nan)
%!assert <*59229> (nan:zero:nan, nan)
%!assert <*59229> (nan:pt3:-one, nan)
%!assert <*59229> (nan:pt3:-inf, nan)
%!assert <*59229> (nan:pt3:zero, nan)
%!assert <*59229> (nan:pt3:one, nan)
%!assert <*59229> (nan:pt3:inf, nan)
%!assert <*59229> (nan:pt3:nan, nan)
%!assert <*59229> (nan:inf:-one, nan)
%!assert <*59229> (nan:inf:-inf, nan)
%!assert <*59229> (nan:inf:zero, nan)
%!assert <*59229> (nan:inf:one, nan)
%!assert <*59229> (nan:inf:inf, nan)
%!assert <*59229> (nan:inf:nan, nan)
%!assert <*59229> (nan:nan:-one, nan)
%!assert <*59229> (nan:nan:-inf, nan)
%!assert <*59229> (nan:nan:zero, nan)
%!assert <*59229> (nan:nan:one, nan)
%!assert <*59229> (nan:nan:inf, nan)
%!assert <*59229> (nan:nan:nan, nan)
%!assert <*59229> (-inf:-pt3:nan, nan)
%!assert <*59229> (-inf:-inf:nan, nan)
%!assert <*59229> (-inf:zero:nan, nan)
%!assert <*59229> (-inf:pt3:nan, nan)
%!assert <*59229> (-inf:inf:nan, nan)
%!assert <*59229> (zero:-pt3:nan, nan)
%!assert <*59229> (zero:-inf:nan, nan)
%!assert <*59229> (zero:zero:nan, nan)
%!assert <*59229> (zero:pt3:nan, nan)
%!assert <*59229> (zero:inf:nan, nan)
%!assert <*59229> (inf:-pt3:nan, nan)
%!assert <*59229> (inf:-inf:nan, nan)
%!assert <*59229> (inf:zero:nan, nan)
%!assert <*59229> (inf:pt3:nan, nan)
%!assert <*59229> (inf:inf:nan, nan)
%!assert <*59229> (-inf:nan:-one, nan)
%!assert <*59229> (-inf:nan:-inf, nan)
%!assert <*59229> (-inf:nan:zero, nan)
%!assert <*59229> (-inf:nan:one, nan)
%!assert <*59229> (-inf:nan:inf, nan)
%!assert <*59229> (-inf:nan:nan, nan)
%!assert <*59229> (zero:nan:-one, nan)
%!assert <*59229> (zero:nan:-inf, nan)
%!assert <*59229> (zero:nan:zero, nan)
%!assert <*59229> (zero:nan:one, nan)
%!assert <*59229> (zero:nan:inf, nan)
%!assert <*59229> (zero:nan:nan, nan)
%!assert <*59229> (inf:nan:-one, nan)
%!assert <*59229> (inf:nan:-inf, nan)
%!assert <*59229> (inf:nan:zero, nan)
%!assert <*59229> (inf:nan:one, nan)
%!assert <*59229> (inf:nan:inf, nan)
%!assert <*59229> (inf:nan:nan, nan)
%!assert <*59229> (inf:-pt3:inf, nan)
%!assert <*59229> (-inf:-pt3:-inf, nan)
%!assert <*59229> (inf:pt3:inf, nan)
%!assert <*59229> (-inf:pt3:-inf, nan)
%!assert <*59229> (-inf:-inf:-inf, nan)
%!assert <*59229> (zero:-inf:-inf, nan)
%!assert <*59229> (inf:-inf:-one, nan)
%!assert <*59229> (inf:-inf:-inf, nan)
%!assert <*59229> (inf:-inf:zero, nan)
%!assert <*59229> (inf:-inf:one, nan)
%!assert <*59229> (inf:-inf:inf, nan)
%!assert <*59229> (-inf:inf:-one, nan)
%!assert <*59229> (-inf:inf:-inf, nan)
%!assert <*59229> (-inf:inf:zero, nan)
%!assert <*59229> (-inf:inf:one, nan)
%!assert <*59229> (-inf:inf:inf, nan)
%!assert <*59229> (zero:inf:inf, nan)
%!assert <*59229> (inf:inf:inf, nan)
%!assert <*59229> (zero:zero:-one, mt_row)
%!assert <*59229> (zero:zero:zero, mt_row)
%!assert <*59229> (zero:zero:one, mt_row)
%!assert <*59229> (zero:zero:-inf, mt_row)
%!assert <*59229> (zero:zero:inf, mt_row)
%!assert <*59229> (-inf:zero:-one, mt_row)
%!assert <*59229> (-inf:zero:zero, mt_row)
%!assert <*59229> (-inf:zero:one, mt_row)
%!assert <*59229> (-inf:zero:-inf, mt_row)
%!assert <*59229> (-inf:zero:inf, mt_row)
%!assert <*59229> (inf:zero:-one, mt_row)
%!assert <*59229> (inf:zero:zero, mt_row)
%!assert <*59229> (inf:zero:one, mt_row)
%!assert <*59229> (inf:zero:-inf, mt_row)
%!assert <*59229> (inf:zero:inf, mt_row)
%!assert <*59229> (zero:pt3:-one, mt_row)
%!assert <*59229> (zero:pt3:-inf, mt_row)
%!assert <*59229> (inf:pt3:-one, mt_row)
%!assert <*59229> (inf:pt3:zero, mt_row)
%!assert <*59229> (inf:pt3:one, mt_row)
%!assert <*59229> (inf:pt3:-inf, mt_row)
%!assert <*59229> (zero:inf:-one, mt_row)
%!assert <*59229> (zero:inf:-inf, mt_row)
%!assert <*59229> (inf:inf:-one, mt_row)
%!assert <*59229> (inf:inf:zero, mt_row)
%!assert <*59229> (inf:inf:one, mt_row)
%!assert <*59229> (inf:inf:-inf, mt_row)
%!assert <*59229> (zero:-pt3:one, mt_row)
%!assert <*59229> (zero:-pt3:inf, mt_row)
%!assert <*59229> (-inf:-pt3:-one, mt_row)
%!assert <*59229> (-inf:-pt3:zero, mt_row)
%!assert <*59229> (-inf:-pt3:one, mt_row)
%!assert <*59229> (-inf:-pt3:inf, mt_row)
%!assert <*59229> (zero:-inf:one, mt_row)
%!assert <*59229> (zero:-inf:inf, mt_row)
%!assert <*59229> (-inf:-inf:-one, mt_row)
%!assert <*59229> (-inf:-inf:zero, mt_row)
%!assert <*59229> (-inf:-inf:one, mt_row)
%!assert <*59229> (-inf:-inf:inf, mt_row)
%!assert <*59229> (zero:-inf:-one, zero)
%!assert <*59229> (zero:-inf:zero, zero)
%!assert <*59229> (zero:inf:zero, zero)
%!assert <*59229> (zero:inf:one, zero)
%!assert <*59229> (zero:-pt3:zero, zero)
%!assert <*59229> (zero:pt3:zero, zero)
%!assert <*59229> (zero:-pt3:-one, [zero, -pt3, -pt6, -pt9], epsilon)
%!assert <*59229> (zero:pt3:one, [zero, pt3, pt6, pt9], epsilon)
%!error <range with infinite number of elements cannot be stored> zero:-pt3:-inf
%!error <range with infinite number of elements cannot be stored> inf:-pt3:-one
%!error <range with infinite number of elements cannot be stored> inf:-pt3:zero
%!error <range with infinite number of elements cannot be stored> inf:-pt3:one
%!error <range with infinite number of elements cannot be stored> inf:-pt3:-inf
%!error <range with infinite number of elements cannot be stored> zero:pt3:inf
%!error <range with infinite number of elements cannot be stored> -inf:pt3:-one
%!error <range with infinite number of elements cannot be stored> -inf:pt3:zero
%!error <range with infinite number of elements cannot be stored> -inf:pt3:one
%!error <range with infinite number of elements cannot be stored> -inf:pt3:inf

## Again, this time with singles.
%!shared mt_row, inf, nan, zero, pt3, pt6, pt9, one, epsilon
%! mt_row = zeros (1, 0, 'single');
%! inf = Inf ('single');
%! nan = NaN ('single');
%! zero = single (0.0);
%! pt3 = single (0.3);
%! pt6 = single (0.6);
%! pt9 = single (0.9);
%! one = single (1.0);
%! epsilon = eps ('single');
%!assert <*59229> (nan:-pt3:-one, nan)
%!assert <*59229> (nan:-pt3:-inf, nan)
%!assert <*59229> (nan:-pt3:zero, nan)
%!assert <*59229> (nan:-pt3:one, nan)
%!assert <*59229> (nan:-pt3:inf, nan)
%!assert <*59229> (nan:-pt3:nan, nan)
%!assert <*59229> (nan:-inf:-one, nan)
%!assert <*59229> (nan:-inf:-inf, nan)
%!assert <*59229> (nan:-inf:zero, nan)
%!assert <*59229> (nan:-inf:one, nan)
%!assert <*59229> (nan:-inf:inf, nan)
%!assert <*59229> (nan:-inf:nan, nan)
%!assert <*59229> (nan:zero:-one, nan)
%!assert <*59229> (nan:zero:-inf, nan)
%!assert <*59229> (nan:zero:zero, nan)
%!assert <*59229> (nan:zero:one, nan)
%!assert <*59229> (nan:zero:inf, nan)
%!assert <*59229> (nan:zero:nan, nan)
%!assert <*59229> (nan:pt3:-one, nan)
%!assert <*59229> (nan:pt3:-inf, nan)
%!assert <*59229> (nan:pt3:zero, nan)
%!assert <*59229> (nan:pt3:one, nan)
%!assert <*59229> (nan:pt3:inf, nan)
%!assert <*59229> (nan:pt3:nan, nan)
%!assert <*59229> (nan:inf:-one, nan)
%!assert <*59229> (nan:inf:-inf, nan)
%!assert <*59229> (nan:inf:zero, nan)
%!assert <*59229> (nan:inf:one, nan)
%!assert <*59229> (nan:inf:inf, nan)
%!assert <*59229> (nan:inf:nan, nan)
%!assert <*59229> (nan:nan:-one, nan)
%!assert <*59229> (nan:nan:-inf, nan)
%!assert <*59229> (nan:nan:zero, nan)
%!assert <*59229> (nan:nan:one, nan)
%!assert <*59229> (nan:nan:inf, nan)
%!assert <*59229> (nan:nan:nan, nan)
%!assert <*59229> (-inf:-pt3:nan, nan)
%!assert <*59229> (-inf:-inf:nan, nan)
%!assert <*59229> (-inf:zero:nan, nan)
%!assert <*59229> (-inf:pt3:nan, nan)
%!assert <*59229> (-inf:inf:nan, nan)
%!assert <*59229> (zero:-pt3:nan, nan)
%!assert <*59229> (zero:-inf:nan, nan)
%!assert <*59229> (zero:zero:nan, nan)
%!assert <*59229> (zero:pt3:nan, nan)
%!assert <*59229> (zero:inf:nan, nan)
%!assert <*59229> (inf:-pt3:nan, nan)
%!assert <*59229> (inf:-inf:nan, nan)
%!assert <*59229> (inf:zero:nan, nan)
%!assert <*59229> (inf:pt3:nan, nan)
%!assert <*59229> (inf:inf:nan, nan)
%!assert <*59229> (-inf:nan:-one, nan)
%!assert <*59229> (-inf:nan:-inf, nan)
%!assert <*59229> (-inf:nan:zero, nan)
%!assert <*59229> (-inf:nan:one, nan)
%!assert <*59229> (-inf:nan:inf, nan)
%!assert <*59229> (-inf:nan:nan, nan)
%!assert <*59229> (zero:nan:-one, nan)
%!assert <*59229> (zero:nan:-inf, nan)
%!assert <*59229> (zero:nan:zero, nan)
%!assert <*59229> (zero:nan:one, nan)
%!assert <*59229> (zero:nan:inf, nan)
%!assert <*59229> (zero:nan:nan, nan)
%!assert <*59229> (inf:nan:-one, nan)
%!assert <*59229> (inf:nan:-inf, nan)
%!assert <*59229> (inf:nan:zero, nan)
%!assert <*59229> (inf:nan:one, nan)
%!assert <*59229> (inf:nan:inf, nan)
%!assert <*59229> (inf:nan:nan, nan)
%!assert <*59229> (inf:-pt3:inf, nan)
%!assert <*59229> (-inf:-pt3:-inf, nan)
%!assert <*59229> (inf:pt3:inf, nan)
%!assert <*59229> (-inf:pt3:-inf, nan)
%!assert <*59229> (-inf:-inf:-inf, nan)
%!assert <*59229> (zero:-inf:-inf, nan)
%!assert <*59229> (inf:-inf:-one, nan)
%!assert <*59229> (inf:-inf:-inf, nan)
%!assert <*59229> (inf:-inf:zero, nan)
%!assert <*59229> (inf:-inf:one, nan)
%!assert <*59229> (inf:-inf:inf, nan)
%!assert <*59229> (-inf:inf:-one, nan)
%!assert <*59229> (-inf:inf:-inf, nan)
%!assert <*59229> (-inf:inf:zero, nan)
%!assert <*59229> (-inf:inf:one, nan)
%!assert <*59229> (-inf:inf:inf, nan)
%!assert <*59229> (zero:inf:inf, nan)
%!assert <*59229> (inf:inf:inf, nan)
%!assert <*59229> (zero:zero:-one, mt_row)
%!assert <*59229> (zero:zero:zero, mt_row)
%!assert <*59229> (zero:zero:one, mt_row)
%!assert <*59229> (zero:zero:-inf, mt_row)
%!assert <*59229> (zero:zero:inf, mt_row)
%!assert <*59229> (-inf:zero:-one, mt_row)
%!assert <*59229> (-inf:zero:zero, mt_row)
%!assert <*59229> (-inf:zero:one, mt_row)
%!assert <*59229> (-inf:zero:-inf, mt_row)
%!assert <*59229> (-inf:zero:inf, mt_row)
%!assert <*59229> (inf:zero:-one, mt_row)
%!assert <*59229> (inf:zero:zero, mt_row)
%!assert <*59229> (inf:zero:one, mt_row)
%!assert <*59229> (inf:zero:-inf, mt_row)
%!assert <*59229> (inf:zero:inf, mt_row)
%!assert <*59229> (zero:pt3:-one, mt_row)
%!assert <*59229> (zero:pt3:-inf, mt_row)
%!assert <*59229> (inf:pt3:-one, mt_row)
%!assert <*59229> (inf:pt3:zero, mt_row)
%!assert <*59229> (inf:pt3:one, mt_row)
%!assert <*59229> (inf:pt3:-inf, mt_row)
%!assert <*59229> (zero:inf:-one, mt_row)
%!assert <*59229> (zero:inf:-inf, mt_row)
%!assert <*59229> (inf:inf:-one, mt_row)
%!assert <*59229> (inf:inf:zero, mt_row)
%!assert <*59229> (inf:inf:one, mt_row)
%!assert <*59229> (inf:inf:-inf, mt_row)
%!assert <*59229> (zero:-pt3:one, mt_row)
%!assert <*59229> (zero:-pt3:inf, mt_row)
%!assert <*59229> (-inf:-pt3:-one, mt_row)
%!assert <*59229> (-inf:-pt3:zero, mt_row)
%!assert <*59229> (-inf:-pt3:one, mt_row)
%!assert <*59229> (-inf:-pt3:inf, mt_row)
%!assert <*59229> (zero:-inf:one, mt_row)
%!assert <*59229> (zero:-inf:inf, mt_row)
%!assert <*59229> (-inf:-inf:-one, mt_row)
%!assert <*59229> (-inf:-inf:zero, mt_row)
%!assert <*59229> (-inf:-inf:one, mt_row)
%!assert <*59229> (-inf:-inf:inf, mt_row)
%!assert <*59229> (zero:-inf:-one, zero)
%!assert <*59229> (zero:-inf:zero, zero)
%!assert <*59229> (zero:inf:zero, zero)
%!assert <*59229> (zero:inf:one, zero)
%!assert <*59229> (zero:-pt3:zero, zero)
%!assert <*59229> (zero:pt3:zero, zero)
%!assert <*59229> (zero:-pt3:-one, [zero, -pt3, -pt6, -pt9], epsilon)
%!assert <*59229> (zero:pt3:one, [zero, pt3, pt6, pt9], epsilon)
%!error <range with infinite number of elements cannot be stored> zero:-pt3:-inf
%!error <range with infinite number of elements cannot be stored> inf:-pt3:-one
%!error <range with infinite number of elements cannot be stored> inf:-pt3:zero
%!error <range with infinite number of elements cannot be stored> inf:-pt3:one
%!error <range with infinite number of elements cannot be stored> inf:-pt3:-inf
%!error <range with infinite number of elements cannot be stored> zero:pt3:inf
%!error <range with infinite number of elements cannot be stored> -inf:pt3:-one
%!error <range with infinite number of elements cannot be stored> -inf:pt3:zero
%!error <range with infinite number of elements cannot be stored> -inf:pt3:one
%!error <range with infinite number of elements cannot be stored> -inf:pt3:inf

## Tests with different input classes
%!error <invalid types found in range> ({1}:1:5)
%!error <invalid types found in range> (1:{1}:5)
%!error <invalid types found in range> (1:1:{5})
%!error <incompatible types found in range> (int8(1):int16(1):5)
%!error <incompatible types found in range> (int8(1):1:int16(5))

## Tests with mixed integer/floating point values
%!error <colon operator lower bound invalid> (1.5:uint8(1):5)
%!error <colon operator lower bound invalid> (-1:uint8(1):5)
%!error <colon operator increment invalid> (uint8(1):1.5:5)
%!error <colon operator increment invalid> (uint8(1):NaN:5)
%!error <colon operator upper bound invalid> (uint8(1):1:5.5)
%!error <colon operator upper bound invalid> (uint8(1):1:256)
%!error <colon operator upper bound invalid> (uint8(1):-1:-6)

## Extreme integer values.
%!test <*61132>
%! types = {"int8", "int16", "int32", "int64", ...
%!          "uint8", "uint16", "uint32", "uint64"};
%! for i = 1:numel (types)
%!   cls = types{i};
%!   lo = intmin (cls);
%!   hi = intmax (cls);
%!   n = 99;
%!   rlo = lo:(lo+n);
%!   rhi = (hi-n):hi;
%!   assert (class (rlo), cls);
%!   assert (class (rhi), cls);
%!   assert (numel (rlo), n+1);
%!   assert (numel (rhi), n+1);
%! endfor

## Test that ranges do not exceed limits for integer types

## Ascending ranges, signed and unsigned
%!test <*61300>
%! types = {@int8, @int16, @int32, @int64, @uint8, @uint16, @uint32, @uint64};
%! start = 0:5;
%! finish = start + 6 * floor ((100 - start) / 6);
%! for i_type = 1:numel (types)
%!   for i_start = 1:numel(start)
%!     assert ((types{i_type} (start(i_start)) : 6 : 100)([1,end]), ...
%!             [types{i_type}(start(i_start)), types{i_type}(finish(i_start))]);
%!   endfor
%! endfor

## Descending ranges, signed
%!test <*61300>
%! types = {@int8, @int16, @int32, @int64};
%! start = 100:-1:95;
%! finish = start - 6 * floor (start / 6);
%! for i_type = 1:numel (types)
%!   for i_start = 1:numel(start)
%!     assert ((types{i_type} (start(i_start)) : -6 : 0)([1,end]), ...
%!             [types{i_type}(start(i_start)), types{i_type}(finish(i_start))]);
%!   endfor
%! endfor

## Descending ranges, unsigned
%!test <*61132>
%! types = {@uint8, @uint16, @uint32, @uint64};
%! start = 100:-1:95;
%! finish = start - 6 * floor (start / 6);
%! for i_type = 1:numel (types)
%!   for i_start = 1:numel(start)
%!     assert ((types{i_type} (start(i_start)) : -6 : 0)([1,end]), ...
%!             [types{i_type}(start(i_start)), types{i_type}(finish(i_start))]);
%!   endfor
%! endfor

## Class of range operator output
%!test
%! types = {@double, @single, ...
%!          @int8, @int16, @int32, @int64, ...
%!          @uint8, @uint16, @uint32, @uint64};
%! for i_type = 1:numel (types)
%!   ## with implicit increment
%!   assert (class (types{i_type} (0) : types{i_type} (10)), ...
%!           func2str (types{i_type}));
%!   ## with matching increment
%!   assert (class (types{i_type} (0) : types{i_type} (2) : types{i_type} (10)), ...
%!           func2str (types{i_type}));
%!   ## with double increment (ascending)
%!   assert (class (types{i_type} (0) : 2 : types{i_type} (10)), ...
%!           func2str (types{i_type}));
%!   ## with double increment (descending)
%!   assert (class (types{i_type} (10) : -2 : types{i_type} (0)), ...
%!           func2str (types{i_type}));
%! endfor

## Signed integer ranges with large span
%!test <*61788>  # ascending ranges
%! assert (int8 (-100) : int8 (50) : int8 (100), ...
%!         int8 ([-100, -50, 0, 50, 100]));
%! assert (int16 (-3e4) : int16 (1.5e4) : int16 (3e4), ...
%!         int16 ([-3e4, -1.5e4, 0, 1.5e4, 3e4]));
%! assert (int32 (-2e9) : int32 (1e9) : int32 (2e9), ...
%!         int32 ([-2e9, -1e9, 0, 1e9, 2e9]));
%! assert (int64 (-9e18) : int64 (4.5e18) : int64 (9e18), ...
%!         [int64(-9e18), int64(-4.5e18), 0, int64(4.5e18), int64(9e18)]);
%!test <*61788>  # descending ranges
%! assert (int8 (100) : int8 (-50) : int8 (-100), ...
%!         int8 ([100, 50, 0, -50, -100]));
%! assert (int16 (3e4) : int16 (-1.5e4) : int16 (-3e4), ...
%!         int16 ([3e4, 1.5e4, 0, -1.5e4, -3e4]));
%! assert (int32 (2e9) : int32 (-1e9) : int32 (-2e9), ...
%!         int32 ([2e9, 1e9, 0, -1e9, -2e9]));
%! assert (int64 (9e18) : int64 (-4.5e18) : int64 (-9e18), ...
%!         [int64(9e18), int64(4.5e18), 0, int64(-4.5e18), int64(-9e18)]);

## integer ranges with double increments
%!test  # ascending ranges
%! types = {@int8, @int16, @int32, @int64, ...
%!          @uint8, @uint16, @uint32, @uint64};
%! for i_type = 1:numel (types)
%!   assert (types{i_type} (0) : 2 : types{i_type} (10), ...
%!           types{i_type} ([0, 2, 4, 6, 8, 10]));
%! endfor
%!test  # descending ranges
%! types = {@int8, @int16, @int32, @int64, ...
%!          @uint8, @uint16, @uint32, @uint64};
%! for i_type = 1:numel (types)
%!   assert (types{i_type} (10) : -2 : types{i_type} (0), ...
%!           types{i_type} ([10, 8, 6, 4, 2, 0]));
%! endfor

## integer range with large double increments
%!test <*62212>  # ascending ranges
%! types = {"int8", "int16", "int32", "int64"};
%! for i_type = 1:numel (types)
%!   assert (intmin (types{i_type}) : -double (intmin (types{i_type})) : intmax (types{i_type}), ...
%!           [intmin(types{i_type}), 0]);
%!   assert (intmin (types{i_type}) : -2*double (intmin (types{i_type})) : intmax (types{i_type}), ...
%!           intmin (types{i_type}));
%!   if (! strcmp (types, "int64"))
%!     ## The increment cannot be represented in double precision for "int64"
%!     assert (intmin (types{i_type}) : 2*double (intmax (types{i_type})) : intmin (types{i_type}), ...
%!             [intmin(types{i_type}), intmax(types{i_type})-1]);
%!   endif
%! endfor
%!test <*62212>  # descending ranges
%! types = {"int8", "int16", "int32", "int64"};
%! for i_type = 1:numel (types)
%!   assert (intmax (types{i_type}) : double (intmin (types{i_type})) : intmin (types{i_type}), ...
%!           [intmax(types{i_type}), -1]);
%!   assert (intmax (types{i_type}) : 2*double (intmin (types{i_type})) : intmin (types{i_type}), ...
%!           intmax (types{i_type}));
%!   if (! strcmp (types, "int64"))
%!     ## The increment cannot be represented in double precision for "int64"
%!     assert (intmax (types{i_type}) : -2*double (intmax (types{i_type})) : intmin (types{i_type}), ...
%!             [intmax(types{i_type}), -intmax(types{i_type})]);
%!   endif
%! endfor

## integer range near intmax
%!test
%! types = {"int8", "int16", "int32", "int64", ...
%!          "uint8", "uint16", "uint32", "uint64"};
%! for i_type = 1:numel (types)
%!   hi = intmax (types{i_type});
%!   lo = hi - 5;
%!   ## ascending range
%!   assert (lo:hi, ...
%!           intmax(types{i_type}) - 5 + (0:5));
%!   ## descending range
%!   assert (hi:-1:lo, ...
%!           intmax(types{i_type}) - 5 + (5:-1:0));
%! endfor

## integer range near intmin
%!test
%! types = {"int8", "int16", "int32", "int64", ...
%!          "uint8", "uint16", "uint32", "uint64"};
%! for i_type = 1:numel (types)
%!   lo = intmin (types{i_type});
%!   hi = lo + 5;
%!   ## ascending range
%!   assert (lo:hi, ...
%!           intmin(types{i_type}) + (0:5));
%!   ## descending range
%!   assert (hi:-1:lo, ...
%!           intmin(types{i_type}) + (5:-1:0));
%! endfor
