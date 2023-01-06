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
## @deftypefn  {} {@var{B} =} bitset (@var{A}, @var{n})
## @deftypefnx {} {@var{B} =} bitset (@var{A}, @var{n}, @var{val})
## Set or reset bit(s) at position @var{n} of the unsigned integers in @var{A}.
##
## The least significant bit is @var{n} = 1.  @w{@var{val} = 0} resets bits and
## @w{@var{val} = 1} sets bits.  If no @var{val} is specified it defaults to
## 1 (set bit).  All inputs must be the same size or scalars.
##
## Example 1: Set multiple bits
##
## @example
## @group
## x = bitset (1, 3:5)
##   @result{} x =
##
##    5    9   17
##
## dec2bin (x)
##   @result{}
##      00101
##      01001
##      10001
## @end group
## @end example
##
## Example 2: Reset and set bits
##
## @example
## @group
## x = bitset ([15 14], 1, [0 1])
##   @result{} x =
##
##    14    15
## @end group
## @end example
## @seealso{bitand, bitor, bitxor, bitget, bitcmp, bitshift, intmax, flintmax}
## @end deftypefn

function B = bitset (A, n, val = true)

  if (nargin < 2)
    print_usage ();
  endif

  if (any (A(:) < 0))
    error ("bitset: A must be >= 0");
  endif

  [size_err, A, n, val] = common_size (A, n, val);
  if (size_err)
    error ("bitset: A, N, and VAL must be the same size or scalar");
  endif

  ## Special case of empty input
  if (isempty (A))
    B = [];
    return;
  endif

  sz = size (A);
  cls = class (A);

  if (isfloat (A) && isreal (A))
    Bmax = flintmax (cls);
    Amax = ceil (log2 (Bmax));
  elseif (isinteger (A))
    Bmax = intmax (cls);
    Amax = ceil (log2 (Bmax));
  else
    error ("bitset: invalid class %s", cls);
  endif

  if (any (n(:) < 1) || any (n(:) > Amax))
    error ("bitset: N must be in the range [1,%d]", Amax);
  endif

  mask = bitshift (cast (1, cls), uint8 (n) - uint8 (1));

  on = logical (val);
  off = ! on;

  if (isscalar (mask))
    onmask = mask;
    offmask = mask;
  else
    onmask = mask(on);
    offmask = mask(off);
  endif

  B = zeros (sz, cls);
  B(on) = bitor (A(on), onmask);
  B(off) = bitand (A(off), bitcmp (offmask));

endfunction


%!test
%! assert (bitset ([0, 10], [3, 3]), [4, 14]);
%! assert (bitset (single ([0, 10]), [3, 3]), single ([4, 14]));
%! pfx = {"", "u"};
%! for i = 1:2
%!   for prec = [8, 16, 32, 64]
%!     fcn = str2func (sprintf ("%sint%d", pfx{i}, prec));
%!     assert (bitset (fcn ([0, 10]), [3, 3]), fcn ([4, 14]));
%!   endfor
%! endfor

## Special case of empty input
%!assert (bitset ([], 1), [])

%!assert <*36458> (bitset (uint8 ([1, 2;3 4]), 1, [0 1; 0 1]),
%!                 uint8 ([0, 3; 2 5]))

%!assert (bitset (1:5, 1), [1, 3, 3, 5, 5])
%!assert (bitset (1:5, 1, [1, 1, 1, 1, 1]), [1, 3, 3, 5, 5])
%!assert <*54110> (bitset (1:5, 1, 1), [1, 3, 3, 5, 5])
%!assert (bitset (1:5, 1, [1, 1, 1, 1, 0]), [1, 3, 3, 5, 4])

## Test input validation
%!error <Invalid call> bitset ()
%!error <Invalid call> bitset (1)
%!error <A must be .= 0> bitset (-1, 2)
%!error <must be the same size or scalar> bitset ([1 2], [1 2 3])
%!error <must be the same size or scalar> bitset (1, [1 2], [1 2 3])
%!error <invalid class char> bitset ("1", 2)
%!error <N must be in the range \[1,53\]> bitset (0, 0)
%!error <N must be in the range \[1,53\]> bitset (0, 55)
%!error <N must be in the range \[1,24\]> bitset (single (0), 0)
%!error <N must be in the range \[1,24\]> bitset (single (0), 26)
%!error <N must be in the range \[1,7\]> bitset (int8 (0), 0)
%!error <N must be in the range \[1,7\]> bitset (int8 (0), 9)
%!error <N must be in the range \[1,8\]> bitset (uint8 (0), 9)
%!error <N must be in the range \[1,15\]> bitset (int16 (0), 17)
%!error <N must be in the range \[1,16\]> bitset (uint16 (0), 17)
%!error <N must be in the range \[1,31\]> bitset (int32 (0), 33)
%!error <N must be in the range \[1,32\]> bitset (uint32 (0), 33)
%!error <N must be in the range \[1,63\]> bitset (int64 (0), 65)
%!error <N must be in the range \[1,64\]> bitset (uint64 (0), 65)
