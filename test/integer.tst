########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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

## Test saturation mechanics of lower bound
%!test
%! for cls = {"int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64"}
%!   clsmin = intmin (cls{1});
%!   xplus = clsmin + (-1);
%!   assert (xplus, clsmin);
%!   xminus = clsmin -1;
%!   assert (xminus, clsmin);
%!   xmult = clsmin * 2;
%!   assert (xmult, clsmin);
%!   xdiv = clsmin / 0.5;
%!   assert (xdiv, clsmin);
%! endfor

## Test saturation mechanics of upper bound
%!test
%! for cls = {"int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64"}
%!   clsmax = intmax (cls{1});
%!   xplus = clsmax + 1;
%!   assert (xplus, clsmax);
%!   xminus = clsmax - (-1);
%!   assert (xminus, clsmax);
%!   xmult = clsmax * 2;
%!   assert (xmult, clsmax);
%!   xdiv = clsmax / 0.5;
%!   assert (xdiv, clsmax);
%! endfor

## Tests for binary constants
%!assert (0b1, uint8 (2^0))
%!assert (0b10000000, uint8 (2^7))
%!assert (0b11111111, intmax ("uint8"))
%!assert (0b100000000, uint16 (2^8))
%!assert (0b1000000000000000, uint16 (2^15))
%!assert (0b1111111111111111, intmax ("uint16"))
%!assert (0b10000000000000000, uint32 (2^16))
%!assert (0b10000000000000000000000000000000, uint32 (2^31))
%!assert (0b11111111111111111111111111111111, intmax ("uint32"))
%!assert (0b100000000000000000000000000000000, uint64 (2^32))
%!assert (0b1000000000000000000000000000000000000000000000000000000000000000, uint64 (2^63))
%!assert (0b1111111111111111111111111111111111111111111111111111111111111111, intmax ("uint64"))
%!error <too many digits for binary constant> eval ("0b11111111111111111111111111111111111111111111111111111111111111111")

%!assert (0b1u16, uint16 (2^0))
%!assert (0b10000000u16, uint16 (2^7))

%!assert (0b1u32, uint32 (2^0))
%!assert (0b10000000u32, uint32 (2^7))
%!assert (0b1000000000000000u32, uint32 (2^15))

%!assert (0b1u64, uint64 (2^0))
%!assert (0b10000000u64, uint64 (2^7))
%!assert (0b1000000000000000u64, uint64 (2^15))
%!assert (0b10000000000000000000000000000000u64, uint64 (2^31))

%!assert (0b1s16, int16 (2^0))
%!assert (0b10000000s16, int16 (2^7))

%!assert (0b1s32, int32 (2^0))
%!assert (0b10000000s32, int32 (2^7))
%!assert (0b1000000000000000s32, int32 (2^15))

%!assert (0b1s64, int64 (2^0))
%!assert (0b10000000s64, int64 (2^7))
%!assert (0b1000000000000000s64, int64 (2^15))
%!assert (0b10000000000000000000000000000000s64, int64 (2^31))

## Tests for hexadecimal constants
%!assert (0x1, uint8 (2^0))
%!assert (0x80, uint8 (2^7))
%!assert (0xff, intmax ("uint8"))
%!assert (0x100, uint16 (2^8))
%!assert (0x8000, uint16 (2^15))
%!assert (0xffff, intmax ("uint16"))
%!assert (0x10000, uint32 (2^16))
%!assert (0x80000000, uint32 (2^31))
%!assert (0xffffffff, intmax ("uint32"))
%!assert (0x100000000, uint64 (2^32))
%!assert (0x8000000000000000, uint64 (2^63))
%!assert (0xffffffffffffffff, intmax ("uint64"))
%!error <too many digits for hexadecimal constant> eval ("0xfffffffffffffffff")

%!assert (0x1u16, uint16 (2^0))
%!assert (0x80u16, uint16 (2^7))

%!assert (0x1u32, uint32 (2^0))
%!assert (0x80u32, uint32 (2^7))
%!assert (0x8000u32, uint32 (2^15))

%!assert (0x1u64, uint64 (2^0))
%!assert (0x80u64, uint64 (2^7))
%!assert (0x8000u64, uint64 (2^15))
%!assert (0x80000000u64, uint64 (2^31))

%!assert (0x1s16, int16 (2^0))
%!assert (0x80s16, int16 (2^7))

%!assert (0x1s32, int32 (2^0))
%!assert (0x80s32, int32 (2^7))
%!assert (0x8000s32, int32 (2^15))

%!assert (0x1s64, int64 (2^0))
%!assert (0x80s64, int64 (2^7))
%!assert (0x8000s64, int64 (2^15))
%!assert (0x80000000s64, int64 (2^31))

## Tests for decimal constants with extreme values

%!assert (uint64 (9007199254740992), uint64 (flintmax ()))
%!assert (int64 (9007199254740992), int64 (flintmax ()))
%!assert (uint64 (-9007199254740992), uint64 (-flintmax ()))
%!assert (int64 (-9007199254740992), int64 (-flintmax ()))

%!assert (uint64 (9007199254740993), uint64 (flintmax ())+1)
%!assert (int64 (9007199254740993), int64 (flintmax ())+1)
%!assert (uint64 (-9007199254740993), uint64 (-flintmax ())-1)
%!assert (int64 (-9007199254740993), int64 (-flintmax ())-1)

%!assert (uint64 (18446744073709551615), intmax ("uint64"))

%!assert (int64 (9223372036854775807), intmax ("int64"))
%!assert (int64 (-9223372036854775808), intmin ("int64"))

%!test
%! a = int64 ([9223372036854775803; 9223372036854775804; 9223372036854775805; 9223372036854775806; 9223372036854775807]);
%! bval = int64 (9223372036854775807);
%! b = [bval; bval; bval; bval; bval];
%! assert (a, b);

%!test
%! a = int64 ([int64(9223372036854775803); 9223372036854775804; 9223372036854775805; 9223372036854775806; 9223372036854775807]);
%! b0val = int64 (9223372036854775803);
%! bval = int64 (9223372036854775807);
%! b = [b0val; bval; bval; bval; bval];
%! assert (a, b);
