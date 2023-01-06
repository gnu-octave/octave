########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{bstr} =} dec2bin (@var{d})
## @deftypefnx {} {@var{bstr} =} dec2bin (@var{d}, @var{len})
## Return a string of ones and zeros representing the conversion of the integer
## @var{d} to a binary number.
##
## If @var{d} is a matrix or cell array, return a string matrix with one row
## for each element in @var{d}, padded with leading zeros to the width of the
## largest value.
##
## The optional second argument, @var{len}, specifies the minimum number of
## digits in the result.
##
## For negative elements of @var{d}, return the binary value of the two's
## complement.  The result is padded with leading ones to 8, 16, 32, or 64
## bits as appropriate for the magnitude of the input.  Positive input
## elements are padded with leading zeros to the same width.  If the second
## argument @var{len} exceeds that calculated width, the result is further
## padded with leading zeros, for compatibility with @sc{matlab}.
##
## Examples:
##
## @example
## @group
## dec2bin (14)
##      @result{} "1110"
##
## dec2bin (-14)
##      @result{} "11110010"
## @end group
## @end example
##
## Known @sc{matlab} Incompatibility: @sc{matlab}'s @code{dec2bin} allows
## non-integer values for @var{d} as of Release 2022b, but is inconsistent
## with truncation versus rounding and is also inconsistent with its own
## @code{dec2hex} function.  For self-consistency, Octave gives an error for
## non-integer inputs.  Users requiring compatible code for non-integer inputs
## should make use of @code{fix} or @code{round} as appropriate.
## @seealso{bin2dec, dec2base, dec2hex}
## @end deftypefn

function bstr = dec2bin (d, len)

  if (nargin == 0)
    print_usage ();
  endif

  if (iscell (d))
    d = cell2mat (d);
  endif

  if (! isnumeric (d) || iscomplex (d) || any (d(:) != round (d(:))))
    error ("dec2bin: input must be integers");
  endif

  ## Create column vector for algorithm (output is always col. vector anyways)
  d = d(:);

  neg = (d < 0);  # keep track of which elements are negative
  if (any (neg))  # must be a signed type
    ## Cast to a suitable signed integer type, then to unsigned.
    ## Ensure that the left-most bit of the unsigned number is 1,
    ## to signify negative input.
    tmp = int64 (d);
    if (all (tmp >= -128 & tmp <= 127))
      d = int8 (d);
      d(neg) = (d(neg) + intmax (d)) + 1;
      d = uint8 (d);
      d(neg) += uint8 (128);
    elseif (all (tmp >= -32768 & tmp <= 32767))
      d = int16 (d);
      d(neg) = (d(neg) + intmax (d)) + 1;
      d = uint16 (d);
      d(neg) += uint16 (32768);
    elseif (all (tmp >= -2147483648 & tmp <= 2147483647))
      d = int32 (d);
      d(neg) = (d(neg) + intmax (d)) + 1;
      d = uint32 (d);
      d(neg) += uint32 (2147483648);
    else
      d = int64 (d);
      d(neg) = (d(neg) + intmax (d)) + 1;
      d = uint64 (d);
      d(neg) += uint64 (9223372036854775808);
    endif
  endif

  if (nargin == 1)
    bstr = dec2base (d, 2);
  else
    bstr = dec2base (d, 2, len);
  endif

endfunction


%!assert (dec2bin (3), "11")
%!assert (dec2bin (14), "1110")
%!assert (dec2bin (14, 6), "001110")
%!assert (dec2bin ([1, 2; 3, 4]), ["001"; "011"; "010"; "100"])
%!assert (dec2bin ({1, 2; 3, 4}), ["001"; "011"; "010"; "100"])
%!assert (dec2bin ({1, 2; 3, 4}, 4), ["0001"; "0011"; "0010"; "0100"])

## Test negative inputs
%!assert (dec2bin (-3), "11111101")
%!assert (dec2bin (-3, 3), "11111101")
%!assert (dec2bin (-3, 9), "011111101")
%!assert (dec2bin (-2^7 - 1), "1111111101111111")
%!assert (dec2bin (-2^15 - 1), "11111111111111110111111111111111")
%!assert (dec2bin (-2^31 - 1),
%!        "1111111111111111111111111111111101111111111111111111111111111111")
%!assert (dec2bin (-2^52),
%!        "1111111111110000000000000000000000000000000000000000000000000000")
%!assert (dec2bin (-2^63),
%!        "1000000000000000000000000000000000000000000000000000000000000000")
%!assert (dec2bin (int64 (-2) ^ 63),
%!        "1000000000000000000000000000000000000000000000000000000000000000")
%!assert (dec2bin (int64 (-2) ^ 63 - 1),
%!        "1000000000000000000000000000000000000000000000000000000000000000")
%!assert (dec2bin (int64 (-2) ^ 63 + 1),
%!        "1000000000000000000000000000000000000000000000000000000000000001")
%!assert (dec2bin ([-1, -2; -3, -4]),
%!        ["11111111"; "11111101"; "11111110"; "11111100"])
%!assert (dec2bin ([1, 2; 3, -4]),
%!        ["00000001"; "00000011"; "00000010"; "11111100"])
%!assert (dec2bin ({1, 2; 3, -4}),
%!        ["00000001"; "00000011"; "00000010"; "11111100"])

## Test input validation
%!error <Invalid call> dec2bin ()
%!error <input must be integer> dec2bin (+2.1)
%!error <input must be integer> dec2bin (-2.1)
%!error <input must be integer> dec2bin (+2.9)
%!error <input must be integer> dec2bin (-2.9)
%!error <input must be integer> dec2bin (1+i)

