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
## elements are padded with leading zeros to the same width.
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
## Programming tip: @code{dec2bin} discards any fractional part of the input.
## If you need the fractional part to be converted too, call @code{dec2base}
## with a nonzero number of decimal places.  You can also use @code{fix} or
## @code{round} on fractional inputs to ensure predictable rounding behavior.
##
## @seealso{bin2dec, dec2base, dec2hex}
## @end deftypefn

function bstr = dec2bin (d, len)

  if (nargin == 0)
    print_usage ();
  endif

  if (iscell (d))
    d = cell2mat (d);
  endif
  d = d(:);

  if (nargin == 1)
    bstr = dec2base (d, 2);  # this will use a default len picked by dec2base
  else  # nargin == 2
    bstr = dec2base (d, 2, len);
  endif

  if (all (d >= 0))
    return
  endif

  ## If we are here, there are negative inputs, so we need to
  ## left-pad those outputs with ones to Matlab-compatible lengths.
  len = columns (bstr);
  if (all (d >= -128 & d <= 127))
    len = max (len, 8);  # pad to 8 bits
  elseif (all (d >= -32768 & d <= 32767))
    len = max (len, 16);  # pad to 16 bits
  elseif (all (d >= -2147483648 & d <= 2147483647))
    len = max (len, 32);  # pad to 32 bits
  else
    len = max (len, 64);  # pad to 64 bits
  endif

  tmp = repmat (' ', rows (bstr), len);
  tmp(:, (end+1-columns(bstr)):end) = bstr;  # left-pad with spaces
  bstr = tmp;

  ## Change spaces to "1" for negative inputs
  tmp = bstr(d < 0, :);
  tmp(tmp == ' ') = '1';
  bstr(d < 0, :) = tmp;

  ## Change all other spaces to "0".
  bstr(bstr == ' ') = '0';

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
%!assert (dec2bin (-3, 9), "111111101")
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

## Test fractional inputs
%!assert (dec2bin (+2.1), "10")
%!assert (dec2bin (-2.1), "11111110")
%!assert (dec2bin (+2.9), "10")
%!assert (dec2bin (-2.9), "11111110")

## Test input validation
%!error <Invalid call> dec2bin ()
%!error <input must be real> dec2bin (1+i);

