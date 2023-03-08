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
## @deftypefn  {} {@var{hstr} =} dec2hex (@var{d})
## @deftypefnx {} {@var{hstr} =} dec2hex (@var{d}, @var{len})
## Return a string representing the conversion of the integer @var{d} to a
## hexadecimal (base16) number.
##
## If @var{d} is negative, return the hexadecimal complement of @var{d}.
##
## If @var{d} is a matrix or cell array, return a string matrix with one row
## for each element in @var{d}, padded with leading zeros to the width of the
## largest value.
##
## The optional second argument, @var{len}, specifies the minimum number of
## digits in the result.
##
## Examples:
##
## @example
## @group
## dec2hex (2748)
##      @result{} "ABC"
##
## dec2hex (-2)
##      @result{} "FE"
## @end group
## @end example
##
## Programming tip: @code{dec2hex} discards any fractional part of the input.
## If you need the fractional part to be converted too, call @code{dec2base}
## with a nonzero number of decimal places.  You can also use @code{fix} or
## @code{round} on fractional inputs to ensure predictable rounding behavior.
##
## @seealso{hex2dec, dec2base, dec2bin}
## @end deftypefn

function hstr = dec2hex (d, len)

  if (nargin == 0)
    print_usage ();
  endif

  if (iscell (d))
    d = cell2mat (d);
  endif
  d = d(:);

  neg = (d < 0);

  if (nargin == 2)
    d = dec2bin (d, len*4);
  else
    d = dec2bin (d);
  endif

  ## Left-pad to a multiple of 4 columns.
  n = mod (columns (d), 4);
  if (n > 0)
    tmp = "01"(neg + 1);  # leftpad with "0" for positive, "1" for negative
    d = [repmat(tmp(:), 1, 4 - n), d];
  endif

  d -= '0';  # convert to numeric
  d = d(:, 1:4:end) * 8 + d(:, 2:4:end) * 4 + d(:, 3:4:end) * 2 + d(:, 4:4:end);
  ## Elements of d are now in the range 0 to 15.

  hstr = "0123456789ABCDEF"(d+1);
  if (rows (hstr) < rows (d))  # this edge case happens when
    hstr = hstr(:);            # passing multiple inputs in the range 0 to 15.
    ## If we don't manually convert it to column, we'd get all those
    ## hex digits on the same line as one big string instead of one per line.
    ## Good test for this:    dec2hex (0:15)
    ## compared with:         dec2hex (uint64 (81985529216486895), 16)
  endif

endfunction


%!assert (dec2hex (2748), "ABC")
%!assert (dec2hex (2748, 5), "00ABC")
%!assert (dec2hex ([2748, 2746]), ["ABC"; "ABA"])
%!assert (dec2hex ({2748, 2746}), ["ABC"; "ABA"])
%!assert (dec2hex ({2748, 2746}, 4), ["0ABC"; "0ABA"])

## Test negative inputs
%!assert (dec2hex (-3), "FD")
%!assert (dec2hex (-3, 1), "FD")
%!assert (dec2hex (-3, 3), "FFD")
%!assert (dec2hex (-2^7 - 1), "FF7F")
%!assert (dec2hex (-2^15 - 1), "FFFF7FFF")
%!assert (dec2hex (-2^31 - 1), "FFFFFFFF7FFFFFFF")
%!assert (dec2hex (-2^52), "FFF0000000000000")
%!assert (dec2hex (-2^63), "8000000000000000")
%!assert (dec2hex (int64 (-2) ^ 63), "8000000000000000")
%!assert (dec2hex (int64 (-2) ^ 63 - 1), "8000000000000000")
%!assert (dec2hex (int64 (-2) ^ 63 + 1), "8000000000000001")
%!assert (dec2hex ([-1, -2; -3, -4]), ["FF"; "FD"; "FE"; "FC"])
%!assert (dec2hex ([1, 2; 3, -4]), ["01"; "03"; "02"; "FC"])
%!assert (dec2hex ({1, 2; 3, -4}), ["01"; "03"; "02"; "FC"])

## Test that the output is of the correct shape.
## Next line should return a column vector:
%!assert (dec2hex (0:15), "0123456789ABCDEF"(:))
## Next line should return a row vector:
%!assert (dec2hex (uint64 (18364758544493064720)), "FEDCBA9876543210")

## Test input validation
%!error <Invalid call> dec2hex ()

