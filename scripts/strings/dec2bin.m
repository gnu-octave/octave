########################################################################
##
## Copyright (C) 1996-2022 The Octave Project Developers
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
## If @var{d} is negative, return the two's complement binary value of @var{d}.
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
## dec2bin (14)
##      @result{} "1110"
##
## dec2bin (-14)
##      @result{} "11110010"
## @end group
## @end example
##
## Programming Notes: The largest negative value that can be converted into
## two's complement is @code{- (flintmax () / 2)}.
##
## Known @sc{matlab} Incompatibility: @sc{matlab}'s @code{dec2bin} allows
## non-integer values for @var{d}, truncating the value using the equivalent
## of @code{fix (@var{d})} for positive values, but, as of R2020b and in
## conflict with published documentation, appears to use
## @code{round (@var{d})} for negative values.  To be consistent with
## @code{dec2hex} and @code{dec2base}, Octave produces an error for non-integer
## valued inputs for @var{d}.  Users wanting compatible code for non-integer
## valued inputs should make use of @code{fix} or @code{round} as appropriate.
## @seealso{bin2dec, dec2base, dec2hex}
## @end deftypefn

function bstr = dec2bin (d, len)

  if (nargin == 0)
    print_usage ();
  endif

  if (iscell (d))
    d = cell2mat (d);
  endif
  ## Create column vector for algorithm (output is always col. vector anyways)
  d = d(:);

  lt_zero_idx = (d < 0);
  if (any (lt_zero_idx))
    ## FIXME: Need an algorithm that works with larger values such as int64.
    if (any (d(lt_zero_idx) < -2^52))
      error ("dec2bin: negative inputs cannot be less than -flintmax () / 2");
    elseif (any (d(lt_zero_idx) < intmin ("int32")))
      d(lt_zero_idx) += flintmax ();
    elseif (any (d < intmin ("int16")))
      d(lt_zero_idx) += double (intmax ("uint32")) + 1;
    elseif (any (d < intmin ("int8")))
      d(lt_zero_idx) += double (intmax ("uint16"))+ 1;
    else
      d(lt_zero_idx) += double (intmax ("uint8")) + 1;
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
%!assert (dec2bin (-2^7 -1), "1111111101111111")
%!assert (dec2bin (-2^15 -1), "11111111111111110111111111111111")
## FIXME: Matlab generates a string that is 64 characters long
%!assert (dec2bin (-2^31 -1),
%!        "11111111111111111111101111111111111111111111111111111")
%!assert (dec2bin (-2^52),
%!        "10000000000000000000000000000000000000000000000000000")
## FIXME: Uncomment when support for int64 is added
%!#assert (dec2bin (-2^63),
%!        "1000000000000000000000000000000000000000000000000000000000000000")
%!#test
%! assert (dec2bin (int64 (-2^63)),
%!        "1000000000000000000000000000000000000000000000000000000000000000");
%!#test
%! assert (dec2bin (int64 (-2^63) -1),
%!        "1000000000000000000000000000000000000000000000000000000000000000");
%!#test
%! assert (dec2bin (int64 (-2^63) +1),
%!        "1000000000000000000000000000000000000000000000000000000000000001");
%!assert (dec2bin ([-1, -2; -3, -4]),
%!        ["11111111"; "11111101"; "11111110"; "11111100"])
%!assert (dec2bin ([1, 2; 3, -4]),
%!        ["00000001"; "00000011"; "00000010"; "11111100"])
%!assert (dec2bin ({1, 2; 3, -4}),
%!        ["00000001"; "00000011"; "00000010"; "11111100"])

## Test input validation
%!error <Invalid call> dec2bin ()
%!error <negative inputs cannot be less than> dec2bin (- flintmax ())
