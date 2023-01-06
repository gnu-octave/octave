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
## @deftypefn {} {@var{d} =} bin2dec (@var{str})
## Return the decimal number corresponding to the binary number represented
## by the string @var{str}.
##
## For example:
##
## @example
## @group
## bin2dec ("1110")
##      @result{} 14
## @end group
## @end example
##
## Spaces are ignored during conversion and may be used to make the binary
## number more readable.
##
## @example
## @group
## bin2dec ("1000 0001")
##      @result{} 129
## @end group
## @end example
##
## If @var{str} is a string matrix, return a column vector with one converted
## number per row of @var{str}; Invalid rows evaluate to NaN@.
##
## If @var{str} is a cell array of strings, return a column vector with one
## converted number per cell element in @var{str}.
## @seealso{dec2bin, base2dec, hex2dec}
## @end deftypefn

function d = bin2dec (str)

  if (nargin != 1)
    print_usage ();
  endif

  d = base2dec (str, 2);

endfunction


%!assert (bin2dec ("0000"), 0)
%!assert (bin2dec ("1110"), 14)
%!assert (bin2dec ("11111111111111111111111111111111111111111111111111111"),
%!        2^53-1)
%!assert (bin2dec ({"1110", "1111"}), [14; 15])
%!assert (bin2dec ("1 0 1"), 5)
%!assert (bin2dec (char ("1 0 1", "   1111")), [5; 15])

## Test input validation
%!error <Invalid call> bin2dec ()
%!error <STR must be a string> bin2dec (1)
