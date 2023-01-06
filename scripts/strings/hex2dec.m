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
## @deftypefn {} {@var{d} =} hex2dec (@var{str})
## Return the integer corresponding to the hexadecimal number represented by
## the string @var{str}.
##
## For example:
##
## @example
## @group
## hex2dec ("12B")
##       @result{} 299
## hex2dec ("12b")
##       @result{} 299
## @end group
## @end example
##
## If @var{str} is a string matrix, return a column vector with one converted
## number per row of @var{str}; Invalid rows evaluate to NaN@.
##
## If @var{str} is a cell array of strings, return a column vector with one
## converted number per cell element in @var{str}.
##
## @seealso{dec2hex, base2dec, bin2dec}
## @end deftypefn

function d = hex2dec (str)

  if (nargin != 1)
    print_usage ();
  endif

  d = base2dec (str, 16);

endfunction


%!assert (hex2dec ("0000"), 0)
%!assert (hex2dec ("1FFFFFFFFFFFFF"), 2^53-1)
%!assert (hex2dec (["12b"; "12B"]), [299; 299])
%!assert (hex2dec ({"A1", "1A"}), [161; 26])

## Test input validation
%!error <Invalid call> hex2dec ()
%!error <STR must be a string> hex2dec (1)
