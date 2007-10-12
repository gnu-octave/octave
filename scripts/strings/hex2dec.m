## Copyright (C) 2000 Daniel Calvelo
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} hex2dec (@var{s})
## Return the integer corresponding to the hexadecimal number stored
## in the string @var{s}.  For example,
##
## @example
## hex2dec ("12B")
##      @result{} 299
## hex2dec ("12b")
##      @result{} 299
## @end example
##
## If @var{s} is a string matrix, returns a column vector of converted
## numbers, one per row of @var{s}.  Invalid rows evaluate to NaN.
## @seealso{dec2hex, base2dec, dec2base, bin2dec, dec2bin}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function d = hex2dec (h)

  if (nargin != 1)
    print_usage ();
  else
    d = base2dec (h, 16);
  endif

endfunction
