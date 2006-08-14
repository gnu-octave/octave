## Copyright (C) 2000 Daniel Calvelo
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} bin2dec (@var{s})
## Return the decimal number corresponding to the binary number stored
## in the string @var{s}.  For example,
##
## @example
## bin2dec ("1110")
##      @result{} 14
## @end example
##
## If @var{s} is a string matrix, returns a column vector of converted
## numbers, one per row of @var{s}.  Invalid rows evaluate to NaN.
## @seealso{dec2hex, base2dec, dec2base, hex2dec, dec2bin}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function d = bin2dec (h)

  if (nargin == 1 && ischar (h))
    n = rows (h);
    d = zeros (n, 1);
    for i = 1:n
      s = h(i,:);
      s = s(! isspace (s));
      d(i) = base2dec (s, 2);
    endfor
  else
    print_usage ();
  endif

endfunction
