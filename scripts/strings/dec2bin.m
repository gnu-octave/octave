## Copyright (C) 2001 Daniel Calvelo
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} dec2bin (@var{n})
## Return a binary number corresponding the nonnegative decimal number
## @var{n}, as a string of ones and zeros.  For example,
##
## @example
## dec2bin (14)
##      @result{} "1110"
## @end example
##
## If @var{n} is a vector, returns a string matrix, one row per value,
## padded with leading zeros to the width of the largest value.
## @end deftypefn
##
## @seealso{bin2dec, dec2base, base2dec, hex2dec, dec2hex}

## Author: Daniel Calvelo
## 2001-02-02 Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function h = dec2bin (d)

  if (nargin != 1)
    usage ("dec2bin (b)");
  else
    h = dec2base (d, 2);
  endif

endfunction
