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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} dec2hex (@var{n})
## Return the hexadecimal string corresponding to the nonnegative 
## integer @var{n}.  For example,
##
## @example
## dec2hex (2748)
##      @result{} "ABC"
## @end example
##
## If @var{n} is a vector, returns a string matrix, one row per value,
## padded with leading zeros to the width of the largest value.
## @end deftypefn
##
## @seealso{hex2dec, dec2base, base2dec, bin2dec, dec2bin}

## Author: Daniel Calvelo
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function h = dec2hex (d)

  if (nargin != 1)
    usage ("dec2hex (b)");
  else
    h = dec2base (d, 16);
  endif

endfunction
