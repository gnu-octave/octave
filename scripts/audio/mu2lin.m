### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

## usage: y = mu2lin (x [, bit])
##
## If x is a vector of audio data with mu-law encoding, mu2lin (x)
## holds the same data with linear encoding.
## The optional argument bit specifies whether the input data is 
## 8 bit (default) or 16 bit.

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 18 October 1994
## Adapted-By: jwe

function y = mu2lin (x, bit)
  
  if (nargin == 1)
    bit = 8;
  elseif (nargin == 2)
    if (bit != 8 && bit != 16)
      error ("mu2lin: bit must be either 8 or 16");
    endif
  else
    usage ("y = mu2lin (x [, bit])");
  endif

  if (! is_vector (x))
    error ("mu2lin: x must be a vector");
  endif
  
  exp_lut = [0; 132; 396; 924; 1980; 4092; 8316; 16764];

  ## invert x bitwise
  x = 255 - x;

  ## determine sign of y
  sig = (x > 127);

  ## determine exponent and fraction of y
  e = fix (x / 16) - 8 .* sig + 1;
  f = rem (x, 16);

  sig = 1 - 2 .* sig;
  y = (pow2 (f, e + 2) + exp_lut (e)) .* sig;

  ## convert to 8-bit
  if (bit == 8)
    ld = max (abs (y));
    if (ld < 16384)
      sc = 64 / ld;
    else
      sc = 1 / 256;
    endif
    y = fix (y * sc);
  endif

endfunction
