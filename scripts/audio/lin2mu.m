## Copyright (C) 1996 John W. Eaton
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

## usage:  y = lin2mu (x)
##
## x is a vector of an 8- or 16-bit linearly encoded audio sample,
## This is transformed into a mu-law encoded vector.

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 17 October 1994
## Adapted-By: jwe

function y = lin2mu (x)
  
  if (nargin != 1)
    usage ("y = lin2mu (x)");
  endif

  if (! is_vector (x))
    error ("lin2mu: x must be a vector");
  endif
  
  ## transform 8-bit format to 16-bit
  if (max (abs (x)) <= 128)
    x = 256 .* x;
  endif

  ## determine sign of x, set sign(0) = 1.
  sig = sign(x) + (x == 0);

  ## take absolute value of x, but force it to be smaller than 32636;
  ## add bias 
  x = min (abs (x), 32635 * ones (size (x))) + 132;

  ## find exponent and fraction of bineary representation
  [f, e] = log2 (x);

  y = 64 * sig - 16 * e - fix (32 * f) + 335;

endfunction
