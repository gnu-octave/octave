## Copyright (C) 1998 John W. Eaton
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

## usage: beta (a, b, x)
##
## Compute the incomplete beta function
##
##  betai (a, b, x) = beta(a,b)^(-1) \int_0^x t^(a-1) (1-t)^(b-1) dt
##
## The sizes of x, a, and b must agree.

## Author: jwe
## Created: 30 Jan 1998

function retval = betai (a, b, x)

  if (nargin == 3)
    retval = betainc (x, a, b);
  else
    usage ("betai (a, b, x)");
  endif

endfunction
