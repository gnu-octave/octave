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

## usage: gammai (a, x)
##
## Compute the incomplete gamma function
##
##   gammai (a, x) = (\int_0^x exp(-t) t^(a-1) dt) / gamma(a).
##
## If a is scalar, then gammai (a, x) is returned for each element of x
## and vice versa.
##
## If neither a nor x is scalar, the sizes of a and x must agree, and
## gammainc is applied for corresponding elements of x and a.

## Author: jwe
## Created: 30 Jan 1998

function retval = gammai (a, x)

  if (nargin == 2)
    retval = gammainc (x, a);
  else
    usage ("gammai (a, x)");
  endif

endfunction
