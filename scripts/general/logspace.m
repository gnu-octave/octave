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

## usage: logspace (x1, x2, n)
##
## Return a vector of n logarithmically equally spaced points between
## x1 and x2 inclusive.
##
## If the final argument is omitted, n = 50 is assumed.
##
## All three arguments must be scalars.
##
## Note that if if x2 is pi, the points are between 10^x1 and pi, NOT
## 10^x1 and 10^pi.
##
## Yes, this is pretty stupid, because you could achieve the same
## result with logspace (x1, log10 (pi)), but Matlab does this, and
## claims that is useful for signal processing applications.
##
## See also: linspace

## Author: jwe

function retval = logspace (x1, x2, n)

  if (nargin == 2)
    npoints = 50;
  elseif (nargin == 3)
    if (length (n) == 1)
      npoints = fix (n);
    else
      error ("logspace: arguments must be scalars");
    endif
  else
    usage ("logspace (x1, x2 [, n])");
  endif

  if (npoints < 2)
    error ("logspace: npoints must be greater than 2");
  endif

  if (length (x1) == 1 && length (x2) == 1)
    x2_tmp = x2;
    if (x2 == pi)
      x2_tmp = log10 (pi);
    endif
    retval = 10 .^ (linspace (x1, x2_tmp, npoints));
  else
    error ("logspace: arguments must be scalars");
  endif

endfunction
