## Copyright (C) 1996, 2000, 2002, 2003, 2004, 2005, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} freqchkw (@var{w})
## Used by @command{__freqresp__} to check that input frequency vector @var{w}
## is valid.
## Returns boolean value.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1996

function USEW = freqchkw (w)

  if (isempty (w))
    USEW = 0;
  elseif (! isvector (w))
    error ("w (%dx%d): must be [], a vector or a scalar",
	   rows (w), columns (w));
  elseif (max (abs (imag(w))) != 0 && min (real (w)) <= 0)
    error ("w must have real positive entries");
  else
    w = sort (w);
    USEW = 1;   ## vector provided (check values later)
  endif

endfunction
