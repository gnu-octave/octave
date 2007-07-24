## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} ocean (@var{n})
## Create color colormap.  The argument @var{n} should be a scalar.  If it
## is omitted, the length of the current colormap or 64 is assumed.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function map = ocean (number)

  if (nargin == 0)
    number = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (number))
      error ("ocean: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  cutin = fix (number/3);

  dr = (number - 1) / cutin;

  r = prepad ([0:dr:(number-1)], number)';

  dg = (number - 1) / (2 * cutin);

  g = prepad([0:dg:(number-1)], number)';

  b = [0:(number-1)]';

  map = [ r, g, b ] / (number - 1);

endfunction
