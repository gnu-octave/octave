## Copyright (C) 2000  Kai Habel
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
## @deftypefn {Function File} {} pink (@var{n})
## Create color colormap. This colormap gives a sephia tone on black and
## white images. The argument @var{n} should be a scalar.  If it
## is omitted, the length of the current colormap or 64 is assumed.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function map = pink (number)

  if (nargin == 0)
    number = rows (colormap);
  elseif (nargin == 1)
    if (! is_scalar (number))
      error ("pink: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (number == 1)
    map = [0, 0, 0];  
  elseif (number > 1)
    x = linspace (0, 1, number)';
    r = (x < 3/8) .* (14/9 * x) + (x >= 3/8) .* (2/3 * x + 1/3);
    g = (x < 3/8) .* (2/3 * x)\
      + (x >= 3/8 & x < 3/4) .* (14/9 * x - 1/3)\
      + (x >= 3/4) .* (2/3 * x + 1/3);
    b = (x < 3/4) .* (2/3 * x) + (x >= 3/4) .* (2 * x - 1);

    map = sqrt ([r, g, b]);
  else
    map = [];
  endif

endfunction
