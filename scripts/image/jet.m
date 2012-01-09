## Copyright (C) 1999-2012 Kai Habel
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
## @deftypefn  {Function File} {@var{map} =} jet ()
## @deftypefnx {Function File} {@var{map} =} jet (@var{n})
## Create color colormap.  This colormap ranges from dark blue through blue,
## cyan, green, yellow, red, to dark red.
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function map = jet (n)

  if (nargin == 0)
    n = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("jet: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (n == 1)
    map = [0, 0, 0.5];
  elseif (n > 1)
    x = linspace(0, 1, n)';
    r = (x >= 3/8 & x < 5/8) .* (4 * x - 3/2)\
      + (x >= 5/8 & x < 7/8) + (x >= 7/8) .* (-4 * x + 9/2);
    g = (x >= 1/8 & x < 3/8) .* (4 * x - 1/2)\
      + (x >= 3/8 & x < 5/8) + (x >= 5/8 & x < 7/8) .* (-4 * x + 7/2);
    b = (x < 1/8) .* (4 * x + 1/2) + (x >= 1/8 & x < 3/8)\
      + (x >= 3/8 & x < 5/8) .* (-4 * x + 5/2);
    map = [r, g, b];
  else
    map = [];
  endif

endfunction

%!demo
%! ## Show the 'jet' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat (1:64, 64, 1)')
%! axis ([1, 64, 0, 1], "ticy", "xy")
%! colormap (jet (64))

