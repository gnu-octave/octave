## Copyright (C) 2000-2012 Kai Habel
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
## @deftypefn  {Function File} {@var{map} =} pink ()
## @deftypefnx {Function File} {@var{map} =} pink (@var{n})
## Create color colormap.  This colormap varies from black to white with
## shades of gray-pink.  It gives a sepia tone when used on grayscale images.
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function map = pink (n)

  if (nargin == 0)
    n = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("pink: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (n == 1)
    map = [0, 0, 0];
  elseif (n > 1)
    x = linspace (0, 1, n)';
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

%!demo
%! ## Show the 'pink' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat (1:64, 64, 1)')
%! axis ([1, 64, 0, 1], "ticy", "xy")
%! colormap (pink (64))

