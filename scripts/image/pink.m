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

## PKG_ADD: colormap ("register", "pink");
## PKG_DEL: colormap ("unregister", "pink");

function map = pink (n)

  if (nargin == 0)
    n = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("pink: N must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (n == 1)
    map = sqrt ([1/3, 1/3, 1/3]);
  elseif (n == 2)
    map = sqrt ([1/3, 1/3, 1/6
                  1    1    1 ]);
  elseif (n > 2)
    x = [0:(n-1)]' / (n-1);
    idx = floor (3/8 * n);
    base = floor (n/8);
    switch (mod (n, 8))
      case {0, 1, 2}
        base = 1 / (9*base);
      case {3, 4, 5}
        base = 1 / (9*base + 3);
      case {6, 7}
        base = 1 / (9*base + 6);
    endswitch

    nel = idx;   # number of elements
    r(1:idx,1) = linspace (base, 2/3*x(idx) + 1/3, nel);
    r(idx+1:n,1) = 2/3*x(idx+1:n) + 1/3;

    g(1:idx,1) = 2/3*x(1:idx);
    g(idx:2*idx,1) = linspace (2/3*x(idx), 2/3*x(2*idx) + 1/3, nel+1);
    g(2*idx+1:n,1) = 2/3*x(2*idx+1:n) + 1/3;

    nel = n - 2*idx + 1;
    b(1:2*idx,1) = 2/3*x(1:2*idx);
    b(2*idx:n,1) = linspace (2/3*x(2*idx), 1, nel);

    map = sqrt ([r, g, b]);
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'pink' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (pink (64));

