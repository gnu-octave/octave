########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{map} =} pink ()
## @deftypefnx {} {@var{map} =} pink (@var{n})
## Create color colormap.  This colormap varies from black to white with
## shades of gray-pink.
##
## This colormap gives a sepia tone when used on grayscale images.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

function map = pink (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("pink: N must be a scalar");
    endif
    n = double (n);
  else
    hf = get (0, "currentfigure");
    if (! isempty (hf))
      n = rows (get (hf, "colormap"));
    else
      n = 64;
    endif
  endif
  if (n == 1)
    map = sqrt ([1/3, 1/3, 1/3]);
  elseif (n == 2)
    map = sqrt ([1/3, 1/3, 1/6
                  1    1    1 ]);
  elseif (n > 2)
    x = [0:(n-1)]' / (n-1);
    idx = floor (3/8 * n);
    base = 1 / (3 * idx);

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
%! ## Show the 'pink' colormap profile and as an image
%! cmap = pink (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);
