########################################################################
##
## Copyright (C) 1999-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{map} =} hsv ()
## @deftypefnx {} {@var{map} =} hsv (@var{n})
## Create color colormap.  This colormap begins with red, changes through
## yellow, green, cyan, blue, and magenta, before returning to red.
##
## It is useful for displaying periodic functions.  The map is obtained by
## linearly varying the hue through all possible values while keeping constant
## maximum saturation and value.  The equivalent code is
## @code{hsv2rgb ([(0:N-1)'/N, ones(N,2)])}.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

function map = hsv (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("hsv: N must be a scalar");
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
    map = [1, 0, 0];
  elseif (n > 1)
    hue = [0:n-1]' / n;
    map = hsv2rgb ([hue, ones(n,1), ones(n,1)]);
  else
    map = zeros (0, 3);
  endif

endfunction


## A better demo of this colormap would be to plot the hsv values.
%!demo
%! ## Show the 'hsv' colormap profile and as an image
%! cmap = hsv (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);
