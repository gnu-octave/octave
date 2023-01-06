########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{map} =} ocean ()
## @deftypefnx {} {@var{map} =} ocean (@var{n})
## Create color colormap.  This colormap varies from black to white with shades
## of blue.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

function map = ocean (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("ocean: N must be a scalar");
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
    map = [0, 0, 0];
  elseif (n > 1)
    cutin = fix (n/3);

    dr = (n - 1) / cutin;
    r = prepad ([0:dr:(n-1)], n)';

    dg = (n - 1) / (2 * cutin);
    g = prepad ([0:dg:(n-1)], n)';

    b = [0:(n-1)]';

    map = [r, g, b] / (n - 1);
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'ocean' colormap profile and as an image
%! cmap = ocean (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);
