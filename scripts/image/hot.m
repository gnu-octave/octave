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
## @deftypefn  {} {@var{map} =} hot ()
## @deftypefnx {} {@var{map} =} hot (@var{n})
## Create color colormap.  This colormap ranges from black through dark red,
## red, orange, yellow, to white.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

function map = hot (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("hot: N must be a scalar");
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
    map = [1, 1, 1];
  elseif (n == 2)
    map = [1, 1, 1/2
           1, 1,  1 ];
  elseif (n > 2)
    idx = floor (3/8 * n);
    nel = idx;

    r = ones (n, 1);
    r(1:idx, 1) = [1:nel]' / nel;

    g = zeros (n, 1);
    g(idx+1:2*idx, 1) = r(1:idx);
    g(2*idx+1:end, 1) = 1;

    idx = 2*idx + 1;   # approximately 3/4 *n
    nel = n - idx + 1;

    b = zeros (n, 1);
    b(idx:end, 1) = [1:nel]' / nel;

    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'hot' colormap profile and as an image
%! cmap = hot (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);
