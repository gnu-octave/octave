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
## @deftypefn  {} {@var{map} =} bone ()
## @deftypefnx {} {@var{map} =} bone (@var{n})
## Create color colormap.  This colormap varies from black to white with
## gray-blue shades.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

function map = bone (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("bone: N must be a scalar");
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
    map = [1/8 1/8 1/8];
  elseif (n == 2)
    map = [1/16 1/8 1/8
            1    1   1 ];
  elseif (n > 2)
    x = [0:n-1]' / (n-1);

    idx = floor (3/4*n);
    nel = n - idx + 1;    # number of elements
    rem = mod (n, 8);
    switch (rem)
      case {2, 4}
        base = 1 / (16 + 2*(n-rem));
      case {5, 7}
        base = 1 / (24 + 2*(n-rem));
      otherwise
        base = 0;
    endswitch
    r(1:idx,1) = 7/8 * x(1:idx);
    r(idx:n,1) = linspace (7/8 * x(idx) + base, 1, nel);

    idx = floor (3/8 * n);
    nel = idx + 1;
    g(1:idx,1) = 7/8 * x(1:idx);
    g(idx:2*idx,1) = linspace (7/8 * x(idx), 7/8 * x(2*idx) + 1/8, nel);
    g(2*idx+1:n,1) = 7/8 * x(2*idx+1:n) + 1/8;

    base = 1 / (8*idx);
    nel = idx;
    b(1:idx,1) = linspace (base, 7/8 * x(idx) + 1/8, nel);
    b(idx:n,1) = 7/8 * x(idx:n) + 1/8;

    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'bone' colormap profile and as an image
%! cmap = bone (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);
