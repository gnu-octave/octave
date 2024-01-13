########################################################################
##
## Copyright (C) 1999-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{map} =} spring ()
## @deftypefnx {} {@var{map} =} spring (@var{n})
## Create color colormap.  This colormap varies from magenta to yellow.
##
## The argument @var{n} must be a scalar.
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
## @seealso{colormap}
## @end deftypefn

function map = spring (n)

  if (nargin == 1)
    if (! (isscalar (n) && isreal (n) && n == fix (n)))
      error ("spring: N must be a scalar integer");
    endif
    n = double (n);
  else
    hf = get (0, "currentfigure");
    if (! isempty (hf))
      n = rows (get (hf, "colormap"));
    else
      n = 256;
    endif
  endif

  if (n == 1)
    map = [1, 0, 1];
  elseif (n > 1)
    r = ones (n, 1);
    g = [0:(n-1)]' / (n - 1);
    b = 1 - g;
    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'spring' colormap profile and as an image
%! cmap = spring (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);


%!assert (size (spring ()), [256, 3])
%!assert (size (spring (16)), [16, 3])
%!assert (all ((spring ()(:, 1)) == 1), true)
%!assert (spring (11)(:,2:3), [[0:0.1:1]; [1:-0.1:0]]', eps)

%!assert (spring (1), [1, 0, 1])
%!assert (spring (true), double ([1, 0, 1]))
%!assert (spring (char (1)), double ([1, 0, 1]))
%!assert (spring (int32 (1)), double ([1, 0, 1]))

%!assert (spring (0), zeros (0, 3))
%!assert (spring (-1), zeros (0, 3))

## Input validation
%!error <N must be a scalar integer> spring ("foo")
%!error <N must be a scalar integer> spring ([1, 2, 3])
%!error <N must be a scalar integer> spring ({1, 2, 3})
