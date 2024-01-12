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
## @deftypefn  {} {@var{map} =} copper ()
## @deftypefnx {} {@var{map} =} copper (@var{n})
## Create color colormap.  This colormap varies from black to a light copper
## tone.
##
## The argument @var{n} must be a scalar.
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
## @seealso{colormap}
## @end deftypefn

function map = copper (n)

  if (nargin == 1)
    if (! (isscalar (n) && isreal (n) && n == fix (n)))
      error ("copper: N must be a scalar integer");
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
    map = [0, 0, 0];
  elseif (n > 1)
    x = [0:(n-1)]' / (n - 1);
    r = (x < 4/5) .* (5/4 * x) ...
      + (x >= 4/5);
    g = 0.7812 * x;
    b = 0.4975 * x;
    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'copper' colormap profile and as an image
%! cmap = copper (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);


%!assert (size (copper ()), [256, 3])
%!assert (size (copper (16)), [16, 3])

%!assert (copper (1), [0, 0, 0])
%!assert (copper (true), double ([0, 0, 0]))
%!assert (copper (char (1)), double ([0, 0, 0]))
%!assert (copper (int32 (1)), double ([0, 0, 0]))

%!assert (copper (0), zeros (0, 3))
%!assert (copper (-1), zeros (0, 3))

%!test
%! a = [0.25, 0.15624, 0.0995] .* [0:5]';
%! a(6) = 1;
%! assert (copper (6), a, eps)

## Input validation
%!error <N must be a scalar integer> copper ("foo")
%!error <N must be a scalar integer> copper ([1, 2, 3])
%!error <N must be a scalar integer> copper ({1, 2, 3})
