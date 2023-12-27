########################################################################
##
## Copyright (C) 1994-2024 The Octave Project Developers
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
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
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
      n = 256;
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


%!assert (size (ocean ()), [256, 3])
%!assert (size (ocean (16)), [16, 3])

%!assert (ocean (1), [0, 0, 0])
%!assert (ocean (true), double ([0, 0, 0]))
%!assert (ocean (char (1)), double ([0, 0, 0]))
%!assert (ocean (int32 (1)), double ([0, 0, 0]))

%!assert (ocean (0), zeros (0, 3))
%!assert (ocean (-1), zeros (0, 3))

%!assert (ocean()(1,:), [0, 0, 0])
%!assert (ocean()(end,:), [1, 1, 1])

%!test
%! a = zeros (11, 3);
%! a([9:11],1) = [1/3, 2/3, 1];
%! a([6:11], 2) = 1/6:1/6:1;
%! a(:, 3) = 0:.1:1;
%! assert (ocean (11), a, eps)

## Input validation
%!error <N must be a scalar> ocean ("foo")
%!error <N must be a scalar> ocean ([1, 2, 3])
%!error <N must be a scalar> ocean ({1, 2, 3})
