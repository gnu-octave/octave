########################################################################
##
## Copyright (C) 2012-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{map} =} lines ()
## @deftypefnx {} {@var{map} =} lines (@var{n})
## Create color colormap.  This colormap is composed of the list of colors
## in the current axes @qcode{"ColorOrder"} property.  The default is blue,
## orange, yellow, purple, green, light blue, and dark red.
##
## The argument @var{n} must be a scalar.
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
## @seealso{colormap}
## @end deftypefn

function map = lines (n)

  hf = get (groot, "currentfigure");
  if (nargin == 1)
    if (! isscalar (n))
      error ("lines: N must be a scalar");
    endif
    n = double (n);
  else
    if (! isempty (hf))
      n = rows (get (hf, "colormap"));
    else
      n = 256;
    endif
  endif

  if (n == 1)
    map = [0, 0, 1];
  elseif (n > 1)
    hax = get (hf, "currentaxes");
    if (! isempty (hax))
      C = get (hax, "colororder");
    else
      C = get (groot, "defaultaxescolororder");
    endif
    nr = rows (C);
    map = C(rem (0:(n-1), nr) + 1, :);
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'lines' colormap profile and as an image
%! cmap = lines (21); # default has 7 colors, therefore cycle 3 times
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);


%!assert (size (lines ()), [256, 3])
%!assert (size (lines (16)), [16, 3])

%!assert (lines (1), [0, 0, 1])
%!assert (lines (true), double ([0, 0, 1]))
%!assert (lines (char (1)), double ([0, 0, 1]))
%!assert (lines (int32 (1)), double ([0, 0, 1]))

%!assert (lines (0), zeros (0, 3))
%!assert (lines (-1), zeros (0, 3))

%!test
%! a = get (groot, "defaultaxescolororder");
%! nr = rows (a);
%! assert (lines (3*nr), [a;a;a]);

## Input validation
%!error <function called with too many inputs> lines (1, 2)
%!error <N must be a scalar> lines ("foo")
%!error <N must be a scalar> lines ([1, 2, 3])
%!error <N must be a scalar> lines ({1, 2, 3})
