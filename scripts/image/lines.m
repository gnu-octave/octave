## Copyright (C) 2012-2018 Rik Wehbring
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

## -*- texinfo -*-
## @deftypefn  {} {@var{map} =} lines ()
## @deftypefnx {} {@var{map} =} lines (@var{n})
## Create color colormap.  This colormap is composed of the list of colors
## in the current axes @qcode{"ColorOrder"} property.  The default is blue,
## orange, yellow, purple, green, light blue, and dark red.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

function map = lines (n)

  if (nargin > 1)
    print_usage ();
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("lines: N must be a scalar");
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
    map = [0, 0, 1];
  elseif (n > 1)
    C = get (gca, "colororder");
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
