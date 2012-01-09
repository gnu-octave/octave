## Copyright (C) 1999-2012 Kai Habel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{map} =} hot ()
## @deftypefnx {Function File} {@var{map} =} hot (@var{n})
## Create color colormap.  This colormap ranges from black through dark red,
## red, orange, yellow, to white.
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function map = hot (n)

  if (nargin == 0)
    n = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("hot: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (n == 1)
    map = [0, 0, 0];
  elseif (n > 1)
    x = linspace (0, 1, n)';
    r = (x < 2/5) .* (5/2 * x) + (x >= 2/5);
    g = (x >= 2/5 & x < 4/5) .* (5/2 * x - 1) + (x >= 4/5);
    b = (x >= 4/5) .* (5*x - 4);
    map = [r, g, b];
  else
    map = [];
  endif

endfunction

%!demo
%! ## Show the 'hot' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat (1:64, 64, 1)')
%! axis ([1, 64, 0, 1], "ticy", "xy")
%! colormap (hot (64))

