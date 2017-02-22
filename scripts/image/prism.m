## Copyright (C) 1999-2017 Kai Habel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{map} =} prism ()
## @deftypefnx {} {@var{map} =} prism (@var{n})
## Create color colormap.  This colormap cycles through red, orange, yellow,
## green, blue and violet with each index change.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function map = prism (n = rows (colormap ()))

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("prism: N must be a scalar");
  endif

  if (n == 1)
    map = [1 0 0];
  elseif (n > 1)
    C = [1, 0, 0; 1, 1/2, 0; 1, 1, 0; 0, 1, 0; 0, 0, 1; 2/3, 0, 1];
    map = C(rem (0:(n-1), 6) + 1, :);
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'prism' colormap profile and as an image
%! cmap = prism (18); # 6 colors, therefore cycle 3 times
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);
