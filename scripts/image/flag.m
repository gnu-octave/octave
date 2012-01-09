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
## @deftypefn  {Function File} {@var{map} =} flag ()
## @deftypefnx {Function File} {@var{map} =} flag (@var{n})
## Create color colormap.  This colormap cycles through red, white, blue
## and black with each index change.
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function map = flag (n)

  if (nargin == 0)
    n = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("flag: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  p = [1, 0, 0; 1, 1, 1; 0, 0, 1; 0, 0, 0];
  if (rem(n,4) == 0)
    map = kron (ones (n / 4, 1), p);
  else
    m1 = kron (ones (fix (n / 4), 1), p);
    m2 = p(1:rem (n, 4), :);
    map = [m1; m2];
  endif

endfunction

%!demo
%! ## Show the 'flag' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat (1:64, 64, 1)')
%! axis ([1, 64, 0, 1], "ticy", "xy")
%! colormap (flag (64))

