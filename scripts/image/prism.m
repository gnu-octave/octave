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
## @deftypefn  {Function File} {@var{map} =} prism ()
## @deftypefnx {Function File} {@var{map} =} prism (@var{n})
## Create color colormap.  This colormap cycles through red, orange, yellow,
## green, blue and violet with each index change.
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function map = prism (n)

  if (nargin == 0)
    n = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("prism: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  p = [1, 0, 0; 1, 1/2, 0; 1, 1, 0; 0, 1, 0; 0, 0, 1; 2/3, 0, 1];

  if (rem (n, 6) == 0)
    map = kron(ones (fix (n / 6), 1), p);
  else
    map = [kron(ones (fix (n / 6), 1), p); p(1:rem (n, 6), :)];
  endif

endfunction

%!demo
%! ## Show the 'prism' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat (1:64, 64, 1)')
%! axis ([1, 64, 0, 1], "ticy", "xy")
%! colormap (prism (64))

