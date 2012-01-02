## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {@var{map} =} gmap40 ()
## @deftypefnx {Function File} {@var{map} =} gmap40 (@var{n})
## Create color colormap.  The colormap consists of red, green, blue, yellow,
## magenta and cyan.  This colormap is specifically designed for users of
## gnuplot 4.0 where these 6 colors are the allowable ones for patch objects.
## The argument @var{n} must be a scalar.
## If unspecified, a length of 6 is assumed.  Larger values
## of @var{n} result in a repetition of the above colors.
## @seealso{colormap}
## @end deftypefn

function map = gmap40 (n)

  if (nargin == 0)
    n = 6;
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("gmap40: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (n >= 1)
    map = repmat ([1, 0, 0; 0, 1, 0; 0, 0, 1; 1, 1, 0; 1, 0, 1; 0, 1, 1],
          ceil (n / 6), 1) (1:n, :);
  else
    map = [];
  endif

endfunction

%!demo
%! ## Show the 'gmap40' colormap as an image
%! image (1:6, linspace (0, 1, 6), repmat (1:6, 6, 1)')
%! axis ([1, 6, 0, 1], "ticy", "xy")
%! colormap (gmap40 (6))

