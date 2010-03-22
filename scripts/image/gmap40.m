## Copyright (C) 2007, 2008, 2009 David Bateman
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
## @deftypefn {Function File} {} gmap40 (@var{n})
## Create a color colormap.  The colormap is red, green, blue, yellow,
## magenta and cyan.  These are the colors that are allowed with patch
## objects using gnuplot 4.0, and so this colormap function is specially
## designed for users of gnuplot 4.0.  The argument @var{n} should be 
## a scalar.  If it is omitted, a length of 6 is assumed.  Larger values
## of @var{n} result in a repetition of the above colors
## @seealso{colormap}
## @end deftypefn

function map = gmap40 (number)

  if (nargin == 0)
    number = 6;
  elseif (nargin == 1)
    if (! isscalar (number))
      error ("gmap40: argument must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (number >= 1)
    map = repmat ([1, 0, 0; 0, 1, 0; 0, 0, 1; 1, 1, 0; 1, 0, 1; 0, 1, 1],
          ceil (number / 6), 1) (1:number, :);
  else
    map = [];
  endif

endfunction

%!demo
%! ## Show the 'gmap40' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat (1:64, 64, 1)')
%! axis ([1, 64, 0, 1], "ticy", "xy")
%! colormap gmap40

