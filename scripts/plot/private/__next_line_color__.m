## Copyright (C) 2007-2012 John W. Eaton
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
## @deftypefn {Function File} {@var{rgb} =} __next_line_color__ (@var{reset})
## Undocumented internal function.
## @end deftypefn

## Return the next line color in the rotation.

## Author: jwe

function rgb = __next_line_color__ (reset)

  persistent color_rotation;
  persistent num_colors;
  persistent color_index;

  if (nargin < 2)
    if (nargin == 1)
      if (reset || isempty (color_rotation))
        color_rotation = get (gca (), "colororder");
        num_colors = rows (color_rotation);
        color_index = 1;
      endif
    elseif (! isempty (color_rotation))
      rgb = color_rotation(color_index,:);
      if (++color_index > num_colors)
        color_index = 1;
        __next_line_style__ ("incr");
      endif
    else
      error ("__next_line_color__: color_rotation not initialized");
    endif
  else
    print_usage ();
  endif

endfunction
