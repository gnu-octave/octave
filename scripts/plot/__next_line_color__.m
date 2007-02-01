## Copyright (C) 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __next_line_color__ (@var{reset})
## Return the next line color in the rotation.
## @end deftypefn

## Author: jwe

function rgb = __next_line_color__ (reset)

  persistent color_rotation = [ 0,    0,    1;
				0,    0.5,  0;
				1,    0,    0;
				0,    0.75, 0.75;
				0.75, 0,    0.75;
				0.75, 0.75, 0;
				0.25, 0.25, 0.25];

  persistent num_colors = rows (color_rotation);
  persistent color_index = 1;

  if (nargin < 2)
    if (nargin == 1 && reset)
      color_index = 1;
    else
      color_index
      rgb = color_rotation(color_index,:)
      if (++color_index > num_colors)
	color_index = 1;
      endif
    endif
  else
    print_usage ();
  endif

endfunction
