## Copyright (C) 2010-2012 David Bateman
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
## @deftypefn {Function File} {@var{style} =} __next_line_style__ (@var{reset})
## Undocumented internal function.
## @end deftypefn

## Return the next line style in the rotation.


function [linestyle, marker] = __next_line_style__ (reset)

  persistent style_rotation;
  persistent num_styles;
  persistent style_index;

  if (nargin < 2)
    if (nargin == 1)
      if (ischar (reset) && strncmp (reset, "incr", 4))
        if (isempty (style_rotation))
          error ("__next_line_style__: style_rotation not initialized");
        elseif (++style_index > num_styles)
          style_index = 1;
        endif
      elseif (reset || isempty (style_rotation))
        style_rotation = get (gca (), "linestyleorder");
        if (ischar (style_rotation))
          style_rotation = strsplit (style_rotation, "|");
        endif
        num_styles = length (style_rotation);
        style_index = 1;
      endif
    elseif (! isempty (style_rotation))
      options = __pltopt__ ("__next_line_style__",
                            style_rotation (style_index));
      linestyle = options.linestyle;
      marker = options.marker;
    else
      error ("__next_line_style__: style_rotation not initialized");
    endif
  else
    print_usage ();
  endif

endfunction
