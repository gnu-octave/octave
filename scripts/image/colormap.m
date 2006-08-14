## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} colormap (@var{map})
## @deftypefnx {Function File} {} colormap ("default")
## Set the current colormap.
##
## @code{colormap (@var{map})} sets the current colormap to @var{map}.  The
## color map should be an @var{n} row by 3 column matrix.  The columns
## contain red, green, and blue intensities respectively.  All entries
## should be between 0 and 1 inclusive.  The new colormap is returned.
##
## @code{colormap ("default")} restores the default colormap (a gray scale
## colormap with 64 entries).  The default colormap is returned.
##
## With no arguments, @code{colormap} returns the current color map.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function cmap = colormap (map)

  persistent current_color_map = gray ();

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 1)

    if (ischar (map))
      if (strcmp (map, "default"))
        map = gray ();
      else
        map = eval (map);
      endif
    endif

    if (! isempty (map))
      if (columns (map) != 3)
        error ("colormap: map must have 3 columns: [R,G,B]");
      endif
      if (min (min (map)) < 0 || max (max (map)) > 1)
        error ("colormap: map must have values in [0,1]");
      endif
      ## Set the new color map
      current_color_map = map;
    endif

  endif

  ## Return current color map.
  cmap = current_color_map;

endfunction
