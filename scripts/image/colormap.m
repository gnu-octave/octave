# Copyright (C) 1995 John W. Eaton
# 
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function cmap = colormap (map)

# Set the current colormap.
# 
# colormap (map) sets the current colormap to map.  map should be an n
# row by 3 column matrix. The columns contain red, green, and blue
# intensities respectively.  All entries should be between 0 and 1
# inclusive. The new colormap is returned.
# 
# colormap ("default") restores the default colormap (a gray scale
# colormap with 64 entries). The default colormap is returned.
# 
# colormap with no arguments returns the current colormap.

# Written by Tony Richardson (amr@mpl.ucsd.edu) July 1994.

  global CURRENT_COLOR_MAP

  cmap_name = "CURRENT_COLOR_MAP";

  if (nargin == 1)
    if (isstr (map))
      if (strcmp (map, "default"))
        CURRENT_COLOR_MAP = gray;
      else
        error ("invalid argument");
      endif
    else
# Set the new color map
      CURRENT_COLOR_MAP = map;
    endif
  elseif (nargin == 0 && exist (cmap_name) == 0)
# If global color map doesn't exist, create the default map.
    CURRENT_COLOR_MAP = gray;
  else
    usage ("colormap (map)");
  endif

# Return current color map.

  cmap = CURRENT_COLOR_MAP;

endfunction
