## Copyright (C) 1996 John W. Eaton
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## Set the current colormap.
## 
## colormap (map) sets the current colormap to map.  map should be an n
## row by 3 column matrix. The columns contain red, green, and blue
## intensities respectively.  All entries should be between 0 and 1
## inclusive. The new colormap is returned.
## 
## colormap ("default") restores the default colormap (a gray scale
## colormap with 64 entries). The default colormap is returned.
## 
## colormap with no arguments returns the current colormap.

## Author: Tony Richardson <amr@mpl.ucsd.edu>
## Created: July 1994
## Adapted-By: jwe

function cmap = colormap (map)

  global CURRENT_COLOR_MAP

  if (nargin > 1)
    usage ("colormap (map)");
  endif

  if (nargin == 1)
    if (isstr (map))
      if (strcmp (map, "default"))
        CURRENT_COLOR_MAP = gray;
      else
        error ("invalid argument");
      endif
    else
      ## Set the new color map
      CURRENT_COLOR_MAP = map;
    endif
  elseif (! exist ("CURRENT_COLOR_MAP"))
    ## If global color map doesn't exist, create the default map.
    CURRENT_COLOR_MAP = gray;
  endif

  ## Return current color map.

  cmap = CURRENT_COLOR_MAP;

endfunction
