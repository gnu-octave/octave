function cmap = colormap(map)
#Set the current colormap.
#
#colormap(map) sets the current colormap to map.  map should be an n row
#by 3 column matrix. The columns contain red, green, and blue intensities
#respectively.  All entries should be between 0 and 1 inclusive. The new
#colormap is returned.
#
#colormap("default") restores the default colormap (a gray scale colormap
#with 64 entries). The default colormap is returned.
#
#colormap with no arguments returns the current colormap.

#Author:
# Tony Richardson
# amr@mpl.ucsd.edu
# July 1994

  global CURRENT_COLOR_MAP

  cmap_name = "CURRENT_COLOR_MAP";

  if(nargin == 1)
    if(isstr(map))
      if(strcmp(map,"default"))
        CURRENT_COLOR_MAP = gray;
      else
        error("invalid argument");
      endif
    else
      # Set the new color map
      CURRENT_COLOR_MAP = map;
    endif
  elseif(exist(cmap_name) == 0)
    # If global color map doesn't exist, create the default map.
    CURRENT_COLOR_MAP = gray;
  endif

  # Return current color map.
  cmap = CURRENT_COLOR_MAP;
endfunction
