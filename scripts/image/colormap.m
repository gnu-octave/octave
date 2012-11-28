## Copyright (C) 1994-2012 John W. Eaton
## Copyright (C) 2012 Carnë Draug
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
## @deftypefn  {Function File} {@var{cmap} =} colormap ()
## @deftypefnx {Function File} {@var{cmap} =} colormap (@var{map})
## @deftypefnx {Function File} {@var{cmap} =} colormap ("default")
## @deftypefnx {Function File} {@var{cmap} =} colormap ("list")
## @deftypefnx {Function File} {@var{cmap} =} colormap ("register", "name")
## @deftypefnx {Function File} {@var{cmap} =} colormap ("unregister", "name")
## Query or set the current colormap.
##
## @code{colormap (@var{map})} sets the current colormap to @var{map}.  The
## colormap should be an @var{n} row by 3 column matrix.  The columns
## contain red, green, and blue intensities respectively.  All entries
## must be between 0 and 1 inclusive.  The new colormap is returned.
##
## @code{colormap ("default")} restores the default colormap (the
## @code{jet} map with 64 entries).  The default colormap is returned.
##
## @code{colormap ("list")} returns a cell array with all the available
## colormaps.  The options `register' and `unregister' will add or remove the
## colormap @var{name} to it.
##
## With no arguments, @code{colormap} returns the current color map.
## @seealso{jet}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function cmap = colormap (map, name)

  if (nargin > 2)
    print_usage ();
  endif

  persistent map_list = cell ();

  if (nargin == 1)

    if (ischar (map))
      if (strcmp (map, "default"))
        map = jet (64);
      elseif (strcmp (map, "list"))
        cmap = map_list;
        return;
      else
        map = feval (map);
      endif
    endif

    if (! isempty (map))
      if (! ismatrix (map) || ndims (map) != 2 || columns (map) != 3)
        error ("colormap: MAP must be an N x 3 ([R,G,B]) matrix");
      endif
      if (any (map(:) < 0) || any (map(:) > 1))
        error ("colormap: all MAP values must be in the range [0,1]");
      endif
      ## Set the new color map
      set (gcf (), "colormap", map);
    endif

  elseif (nargin == 2)
    if (! ischar (map) || all (! strcmp (map, {"register", "unregister"})))
      print_usage ();
    elseif (! ischar (name))
      error ("colormap: to register/unregister a colormap, NAME must be a string");
    elseif (strcmp (map, "register"))
      map_list{end+1} = name;
    elseif (strcmp (map, "unregister"))
      map_list(strcmp (name, map_list)) = [];
    endif
  endif

  ## Return current color map.
  if (nargout > 0 || (nargout == 0 && nargin == 0))
    cmap = get (gcf (), "colormap");
  endif

endfunction


%% 
