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
## @deftypefnx {Function File} {@var{cmap} =} colormap ("register", "@var{name}")
## @deftypefnx {Function File} {@var{cmap} =} colormap ("unregister", "@var{name}")
## Query or set the current colormap.
##
## With no input arguments, @code{colormap} returns the current color map.
##
## @code{colormap (@var{map})} sets the current colormap to @var{map}.  The
## colormap should be an @var{n} row by 3 column matrix.  The columns
## contain red, green, and blue intensities respectively.  All entries
## must be between 0 and 1 inclusive.  The new colormap is returned.
##
## @code{colormap ("default")} restores the default colormap (the
## @code{jet} map with 64 entries).  The default colormap is returned.
##
## @code{colormap ("list")} returns a cell array with all of the available
## colormaps.  The options "register" and "unregister" will add or remove
## the colormap @var{name} from this list.
##
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
  persistent do_warning = true;

  if (nargin == 1)

    if (ischar (map))
      if (strcmp (map, "default"))
        map = jet (64);
      elseif (strcmp (map, "list"))
        cmap = map_list;
        return;
      else
        ## FIXME: This syntax is deprecated.  It is no longer mentioned in
        ##        documentation and should probably be removed in Octave 3.12.
        if (do_warning)
          warning (["colormap: deprecated syntax 'colormap (\"%s\")'.  " ...
                    "Use 'colormap (%s (64))'"], map, map);
          do_warning = false;
        endif
        map = feval (map);
      endif
    endif

    if (! isempty (map))
      if (! (isnumeric (map) && isreal (map)
             && ndims (map) == 2 && columns (map) == 3))
        error ("colormap: MAP must be a real-valued N x 3 ([R,G,B]) matrix");
      endif
      if (any (map(:) < 0) || any (map(:) > 1))
        error ("colormap: all MAP values must be in the range [0,1]");
      endif
      ## Set the new color map
      set (gcf (), "colormap", map);
    endif

  elseif (nargin == 2)
    if (! ischar (map) || ! any (strcmp (map, {"register", "unregister"})))
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


%!demo
%! ## Create an image for displaying a colormap
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! ## Show 'jet' colormap
%! colormap (jet (64));
%! title "colormap (jet (64))"
%! disp ("Press a key to continue");
%! pause ();
%! ## Show 'colorcube' colormap
%! colormap (colorcube (64));
%! title "colormap (colorcube (64))"

%!test
%! figure ("visible", "off");
%! cmaptst = [0 1 0; 1 0 1; 1 1 1];
%! cmap = colormap (cmaptst);
%! assert (cmap, cmaptst);
%! cmap = colormap ();
%! assert (cmap, cmaptst);
%! cmap = (get (gcf, "colormap"));
%! assert (cmap, cmaptst);
%! colormap ("default");
%! assert (colormap (), jet (64));
%! colormap ("ocean");
%! assert (colormap, ocean (64));
%! close ();  # done with temp. figure

%!test
%! cmaplst = colormap ("list");
%! assert (iscell (cmaplst));
%! colormap ("register", "__mycmap__"); 
%! cmaplst2 = colormap ("list");
%! assert (numel (cmaplst2), numel (cmaplst) + 1);
%! assert (any (strcmp (cmaplst2, "__mycmap__")));
%! colormap ("unregister", "__mycmap__"); 
%! cmaplst2 = colormap ("list");
%! assert (numel (cmaplst2), numel (cmaplst));
%! assert (! any (strcmp (cmaplst2, "__mycmap__")));
%! ## Unregister again and verify that nothing has happened
%! colormap ("unregister", "__mycmap__"); 
%! cmaplst3 = colormap ("list");
%! assert (isequal (cmaplst2, cmaplst3));

## Test input validation
%!error colormap (1,2,3)
%!error <MAP must be a real-valued N x 3> colormap ({1,2,3})
%!error <MAP must be a real-valued N x 3> colormap ([1 i 1])
%!error <MAP must be a real-valued N x 3> colormap (ones(3,3,3))
%!error <MAP must be a real-valued N x 3> colormap ([1 0 1 0])
%!error <all MAP values must be in the range> colormap ([-1 0 0])
%!error <all MAP values must be in the range> colormap ([2 0 0])
%!error colormap ("invalid", "name")
%!error <NAME must be a string> colormap ("register", 1)

