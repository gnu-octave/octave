########################################################################
##
## Copyright (C) 1994-2024 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{cmap} =} colormap ()
## @deftypefnx {} {@var{cmap} =} colormap (@var{map})
## @deftypefnx {} {@var{cmap} =} colormap (@qcode{"default"})
## @deftypefnx {} {@var{cmap} =} colormap (@var{map_name})
## @deftypefnx {} {@var{cmap} =} colormap (@var{hax}, @dots{})
## @deftypefnx {} {@var{cmap} =} colormap (@var{hfig}, @dots{})
## @deftypefnx {} {} colormap @var{map_name}
## Query or set the current colormap.
##
## With no input arguments, @code{colormap} returns the current color map.  If
## there is no current figure, a new figure will be opened and the default
## color map will be returned.
##
## @code{colormap (@var{map})} sets the current colormap to @var{map}.  The
## colormap should be an @var{n} row by 3 column matrix.  The columns
## contain red, green, and blue intensities respectively.  All entries
## must be between 0 and 1 inclusive.  The new colormap is returned.
##
## @code{colormap (@qcode{"default"})} restores the default colormap (the
## @code{viridis} map with 256 entries).  The default colormap is returned.
##
## The map may also be specified by a string, @var{map_name}, which
## is the name of a function that returns a colormap.
##
## If the first argument @var{hax} is an axes handle, then the colormap for
## the specified axes is queried or set.  If the first argument @var{hfig} is a
## figure handle, then the colormap for the specified figure is queried or set.
##
## For convenience, it is also possible to use this function with the
## command form, @code{colormap @var{map_name}}.
##
## The list of built-in colormaps is:
##
## @c FIXME: It would be nice to display the actual colormap as an image
## @c        in the PDF version of the documentation.
## @multitable @columnfractions 0.15 .85
## @headitem Map @tab Description
## @item viridis @tab default
## @item turbo @tab colormap traversing blue, cyan, green, yellow, red; modern replacement for jet.
## @item jet @tab colormap traversing blue, cyan, green, yellow, red.
## @item cubehelix @tab colormap traversing black, blue, green, red, white with increasing intensity.
## @item hsv @tab cyclic colormap traversing Hue, Saturation, Value space.
## @item rainbow @tab colormap traversing red, yellow, blue, green, violet.
## @item ------------- @tab ---------------------------------------------------------------------------------------------
## @item hot @tab colormap traversing black, red, orange, yellow, white.
## @item cool @tab colormap traversing cyan, purple, magenta.
## @item spring @tab colormap traversing magenta to yellow.
## @item summer @tab colormap traversing green to yellow.
## @item autumn @tab colormap traversing red, orange, yellow.
## @item winter @tab colormap traversing blue to green.
## @item ------------- @tab ---------------------------------------------------------------------------------------------
## @item gray @tab colormap traversing black to white in shades of gray.
## @item bone @tab colormap traversing black, gray-blue, white.
## @item copper @tab colormap traversing black to light copper.
## @item pink @tab colormap traversing black, gray-pink, white.
## @item ocean @tab colormap traversing black, dark-blue, white.
## @item ------------- @tab ---------------------------------------------------------------------------------------------
## @item colorcube @tab equally spaced colors in RGB color space.
## @item flag @tab cyclic 4-color map of red, white, blue, black.
## @item lines @tab cyclic colormap with colors from axes @qcode{"ColorOrder"} property.
## @item prism @tab cyclic 6-color map of red, orange, yellow, green, blue, violet.
## @item ------------- @tab ---------------------------------------------------------------------------------------------
## @item white @tab all white colormap (no colors).
## @end multitable
## @seealso{viridis, turbo, jet, cubehelix, hsv, rainbow, hot, cool, spring,
## summer, autumn, winter, gray, bone, copper, pink, ocean, colorcube, flag,
## lines, prism, white}
## @end deftypefn

function cmap = colormap (varargin)

  hax = [];
  if (nargin > 0)
    if (isscalar (varargin{1}) && ishghandle (varargin{1}))
      htmp = varargin{1};
      if (isaxes (htmp))
        hax = htmp;
      elseif (isfigure (htmp))
        hax = [];
      else
        error ("colormap: target argument must be a figure or axes handle");
      endif
      ## Delete first item in list that was just processed.
      varargin(1) = [];
      nargin = nargin - 1;
    endif
  endif

  if (nargin > 1)
    print_usage ();
  endif

  if (! isempty (hax))
    have_fig = false;
    cf = hax;
  else
    have_fig = true;
    cf = get (0, "currentfigure");
  endif

  if (nargin == 1)
    map = varargin{1};
    if (ischar (map))
      map = lower (map);
      if (strcmp (map, "default"))
        map = viridis (256);
      else
        try
          map = feval (map);
        catch
          error ("colormap: failed to set MAP <%s>", map);
        end_try_catch
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
      if (isempty (cf))
        cf = gcf ();
      endif
      ## Set the new color map
      set (cf, "colormap", map);
      if (have_fig)
        ## Matlab Compatibility: Also clear any axes colormaps as of R2018A.
        hax = findobj (cf, '-depth', 1, 'type', 'axes');
        set (hax, 'colormap', []);
      endif
    endif
  endif

  ## Return current color map.
  if (nargout > 0 || nargin == 0)
    if (isempty (cf))
      cf = gcf ();
    endif
    cmap = get (cf, "colormap");
  endif

endfunction


%!demo
%! ## Create an image for displaying a colormap
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! ## Show 'viridis' colormap
%! colormap (viridis (64));
%! title "colormap (viridis (64))"
%! disp ("Press a key to continue");
%! pause ();
%! ## Show 'colorcube' colormap
%! colormap (colorcube (64));
%! title "colormap (colorcube (64))"

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   cmaptst = [0 1 0; 1 0 1; 1 1 1];
%!   cmap = colormap (cmaptst);
%!   assert (cmap, cmaptst);
%!   cmap = colormap ();
%!   assert (cmap, cmaptst);
%!   cmap = (get (gcf, "colormap"));
%!   assert (cmap, cmaptst);
%!   colormap ("default");
%!   assert (colormap (), viridis (256));
%!   colormap ("ocean");
%!   assert (colormap, ocean (256));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test <*65674>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax1 = subplot (2, 1, 1);
%!   hax2 = subplot (2, 1, 2);
%!   cmaptst = [0 1 0; 1 0 1; 1 1 1];
%!   colormap (cmaptst);
%!   colormap (hax1, jet (16));
%!   colormap (hax2, pink (16));
%!   assert (map = colormap (hf), cmaptst);
%!   assert (map = colormap (hax1), jet (16));
%!   assert (map = colormap (hax2), pink (16));
%!   colormap (gray (4));
%!   assert (map = colormap (hf), gray (4));
%!   assert (map = colormap (hax1), gray (4));
%!   assert (map = colormap (hax2), gray (4));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <target argument must be a figure or axes handle> colormap (0)
%!error <Invalid call> colormap (1,2,3)
%!error <failed to set MAP .invalid_map_name.> colormap ("invalid_map_name")
%!error <MAP must be a real-valued N x 3> colormap ({1,2,3})
%!error <MAP must be a real-valued N x 3> colormap ([1 i 1])
%!error <MAP must be a real-valued N x 3> colormap (ones (3,3,3))
%!error <MAP must be a real-valued N x 3> colormap ([1 0 1 0])
%!error <all MAP values must be in the range> colormap ([-1 0 0])
%!error <all MAP values must be in the range> colormap ([2 0 0])
