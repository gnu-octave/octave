## Copyright (C) 2008-2017 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} colorbar
## @deftypefnx {} {} colorbar (@dots{}, @var{loc})
## @deftypefnx {} {} colorbar (@var{delete_option})
## @deftypefnx {} {} colorbar (@var{hcb}, @dots{})
## @deftypefnx {} {} colorbar (@var{hax}, @dots{})
## @deftypefnx {} {} colorbar (@dots{}, "peer", @var{hax}, @dots{})
## @deftypefnx {} {} colorbar (@dots{}, "location", @var{loc}, @dots{})
## @deftypefnx {} {} colorbar (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {@var{h} =} colorbar (@dots{})
## Add a colorbar to the current axes.
##
## A colorbar displays the current colormap along with numerical rulings
## so that the color scale can be interpreted.
##
## The optional input @var{loc} determines the location of the colorbar.  If
## present, it must be the last argument to @code{colorbar}.  Valid values for
## @var{loc} are
##
## @table @asis
## @item @qcode{"EastOutside"}
## Place the colorbar outside the plot to the right.  This is the default.
##
## @item @qcode{"East"}
## Place the colorbar inside the plot to the right.
##
## @item @qcode{"WestOutside"}
## Place the colorbar outside the plot to the left.
##
## @item @qcode{"West"}
## Place the colorbar inside the plot to the left.
##
## @item @qcode{"NorthOutside"}
## Place the colorbar above the plot.
##
## @item @qcode{"North"}
## Place the colorbar at the top of the plot.
##
## @item @qcode{"SouthOutside"}
## Place the colorbar under the plot.
##
## @item @qcode{"South"}
## Place the colorbar at the bottom of the plot.
## @end table
##
## To remove a colorbar from a plot use any one of the following keywords for
## the @var{delete_option}: @qcode{"off"}, @qcode{"delete"}, @qcode{"hide"}.
##
## If the first argument @var{hax} is an axes handle, then the colorbar is
## added to this axis, rather than the current axes returned by @code{gca}.
## Alternatively, If the argument @qcode{"peer"} is given, then the following
## argument is treated as the axes handle in which to add the colorbar.  The
## @qcode{"peer"} calling syntax may be removed in the future and is not
## recommended.
##
## If the first argument @var{hcb} is a handle to a colorbar object, then
## operate on this colorbar directly.
##
## Additional property/value pairs are passed directly to the underlying axes
## object.
##
## The optional return value @var{h} is a graphics handle to the created
## colorbar object.
##
## Implementation Note: A colorbar is created as an additional axes to the
## current figure with the @qcode{"tag"} property set to @qcode{"colorbar"}.
## The created axes object has the extra property @qcode{"location"} which
## controls the positioning of the colorbar.
## @seealso{colormap}
## @end deftypefn

function h = colorbar (varargin)

  [hcb, varargin, nargin] = __plt_get_axis_arg__ ("colorbar", varargin{:});

  if (hcb && ! strcmp (get (hcb, "tag"), "colorbar"))
    hax = hcb;
    hcb = [];
  else
    hax = [];
  endif
  loc = "";
  args = {};
  deleting = false;

  i = 1;
  while (i <= nargin)
    arg = varargin{i++};
    if (! ischar (arg))
      error ("colorbar: expected string argument at position %d", i-1);
    endif

    switch (tolower (arg))
      case {"north", "south", "east", "west", ...
            "northoutside", "southoutside", "eastoutside", "westoutside"}
        if (i <= nargin)
          error ("colorbar: LOC specification must occur as final argument");
        endif
        loc = tolower (arg);

      case "location"
        if (i > nargin)
          error ('colorbar: missing value after "location"');
        endif
        loc = tolower (varargin{i++});

      case {"delete", "hide", "off", "none"}
        deleting = true;

      case "peer"
        if (i > nargin)
          error ('colorbar: missing axes handle after "peer"');
        endif

        hax = varargin{i++};
        if (! isscalar (hax) || ! isaxes (hax))
          error ('colorbar: invalid axes handle following "peer"');
        endif

      otherwise
        ## Property/Value pair
        if (i > nargin)
          error ("colorbar: PROP/VAL inputs must occur in pairs");
        endif
        args{end+1} = arg;
        args{end+1} = varargin{i++};

    endswitch
  endwhile

  ## Handle changes to existing colorbar
  if (! isempty (hcb))
    if (deleting)
      delete (hcb);
      if (nargout > 0)
        h = [];
      endif
      return;
    else
      ## FIXME: No listener on location property so have to re-create
      ##        colorbar whenever an option changes.
      ##        re-instate this code if listener is developed.
      ## if (! isempty (loc))
      ##   set (hcb, "location", loc);
      ## endif
      ## if (! isempty (args))
      ##   set (hcb, args{:});
      ## endif
      hax = get (ancestor (hcb, "figure"), "currrentaxes");
    endif
  endif

  if (isempty (loc))
    loc = "eastoutside";
  endif
  if (isempty (hax))
    hax = gca ();
  endif

  ## Remove existing colorbar
  hpar = ancestor (hax, "figure");
  hcb = findall (hpar, "tag", "colorbar", "type", "axes", "axes", hax);
  if (! isempty (hcb))
    delete (hcb);
  endif

  if (! deleting)
    ## Create a colorbar

    ## Special handling for plotyy which has two axes objects
    if (isprop (hax, "__plotyy_axes__"))
      hyy = get (hax, "__plotyy_axes__");

      ## Use axis which is appropriate for legend location.
      ## necessary for plotyy figures where there are two axes.
      if (strfind (loc, "east"))
        hax = hyy(2);
      else
        hax = hyy(1);
      endif
    endif

    ## FIXME: Matlab does not require the "position" property to be active.
    ##        Is there a way to determine the plotbox position for the
    ##        gnuplot graphics toolkit with the outerposition is active?
    set (hax, "activepositionproperty", "position");
    props = get (hax);
    props.__cbar_hax__ = hax;
    position = props.position;

    clen = rows (get (hax, "colormap"));
    cext = get (hax, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    [pos, cpos, vertical, mirror] = __position_colorbox__ (loc, props, hpar);
    set (hax, "position", pos);

    hcb = __go_axes__ (hpar, "tag", "colorbar",
                             "activepositionproperty", "position",
                             "position", cpos,
                             "box", "on");
    addproperty ("location", hcb, "radio",
                 "eastoutside|east|westoutside|west|northoutside|north|southoutside|south|manual",
                 loc);
    addproperty ("axes", hcb, "handle", hax);

    if (vertical)
      ## Use low-level form to avoid calling newplot which changes axes
      hi = image (hcb, "xdata", [0,1], "ydata", [cmin, cmax],
                       "cdata", [1 : clen]');
      if (mirror)
        set (hcb, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "ylim", cext, "ylimmode", "manual",
                  "yaxislocation", "right", "layer", "top", args{:});
      else
        set (hcb, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "ylim", cext, "ylimmode", "manual",
                  "yaxislocation", "left", "layer", "top", args{:});
      endif
    else
      hi = image (hcb, "xdata", [cmin, cmax], "ydata", [0,1],
                       "cdata", [1 : clen]);
      if (mirror)
        set (hcb, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xlim", cext, "xlimmode", "manual",
                  "xaxislocation", "top", "layer", "top", args{:});
      else
        set (hcb, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xlim", cext, "xlimmode", "manual",
                  "xaxislocation", "bottom", "layer", "top", args{:});
      endif
    endif

    ## Dummy object placed on axes to delete colorbar when axes is deleted.
    ctext = text (0, 0, "", "tag", "colorbar",
                  "visible", "off", "handlevisibility", "off",
                  "xliminclude", "off", "yliminclude", "off",
                  "zliminclude", "off",
                  "deletefcn", {@cb_axes_deleted, hcb});

    set (hcb, "deletefcn", {@cb_restore_axes, hax, props});

    if (vertical)
      addlistener (hcb, "yscale", {@cb_error_on_logscale, "yscale"});
    else
      addlistener (hcb, "xscale", {@cb_error_on_logscale, "xscale"});
    endif
    addlistener (hpar, "colormap", {@cb_colormap, hi, vertical, clen});
    ## FIXME: listener on axes colormap does not work yet.
    addlistener (hax, "colormap", {@cb_colormap, hi, vertical, clen});
    addlistener (hax, "clim", {@cb_clim, hi, vertical});
    addlistener (hax, "dataaspectratio", {@cb_colorbar_axis, hcb, props});
    addlistener (hax, "dataaspectratiomode", {@cb_colorbar_axis, hcb, props});
    addlistener (hax, "plotboxaspectratio", {@cb_colorbar_axis, hcb, props});
    addlistener (hax, "plotboxaspectratiomode",{@cb_colorbar_axis, hcb, props});
    addlistener (hax, "position", {@cb_colorbar_axis, hcb, props});

  endif

  if (nargout > 0)
    h = hcb;
  endif

endfunction

## Axes to which colorbar was attached has been deleted.  Delete colorbar.
function cb_axes_deleted (~, ~, hcb, orig_props)
  if (isaxes (hcb))
    set (hcb, "deletefcn", []);
    delete (hcb);
  endif
endfunction

## Error on attempt to set logscale on colorbar axes
function cb_error_on_logscale (hcb, ~, scale)
  if (strcmp (get (hcb, scale), "log"))
    set (hcb, scale, "linear");
    error ("colorbar: Only linear colorbars are possible");
  endif
endfunction

## Restore position of axes object when colorbar is deleted.
function cb_restore_axes (hcb, ~, hax, orig_props)

  hf = ancestor (hax, "figure");
  if (strcmp (get (hf, "beingdeleted"), "on"))
    ## Skip restoring axes if entire figure is being destroyed.
    return;
  endif

  if (isaxes (hax))
    ## FIXME: It is wrong to delete every listener for colormap on figure,
    ##        but we don't have a way of deleting just this instance.
    dellistener (hf, "colormap");
    dellistener (hax, "dataaspectratio");
    dellistener (hax, "dataaspectratiomode");
    dellistener (hax, "plotboxaspectratio");
    dellistener (hax, "plotboxaspectratiomode");
    dellistener (hax, "position");

    ## Restore original axes position
    units = get (hax, "units");
    set (hax, "units", orig_props.units);
    set (hax, "position", orig_props.position,
              "outerposition", orig_props.outerposition,
              "activepositionproperty", orig_props.activepositionproperty);
    set (hax, "units", units);
  endif

endfunction

## Update colorbar when clim has changed
function cb_clim (hax, ~, hi, vert)

  if (isaxes (hax))
    clen = rows (get (hax, "colormap"));
    cext = get (hax, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    hiax = get (hi, "parent");
    if (vert)
      set (hi, "ydata", [cmin, cmax]);
      set (hiax, "ylim", cext);
    else
      set (hi, "xdata", [cmin, cmax]);
      set (hiax, "xlim", cext);
    endif
  endif

endfunction

## Update colorbar when changes to axes or figure colormap have occurred.
function cb_colormap (h, d, hi, vert, init_sz)
  persistent sz = init_sz;

  if (ishghandle (h))
    clen = rows (get (h, "colormap"));
    if (clen != sz)
      if (vert)
        set (hi, "cdata", [1:clen]');
      else
        set (hi, "cdata", [1:clen]);
      endif
      sz = clen;
      ## Also update limits on colorbar axes or there will be white gaps
      cb_clim (get (hi, "parent"), d, hi, vert);
    endif
  endif

endfunction

## Update positioning of colorbar when original axes has changed position.
function cb_colorbar_axis (hax, ~, hcb, orig_props)

  if (isaxes (hcb))
    loc = get (hcb, "location");
    props = get (hax);
    props.__cbar_hax__ = hax;
    props.position = orig_props.position;
    props.outerposition = orig_props.outerposition;
    [pos, cpos, vertical, mirror] = ...
       __position_colorbox__ (loc, props, ancestor (hax, "figure"));

    if (vertical)
      if (mirror)
        set (hcb, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "yaxislocation", "right", "position", cpos);
      else
        set (hcb, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "yaxislocation", "left", "position", cpos);
      endif
    else
      if (mirror)
        set (hcb, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xaxislocation", "top", "position", cpos);
      else
        set (hcb, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xaxislocation", "bottom", "position", cpos);
      endif
    endif

  endif

endfunction

## FIXME: The algorithm for positioning in legend.m is much more sophisticated
##        and should be borrowed for colorbar.  One problem is that colorbar
##        positioning does not take in to account multi-line axes labels
function [pos, cpos, vertical, mirr] = __position_colorbox__ (cbox, props, cf)

  ## This will always represent the position prior to adding the colorbar.
  pos = props.position;
  sz = pos(3:4);

  if (strcmp (props.plotboxaspectratiomode, "manual")
      || strcmp (props.dataaspectratiomode, "manual"))
    if (isempty (strfind (cbox, "outside")))
      scale = 1.0;
    else
      scale = 0.8;
    endif
    if (isempty (strfind (cbox, "east")) && isempty (strfind (cbox, "west")))
      scale = [1, scale];
    else
      scale = [scale, 1];
    endif
    if (strcmp (get (cf, "__graphics_toolkit__"), "gnuplot")
        && strcmp (props.activepositionproperty, "outerposition"))
      props.outerposition = props.outerposition .* [1, 1, scale];
      off = 0.5 * (props.outerposition (3:4) - __actual_axis_position__ (props)(3:4));
    else
      props.position = props.position .* [1, 1, scale];
      off = 0.5 * (props.position (3:4) - __actual_axis_position__ (props)(3:4));
    endif
  else
    off = 0.0;
  endif

  switch (cbox)
    case "northoutside"
      origin = pos(1:2) + [0., 0.9] .* sz + [1, -1] .* off;
      sz .*= [1.0, 0.06];
      pos(4) = 0.8 * pos(4);
      mirr = true;
      vertical = false;
    case "north"
      origin = pos(1:2) + [0.05, 0.9] .* sz + [1, -1] .* off;
      sz .*= [1.0, 0.06] * 0.9;
      mirr = false;
      vertical = false;
    case "southoutside"
      origin = pos(1:2) + off;
      sz .*= [1.0, 0.06];
      pos(2) = pos(2) + pos(4) * 0.2;
      pos(4) = 0.8 * pos(4);
      mirr = false;
      vertical = false;
    case "south"
      origin = pos(1:2) + [0.05, 0.05] .* sz + off;
      sz .*= [1.0, 0.06] * 0.9;
      mirr = true;
      vertical = false;
    case "eastoutside"
      origin = pos(1:2) + [0.9, 0] .* sz + [-1, 1] .* off;
      sz .*= [0.06, 1.0];
      pos(3) = 0.8 * pos(3);
      mirr = true;
      vertical = true;
    case "east"
      origin = pos(1:2) + [0.9, 0.05] .* sz + [-1, 1] .* off;
      sz .*= [0.06, 1.0] * 0.9;
      mirr = false;
      vertical = true;
    case "westoutside"
      origin = pos(1:2) + off;
      sz .*= [0.06, 1.0];
      pos(1) = pos(1) + pos(3) * 0.2;
      pos(3) = 0.8 * pos(3);
      mirr = false;
      vertical = true;
    case "west"
      origin = pos(1:2) + [0.05, 0.05] .* sz + off;
      sz .*= [0.06, 1.0] .* 0.9;
      mirr = true;
      vertical = true;
  endswitch

  cpos = [origin, sz];

  if (strcmp (props.plotboxaspectratiomode, "manual")
      || strcmp (props.dataaspectratiomode, "manual"))
    props.position = pos;
    actual_pos = __actual_axis_position__ (props);
    if (strfind (cbox, "outside"))
      scale = 1.0;
    else
      scale = 0.9;
    endif
    if (sz(1) > sz(2))
      ## Ensure north or south colorbars are the proper length
      dx = (1-scale)*actual_pos(3);
      cpos(1) = actual_pos(1) + dx/2;
      cpos(3) = actual_pos(3) - dx;
    else
      ## Ensure east or west colorbars are the proper height
      dy = (1-scale)*actual_pos(4);
      cpos(2) = actual_pos(2) + dy/2;
      cpos(4) = actual_pos(4) - dy;
    endif
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ();
%! title ("colorbar() example");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ("westoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ("peer", gca, "northoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ("southoutside");

%!demo
%! clf;
%! colormap ("default");
%! contour (peaks ());
%! colorbar ("west");

%!demo
%! clf;
%! colormap ("default");
%! subplot (2,2,1);
%!  contour (peaks ());
%!  colorbar ("east");
%! subplot (2,2,2);
%!  contour (peaks ());
%!  colorbar ("west");
%! subplot (2,2,3);
%!  contour (peaks ());
%!  colorbar ("north");
%! subplot (2,2,4);
%!  contour (peaks ());
%!  colorbar ("south");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,2,1);
%!  imagesc (x);
%!  colorbar ();
%! subplot (2,2,2);
%!  imagesc (x);
%!  colorbar ("westoutside");
%! subplot (2,2,3);
%!  imagesc (x);
%!  colorbar ("northoutside");
%! subplot (2,2,4);
%!  imagesc (x);
%!  colorbar ("southoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ();
%! subplot (1,2,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ("westoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ("northoutside");
%! subplot (1,2,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ("southoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ();
%! subplot (2,1,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ("westoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ("northoutside");
%! subplot (2,1,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ("southoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  colorbar ();
%! subplot (1,2,2);
%!  imagesc (x);
%!  colorbar ("westoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  colorbar ("northoutside");
%! subplot (1,2,2);
%!  imagesc (x);
%!  colorbar ("southoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  colorbar ();
%! subplot (2,1,2);
%!  imagesc (x);
%!  colorbar ("westoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  colorbar ("northoutside");
%! subplot (2,1,2);
%!  imagesc (x);
%!  colorbar ("southoutside");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  contour (x);
%!  axis square;
%!  colorbar ("east");
%!  xlim ([1, 64]);
%!  ylim ([1, 64]);
%! subplot (1,2,2);
%!  contour (x);
%!  colorbar ("west");
%!  xlim ([1, 64]);
%!  ylim ([1, 64]);

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! contour (x);
%! xlim ([1, 64]);
%! ylim ([1, 64]);
%! colorbar ();
%! colorbar off;
%! title ("colorbar off");

%!demo
%! clf;
%! colormap ("default");
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! contour (x);
%! xlim ([1, 64]);
%! ylim ([1, 64]);
%! colorbar ();

%!demo
%! clf;
%! colormap ("default");
%! imagesc (log10 (1 ./ hilb (99)));
%! h = colorbar ();
%! ytick = get (h, "ytick");
%! set (h, "yticklabel", sprintf ("10^{%g}|", ytick));

%!demo
%! clf;
%! colormap ("default");
%! n = 5; x = linspace (0,5,n); y = linspace (0,1,n);
%! imagesc (1 ./ hilb (n));
%! axis equal;
%! colorbar ();

%!demo
%! clf;
%! colormap ("default");
%! n = 5; x = linspace (0,5,n); y = linspace (0,1,n);
%! imagesc (x, y, 1 ./ hilb (n));
%! axis equal;
%! colorbar ();

%!demo
%! clf;
%! colormap ("default");
%! n = 5; x = linspace (0,5,n); y = linspace (0,1,n);
%! imagesc (y, x, 1 ./ hilb (n));
%! axis equal;
%! colorbar ();

## This requires that the axes position be properly determined for "axis equal"
%!demo
%! clf;
%! colormap ("default");
%! axes;
%! colorbar ();
%! hold on;
%! contour (peaks ());
%! hold off;

%!demo
%! clf;
%! colormap ("default");
%! plot ([0, 2]);
%! colorbar ("east");
%! axis square;

%!demo
%! clf;
%! colormap ("default");
%! plot ([0, 2]);
%! colorbar ("eastoutside");
%! axis square;

%!demo
%! clf;
%! colormap ("default");
%! pcolor (peaks (20));
%! shading interp;
%! axis ("tight", "square");
%! colorbar ();
#%! axes ("color","none","box","on","activepositionproperty","position");

%!demo
%! clf;
%! colormap ("default");
%! plot ([0, 2]);
%! colorbar ("east");
%! axis equal;

%!demo
%! clf;
%! colormap ("default");
%! plot ([0, 2]);
%! colorbar ("eastoutside");
%! axis equal;


## Test input validation
%!error <expected string argument at position 1> colorbar (-pi)
%!error <LOC specification must occur as final arg> colorbar ("east", "p", "v")
%!error <missing value after "location"> colorbar ("location")
%!error <missing axes handle after "peer"> colorbar ("peer")
%!error <invalid axes handle following "peer"> colorbar ("peer", -1)
%!error <PROP/VAL inputs must occur in pairs> colorbar ("PROP")

