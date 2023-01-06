########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## The optional input @nospell{@var{loc}} determines the location of the
## colorbar.  If present, it must be the last argument to @code{colorbar}.
## Valid values for @nospell{@var{loc}} are
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
## added to this axes, rather than the current axes returned by @code{gca}.
## Alternatively, If the argument @qcode{"peer"} is given, then the following
## argument is treated as the axes handle in which to add the colorbar.  The
## @qcode{"peer"} calling syntax may be removed in the future and is not
## recommended.
##
## If the first argument @var{hcb} is a handle to a colorbar object, then
## operate on this colorbar directly.
##
## Additional property/value pairs are passed directly to the underlying axes
## object.  The full list of properties is documented at
## @ref{Axes Properties}.
##
## The optional return value @var{h} is a graphics handle to the created
## colorbar object.
##
## Implementation Note: A colorbar is created as an additional axes object
## with the @qcode{"tag"} property set to @qcode{"colorbar"}.  The created
## object has the extra property @qcode{"location"} which controls the
## positioning of the colorbar.
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
  cbpos = [];
  args = {};
  delete_cbar = false;

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

      case {"delete", "hide", "off"}
        delete_cbar = true;

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
        if (strcmpi (arg, "position"))
          loc = "manual";
          cbpos = args{end};
        endif

    endswitch
  endwhile

  if (isempty (loc))
    loc = "eastoutside";
  else
    ## Validate location
    if (! any (strcmp (loc, {"eastoutside"; "east"; "westoutside"; "west";
                             "northoutside"; "north"; "southoutside"; "south";
                             "manual"})))
      error ("colorbar: unrecognized colorbar location");
    endif
  endif

  ## Handle deletion case early and return
  if (delete_cbar)
    if (isempty (hcb))
      if (isempty (hax))
        hax = get (get (0, "currentfigure"), "currentaxes");
      endif
      try
        hcb = get (hax, "__colorbar_handle__");
      end_try_catch
    endif

    delete (hcb);

    if (nargout > 0)
      h = [];
    endif
    return;
  endif

  ## Handle changes to specified colorbar
  if (! isempty (hcb))
    ## FIXME: No listener on location property so have to re-create
    ##        colorbar whenever an option changes.
    ##        re-instate this code if listener is developed.
    ## if (! isempty (loc))
    ##   set (hcb, "location", loc);
    ## endif
    ## if (! isempty (args))
    ##   set (hcb, args{:});
    ## endif
    hax = get (hcb, "__axes_handle__");
  else
    ## Find any colorbar associated with this axes
    if (isempty (hax))
      hax = gca ();
    endif
    try
      hcb = get (hax, "__colorbar_handle__");
    end_try_catch
  endif

  ## New or existing colorbar?
  new_colorbar = isempty (hcb);

  if (! new_colorbar)
    ## Restore original axes position before applying new colorbar settings
    orig_props = get (hcb, "deletefcn"){3};
    units = get (hax, "units");
    set (hax, "units", orig_props.units,
              "position", orig_props.position,
              "outerposition", orig_props.outerposition,
              "positionconstraint", orig_props.positionconstraint);
    set (hax, "units", units);
  endif

  ## Create a colorbar

  ## Special handling for plotyy which has two axes objects
  if (isprop (hax, "__plotyy_axes__"))
    hyy = get (hax, "__plotyy_axes__");

    ## Use axis which is appropriate for legend location;
    ## Necessary for plotyy figures where there are two axes.
    if (strfind (loc, "east"))
      hax = hyy(2);
    else
      hax = hyy(1);
    endif
  endif

  ## Preamble code to restore figure and axes after colorbar creation
  hfig = ancestor (hax, "figure");
  origfig = get (0, "currentfigure");
  if (origfig != hfig)
    set (0, "currentfigure", hfig);
  else
    origfig = [];
  endif
  origaxes = get (hfig, "currentaxes");
  unwind_protect
    ## FIXME: Matlab does not require the "position" property to be active.
    ##        Is there a way to determine the plotbox position for the
    ##        gnuplot graphics toolkit when the outerposition is active?
    set (hax, "positionconstraint", "innerposition");
    props = get (hax);
    props.__axes_handle__ = hax;
    position = props.position;

    cmap = get (hax, "colormap");
    clen = rows (cmap);
    cext = get (hax, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    hpar = get (hax, "parent");

    if (isempty (cbpos))
      ## auto positioning
      ## FIXME: Should handle user-specified "AxisLocation" property (mirror).
      [axpos, cbpos, vertical, mirror] = ...
        calc_cbar_position (loc, props, ancestor (hpar, "figure"));
      set (hax, "position", axpos);
    endif

    ## Create colorbar axes if necessary
    if (new_colorbar)
      hcb = axes ("parent", hpar, "tag", "colorbar",
                  "positionconstraint", "innerposition",
                  "units", get (hax, "units"), "position", cbpos,
                  "colormap", cmap,
                  "box", "on", "xdir", "normal", "ydir", "normal");

      addproperty ("axislocation", hcb, "radio", "{out}|in");
      addproperty ("axislocationmode", hcb, "radio", "{auto}|manual");
      addproperty ("label", hcb, "handle", get (hcb, "ylabel"));
      addproperty ("direction", hcb, "AxesYdir", "normal");
      addproperty ("limits", hcb, "AxesYlim");
      addproperty ("limitsmode", hcb, "AxesYlimMode", "auto");
      addproperty ("location", hcb, "radio",
                   "{eastoutside}|east|westoutside|west|northoutside|north|southoutside|south|manual",
                   loc);
      addproperty ("tickdirection", hcb, "AxesTickdir", "in");
      ## FIXME: Matlab uses just a scalar for ticklength, but axes already
      ##        has a 2-element ticklength property which cannot be overridden.
      ## addproperty ("ticklength", hcb, "double", 0.01);

      ## Add a pointer from colorbar directly to axes
      addproperty ("__axes_handle__", hcb, "handle", hax);
      ## Also add a pointer back from axes to this colorbar
      try
        addproperty ("__colorbar_handle__", hax, "handle", hcb);
      catch
        set (hax, "__colorbar_handle__", hcb);
      end_try_catch
      addproperty ("__vertical__", hcb, "boolean", vertical);
      ## Use low-level form to avoid calling newplot which changes axes
      hi = image (hcb, "xdata", [0,1], "ydata", [cmin, cmax],
                       "cdata", [1 : clen]');
    else
      ## Change settings of existing colorbar
      set (hcb, "parent", hpar, "position", cbpos, "location", loc);
      ## Fetch image handle from existing colorbar
      hi = get (hcb, "children");
    endif

    if (vertical)
      set (hi, "xdata", [0,1], "ydata", [cmin, cmax], "cdata", [1 : clen]');
      if (mirror)
        set (hcb, "xtick", [], "xlim", [-0.5, 1.5],
                  "ytickmode", "auto", "ylim", cext,
                  "yaxislocation", "right", "label", get (hcb, "ylabel"),
                  "__vertical__", vertical,
                  "layer", "top", args{:});
      else
        set (hcb, "xtick", [], "xlim", [-0.5, 1.5],
                  "ytickmode", "auto", "ylim", cext,
                  "yaxislocation", "left", "label", get (hcb, "ylabel"),
                  "__vertical__", vertical,
                  "layer", "top", args{:});
      endif
    else
      set (hi, "xdata", [cmin, cmax], "ydata", [0,1], "cdata", [1 : clen]);
      if (mirror)
        set (hcb, "ytick", [], "ylim", [-0.5, 1.5],
                  "xtickmode", "auto", "xlim", cext,
                  "xaxislocation", "top", "label", get (hcb, "xlabel"),
                  "__vertical__", vertical,
                  "layer", "top", args{:});
      else
        set (hcb, "ytick", [], "ylim", [-0.5, 1.5],
                  "xtickmode", "auto", "xlim", cext,
                  "xaxislocation", "bottom", "label", get (hcb, "xlabel"),
                  "__vertical__", vertical,
                  "layer", "top", args{:});
      endif
    endif

    ## Add listeners, but only to a new colorbar
    if (new_colorbar)
      ## Dummy object placed on axes to delete colorbar when axes is deleted.
      ctext = text (0, 0, "", "tag", "colorbar", "parent", hax,
                    "visible", "off", "handlevisibility", "off",
                    "xliminclude", "off", "yliminclude", "off",
                    "zliminclude", "off",
                    "deletefcn", {@cb_axes_deleted, hax, hcb});

      set (hcb, "deletefcn", {@cb_restore_axes, hax, props});

      addlistener (hcb, "xscale", {@cb_error_on_logscale, "xscale"});
      addlistener (hcb, "yscale", {@cb_error_on_logscale, "yscale"});
      addlistener (hcb, "tickdirection", @cb_tickdirection);

      if (strcmp (get (hpar, "type"), "figure"))
        addlistener (hpar, "colormap", {@cb_colormap, ...
                                        hax, hcb, hi, clen});
      endif
      addlistener (hax, "colormap", {@cb_colormap, hax, hcb, hi, clen});
      addlistener (hax, "clim", {@cb_clim, hcb, hi});
      addlistener (hax, "dataaspectratio", {@cb_colorbar_axis, hcb, props});
      addlistener (hax, "dataaspectratiomode", {@cb_colorbar_axis, ...
                                                hcb, props});
      addlistener (hax, "plotboxaspectratio", {@cb_colorbar_axis, hcb, props});
      addlistener (hax, "plotboxaspectratiomode", {@cb_colorbar_axis, ...
                                                   hcb, props});
      addlistener (hax, "position", {@cb_colorbar_axis, hcb, props});

      ## FIXME: Need listeners for colorbar: axislocation, axislocationmode,
      ##        direction, limits, limitsmode, location.
    endif
  unwind_protect_cleanup
    set (hfig, "currentaxes", origaxes);
    if (! isempty (origfig))
      set (0, "currentfigure", origfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = hcb;
  endif

endfunction

## Axes to which colorbar was attached is being deleted/reset. Delete colorbar.
function cb_axes_deleted (~, ~, hax, hcb)

  if (isaxes (hcb))
    if (strcmp (get (hax, "beingdeleted"), "on"))
      ## Axes are being deleted.  Disable call to cb_restore_axes.
      set (hcb, "deletefcn", []);
    endif
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

## Colorbar "TickDirection" callback which just maps to axes "TickDir"
function cb_tickdirection (hcb, ~)
  set (hcb, "tickdir", get (hcb, "tickdirection"));
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
    set (hax, "units", orig_props.units,
              "position", orig_props.position,
              "outerposition", orig_props.outerposition,
              "positionconstraint", orig_props.positionconstraint);
    set (hax, "units", units);

    ## Nullify colorbar link (can't delete properties yet)
    set (hax, "__colorbar_handle__", []);
  endif

endfunction

## Update colorbar when clim has changed
function cb_clim (hax, ~, hcb, hi)

  if (isaxes (hax) && isaxes (hcb))
    clen = rows (get (hax, "colormap"));
    cext = get (hax, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    if (strcmp (get (hcb, "__vertical__"), "on"))
      set (hi, "ydata", [cmin, cmax]);
      set (hcb, "ylim", cext);
    else
      set (hi, "xdata", [cmin, cmax]);
      set (hcb, "xlim", cext);
    endif
  endif

endfunction

## Update colorbar when changes to axes or figure colormap have occurred.
function cb_colormap (h, ~, hax, hcb, hi, init_sz)
  persistent sz = init_sz;

  if (ishghandle (h))
    cmap = get (h, "colormap");
    set (hcb, "colormap", cmap);
    clen = rows (cmap);
    if (clen != sz)
      if (strcmp (get (hcb, "__vertical__"), "on"))
        set (hi, "cdata", [1:clen]');
      else
        set (hi, "cdata", [1:clen]);
      endif
      sz = clen;
      ## Also update limits on colorbar axes or there will be white gaps
      cb_clim (hax, d, hcb, hi);
    endif
  endif

endfunction

## Update positioning of colorbar when original axes has changed position.
function cb_colorbar_axis (hax, ~, hcb, orig_props)

  if (isaxes (hcb))
    loc = get (hcb, "location");
    if (strcmp (loc, "manual"))
      return;  # Use user-specified positioning
    endif

    props = get (hax);
    props.__axes_handle__ = hax;
    props.position = orig_props.position;
    props.outerposition = orig_props.outerposition;
    [axpos, cbpos, vertical, mirror] = ...
       calc_cbar_position (loc, props, ancestor (hax, "figure"));

    set (hcb, "position", cbpos);
  endif

endfunction

## FIXME: The algorithm for positioning in legend.m is much more sophisticated
##        and should be borrowed for colorbar.  One problem is that colorbar
##        positioning does not take in to account multi-line axes labels.
## FIXME: Should handle user-specified "AxisLocation" property (mirror var).
function [axpos, cbpos, vertical, mirr] = calc_cbar_position (loc, props, cf)

  ## This will always represent the position prior to adding the colorbar.
  axpos = props.position;
  sz = axpos(3:4);

  if (strcmp (props.plotboxaspectratiomode, "manual")
      || strcmp (props.dataaspectratiomode, "manual"))
    if (isempty (strfind (loc, "outside")))
      scale = 1.0;
    else
      scale = 0.8;
    endif
    if (isempty (strfind (loc, "east")) && isempty (strfind (loc, "west")))
      scale = [1, scale];
    else
      scale = [scale, 1];
    endif
    if (strcmp (get (cf, "__graphics_toolkit__"), "gnuplot")
        && strcmp (props.positionconstraint, "outerposition"))
      props.outerposition = props.outerposition .* [1, 1, scale];
      off = 0.5 * (props.outerposition (3:4) - __actual_axis_position__ (props)(3:4));
    else
      props.position = props.position .* [1, 1, scale];
      off = 0.5 * (props.position (3:4) - __actual_axis_position__ (props)(3:4));
    endif
  else
    off = 0.0;
  endif

  switch (loc)
    case "northoutside"
      origin = axpos(1:2) + [0., 0.9] .* sz + [1, -1] .* off;
      sz .*= [1.0, 0.06];
      axpos(4) = 0.8 * axpos(4);
      mirr = true;
      vertical = false;
    case "north"
      origin = axpos(1:2) + [0.05, 0.9] .* sz + [1, -1] .* off;
      sz .*= [1.0, 0.06] * 0.9;
      mirr = false;
      vertical = false;
    case "southoutside"
      origin = axpos(1:2) + off;
      sz .*= [1.0, 0.06];
      axpos(2) = axpos(2) + axpos(4) * 0.2;
      axpos(4) = 0.8 * axpos(4);
      mirr = false;
      vertical = false;
    case "south"
      origin = axpos(1:2) + [0.05, 0.05] .* sz + off;
      sz .*= [1.0, 0.06] * 0.9;
      mirr = true;
      vertical = false;
    case "eastoutside"
      origin = axpos(1:2) + [0.9, 0] .* sz + [-1, 1] .* off;
      sz .*= [0.06, 1.0];
      axpos(3) = 0.8 * axpos(3);
      mirr = true;
      vertical = true;
    case "east"
      origin = axpos(1:2) + [0.9, 0.05] .* sz + [-1, 1] .* off;
      sz .*= [0.06, 1.0] * 0.9;
      mirr = false;
      vertical = true;
    case "westoutside"
      origin = axpos(1:2) + off;
      sz .*= [0.06, 1.0];
      axpos(1) = axpos(1) + axpos(3) * 0.2;
      axpos(3) = 0.8 * axpos(3);
      mirr = false;
      vertical = true;
    case "west"
      origin = axpos(1:2) + [0.05, 0.05] .* sz + off;
      sz .*= [0.06, 1.0] .* 0.9;
      mirr = true;
      vertical = true;
  endswitch

  cbpos = [origin, sz];

  if (strcmp (props.plotboxaspectratiomode, "manual")
      || strcmp (props.dataaspectratiomode, "manual"))
    props.position = axpos;
    actual_pos = __actual_axis_position__ (props);
    if (strfind (loc, "outside"))
      scale = 1.0;
    else
      scale = 0.9;
    endif
    if (sz(1) > sz(2))
      ## Ensure north or south colorbars are the proper length
      dx = (1-scale)*actual_pos(3);
      cbpos(1) = actual_pos(1) + dx/2;
      cbpos(3) = actual_pos(3) - dx;
    else
      ## Ensure east or west colorbars are the proper height
      dy = (1-scale)*actual_pos(4);
      cbpos(2) = actual_pos(2) + dy/2;
      cbpos(4) = actual_pos(4) - dy;
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
%! axes ();
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
#%! axes ("color", "none", "box", "on", "positionconstraint", "innerposition");

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

## FIXME: need many BIST tests for colorbar

## Test input validation
%!error <expected string argument at position 1> colorbar (-pi)
%!error <LOC specification must occur as final arg> colorbar ("east", "p", "v")
%!error <missing value after "location"> colorbar ("location")
%!error <missing axes handle after "peer"> colorbar ("peer")
%!error <invalid axes handle following "peer"> colorbar ("peer", -1)
%!error <PROP/VAL inputs must occur in pairs> colorbar ("PROP")
%!error <unrecognized colorbar location> colorbar ("location", "foobar")

