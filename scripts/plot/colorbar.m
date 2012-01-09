## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn  {Function File} {} colorbar (@var{s})
## @deftypefnx {Function File} {} colorbar ("peer", @var{h}, @dots{})
## Add a colorbar to the current axes.  Valid values for @var{s} are
##
## @table @asis
## @item "EastOutside"
## Place the colorbar outside the plot to the right.  This is the default.
##
## @item "East"
## Place the colorbar inside the plot to the right.
##
## @item "WestOutside"
## Place the colorbar outside the plot to the left.
##
## @item "West"
## Place the colorbar inside the plot to the left.
##
## @item "NorthOutside"
## Place the colorbar above the plot.
##
## @item "North"
## Place the colorbar at the top of the plot.
##
## @item "SouthOutside"
## Place the colorbar under the plot.
##
## @item "South"
## Place the colorbar at the bottom of the plot.
##
## @item "Off", "None"
## Remove any existing colorbar from the plot.
## @end table
##
## If the argument "peer" is given, then the following argument is treated
## as the axes handle on which to add the colorbar.
## @end deftypefn

function h = colorbar (varargin)
  ax = [];
  loc = "eastoutside";
  args = {};
  deleting = false;

  i = 1;
  while (i <= nargin)
    arg = varargin {i++};
    if (ischar(arg))
      if (strcmpi (arg, "peer"))
        if (i > nargin)
          error ("colorbar: missing axes handle after \"peer\"");
        else
          ax = varargin{i++};
          if (!isscalar (ax) || ! ishandle (ax)
              || ! strcmp (get (ax, "type"), "axes"))
            error ("colorbar: expecting an axes handle following \"peer\"");
          endif
        endif
      elseif (strcmpi (arg, "north") || strcmpi (arg, "south")
              || strcmpi (arg, "east") || strcmpi (arg, "west")
              || strcmpi (arg, "northoutside") || strcmpi (arg, "southoutside")
              || strcmpi (arg, "eastoutside") || strcmpi (arg, "westoutside"))
        loc = tolower (arg);
      elseif (strcmpi (arg, "location") && i <= nargin)
        loc = tolower (varargin{i++});
      elseif (strcmpi (arg, "off") || strcmpi (arg, "none"))
        deleting = true;
      else
        args{end+1} = arg;
      endif
    else
      args{end+1} = arg;
    endif
  endwhile

  if (isempty (ax))
    ax = gca ();
  endif

  showhiddenhandles = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "on");
    cax = findobj (get (ax, "parent"), "tag", "colorbar", "type", "axes", "axes", ax);
    if (! isempty (cax))
      delete (cax);
    endif
  unwind_protect_cleanup
    set (0, "showhiddenhandles", showhiddenhandles);
  end_unwind_protect

  if (! deleting)
    ## FIXME - Matlab does not require the "position" property to be active.
    ##         Is there a way to determine the plotbox position for the
    ##         gnuplot graphics toolkit with the outerposition is active?
    set (ax, "activepositionproperty", "position");
    obj = get (ax);
    obj.__my_handle__ = ax;
    position = obj.position;
    clen = rows (get (get (ax, "parent"), "colormap"));
    cext = get (ax, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    [pos, cpos, vertical, mirror] =  ...
        __position_colorbox__ (loc, obj, ancestor (ax, "figure"));
    set (ax, "position", pos);

    cax = __go_axes__ (get (ax, "parent"), "tag", "colorbar",
                       "handlevisibility", "on",
                       "activepositionproperty", "position",
                       "position", cpos);
    addproperty ("location", cax, "radio",
                 "eastoutside|east|westoutside|west|northoutside|north|southoutside|south",
                 loc);
    addproperty ("axes", cax, "handle", ax);

    if (vertical)
      hi = image (cax, [0,1], [cmin, cmax], [1 : clen]');
      if (mirror)
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
             "ylim", cext, "ylimmode", "manual",
             "yaxislocation", "right", args{:});
      else
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
             "ylim", cext, "ylimmode", "manual",
             "yaxislocation", "left", args{:});
      endif
    else
      hi = image (cax, [cmin, cmax], [0,1], [1 : clen]);
      if (mirror)
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
             "xlim", cext, "xlimmode", "manual",
             "xaxislocation", "top", args{:});
      else
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
             "xlim", cext, "xlimmode", "manual",
             "xaxislocation", "bottom", args{:});
      endif
    endif

    ctext = text (0, 0, "", "tag", "colorbar","visible", "off",
                  "handlevisibility", "off", "xliminclude", "off",
                  "yliminclude", "off", "zliminclude", "off",
                  "deletefcn", {@deletecolorbar, cax, obj});

    set (cax, "deletefcn", {@resetaxis, obj});

    addlistener (ax, "clim", {@update_colorbar_clim, hi, vertical});
    addlistener (ax, "plotboxaspectratio", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "plotboxaspectratiomode", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "dataaspectratio", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "dataaspectratiomode", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "position", {@update_colorbar_axis, cax, obj});

  endif

  if (nargout > 0)
    h = cax;
  endif
endfunction

function deletecolorbar (h, d, hc, orig_props)
  ## Don't delete the colorbar and reset the axis size if the
  ## parent figure is being deleted.
  if (ishandle (hc) && strcmp (get (hc, "type"), "axes")
      && (isempty (gcbf()) || strcmp (get (gcbf(), "beingdeleted"),"off")))
    if (strcmp (get (hc, "beingdeleted"), "off"))
      delete (hc);
    endif
    if (!isempty (ancestor (h, "axes"))
        && strcmp (get (ancestor (h, "axes"), "beingdeleted"), "off"))
      set (ancestor (h, "axes"), "position", orig_props.position, ...
                            "outerposition", orig_props.outerposition, ...
                    "activepositionproperty", orig_props.activepositionproperty);
    endif
  endif
endfunction

function resetaxis (h, d, orig_props)
  if (ishandle (h) && strcmp (get (h, "type"), "axes")
      && (isempty (gcbf()) || strcmp (get (gcbf(), "beingdeleted"),"off"))
      && ishandle (get (h, "axes")))
     set (get (h, "axes"), "position", orig_props.position, ...
                           "outerposition", orig_props.outerposition, ...
                   "activepositionproperty", orig_props.activepositionproperty);
  endif
endfunction

function update_colorbar_clim (h, d, hi, vert)
  if (ishandle (h) && strcmp (get (h, "type"), "image")
      && (isempty (gcbf()) || strcmp (get (gcbf(), "beingdeleted"),"off")))
    clen = rows (get (get (h, "parent"), "colormap"));
    cext = get (h, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    if (vert)
      set (hi, "ydata", [cmin, cmax]);
      set (get (hi, "parent"), "ylim", cext);
    else
      set (hi, "xdata", [cmin, cmax]);
      set (get (hi, "parent"), "xlim", cext);
    endif
  endif
endfunction

function update_colorbar_axis (h, d, cax, orig_props)

  if (ishandle (cax) && strcmp (get (cax, "type"), "axes")
      && (isempty (gcbf()) || strcmp (get (gcbf(), "beingdeleted"),"off")))
    loc = get (cax, "location");
    obj = get (h);
    obj.__my_handle__ = h;
    obj.position = orig_props.position;
    obj.outerposition = orig_props.outerposition;
    [pos, cpos, vertical, mirror] =  ...
        __position_colorbox__ (loc, obj, ancestor (h, "figure"));

    if (vertical)
      if (mirror)
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
             "yaxislocation", "right", "position", cpos);
      else
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
             "yaxislocation", "left", "position", cpos);
      endif
    else
      if (mirror)
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
             "xaxislocation", "top", "position", cpos);
      else
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
             "xaxislocation", "bottom", "position", cpos);
      endif
    endif

  endif
endfunction

function [pos, cpos, vertical, mirr] = __position_colorbox__ (cbox, obj, cf)

  ## This will always represent the position prior to adding the colorbar.
  pos = obj.position;
  sz = pos(3:4);

  if (strcmpi (obj.plotboxaspectratiomode, "manual")
      || strcmpi (obj.dataaspectratiomode, "manual"))
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
        && strcmp (obj.activepositionproperty, "outerposition"))
      obj.outerposition = obj.outerposition .* [1, 1, scale];
      off = 0.5 * (obj.outerposition (3:4) - __actual_axis_position__ (obj)(3:4));
    else
      obj.position = obj.position .* [1, 1, scale];
      off = 0.5 * (obj.position (3:4) - __actual_axis_position__ (obj)(3:4));
    endif
  else
    off = 0.0;
  endif

  switch (cbox)
    case "northoutside"
      origin = pos(1:2) + [0., 0.9] .* sz + [1, -1] .* off;
      sz = sz .* [1.0, 0.06];
      pos(4) = 0.8 * pos(4);
      mirr = true;
      vertical = false;
    case "north"
      origin = pos(1:2) + [0.05, 0.9] .* sz + [1, -1] .* off;
      sz = sz .* [1.0, 0.06] * 0.9;
      mirr = false;
      vertical = false;
    case "southoutside"
      origin = pos(1:2) + off;
      sz = sz .* [1.0, 0.06];
      pos(2) = pos(2) + pos(4) * 0.2;
      pos(4) = 0.8 * pos(4);
      mirr = false;
      vertical = false;
    case "south"
      origin = pos(1:2) + [0.05, 0.05] .* sz + off;
      sz = sz .* [1.0, 0.06] * 0.9;
      mirr = true;
      vertical = false;
    case "eastoutside"
      origin = pos(1:2) + [0.9, 0] .* sz + [-1, 1] .* off;
      sz = sz .* [0.06, 1.0];
      pos(3) = 0.8 * pos(3);
      mirr = true;
      vertical = true;
    case "east"
      origin = pos(1:2) + [0.9, 0.05] .* sz + [-1, 1] .* off;
      sz = sz .* [0.06, 1.0] * 0.9;
      mirr = false;
      vertical = true;
    case "westoutside"
      origin = pos(1:2) + off;
      sz = sz .* [0.06, 1.0];
      pos(1) = pos(1) + pos(3) * 0.2;
      pos(3) = 0.8 * pos(3);
      mirr = false;
      vertical = true;
    case "west"
      origin = pos(1:2) + [0.05, 0.05] .* sz + off;
      sz = sz .* [0.06, 1.0] .* 0.9;
      mirr = true;
      vertical = true;
  endswitch

  cpos = [origin, sz];

  if (strcmpi (obj.plotboxaspectratiomode, "manual")
      || strcmpi (obj.dataaspectratiomode, "manual"))
    obj.position = pos;
    actual_pos = __actual_axis_position__ (obj);
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
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! imagesc(x)
%! colorbar();

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! imagesc(x)
%! colorbar("westoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! imagesc(x)
%! colorbar("peer", gca (), "northoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! imagesc(x)
%! colorbar("southoutside");

%!demo
%! clf
%! contour(peaks())
%! colorbar("west");

%!demo
%! clf
%! subplot(2,2,1)
%! contour(peaks())
%! colorbar("east");
%! subplot(2,2,2)
%! contour(peaks())
%! colorbar("west");
%! subplot(2,2,3)
%! contour(peaks())
%! colorbar("north");
%! subplot(2,2,4)
%! contour(peaks())
%! colorbar("south");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(2,2,1)
%! imagesc(x)
%! colorbar();
%! subplot(2,2,2)
%! imagesc(x)
%! colorbar("westoutside");
%! subplot(2,2,3)
%! imagesc(x)
%! colorbar("northoutside");
%! subplot(2,2,4)
%! imagesc(x)
%! colorbar("southoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(1,2,1)
%! imagesc(x)
%! axis square;
%! colorbar();
%! subplot(1,2,2)
%! imagesc(x)
%! axis square;
%! colorbar("westoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(1,2,1)
%! imagesc(x)
%! axis square;
%! colorbar("northoutside");
%! subplot(1,2,2)
%! imagesc(x)
%! axis square;
%! colorbar("southoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(2,1,1)
%! imagesc(x)
%! axis square;
%! colorbar();
%! subplot(2,1,2)
%! imagesc(x)
%! axis square;
%! colorbar("westoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(2,1,1)
%! imagesc(x)
%! axis square;
%! colorbar("northoutside");
%! subplot(2,1,2)
%! imagesc(x)
%! axis square;
%! colorbar("southoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(1,2,1)
%! imagesc(x)
%! colorbar();
%! subplot(1,2,2)
%! imagesc(x)
%! colorbar("westoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(1,2,1)
%! imagesc(x)
%! colorbar("northoutside");
%! subplot(1,2,2)
%! imagesc(x)
%! colorbar("southoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(2,1,1)
%! imagesc(x)
%! colorbar();
%! subplot(2,1,2)
%! imagesc(x)
%! colorbar("westoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(2,1,1)
%! imagesc(x)
%! colorbar("northoutside");
%! subplot(2,1,2)
%! imagesc(x)
%! colorbar("southoutside");

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! subplot(1,2,1)
%! contour(x)
%! axis square;
%! colorbar("east");
%! xlim ([1, 64])
%! ylim ([1, 64])
%! subplot(1,2,2)
%! contour(x)
%! colorbar("west");
%! xlim ([1, 64])
%! ylim ([1, 64])

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! contour (x)
%! xlim ([1, 64])
%! ylim ([1, 64])
%! colorbar ();
%! colorbar off

%!demo
%! clf
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.');
%! contour (x)
%! xlim ([1, 64])
%! ylim ([1, 64])
%! colorbar ();
%! colorbar ();

%!demo
%! clf
%! imagesc (1./hilb(99));
%! h = colorbar;
%! set (h, 'yscale', 'log');

%!demo
%! clf
%! imagesc (log10 (1 ./ hilb (99)));
%! h = colorbar;
%! ytick = get(h, "ytick");
%! set (h, "yticklabel", sprintf ('10^{%g}|', ytick));

%!demo
%! clf
%! n=5;x=linspace(0,5,n);y=linspace(0,1,n);
%! imagesc(1./hilb(n)); axis equal; colorbar

%!demo
%! clf
%! n=5;x=linspace(0,5,n);y=linspace(0,1,n);
%! imagesc(x,y,1./hilb(n)); axis equal; colorbar

%!demo
%! clf
%! n=5;x=linspace(0,5,n);y=linspace(0,1,n);
%! imagesc(y,x,1./hilb(n)); axis equal; colorbar
## This requires that the axes position be properly determined for "axes equal"

%!demo
%! clf
%! axes
%! colorbar
%! hold on
%! contour(peaks)
%! hold off

%!demo
%! clf
%! plot([0, 2])
%! colorbar ("east")
%! axis square

%!demo
%! clf
%! plot([0, 2])
%! colorbar ("eastoutside")
%! axis square

%!demo
%! clf
%! pcolor (peaks (20))
%! shading ("interp")
%! axis ("tight", "square")
%! colorbar ()
#%! axes('color','none','box','on','activepositionproperty','position')

%!demo
%! clf
%! plot([0, 2])
%! colorbar ("east")
%! axis equal

%!demo
%! clf
%! plot([0, 2])
%! colorbar ("eastoutside")
%! axis equal
