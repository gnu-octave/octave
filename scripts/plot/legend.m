## Copyright (C) 2010-2012 David Bateman
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
## @deftypefn  {Function File} {} legend (@var{str1}, @var{str2}, @dots{})
## @deftypefnx {Function File} {} legend (@var{matstr})
## @deftypefnx {Function File} {} legend (@var{cell})
## @deftypefnx {Function File} {} legend (@dots{}, "location", @var{pos})
## @deftypefnx {Function File} {} legend (@dots{}, "orientation", @var{orient})
## @deftypefnx {Function File} {} legend (@var{hax}, @dots{})
## @deftypefnx {Function File} {} legend (@var{hobjs}, @dots{})
## @deftypefnx {Function File} {} legend (@var{hax}, @var{hobjs}, @dots{})
## @deftypefnx {Function File} {} legend ("@var{option}")
##
## Display a legend for the axes with handle @var{hax}, or the current axes,
## using the specified strings as labels.  Legend entries may be specified
## as individual character string arguments, a character array, or a cell
## array of character strings.  If the handles, @var{hobjs}, are not specified
## then the legend's strings will be associated with the axes' descendants.
## Legend works on line graphs, bar graphs, etc.
## A plot must exist before legend is called.
##
## The optional parameter @var{pos} specifies the location of the legend
## as follows:
##
## @multitable @columnfractions 0.06 0.14 0.80
##
## @headitem @tab @var{pos} @tab
##   location of the legend
##
## @item @tab north @tab
##   center top
##
## @item @tab south @tab
##   center bottom
##
## @item @tab east @tab
##   right center
##
## @item @tab west @tab
##   left center
##
## @item @tab northeast @tab
##   right top (default)
##
## @item @tab northwest @tab
##   left top
##
## @item @tab southeast @tab
##   right bottom
##
## @item @tab southwest @tab
##   left bottom
##
## @item
##
## @item @tab outside @tab
##   can be appended to any location string
## @end multitable
##
## The optional parameter @var{orient} determines if the key elements
## are placed vertically or horizontally.  The allowed values are "vertical"
## or "horizontal" with the default being "vertical".
##
## The following customizations are available using @var{option}:
##
## @table @asis
## @item "show"
##   Show legend on the plot
##
## @item "hide"
##   Hide legend on the plot
##
## @itemx "toggle"
##   Toggles between "hide" and "show"
##
## @item "boxon"
##   Show a box around legend
##
## @item "boxoff"
##   Hide the box around legend
##
## @item "left"
##   Place text to the left of the keys
##
## @item "right"
##   Place text to the right of the keys
##
## @itemx "off"
##   Delete the legend object
## @end table
## @end deftypefn

function [hlegend2, hobjects2, hplot2, text_strings2] = legend (varargin)

  if (nargin > 0
      && (! ishandle (varargin{1})
          || (strcmp (get (varargin{1}, "type"), "axes")
              && ! strcmp (get (varargin{1}, "tag"), "legend"))))
    [ca, varargin, nargs] = __plt_get_axis_arg__ ("legend", varargin{:});
    fig = get (ca, "parent");
  else
    fig = get (0, "currentfigure");
    if (isempty (fig))
      fig = gcf ();
    endif
    ca = gca ();
  endif

  if (ishandle (ca) && isprop (ca, "__plotyy_axes__"))
    plty = get (ca, "__plotyy_axes__");
    if (isscalar (plty) && ishandle (plty))
      ca = [ca, plty];
    elseif (iscell (plty))
      ca = [ca, plty{:}];
    elseif (all (ishandle (plty)))
      ca = [ca, plty(:).'];
    else
      error ("legend.m: This should not happen. File a bug report.")
    endif
    ## Remove duplicates while preserving order
    [~, n] = unique (ca);
    ca = ca (sort (n));
  endif

  if (nargin > 0 && all (ishandle (varargin{1})))
    kids = flipud (varargin{1}(:));
    varargin(1) = [];
  else
    kids = ca;
    kids (strcmp (get (ca, "tag"), "legend")) = [];
    if (isscalar (kids))
      kids = get(kids, "children")(:);
    else
      kids = [get(kids, "children"){:}](:);
    endif
  endif
  nargs = numel (varargin);
  nkids = numel (kids);

  orientation = "default";
  position = "default";
  show = "create";
  textpos = "default";
  box = "default";

  if (nargs > 0)
    pos = varargin{nargs};
    if (isnumeric (pos) && isscalar (pos) && pos == fix (pos))
      if (pos >= -1 && pos <= 4)
        position = [{"northeastoutside", "best", "northeast",
                     "northwest", "southwest", "southeast"}] {pos + 2};
        nargs--;
      else
        error ("legend: invalid position specified");
      endif
    endif
  endif

  while (nargs > 1)
    pos = varargin{nargs-1};
    str = varargin{nargs};
    if (strcmpi (pos, "location")  && ischar (str))
      position = lower (str);
      nargs -= 2;
    elseif (strcmpi (pos, "orientation")  && ischar (str))
      orientation = lower (str);
      nargs -= 2;
    else
      break;
    endif
  endwhile

  ## Validate the orientation
  switch (orientation)
    case {"vertical", "horizontal","default"}
    otherwise
      error ("legend: unrecognized legend orientation");
  endswitch

  ## Validate the position type is valid
  outside = false;
  inout = findstr (position, "outside");
  if (! isempty (inout))
    outside = true;
    position = position(1:inout-1);
  else
    outside = false;
  endif

  switch (position)
    case {"north", "south", "east", "west", "northeast", "northwest", ...
          "southeast", "southwest", "default"}
    case "best"
      warning ("legend: 'Best' not yet implemented for location specifier\n");
      position = "northeast";
    otherwise
      error ("legend: unrecognized legend position");
  endswitch

  hlegend = [];
  fkids = get (fig, "children");
  for i = 1 : numel(fkids)
    if (ishandle (fkids (i)) && strcmp (get (fkids (i), "type"), "axes")
        && (strcmp (get (fkids (i), "tag"), "legend")))
      udata = get (fkids (i), "userdata");
      if (! isempty (intersect (udata.handle, ca)))
        hlegend = fkids (i);
        break;
      endif
    endif
  endfor

  if (nargs == 1)
    arg = varargin{1};
    if (ischar (arg))
      if (rows (arg) == 1)
        str = tolower (deblank (arg));
        switch (str)
          case {"off"}
            delete (hlegend);
            return
          case {"hide"}
            show = "off";
            nargs--;
          case "show"
            show = "on";
            nargs--;
          case "toggle"
            if (isempty (hlegend) || strcmp (get (hlegend, "visible"), "off"))
              show = "on";
            else
              show = "off";
            endif
            nargs--;
          case "boxon"
            box = "on";
            nargs--;
          case "boxoff"
            box = "off";
            nargs--;
          case "left"
            textpos = "left";
            nargs--;
          case "right"
            textpos = "right";
            nargs--;
          otherwise
        endswitch
      else
        varargin = cellstr (arg);
        nargs = numel (varargin);
      endif
    elseif (iscellstr (arg))
      varargin = arg;
      nargs = numel (varargin);
    else
      error ("legend: expecting argument to be a character string");
    endif
  endif

  if (strcmp (show, "off"))
    if (! isempty (hlegend))
      set (get (hlegend, "children"), "visible", "off");
      hlegend = [];
    endif
    hobjects = [];
    hplots  = [];
    text_strings = {};
  elseif (strcmp (show, "on"))
    if (! isempty (hlegend))
      set (get (hlegend, "children"), "visible", "on");
    else
      hobjects = [];
      hplots  = [];
      text_strings = {};
    endif
  elseif (strcmp (box, "on"))
    if (! isempty (hlegend))
      set (hlegend, "visible", "on", "box", "on");
    endif
  elseif (strcmp (box, "off"))
    if (! isempty (hlegend))
      set (hlegend, "box", "off", "visible", "off");
    endif
  elseif (nargs == 0 && !(strcmp (position, "default") &&
                          strcmp (orientation, "default")))
    if (! isempty (hlegend))
      hax = getfield (get (hlegend, "userdata"), "handle");
      [hplots, text_strings] = __getlegenddata__ (hlegend);

      if  (strcmp (position, "default"))
        h = legend (hax, hplots, text_strings, "orientation", orientation);
      elseif (strcmp (orientation, "default"))
        if (outside)
          h = legend (hax, hplots, text_strings, "location",
                      strcat (position, "outside"));
        else
          h = legend (hax, hplots, text_strings, "location", position);
        endif
      else
        if (outside)
          h = legend (hax, hplots, text_strings, "location",
                      strcat (position, "outside"), "orientation", orientation);
        else
          h = legend (hax, hplots, text_strings, "location", position,
                      "orientation", orientation);
        endif
      endif
    endif
  else
    hobjects = [];
    hplots  = [];
    text_strings = {};

    if (nargs > 0)
      have_data = false;
      for k = 1:nkids
        typ = get (kids(k), "type");
        if (strcmp (typ, "line") || strcmp (typ, "surface")
            || strcmp (typ, "patch") || strcmp (typ, "hggroup"))
          have_data = true;
          break;
        endif
      endfor

      if (! have_data)
        warning ("legend: plot data is empty; setting key labels has no effect");
      endif
    endif

    if (strcmp (textpos, "default"))
      warned = false;
      k = nkids;
      for i = 1 : nargs
        arg = varargin{i};
        if (ischar (arg))
          typ = get (kids(k), "type");
          while (k > 0
                 && ! (strcmp (typ, "line") || strcmp (typ, "surface")
                       || strcmp (typ, "patch") || strcmp (typ, "hggroup")))
            typ = get (kids(--k), "type");
          endwhile
          if (k > 0)
            if (strcmp (get (kids(k), "type"), "hggroup"))
              hgkids = get (kids(k), "children");
              for j = 1 : length (hgkids)
                hgobj = get (hgkids (j));
                if (isfield (hgobj, "displayname"))
                  set (hgkids(j), "displayname", arg);
                  hplots = [hplots, hgkids(j)];
                  text_strings = {text_strings{:}, arg};
                  break;
                endif
              endfor
            else
              set (kids(k), "displayname", arg);
              hplots = [hplots, kids(k)];
              text_strings = {text_strings{:}, arg};
            endif

            if (--k == 0)
              break;
            endif
          elseif (! warned)
            break;
          endif
        else
          error ("legend: expecting argument to be a character string");
        endif
      endfor
      if (i < nargs && ! warned)
        warning ("legend: ignoring extra labels");
      endif
    else
      k = nkids;
      while (k > 0)
        typ = get (kids(k), "type");
        while (k > 1
               && ! (strcmp (typ, "line") || strcmp (typ, "surface")
                     || strcmp (typ, "patch") || strcmp (typ, "hggroup")))
          typ = get (kids(--k), "type");
        endwhile
        if (! (strcmp (typ, "line") || strcmp (typ, "surface")
               || strcmp (typ, "patch") || strcmp (typ, "hggroup")))
          break
        endif
        if (k > 0)
          if (strcmp (get (kids(k), "type"), "hggroup"))
            hgkids = get (kids(k), "children");
            for j = 1 : length (hgkids)
              hgobj = get (hgkids (j));
              if (isfield (hgobj, "displayname")
                  && ! isempty (hgobj.displayname))
                hplots = [hplots, hgkids(j)];
                text_strings = {text_strings{:}, hgobj.displayname};
                break;
              endif
            endfor
          else
            if (! isempty (get (kids (k), "displayname")))
              hplots = [hplots, kids(k)];
              text_strings = {text_strings{:}, get(kids (k), "displayname")};
            endif
          endif
          if (--k == 0)
            break;
          endif
        endif
      endwhile
    endif

    if (isempty (hplots))
      if (! isempty (hlegend))
        fkids = get (fig, "children");
        delete (fkids (fkids == hlegend));
        hlegend = [];
        hobjects = [];
        hplots  = [];
        text_strings = {};
      endif
    else
      ## Delete the old legend if it exists
      if (! isempty (hlegend))
        if (strcmp (textpos, "default"))
          textpos = get (hlegend, "textposition");
        endif
        if (strcmp (position, "default"))
          position = get (hlegend, "location");
          inout = findstr (position, "outside");
          if (! isempty (inout))
            outside = true;
            position = position(1:inout-1);
          else
            outside = false;
          endif
        endif
        if (strcmp (orientation, "default"))
          orientation = get (hlegend, "orientation");
        endif
        box = get (hlegend, "box");
        fkids = get (fig, "children");

        delete (hlegend);
        hlegend = [];
      else
        if (strcmp (textpos, "default"))
          textpos = "left";
        endif
        if (strcmp (position, "default"))
          position = "northeast";
        endif
        if (strcmp (orientation, "default"))
          orientation = "vertical";
        endif
        box = "off";
      endif

      ## Get axis size and fontsize in points.
      ## Rely on listener to handle coversion.
      units = get (ca(1), "units");
      fontunits = get (ca(1), "fontunits");
      unwind_protect
        set (ca(1), "units", "points");
        set (ca(1), "fontunits", "points");
        ca_pos = get (ca(1), "position");
        ca_outpos = get (ca(1), "outerposition");
        ca_fontsize = get (ca(1), "fontsize");
      unwind_protect_cleanup
        set (ca(1), "units", units);
        set (ca(1), "fontunits", fontunits);
      end_unwind_protect

      ## Padding between legend entries horizontally and vertically
      xpad = 2;
      ypad = 2;

      ## Length of line segments in the legend in points
      linelength = 15;

      ## Create the axis first
      ## FIXME hlegend should inherit properties from "ca"
      curaxes = get (fig, "currentaxes");
      unwind_protect
        ud = ancestor(hplots, "axes");
        if (!isscalar(ud))
          ud = unique ([ud{:}]);
        endif
        if (isempty (hlegend))
          addprops = true;
          hlegend = axes ("tag", "legend", "userdata", struct ("handle", ud),
                          "box", box,
                          "xtick", [], "ytick", [], "xticklabel", "",
                          "yticklabel", "", "zticklabel", "",
                          "xlim", [0, 1], "ylim", [0, 1], "visible", "off",
                          "activepositionproperty", "position");
        else
          addprops = false;
          axes (hlegend);
          delete (get (hlegend, "children"));
        endif

        ## Add text label to the axis first, checking their extents
        nentries = numel (hplots);
        texthandle = [];
        maxwidth = 0;
        maxheight = 0;
        for k = 1 : nentries
          if (strcmp (textpos, "right"))
            texthandle = [texthandle, text(0, 0, text_strings {k},
                                           "horizontalalignment", "left",
                                           "userdata", hplots(k))];
          else
            texthandle = [texthandle, text(0, 0, text_strings {k},
                                           "horizontalalignment", "right",
                                           "userdata", hplots(k))];
          endif
          units = get (texthandle (end), "units");
          unwind_protect
            set (texthandle (end), "units", "points");
            extents = get (texthandle (end), "extent");
            maxwidth = max (maxwidth, extents (3));
            maxheight = max (maxheight, extents (4));
          unwind_protect_cleanup
            set (texthandle (end), "units", units);
          end_unwind_protect
        endfor

        num1 = nentries;
        if (strcmp (orientation, "vertical"))
          height = nentries * (ypad + maxheight);
          if (outside)
            if (height > ca_pos (4))
              ## Avoid shrinking the height of the axis to zero if outside
              num1 = ca_pos(4) / (maxheight + ypad) / 2;
            endif
          else
            if (height > 0.9 * ca_pos (4))
              num1 = 0.9 * ca_pos(4) / (maxheight + ypad);
            endif
          endif
        else
          width = nentries * (ypad + maxwidth);
          if (outside)
            if (width > ca_pos (3))
              ## Avoid shrinking the width of the axis to zero if outside
              num1 = ca_pos(3) / (maxwidth + ypad) / 2;
            endif
          else
            if (width > 0.9 * ca_pos (3))
              num1 = 0.9 * ca_pos(3) / (maxwidth + ypad);
            endif
          endif
        endif
        num2 = ceil (nentries / num1);

        xstep = 3 * xpad + (maxwidth + linelength);
        if (strcmp (textpos, "right"))
          xoffset = xpad;
          txoffset = 2 * xpad + linelength;
        else
          xoffset = 2 * xpad + maxwidth;
          txoffset = xpad + maxwidth;
        endif
        ystep = (ypad + maxheight);
        yoffset = ystep / 2;

        ## Place the legend in the desired position
        if (strcmp (orientation, "vertical"))
          lpos = [0, 0, num2 * xstep, num1 * ystep];
        else
          lpos = [0, 0, num1 * xstep, num2 * ystep];
        endif
        switch (position)
          case "north"
            if (outside)
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_outpos(2) + ca_outpos(4) - lpos(4) - ypad, lpos(3), ...
                      lpos(4)];

              new_pos = [ca_pos(1), ca_pos(2), ca_pos(3), ca_pos(4) - lpos(4)];
            else
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - ypad, lpos(3), lpos(4)];
            endif
          case "south"
            if (outside)
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_outpos(2) + ypad, lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2) + lpos(4), ca_pos(3), ...
                         ca_pos(4) - lpos(4)];
            else
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_pos(2) + ypad, lpos(3), lpos(4)];
            endif
          case "east"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ca_pos(3) - lpos(3), ca_pos(4)];
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
            endif
          case "west"
            if (outside)
              lpos = [ca_outpos(1) + ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, ...
                      lpos(3), lpos(4)];
              new_pos = [ca_pos(1) + lpos(3), ca_pos(2), ...
                         ca_pos(3) - lpos(3), ca_pos(4)];
            else
              lpos = [ca_pos(1) +  ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
            endif
          case "northeast"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4), lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ca_pos(3) - lpos(3), ca_pos(4)];
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - ypad, lpos(3), lpos(4)];
            endif
          case "northwest"
            if (outside)
              lpos = [ca_outpos(1) + ypad , ca_pos(2) + ca_pos(4) - lpos(4), ...
                      lpos(3), lpos(4)];
              new_pos = [ca_pos(1) + lpos(3), ca_pos(2), ...
                         ca_pos(3) - lpos(3), ca_pos(4)];
            else
              lpos = [ca_pos(1) + ypad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - ypad, lpos(3), lpos(4)];
            endif
          case "southeast"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - ypad, ...
                      ca_pos(2), lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ...
                         ca_pos(3) - lpos(3), ca_pos(4)];
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + ypad, lpos(3), lpos(4)];
            endif
          case "southwest"
            if (outside)
              lpos = [ca_outpos(1) + ypad, ca_pos(2), lpos(3), lpos(4)];
              new_pos = [ca_pos(1) + lpos(3), ca_pos(2), ...
                         ca_pos(3) - lpos(3), ca_pos(4)];
            else
              lpos = [ca_pos(1) + ypad, ca_pos(2) + ypad, lpos(3), lpos(4)];
            endif
        endswitch

        units = get (hlegend, "units");
        unwind_protect
          set (hlegend, "units", "points");
          set (hlegend, "position", lpos);
        unwind_protect_cleanup
          set (hlegend, "units", units);
        end_unwind_protect

        ## Now write the line segments and place the text objects correctly
        xk = 0;
        yk = 0;
        for k = 1 : numel (hplots)
          hobjects = [hobjects, texthandle (k)];
          switch (get (hplots(k), "type"))
          case "line"
            color = get (hplots(k), "color");
            style = get (hplots(k), "linestyle");
            if (! strcmp (style, "none"))
              l1 = line ("xdata", ([xoffset, xoffset + linelength] + xk * xstep) / lpos(3),
                         "ydata", [1, 1] .* (lpos(4) - yoffset - yk * ystep) / lpos(4),
                         "color", color, "linestyle", style, "marker", "none",
                         "userdata", hplots (k));
              hobjects = [hobjects, l1];
            endif
            marker = get (hplots(k), "marker");
            if (! strcmp (marker, "none"))
              l1 = line ("xdata", (xoffset + 0.5 * linelength  + xk * xstep) / lpos(3),
                         "ydata", (lpos(4) - yoffset - yk * ystep) / lpos(4),
                         "color", color, "linestyle", "none", "marker", marker,
                         "markeredgecolor", get (hplots (k), "markeredgecolor"),
                         "markerfacecolor", get (hplots (k), "markerfacecolor"),
                         "markersize", get (hplots (k), "markersize"),
                         "userdata", hplots (k));
              hobjects = [hobjects, l1];
            endif

            addlistener(hplots(k), "color", {@updateline, hlegend, linelength});
            addlistener(hplots(k), "linestyle", {@updateline, hlegend, linelength});
            addlistener(hplots(k), "marker", {@updateline, hlegend, linelength});
            addlistener(hplots(k), "markeredgecolor", {@updateline, hlegend, linelength});
            addlistener(hplots(k), "markerfacecolor", {@updateline, hlegend, linelength});
            addlistener(hplots(k), "markersize", {@updateline, hlegend, linelength});
            addlistener(hplots(k), "displayname", {@updateline, hlegend, linelength});
          case "patch"
          case "surface"
          endswitch
          set (texthandle (k), "position", [(txoffset + xk * xstep) / lpos(3), ...
                                            (lpos(4) - yoffset - yk * ystep) / lpos(4)]);
          if (strcmp (orientation, "vertical"))
            yk++;
            if (yk > num1)
              yk = 0;
              xk++;
            endif
          else
            xk++;
            if (xk > num1)
              xk = 0;
              yk++;
            endif
          endif
        endfor

        ## Add an invisible text object to original axis
        ## that when it is destroyed will remove the legend
        t1 = text (0, 0, "", "parent", ca(1), "tag", "legend",
                   "handlevisibility", "off", "visible", "off",
                   "xliminclude", "off", "yliminclude", "off");
        set (t1, "deletefcn", {@deletelegend1, hlegend});

        ## Resize the axis the legend is attached to if the
        ## legend is "outside" the plot and create listener to
        ## resize axis to original size if the legend is deleted,
        ## hidden or shown
        if (outside)
          for i = 1 : numel (ca)
            units = get (ca(i), "units");
            unwind_protect
              set (ca(i), "units", "points");
              set (ca (i), "position", new_pos);
            unwind_protect_cleanup
              set (ca(i), "units", units);
            end_unwind_protect
          endfor

          set (hlegend, "deletefcn", {@deletelegend2, ca, ...
                                      ca_pos, ca_outpos, t1, hplots});
          addlistener (hlegend, "visible", {@hideshowlegend, ca, ...
                                            ca_pos, new_pos});
        else
          set (hlegend, "deletefcn", {@deletelegend2, ca, [], [], t1, hplots});
        endif

        if (addprops)
          addproperty ("edgecolor", hlegend, "color", [0, 0, 0]);
          addproperty ("textcolor", hlegend, "color", [0, 0, 0]);
          addproperty ("location", hlegend, "radio", "north|south|east|west|{northeast}|southeast|northwest|southwest|northoutside|southoutside|eastoutside|westoutside|northeastoutside|southeastoutside|northwestoutside|southwestoutside");
          addproperty ("orientation", hlegend, "radio",
                       "{vertical}|horizontal");
          addproperty ("string", hlegend, "any", text_strings);
          addproperty ("textposition", hlegend, "radio", "{left}|right");
        else
          set (hlegend, "string", text_strings);
        endif

        if (outside)
          set (hlegend, "location", strcat (position, "outside"),
               "orientation", orientation, "textposition", textpos);
        else
          set (hlegend, "location", position, "orientation", orientation,
               "textposition", textpos);
        endif
        if (addprops)
          addlistener (hlegend, "edgecolor", @updatelegendtext);
          addlistener (hlegend, "textcolor", @updatelegendtext);
          addlistener (hlegend, "interpreter", @updatelegendtext);
          addlistener (hlegend, "location", @updatelegend);
          addlistener (hlegend, "orientation", @updatelegend);
          addlistener (hlegend, "string", @updatelegend);
          addlistener (hlegend, "textposition", @updatelegend);
        endif
      unwind_protect_cleanup
        set (fig, "currentaxes", curaxes);
      end_unwind_protect
    endif
  endif

  if (nargout > 0)
    hlegend2 = hlegend;
    hobjects2 = hobjects;
    hplot2 = hplots;
    text_strings2 = text_strings;
  endif

endfunction

function updatelegend (h, d)
  persistent recursive = false;
  if (! recursive)
    recursive = true;
    unwind_protect
      hax = getfield (get (h, "userdata"), "handle");
      [hplots, text_strings] = __getlegenddata__ (h);
      h = legend (hax, hplots, get (h, "string"));
    unwind_protect_cleanup
      recursive = false;
    end_unwind_protect
  endif
endfunction

function updatelegendtext (h, d)
  hax = get (h, "userdata").handle;
  kids = get (h, "children");
  text_kids = findobj (kids, "-property", "interpreter", "type", "text");
  interpreter = get (h, "interpreter");
  textcolor = get (h, "textcolor");
  set (kids, "interpreter", interpreter, "color", textcolor);
  hobj = cell2mat (get (kids, "userdata"));
  set (hobj, "interpreter", interpreter);
endfunction

function hideshowlegend (h, d, ca, pos1, pos2)
  isvisible = strcmp (get (h, "visible"), "off");
  if (! isvisible)
    kids = get (h, "children");
    for i = 1 : numel (kids)
      if (! strcmp (get (kids(i), "visible"), "off"))
        isvisible = true;
        break;
      endif
    endfor
  endif

  for i = 1 : numel (ca)
    if (ishandle (ca(i)) && strcmp (get (ca(i), "type"), "axes")
        && (isempty (gcbf()) || strcmp (get (gcbf(), "beingdeleted"),"off"))
        && strcmp (get (ca(i), "beingdeleted"), "off"))
      units = get (ca(i), "units");
      unwind_protect
        set (ca(i), "units", "points");
        if (isvisible)
          set (ca(i), "position", pos2);
        else
          set (ca(i), "position", pos1);
        endif
      unwind_protect_cleanup
        set (ca(i), "units", units);
      end_unwind_protect
    endif
  endfor
endfunction

function deletelegend1 (h, d, ca)
  if (ishandle (ca) && strcmp (get (ca, "type"), "axes")
      && (isempty (gcbf()) || strcmp (get (gcbf(), "beingdeleted"),"off"))
      && strcmp (get (ca, "beingdeleted"), "off"))
    delete (ca);
  endif
endfunction

function deletelegend2 (h, d, ca, pos, outpos, t1, hplots)
  for i = 1 : numel (ca)
    if (ishandle (ca(i)) && strcmp (get (ca(i), "type"), "axes")
        && (isempty (gcbf()) || strcmp (get (gcbf(), "beingdeleted"),"off"))
        && strcmp (get (ca(i), "beingdeleted"), "off"))
      if (!isempty (pos) && !isempty(outpos))
        units = get (ca(i), "units");
        unwind_protect
          set (ca(i), "units", "points");
          set (ca(i), "position", pos, "deletefcn", "");
        unwind_protect_cleanup
          set (ca(i), "units", units);
        end_unwind_protect
      endif
    endif
  endfor
  set (t1, "deletefcn", "");
  delete (t1);
  for i = 1 : numel (hplots)
    if (ishandle (hplots(i)) && strcmp (get (hplots (i), "type"), "line"))
      dellistener (hplots (i), "color");
      dellistener (hplots (i), "linestyle");
      dellistener (hplots (i), "marker");
      dellistener (hplots (i), "markeredgecolor");
      dellistener (hplots (i), "markerfacecolor");
      dellistener (hplots (i), "markersize");
      dellistener (hplots (i), "displayname");
    endif
  endfor
endfunction

function updateline (h, d, hlegend, linelength)
  lm = [];
  ll = [];
  kids = get (hlegend, "children");
  for i = 1 : numel (kids)
    if (get (kids (i), "userdata") == h
        && strcmp (get (kids(i), "type"), "line"))
      if (strcmp (get (kids (i), "marker"), "none"))
        ll = kids (i);
      else
        lm = kids (i);
      endif
    endif
  endfor

  linestyle = get (h, "linestyle");
  marker = get (h, "marker");
  displayname = get (h, "displayname");

  if ((isempty (displayname)
       || (strcmp (marker, "none") && strcmp (linestyle, "none")))
       && (! isempty (lm) || isempty (ll)))
    ## An element was removed from the legend. Need to recall the
    ## legend function to recreate a new legend
    [hplots, text_strings] = __getlegenddata__ (hlegend);
    for i = 1 : numel (hplots)
      if (hplots (i) == h)
        hplots(i) = [];
        text_strings(i) = [];
        break;
      endif
    endfor
    legend (hplots, text_strings);
  elseif ((!isempty (displayname)
           && (! strcmp (marker, "none") || ! strcmp (linestyle, "none")))
          && isempty (lm) && isempty (ll))
    ## An element was added to the legend. Need to recall the
    ## legend function to recreate a new legend
    [hplots, text_strings] = __getlegenddata__ (hlegend);
    hplots = [hplots, h];
    text_strings = {text_strings{:}, displayname};
    legend (hplots, text_strings);
  else
    if (! isempty (ll))
      ypos1 = get (ll,"ydata");
      xpos1 = get (ll,"xdata");
      ypos2 = ypos1(1);
      xpos2 = sum(xpos1) / 2;
      delete (ll);
      if (! isempty (lm))
        delete (lm);
      endif
    else
      ypos2 = get (lm,"ydata");
      xpos2 = get (lm,"xdata");
      ypos1 = [ypos2, ypos2];
      xpos1 = xpos2 + [-0.5, 0.5] * linelength;
      delete (lm);
    endif
    if (! strcmp (linestyle, "none"))
      line ("xdata", xpos1, "ydata", ypos1, "color", get (h, "color"),
            "linestyle", get (h, "linestyle"), "marker", "none",
            "userdata", h, "parent", hlegend);
    endif
    if (! strcmp (marker, "none"))
      line ("xdata", xpos2, "ydata", ypos2, "color", get (h, "color"),
            "marker", marker, "markeredgecolor", get (h, "markeredgecolor"),
            "markerfacecolor", get (h, "markerfacecolor"),
            "markersize", get (h, "markersize"), "linestyle", "none",
            "userdata", h, "parent", hlegend);
    endif
  endif
endfunction

%!demo
%! clf
%! x = 0:1;
%! plot (x, x, ";I am Blue;", x, 2*x, ";I am Green;", x, 3*x, ";I am Red;")

%!demo
%! clf
%! x = 0:1;
%! plot (x, x, ";I am Blue;", x, 2*x, x, 3*x, ";I am Red;")
%! title ("Blue and Green keys, with Green mising")

%!demo
%! clf
%! plot(1:10, 1:10, 1:10, fliplr(1:10));
%! title("incline is blue and decline is green");
%! legend({"I am blue", "I am green"}, "location", "east");
%! legend({"I am blue", "I am green"}, "location", "east");
%! legend hide
%! legend show

%!demo
%! clf
%! plot(1:10, 1:10, 1:10, fliplr(1:10));
%! title("Legend is hidden")
%! legend({"I am blue", "I am green"}, "location", "east");
%! legend hide

%!demo
%! clf
%! plot(1:10, 1:10, 1:10, fliplr(1:10));
%! title("Legend with box on")
%! legend({"I am blue", "I am green"}, "location", "east");
%! legend boxon

%!demo
%! clf
%! plot(1:10, 1:10, 1:10, fliplr(1:10));
%! title("Legend with text to the right")
%! legend({"I am blue", "I am green"}, "location", "east");
%! legend right

%!demo
%! clf
%! plot(1:10, 1:10);
%! title("a very long label can sometimes cause problems");
%! legend({"hello world"}, "location", "northeastoutside");

%!demo
%! clf
%! plot(1:10, 1:10);
%! title("a very long label can sometimes cause problems");
%! legend("hello world", "location", "northeastoutside");

%!demo
%! clf
%! labels = {};
%! colororder = get (gca, "colororder");
%! for i = 1:5
%!   h = plot(1:100, i + rand(100,1)); hold on;
%!   set (h, "color", colororder(i,:))
%!   labels = {labels{:}, cstrcat("Signal ", num2str(i))};
%! endfor
%! hold off;
%! title("Signals with random offset and uniform noise")
%! xlabel("Sample Nr [k]"); ylabel("Amplitude [V]");
%! legend(labels, "location", "southoutside");
%! legend("boxon");

%!demo
%! clf
%! labels = {};
%! colororder = get (gca, "colororder");
%! for i = 1:5
%!   h = plot(1:100, i + rand(100,1)); hold on;
%!   set (h, "color", colororder(i,:))
%!   labels = {labels{:}, cstrcat("Signal ", num2str(i))};
%! endfor
%! hold off;
%! title("Signals with random offset and uniform noise")
%! xlabel("Sample Nr [k]"); ylabel("Amplitude [V]");
%! legend(labels{:}, "location", "southoutside")
%! legend("boxon")

%!demo
%! clf
%! x = linspace (0, 10);
%! plot (x, x);
%! hold ("on");
%! stem (x, x.^2, 'g')
%! legend ("linear");
%! hold ("off");

%!demo
%! clf
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! legend ("linear");

%!demo
%! clf
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! legend ("linear", "quadratic");

%!demo
%! clf
%! rand_2x3_data1 = [0.341447, 0.171220, 0.284370; 0.039773, 0.731725, 0.779382];
%! bar (rand_2x3_data1);
%! ylim ([0 1.0]);
%! legend ({"1st Bar", "2nd Bar", "3rd Bar"});

%!demo
%! clf
%! rand_2x3_data2 = [0.44804, 0.84368, 0.23012; 0.72311, 0.58335, 0.90531];
%! bar (rand_2x3_data2);
%! ylim ([0 1.2]);
%! legend ("1st Bar", "2nd Bar", "3rd Bar");
%! legend right

%!demo
%! clf
%! x = 0:0.1:7;
%! h = plot (x, sin(x), x, cos(x), x, sin(x.^2/10), x, cos(x.^2/10));
%! title ("Only the sin() objects have keylabels");
%! legend (h([1, 3]), {"sin(x)", "sin(x^2/10)"}, "location", "southwest");

%!demo
%! clf
%! x = 0:0.1:10;
%! plot (x, sin(x), ";sin(x);")
%! hold all
%! plot (x, cos(x), ";cos(x);")
%! hold off

%!demo
%! clf
%! x = 0:0.1:10;
%! plot (x, sin(x), ";sin(x);")
%! hold all
%! plot (x, cos(x), ";cos(x);")
%! hold off
%! legend ({"sin(x)", "cos(x)"}, "location", "northeastoutside")

%!demo
%! clf
%! x = 0:10;
%! plot (x, rand (11));
%! xlabel ("Indices")
%! ylabel ("Random Values")
%! title ("Legend ""off"" should delete the legend")
%! legend (cellstr (num2str ((1:10)')), "location", "northeastoutside")
%! legend off
%! axis ([0, 10, 0 1])

%!demo
%! clf
%! x = 1:5;
%! subplot (2, 2, 1)
%! plot (x, rand (numel (x)));
%! legend (cellstr (num2str (x')), "location", "northwestoutside")
%! legend boxon
%! subplot (2, 2, 2)
%! plot (x, rand (numel (x)));
%! legend (cellstr (num2str (x')), "location", "northeastoutside")
%! legend boxon
%! subplot (2, 2, 3);
%! plot (x, rand (numel (x)));
%! legend (cellstr (num2str (x')), "location", "southwestoutside")
%! legend boxon
%! subplot (2, 2, 4)
%! plot (x, rand (numel (x)));
%! legend (cellstr (num2str (x')), "location", "southeastoutside")
%! legend boxon

%!demo
%! clf
%! plot (rand (2))
%! title ("Warn of extra labels")
%! legend ("Hello", "World", "interpreter", "foobar")

%!demo
%! clf
%! plot (rand (2))
%! title ("Turn off TeX interpreter")
%! h = legend ("Hello_World", "foo^bar");
%! set (h, "interpreter", "none")

