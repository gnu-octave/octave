########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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
## @deftypefn  {} {} legend ()
## @deftypefnx {} {} legend (@var{str1}, @var{str2}, @dots{})
## @deftypefnx {} {} legend (@var{charmat})
## @deftypefnx {} {} legend (@{@var{cellstr}@})
## @deftypefnx {} {} legend (@dots{}, "location", @var{pos})
## @deftypefnx {} {} legend (@dots{}, "orientation", @var{orient})
## @deftypefnx {} {} legend (@var{hax}, @dots{})
## @deftypefnx {} {} legend (@var{hobjs}, @dots{})
## @deftypefnx {} {} legend (@var{hax}, @var{hobjs}, @dots{})
## @deftypefnx {} {} legend ("@var{option}")
## @deftypefnx {} {} legend (@dots{}, @{@var{cellstr}@}, @var{property}, @var{value}, @dots{})
## @deftypefnx {} {[@var{hleg}, @var{hleg_obj}, @var{hplot}, @var{labels}] =} legend (@dots{})
##
## Display a legend for the current axes using the specified strings as labels.
##
## Legend entries may be specified as individual character string arguments,
## a character array, or a cell array of character strings.  When label names
## might be confused with options to @code{legend}, the labels should be
## protected by specifying them as a cell array of strings.
##
## If the first argument @var{hax} is an axes handle, then add a legend to this
## axes, rather than the current axes returned by @code{gca}.
##
## Legend labels are associated with the axes' children; The first label is
## assigned to the first object that was plotted in the axes, the second label
## to the next object plotted, etc.  To label specific data objects, without
## labeling all objects, provide their graphic handles in the input
## @var{hobjs}.
##
## The optional parameter @var{pos} specifies the location of the legend as
## follows:
##
## @multitable @columnfractions 0.06 0.14 0.80
## @headitem @tab pos @tab location of the legend
## @item @tab north @tab center top
## @item @tab south @tab center bottom
## @item @tab east @tab right center
## @item @tab west @tab left center
## @item @tab northeast @tab right top (default)
## @item @tab northwest @tab left top
## @item @tab southeast @tab right bottom
## @item @tab southwest @tab left bottom
## @sp 1
## @item @tab outside @tab can be appended to any location string @*
## @item @tab         @tab which will place the legend outside the axes
## @end multitable
##
## The optional parameter @var{orient} determines if the legend elements are
## placed vertically or horizontally.  The allowed values are
## @qcode{"vertical"} (default) or @qcode{"horizontal"}.
##
## The following customizations are available using @var{option}:
##
## @table @asis
## @item @qcode{"show"}
##   Show legend on the plot
##
## @item @qcode{"hide"}
##   Hide legend on the plot
##
## @item @qcode{"toggle"}
##   Toggle between @qcode{"hide"} and @qcode{"show"}
##
## @item @qcode{"boxon"}
##   Show a box around legend (default)
##
## @item @qcode{"boxoff"}
##   Hide the box around legend
##
## @item @qcode{"right"}
##   Place label text to the right of the keys (default)
##
## @item @qcode{"left"}
##   Place label text to the left of the keys
##
## @item @qcode{"off"}
##   Delete the legend object
## @end table
##
## The @code{legend} function creates a graphics object which has various
## properties that can be manipulated with @code{get}/@code{set}.
## Alternatively, properties can be set directly when calling @code{legend} by
## including @var{property}/@var{value} pairs.  If using this calling form, the
## labels must be specified as a cell array of strings.  Graphics object
## properties are documented in detail at @ref{Graphics Object Properties}.
##
## The optional output values are
##
## @table @var
## @item hleg
##   The graphics handle of the legend object.
##
## @item hleg_obj
##   Graphics handles to the text, patch, and line objects which form the
##   legend.
##
## @item hplot
##   Graphics handles to the plot objects which were used in making the legend.
##
## @item labels
##   A cell array of strings of the labels in the legend.
## @end table
##
## Implementation Note: The legend label text is either provided in the call to
## @code{legend} or is taken from the @code{DisplayName} property of the
## graphics objects.  Only data objects, such as line, patch, and surface, have
## this property whereas axes, figures, etc.@: do not so they are never present
## in a legend.  If no labels or @code{DisplayName} properties are available,
## then the label text is simply @qcode{"data1"}, @qcode{"data2"}, @dots{},
## @nospell{@qcode{"dataN"}}.  No more than 20 data labels will be
## automatically generated.  To label more, call @code{legend} explicitly and
## provide all labels.
##
## The legend @code{FontSize} property is initially set to 90% of the axes
## @code{FontSize} to which it is attached.  Use @code{set} to override this
## if necessary.
##
## A legend is implemented as an additional axes object with the @code{tag}
## property set to @qcode{"legend"}.  Properties of the legend object may be
## manipulated directly by using @code{set}.
## @end deftypefn

function [hleg, hleg_obj, hplot, labels] = __gnuplot_legend__ (varargin)

  if (nargin > 0
      && (! ishghandle (varargin{1})
          || (strcmp (get (varargin{1}, "type"), "axes")
              && ! strcmp (get (varargin{1}, "tag"), "legend"))))
    [ca, varargin, nargin] = __plt_get_axis_arg__ ("legend", varargin{:});
    if (isempty (ca))
      ca = gca ();
    endif
    hfig = ancestor (ca, "figure");
  else
    hfig = get (0, "currentfigure");
    if (isempty (hfig))
      hfig = gcf ();
    endif
    ca = gca ();
  endif

  ## Special handling for plotyy which has two axes objects
  if (isprop (ca, "__plotyy_axes__"))
    plty = get (ca, "__plotyy_axes__");
    ca = [ca, plty.'];
    ## Remove duplicates while preserving order
    [~, n] = unique (ca, "first");
    ca = ca(sort (n));
  endif

  if (nargin > 0 && all (ishghandle (varargin{1})))
    ## List of plot objects to label given as first argument
    kids = flipud (varargin{1}(:));
    varargin(1) = [];
  else
    ## Find list of plot objects from axes "children"
    kids = ca;
    kids(strcmp (get (ca, "tag"), "legend")) = [];
    if (isscalar (kids))
      kids = get (kids, "children")(:);
    else
      kids = vertcat (flipud (get (kids, "children")){:});
    endif
  endif
  nargs = numel (varargin);
  nkids = numel (kids);

  ## Find any existing legend object associated with axes
  hlegend = [];
  for hax = ca
    try
      hlegend = get (hax, "__legend_handle__");
      if (! isempty (hlegend))
        break;
      endif
    end_try_catch
  endfor

  orientation = "default";
  location = "default";
  show = "create";
  textpos = "default";
  box = "default";
  delete_leg = false;
  find_leg_hdl = (nargs == 0);  # possibly overridden
  propvals = {};

  ## Find "location", "orientation", "textposition" property/value pairs
  foundpos = foundorient = foundtextpos = false;
  i = nargs - 1;
  while (i > 0)
    pos = varargin{i};
    str = varargin{i+1};
    if (strcmpi (pos, "location") && ischar (str))
      if (! foundpos)
        location = lower (str);
        foundpos = true;
      endif
      varargin(i:i+1) = [];
      nargs -= 2;
    elseif (strcmpi (pos, "orientation") && ischar (str))
      if (! foundorient)
        orientation = lower (str);
        foundorient = true;
      endif
      varargin(i:i+1) = [];
      nargs -= 2;
    elseif (strcmpi (pos, "textposition") && ischar (str))
      if (! foundtextpos)
        textpos = lower (str);
        foundtextpos = true;
      endif
      varargin(i:i+1) = [];
      nargs -= 2;
    endif
    i -= 2;
  endwhile

  ## Validate the orientation
  if (! any (strcmp (orientation, {"vertical", "horizontal", "default"})))
    error ("legend: unrecognized legend orientation");
  endif

  ## Validate the texposition
  if (! any (strcmp (textpos, {"left", "right", "default"})))
    error ("legend: unrecognized legend textposition");
  endif

  ## Validate the location type
  outside = false;
  inout = strfind (location, "outside");
  if (! isempty (inout))
    outside = true;
    location = location(1:inout-1);
  else
    outside = false;
  endif

  switch (location)
    case {"north", "south", "east", "west", "northeast", "northwest", ...
          "southeast", "southwest", "default"}
      ## These are all valid locations, do nothing.

    case "best"
      if (outside)
        if (strcmp (orientation, "horizontal"))
          location = "south";
        else
          location = "northeast";
        endif
      else
        warning ("legend: 'best' not yet implemented for location specifier, using 'northeast' instead\n");
        location = "northeast";
      endif

    case "none"
      ## FIXME: Should there be any more error checking on this?

    otherwise
      error ("legend: unrecognized legend location");
  endswitch

  ## Finish input processing based on number of inputs
  if (nargs == 0)
    ## No labels given, create a new legend or return existing one
    if (isempty (hlegend))
      show = "create";
      textpos = "right";
      find_leg_hdl = false;
    endif

  elseif (nargs == 1)
    ## Either OPTION value, single string label, or cellstr of labels.
    arg = varargin{1};
    if (ischar (arg))
      if (rows (arg) == 1)
        str = lower (strtrim (arg));
        switch (str)
          case "off"
            delete_leg = true;
          case "hide"
            show = "off";
            nargs -= 1;
          case "show"
            if (! isempty (hlegend))
              show = "on";
            else
              show = "create";
              textpos = "right";
            endif
            nargs -= 1;
          case "toggle"
            if (isempty (hlegend))
              show = "create";
              textpos = "right";
            elseif (strcmp (get (hlegend, "visible"), "off"))
              show = "on";
            else
              show = "off";
            endif
            nargs -= 1;
          case "boxon"
            box = "on";
            nargs -= 1;
          case "boxoff"
            box = "off";
            nargs -= 1;
          case "left"
            textpos = "left";
            nargs -= 1;
          case "right"
            textpos = "right";
            nargs -= 1;
        endswitch
      else
        ## Character matrix of labels
        varargin = cellstr (arg);
        nargs = numel (varargin);
      endif
    elseif (iscellstr (arg))
      ## Cell array of labels
      varargin = arg;
      nargs = numel (varargin);
    else
      error ("Octave:invalid-fun-call",
             "legend: single argument must be a string or cellstr");
    endif

  elseif (nargs > 1 && iscellstr (varargin{1}))
    ## Cell array of labels followed by property/value pairs
    propvals = varargin(2:end);
    if (rem (numel (propvals), 2) != 0)
      error ("legend: PROPERTY/VALUE arguments must occur in pairs");
    endif
    varargin = {varargin{1}{:}};
    nargs = numel (varargin);
  endif

  have_labels = (nargs > 0);
  hobjects = [];
  hplots = [];
  text_strings = {};

  if (delete_leg)
    delete (hlegend);
    hlegend = [];
  elseif (find_leg_hdl)
    ## Don't change anything about legend.
    ## hleg output will be assigned hlegend value at end of function.
  elseif (strcmp (show, "off"))
    if (! isempty (hlegend))
      set (hlegend, "visible", "off");
      hlegend = [];
    endif
  elseif (strcmp (show, "on"))
    if (! isempty (hlegend))
      set (hlegend, "visible", "on");
      ## NOTE: Matlab sets both "visible" and "box" to "on" for "show on"
      ## set (hlegend, "box", "on");
    endif
  elseif (strcmp (box, "on"))
    if (! isempty (hlegend))
      set (hlegend, "box", "on");
    endif
  elseif (strcmp (box, "off"))
    if (! isempty (hlegend))
      set (hlegend, "box", "off");
    endif
  elseif (! have_labels && ! isempty (hlegend)
          && ! (strcmp (location, "default")
                && strcmp (orientation, "default")))
    ## Changing location or orientation of existing legend
    if (strcmp (location, "default"))
      set (hlegend, "orientation", orientation);
    elseif (strcmp (orientation, "default"))
      if (outside)
        set (hlegend, "location", [location "outside"]);
      else
        set (hlegend, "location", location);
      endif
    else
      if (outside)
        set (hlegend, "location", [location "outside"],
                      "orientation", orientation);
      else
        set (hlegend, "location", location,
                      "orientation", orientation);
      endif
    endif
  else
    ## Create or modify legend object

    if (! isempty (hlegend))
      ## Disable callbacks while modifying an existing legend
      setappdata (hlegend, "nocallbacks", true);
    endif

    if (have_labels)
      ## Check for valid data that can be labeled.
      have_data = false;
      have_dname = false;
      for hkid = kids.'
        typ = get (hkid, "type");
        if (any (strcmp (typ, {"line", "patch", "surface", "hggroup"})))
          have_data = true;
          break;
        endif
      endfor

      if (! have_data)
        warning ("legend: plot data is empty; setting key labels has no effect");
      endif
    else
      ## No labels.  Search for DisplayName property.
      have_dname = false;
      n_dname = 0;
      for hkid = kids.'
        typ = get (hkid, "type");
        if (any (strcmp (typ, {"line", "patch", "surface", "hggroup"})))
          n_dname += 1;  # count of objects which could be labeled
          if (! isempty (get (hkid, "displayname")))
            have_dname = true;
            break;
          endif
        endif
      endfor
      have_data = n_dname > 0;
    endif

    if (have_labels || ! have_dname)
      k = nkids;
      if (! have_labels)
        ## No labels or DisplayName.  Create set of "dataX" labels.
        if (n_dname > 20)
          warning ("legend: labeling only first 20 data objects");
          n_dname = 20;
        endif
        nargs = n_dname;
        varargin = arrayfun (@(x) sprintf ("data%d", x), [1:nargs]',
                             "uniformoutput", false);
        have_labels = true;
      endif
      for i = 1 : nargs
        label = varargin{i};
        if (! ischar (label))
          error ("Octave:invalid-fun-call",
                 "legend: expecting label to be a string");
        endif
        ## Locate an object which can be labeled
        while (k > 0)
          typ = get (kids(k), "type");
          if (any (strcmp (typ, {"line","patch","surface","hggroup"})))
            break;
          endif
          k--;
        endwhile
        if (k > 0)
          set (kids(k), "displayname", label);
          hplots(end+1) = kids(k);
          text_strings(end+1) = label;
          k--;
        else
          if (have_data)
            warning ("legend: ignoring extra labels");
          endif
          break;  # k = 0, no further handles to process
        endif
      endfor

    else
      ## No labels specified but objects have DisplayName property set.
      k = nkids;
      while (k > 0)
        ## Locate object to label
        while (k > 0)
          typ = get (kids(k), "type");
          if (any (strcmp (typ, {"line","patch","surface","hggroup"})))
            break;
          endif
          k--;
        endwhile
        if (k > 0)
          dname = get (kids(k), "displayname");
          if (! isempty (dname))
            hplots(end+1) = kids(k);
            text_strings(end+1) = dname;
          endif
          k--;
        endif
      endwhile
    endif

    if (isempty (hplots))
      ## Nothing to label
      if (! isempty (hlegend))
        delete (hlegend);
        hlegend = [];
        hobjects = [];
        hplots = [];
        text_strings = {};
      endif
    else
      ## Preserve the old legend if it exists
      if (! isempty (hlegend))
        if (strcmp (textpos, "default"))
          textpos = get (hlegend, "textposition");
        endif
        if (strcmp (location, "default"))
          location = get (hlegend, "location");
          inout = strfind (location, "outside");
          if (! isempty (inout))
            outside = true;
            location = location(1:inout-1);
          else
            outside = false;
          endif
        endif
        if (strcmp (orientation, "default"))
          orientation = get (hlegend, "orientation");
        endif
        box = get (hlegend, "box");
      else
        if (strcmp (textpos, "default"))
          textpos = "right";
        endif
        if (strcmp (location, "default"))
          location = "northeast";
        endif
        if (strcmp (orientation, "default"))
          orientation = "vertical";
        endif
        box = "on";
      endif

      ## Use axis which is appropriate for legend location.
      ## This is only necessary for plotyy figures where there are two axes.
      if (numel (ca) == 1)
        cax = ca(1);
      elseif (strfind (location, "east"))
        cax = ca(2);
      else
        cax = ca(1);
      endif
      ## Get axis size and fontsize in points.
      ## Rely on listener to handle conversion.
      units = get (cax, "units");
      unwind_protect
        set (cax, "units", "points", "fontunits", "points");
        if (isempty (hlegend) || ! isprop (hlegend, "unmodified_axes_position"))
          unmodified_axes_position = get (cax, "position");
          unmodified_axes_outerposition = get (cax, "outerposition");
        else
          unmodified_axes_position = get (hlegend, "unmodified_axes_position");
          unmodified_axes_outerposition = get (hlegend, ...
                                               "unmodified_axes_outerposition");
        endif
        ca_pos = unmodified_axes_position;
        ca_outpos = unmodified_axes_outerposition;
        tightinset = get (ca(1), "tightinset");
        for i = 2 : numel (ca)
          tightinset = max (tightinset, get (ca(i), "tightinset"));
        endfor
      unwind_protect_cleanup
        set (cax, "units", units);
      end_unwind_protect

      ## Padding between legend entries horizontally and vertically
      ## measured in points.
      ## FIXME: 3*xpad must be integer or strange off-by-1 pixel issues
      ##        with lines in OpenGL.
      xpad = 2 + 1/3;
      ypad = 4;

      bpad = 8;  # padding of legend box from surrounding axes

      linelength = 15;

      ## Preamble code to restore figure and axes after legend creation
      origfig = get (0, "currentfigure");
      if (origfig != hfig)
        set (0, "currentfigure", hfig);
      else
        origfig = [];
      endif
      origaxes = get (hfig, "currentaxes");
      unwind_protect
        ud = ancestor (hplots, "axes");
        if (! isscalar (ud))
          ud = unique ([ud{:}]);
        endif
        hpar = get (ud(1), "parent");

        if (isempty (hlegend))
          ## Create a legend object (axes + new properties)
          addprops = true;
          hlegend = axes ("parent", hpar, "tag", "legend",
                          "box", box,
                          "xtick", [], "ytick", [],
                          "xlim", [0, 1], "ylim", [0, 1],
                          "positionconstraint", "innerposition");
          setappdata (hlegend, "__axes_handle__", ud);
          try
            addproperty ("__legend_handle__", ud(1), "handle", hlegend);
          catch
            set (ud(1), "__legend_handle__", hlegend);
          end_try_catch

          ## Inherit fontsize from current axis
          ## "fontunits" should be first because it affects interpretation
          ## of "fontsize" property.
          [fontunits, fontsz] = get (ca(1), {"fontunits", "fontsize"}){:};
          fontsz *= 0.90;  # Reduce legend fontsize to 90% of axes fontsize
          set (hlegend, {"fontunits", "fontsize"}, {fontunits, fontsz});
          set (hlegend, "fontunits", "points");  # legend always works in pts.
          ## Also inherit colormap from axes if it is different than figure
          cax_cmap = get (cax, "colormap");
          if (! isequal (cax_cmap, get (hpar, "colormap")))
            set (hlegend, "colormap", cax_cmap);
          endif
          old_hplots = [];
        else
          ## Re-use existing legend.
          addprops = false;
          axes (hlegend);
          delete (get (hlegend, "children"));
          ## Hack: get list of hplots for which addlistener has been called.
          old_hplots = get (hlegend, "deletefcn"){6};
        endif

        if (addprops)
          ## Only required for a newly created legend object
          ## FIXME: "autoupdate" is not implemented.
          addproperty ("autoupdate", hlegend, "radio", "{on}|off");
          addproperty ("edgecolor", hlegend, "color", [0.15, 0.15, 0.15]);
          addproperty ("textcolor", hlegend, "color", [0, 0, 0]);
          locations = {"north", "south", "east", "west", ...
                       "{northeast}", "southeast", "northwest", "southwest", ...
                       "northoutside", "southoutside", ...
                       "eastoutside", "westoutside", ...
                       "northeastoutside", "southeastoutside", ...
                       "northwestoutside", "southwestoutside", "best", ...
                       "bestoutside", "none"};
          addproperty ("location", hlegend, "radio", strjoin (locations, "|"));
          addproperty ("orientation", hlegend, "radio",
                       "{vertical}|horizontal");
          addproperty ("string", hlegend, "any", text_strings);
          addproperty ("interpreter", hlegend, "textinterpreter");
          addproperty ("textposition", hlegend, "radio", "left|{right}");
        endif

        ## Apply any PROPERTY/VALUE pairs given as arguments
        if (! isempty (propvals))
          set (hlegend, propvals{:});
        endif

        ## Special case of PROPERTY "edgecolor" (bug #56968)
        ec_idx = find (strcmpi (propvals, "edgecolor"), 1, "last");
        if (! isempty (ec_idx))
          ec_color = propvals{ec_idx + 1};
          set (hlegend, "xcolor", ec_color, "ycolor", ec_color);
        endif

        ## Text objects in key inherit visual properties from legend object
        legprops = { "fontunits", "fontangle", "fontname", "fontsize", ...
                     "fontweight", "interpreter", "textcolor" };

        txtprops = { "fontunits", [], "fontangle", [] "fontname", [], ...
                     "fontsize", [], "fontweight", [] "interpreter", [], ...
                     "color", [] };
        propvals = get (hlegend, legprops);
        txtprops(2:2:end) = propvals;

        ## Add text labels to the axes first and check their extents
        nentries = numel (hplots);
        texthandle = [];
        maxwidth = maxheight = 0;
        for k = 1 : nentries
          halign = ifelse (strcmp (textpos, "right"), "left", "right");
          texthandle(k) = text (0, 0, text_strings{k},
                                "units", "points",
                                "horizontalalignment", halign,
                                txtprops{:});
          setappdata (texthandle(k), "handle", hplots(k));
          extents = get (texthandle(k), "extent");
          maxwidth = max (maxwidth, extents(3));
          maxheight = max (maxheight, extents(4));
        endfor
        ## Restore units which were forced to points
        set (texthandle, "units", get (0, "DefaultTextUnits"));

        num1 = nentries;
        if (strcmp (orientation, "vertical"))
          height = nentries * (ypad + maxheight);
          if (outside)
            if (height > ca_pos(4))
              ## Avoid shrinking the height of the axis to zero if outside
              num1 = ca_pos(4) / (maxheight + ypad) / 2;
            endif
          else
            if (height > 0.9 * ca_pos(4))
              num1 = 0.9 * ca_pos(4) / (maxheight + ypad);
            endif
          endif
        else
          width = nentries * (ypad + maxwidth);
          if (outside)
            if (width > ca_pos(3))
              ## Avoid shrinking the width of the axis to zero if outside
              num1 = ca_pos(3) / (maxwidth + ypad) / 2;
            endif
          else
            if (width > 0.9 * ca_pos(3))
              num1 = 0.9 * ca_pos(3) / (maxwidth + ypad);
            endif
          endif
        endif
        num2 = ceil (nentries / num1);

        ## Layout is [xpad, linelength, xpad, maxwidth, xpad]
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

        ## Place the legend in the desired location
        if (strcmp (orientation, "vertical"))
          lpos = [0, 0, num2 * xstep, num1 * ystep];
        else
          lpos = [0, 0, num1 * xstep, num2 * ystep];
        endif

        gnuplot = strcmp (get (hfig, "__graphics_toolkit__"), "gnuplot");
        if (gnuplot)
          ## gnuplot places the key (legend) at edge of the figure window.
          ## OpenGL places the legend box at edge of the unmodified axes
          ## position.
          if (isempty (strfind (location, "east")))
            gnuplot_offset = unmodified_axes_outerposition(1) ...
                           + unmodified_axes_outerposition(3) ...
                           - unmodified_axes_position(1) ...
                           - unmodified_axes_position(3);
          else
            gnuplot_offset = unmodified_axes_position(1) ...
                           - unmodified_axes_outerposition(1);
          endif
          ## FIXME: The "fontsize" is added to match the behavior of OpenGL.
          ## This implies that a change in fontsize should trigger a listener
          ## to update the legend.  The "2" was determined using a long legend
          ## key in the absence of any subplots.
          gnuplot_offset -= 2 * get (hlegend, "fontsize");
        else
          gnuplot_offset = 0;
        endif

        ## For legend's outside the associated axes position,
        ## align their edge to the unmodified_axes_outerposition,
        ## and adjust the axes position accordingly.
        switch (location)
          case "north"
            if (outside)
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_outpos(2) + ca_outpos(4) - lpos(4) - bpad/2, ...
                      lpos(3), lpos(4)];

              new_pos = [ca_pos(1), ca_pos(2), ca_pos(3), ca_pos(4) - lpos(4)];
            else
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - bpad, ...
                      lpos(3), lpos(4)];
            endif
          case "south"
            if (outside)
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_outpos(2) + bpad/2, lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ...
                         lpos(2) + lpos(4) + bpad/2 + tightinset(2), ...
                         ca_pos(3), ca_pos(4) - lpos(4)];
            else
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_pos(2) + bpad, lpos(3), lpos(4)];
            endif
          case "east"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - bpad/2, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, ...
                      lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ...
                         lpos(1) - bpad - tightinset(3) - ca_pos(1), ...
                         ca_pos(4)];
              new_pos(3) += gnuplot_offset;
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - bpad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
            endif
          case "west"
            if (outside)
              lpos = [ca_outpos(1) + bpad/2, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, ...
                      lpos(3), lpos(4)];
              new_pos = [lpos(1) + lpos(3) + bpad/2 + tightinset(1), ...
                         ca_pos(2), ca_pos(3) - lpos(3) - bpad/2, ca_pos(4)];
              new_pos([1, 3]) += [-gnuplot_offset, gnuplot_offset];
            else
              lpos = [ca_pos(1) + bpad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
            endif
          case "northeast"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - bpad/2, ...
                      ca_pos(2) + ca_pos(4) - lpos(4), ...
                      lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ...
                         lpos(1) - bpad - tightinset(3) - ca_pos(1), ...
                         ca_pos(4)];
              new_pos(3) += gnuplot_offset;
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - bpad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - bpad, ...
                      lpos(3), lpos(4)];
            endif
          case "northwest"
            if (outside)
              lpos = [ca_outpos(1) + bpad/2, ...
                      ca_pos(2) + ca_pos(4) - lpos(4), ...
                      lpos(3), lpos(4)];
              new_pos = [lpos(1) + lpos(3) + bpad/2 + tightinset(1), ...
                         ca_pos(2), ca_pos(3) - lpos(3) - bpad/2, ca_pos(4)];
              new_pos([1, 3]) += [-gnuplot_offset, gnuplot_offset];
            else
              lpos = [ca_pos(1) + bpad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - bpad, ...
                      lpos(3), lpos(4)];
            endif
          case "southeast"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - bpad/2, ...
                      ca_pos(2), lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ...
                         lpos(1) - bpad - ca_pos(1) - tightinset(3), ...
                         ca_pos(4)];
              new_pos(3) += gnuplot_offset;
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - bpad, ...
                      ca_pos(2) + bpad, lpos(3), lpos(4)];
            endif
          case "southwest"
            if (outside)
              lpos = [ca_outpos(1) + bpad/2, ca_pos(2), lpos(3), lpos(4)];
              new_pos = [lpos(1) + lpos(3) + bpad/2 + tightinset(1), ...
                         ca_pos(2), ca_pos(3) - lpos(3) - bpad/2, ca_pos(4)];
              new_pos([1, 3]) += [-gnuplot_offset, gnuplot_offset];
            else
              lpos = [ca_pos(1) + bpad, ca_pos(2) + bpad, lpos(3), lpos(4)];
            endif
        endswitch

        units = get (hlegend, "units");
        unwind_protect
          set (hlegend, "units", "points", "position", lpos);
        unwind_protect_cleanup
          set (hlegend, "units", units);
        end_unwind_protect

        ## Now write the line segments and place the text objects correctly
        xk = yk = 0;
        for k = 1 : numel (hplots)
          hobjects(end+1) = texthandle(k);
          hplt = hplots(k);
          typ = get (hplt, "type");
          ## For an hggroup, find an underlying primitive object
          if (strcmp (typ, "hggroup"))
            for hgkid = get (hplt, "children").'
              hgkid_type = get (hgkid, "type");
              if (any (strcmp (hgkid_type, {"line","patch","surface"})))
                typ = hgkid_type;
                hplt = hgkid;
                break;
              endif
            endfor
          endif

          switch (typ)

            case "line"
              color = get (hplt, "color");
              style = get (hplt, "linestyle");
              lwidth = min (get (hplt, "linewidth"), 5);
              if (! strcmp (style, "none"))
                l1 = __go_line__ (hlegend, ...
                       "xdata", ([xoffset, xoffset + linelength] + xk * xstep) / lpos(3), ...
                       "ydata", [1, 1] .* (lpos(4) - yoffset - yk * ystep) / lpos(4), ...
                       "color", color, "linestyle", style, ...
                       "linewidth", lwidth, "marker", "none");
                setappdata (l1, "handle", hplt);
                hobjects(end+1) = l1;
              endif
              marker = get (hplt, "marker");
              if (! strcmp (marker, "none"))
                l1 = __go_line__ (hlegend, ...
                       "xdata", (xoffset + 0.5 * linelength  + xk * xstep) / lpos(3), ...
                       "ydata", (lpos(4) - yoffset - yk * ystep) / lpos(4), ...
                       "color", color, "linestyle", "none", ...
                       "linewidth", lwidth, "marker", marker, ...
                       "markeredgecolor", get (hplt, "markeredgecolor"), ...
                       "markerfacecolor", get (hplt, "markerfacecolor"), ...
                       "markersize", min (get (hplt, "markersize"),10));
                setappdata (l1, "handle", hplt);
                hobjects(end+1) = l1;
              endif

              ## Newly labeled objects have listeners added
              if (! any (hplt == old_hplots))
                addlistener (hplt, "color",
                             {@cb_line_listener, hlegend, linelength, false});
                addlistener (hplt, "linestyle",
                             {@cb_line_listener, hlegend, linelength, false});
                addlistener (hplt, "linewidth",
                             {@cb_line_listener, hlegend, linelength, false});
                addlistener (hplt, "marker",
                             {@cb_line_listener, hlegend, linelength, false});
                addlistener (hplt, "markeredgecolor",
                             {@cb_line_listener, hlegend, linelength, false});
                addlistener (hplt, "markerfacecolor",
                             {@cb_line_listener, hlegend, linelength, false});
                addlistener (hplt, "markersize",
                             {@cb_line_listener, hlegend, linelength, false});
                addlistener (hplt, "displayname",
                             {@cb_line_listener, hlegend, linelength, true});
              endif

            case "patch"
              facecolor = get (hplt, "facecolor");
              edgecolor = get (hplt, "edgecolor");
              cdata = get (hplt, "cdata");
              if (! strcmp (facecolor, "none") || ! strcmp (edgecolor, "none"))
                p1 = patch ("xdata", ([0, linelength, linelength, 0] +
                                      xoffset + xk * xstep) / lpos(3),
                            "ydata", (lpos(4) - yoffset -
                                      [yk-0.3, yk-0.3, yk+0.3, yk+0.3] .* ystep) / lpos(4),
                            "facecolor", facecolor, "edgecolor", edgecolor,
                            "cdata", cdata);
                setappdata (p1, "handle", hplt);
              else
                ## non-standard patch only making use of marker styles
                ## such as scatter plot.
                p1 = patch ("xdata", (xoffset + 0.5 * linelength  + xk * xstep) / lpos(3),
                            "ydata", (lpos(4) - yoffset - yk * ystep) / lpos(4),
                            "marker", get (hplt, "marker"),
                            "markeredgecolor",get (hplt,"markeredgecolor"),
                            "markerfacecolor",get (hplt,"markerfacecolor"),
                            "markersize", min (get (hplt,"markersize"),10),
                            "cdata", cdata);
                setappdata (p1, "handle", hplt);
              endif
              hobjects(end+1) = p1;
              ## Copy clim from axes so that colors work out.
              set (hlegend, "clim", get (ca(1), "clim"));

              ## FIXME: Need listeners, as for line objects.
              ##        Changing clim, for example, won't update colors

            case "surface"
              facecolor = get (hplt, "facecolor");
              edgecolor = get (hplt, "edgecolor");
              cdata = sum (get (ca(1), "clim")) / 2;
              if (! strcmp (facecolor, "none") || ! strcmp (edgecolor, "none"))
                p1 = patch ("xdata", ([0, linelength, linelength, 0] +
                                      xoffset + xk * xstep) / lpos(3),
                            "ydata", (lpos(4) - yoffset -
                                      [yk-0.3, yk-0.3, yk+0.3, yk+0.3] .* ystep) / lpos(4),
                            "facecolor", facecolor, "edgecolor", edgecolor,
                            "cdata", cdata);
                setappdata (p1, "handle", hplt);
                hobjects(end+1) = p1;
              endif
              ## FIXME: Need listeners, as for line objects.

          endswitch

          set (texthandle(k), "position",
                              [(txoffset + xk * xstep) / lpos(3), ...
                               (lpos(4) - yoffset - yk * ystep) / lpos(4)]);
          if (strcmp (orientation, "vertical"))
            yk += 1;
            if (yk > num1)
              yk = 0;
              xk += 1;
            endif
          else
            xk += 1;
            if (xk > num1)
              xk = 0;
              yk += 1;
            endif
          endif
        endfor

        ## Add an invisible text object to original axis
        ## that, when it is destroyed, will remove the legend.
        htdel = findall (ca(1), "-depth", 1, "tag", "deletelegend",
                                "type", "text");
        if (isempty (htdel))
          htdel = text (0, 0, "", "parent", ca(1), "tag", "deletelegend",
                        "visible", "off", "handlevisibility", "off",
                        "xliminclude", "off", "yliminclude", "off",
                        "zliminclude", "off");
          set (htdel, "deletefcn", {@cb_axes_deleted, ca, hlegend});
        endif
        if (isprop (hlegend, "unmodified_axes_position"))
          set (hlegend, "unmodified_axes_position",
                         unmodified_axes_position,
                        "unmodified_axes_outerposition",
                         unmodified_axes_outerposition);
        else
          addproperty ("unmodified_axes_position", hlegend,
                       "data", unmodified_axes_position);
          addproperty ("unmodified_axes_outerposition", hlegend,
                       "data", unmodified_axes_outerposition);
        endif

        ## Resize the axis that the legend is attached to if the legend is
        ## "outside" the plot and create a listener to resize axis to original
        ## size if the legend is deleted, hidden, or shown.
        if (outside)
          for i = 1 : numel (ca)
            units = get (ca(i), "units");
            unwind_protect
              set (ca(i), "units", "points");
              if (gnuplot && numel (ca) == 1)
                ## Let gnuplot handle the positioning of the keybox.
                ## This violates strict Matlab compatibility, but reliably
                ## renders an aesthetic result.
                set (ca(i), "position",  unmodified_axes_position,
                            "positionconstraint", "outerposition");
              else
                ## numel (ca) > 1 for axes overlays (like plotyy)
                set (ca(i), "position", new_pos);
              endif
            unwind_protect_cleanup
              set (ca(i), "units", units);
            end_unwind_protect
          endfor

          set (hlegend, "deletefcn", {@cb_restore_axes, ca, ...
                                      unmodified_axes_position, ...
                                      unmodified_axes_outerposition, ...
                                      htdel, hplots});
          addlistener (hlegend, "visible", {@cb_legend_hideshow, ca, ...
                                            unmodified_axes_position, ...
                                            new_pos});
        else
          set (hlegend, "deletefcn", {@cb_restore_axes, ca, [], [], ...
                                      htdel, hplots});
        endif

        if (! addprops)
          ## Remove listeners on existing legend temporarily to stop recursion.
          dellistener (hlegend, "location");
          dellistener (hlegend, "orientation");
          dellistener (hlegend, "string");
          dellistener (hlegend, "textposition");
        endif

        if (! addprops)
          set (hlegend, "string", text_strings);
        endif

        if (outside)
          set (hlegend, "location", [location "outside"],
                        "orientation", orientation, "textposition", textpos);
        else
          set (hlegend, "location", location, "orientation", orientation,
                        "textposition", textpos);
        endif

        if (addprops)
          addlistener (cax, "colormap", {@cb_legend_colormap_update, hlegend});
          addlistener (hlegend, "edgecolor", @cb_legend_text_update);
          addlistener (hlegend, "fontangle", @cb_legend_text_update);
          addlistener (hlegend, "fontname", @cb_legend_text_update);
          addlistener (hlegend, "fontweight", @cb_legend_text_update);
          addlistener (hlegend, "textcolor", @cb_legend_text_update);
          ## Properties which could change size of box, such as fontsize,
          ## require legend to be redrawn.
          ## FIXME: fontsize is changed by print.m function during the
          ##        production of a plot for output.  This screws things up
          ##        because legend tries to return the axes size to what it
          ##        was when the figure was created, versus what it is now
          ##        when the figure is being printed.  Temporary hack is
          ##        good enough for generating the Octave manual which still
          ##        relies on gnuplot for generating images.  See bug #40333.
          if (! gnuplot)
            addlistener (hlegend, "fontsize", @cb_legend_update);
          endif
          addlistener (hlegend, "fontunits", @cb_legend_update);
          addlistener (hlegend, "interpreter", @cb_legend_update);
          addlistener (hlegend, "location", @cb_legend_location);
          addlistener (hlegend, "orientation", @cb_legend_update);
          addlistener (hlegend, "string", @cb_legend_update);
          addlistener (hlegend, "textposition", @cb_legend_update);
          ## FIXME: need to add listeners for tightinset and position
          ##        addlistener (ca, "tightinset", @update????);
          ##        addlistener (ca, "position", @update????);
        else
          ## Restore listeners temporarily disabled during reconstruction.
          addlistener (hlegend, "location", @cb_legend_update);
          addlistener (hlegend, "orientation", @cb_legend_update);
          addlistener (hlegend, "string", @cb_legend_update);
          addlistener (hlegend, "textposition", @cb_legend_update);
        endif

      unwind_protect_cleanup
        set (hfig, "currentaxes", origaxes);
        if (! isempty (origfig))
          set (0, "currentfigure", origfig);
        endif
      end_unwind_protect
    endif
  endif

  ## Restore operation of callbacks
  setappdata (hlegend, "nocallbacks", false);

  if (nargout > 0)
    hleg = hlegend;
    hleg_obj = hobjects;
    hplot = hplots;
    labels = text_strings;
  endif

endfunction

## Colormap of the base axes has changed.
function cb_legend_colormap_update (cax, ~, hlegend)
  set (hlegend, "colormap", get (cax, "colormap"));
endfunction

## A non-text property of legend has changed requiring an update.
function cb_legend_update (hleg, ~)
  persistent recursive = false;

  if (! recursive)
    recursive = true;
    unwind_protect
      hax = getappdata (hleg, "__axes_handle__");
      ## Hack.  Maybe store this somewhere else such as appdata.
      hplots = get (hleg, "deletefcn"){6};
      text_strings = get (hleg, "string");
      position = get (hleg, "unmodified_axes_position");
      outerposition = get (hleg, "unmodified_axes_outerposition");
      units = get (hax, "units");
      set (hax, "units", "points");
      switch (get (hax, "positionconstraint"))
        case "innerposition"
          set (hax, "outerposition", outerposition, "position", position);
        case "outerposition"
          set (hax, "position", position, "outerposition", outerposition);
      endswitch
      if (isscalar (hax))
        set (hax, "units", units);
      else
        set (hax, {"units"}, units);
      endif

      hleg = legend (hax(1), hplots, text_strings);
    unwind_protect_cleanup
      recursive = false;
    end_unwind_protect
  endif

endfunction

## A text property of legend, such as fontname, has changed.
function cb_legend_text_update (hleg, ~)

  kids = get (hleg, "children");
  htext = kids(strcmp (get (kids, "type"), "text"));

  tprops = {"fontangle", "fontname", "fontweight", "color"};
  lprops = {"fontangle", "fontname", "fontweight", "textcolor"};
  set (htext, tprops, get (hleg, lprops));

  ec = get (hleg, "edgecolor");
  set (hleg, "xcolor", ec, "ycolor", ec);

endfunction

## The legend "visible" property has changed.
function cb_legend_hideshow (hleg, ~, ca, orig_pos, new_pos)

  isvisible = strcmp (get (hleg, "visible"), "on");

  ## FIXME: Can't use a single set() call because of linked axes and
  ##        listeners on plotyy graphs.
  ca = ca(isaxes (ca));
  for cax = ca(:).'
    units = get (cax, "units");
    unwind_protect
      set (cax, "units", "points");
      if (isvisible)
        set (cax, "position", new_pos);
      else
        set (cax, "position", orig_pos);
      endif
    unwind_protect_cleanup
      set (cax, "units", units);
    end_unwind_protect
  endfor

endfunction

## The legend "location" property has changed.
function cb_legend_location (hleg, ~)

  ## If it isn't "none", which means manual positioning, then rebuild.
  if (! strcmp (get (hleg, "location"), "none"))
    cb_legend_update (hleg, []);
  endif

endfunction

## Axes to which legend was attached is being deleted/reset.  Delete legend.
function cb_axes_deleted (~, ~, ca, hlegend)

  if (isaxes (hlegend))
    if (strcmp (get (ca(1), "beingdeleted"), "on"))
      ## Axes are being deleted.  Disable call to cb_restore_axes.
      set (hlegend, "deletefcn", []);
    endif
    delete (hlegend);
  endif

endfunction

## Restore position of axes object when legend is deleted.
function cb_restore_axes (~, ~, ca, pos, outpos, htdel, hplots)

  hf = ancestor (ca(1), "figure");
  if (strcmp (get (hf, "beingdeleted"), "on")
      || strcmp (get (ca(1), "beingdeleted"), "on"))
    ## Skip restoring axes if entire figure or axes is being destroyed.
    return;
  endif

  ## Remove text object used to trigger legend delete when axes is deleted
  if (ishghandle (htdel))
    set (htdel, "deletefcn", []);
    delete (htdel);
  endif

  ## Restore original axes positions
  if (! isempty (pos))
    ## FIXME: can't use single call to set() because of weirdness w/plotyy
    for cax = ca(:).'
      if (isaxes (cax))
        units = get (cax, "units");
        unwind_protect
          set (cax, "units", "points", "position", pos);
        unwind_protect_cleanup
          set (cax, "units", units);
        end_unwind_protect
      endif
    endfor
  endif

  ## Remove listeners from plot objects
  for i = 1 : numel (hplots)
    if (isgraphics (hplots(i), "line"))
      dellistener (hplots(i), "color");
      dellistener (hplots(i), "linestyle");
      dellistener (hplots(i), "linewidth");
      dellistener (hplots(i), "marker");
      dellistener (hplots(i), "markeredgecolor");
      dellistener (hplots(i), "markerfacecolor");
      dellistener (hplots(i), "markersize");
      dellistener (hplots(i), "displayname");
    endif
  endfor

  ## Nullify legend link (can't delete properties yet)
  set (ca(1), "__legend_handle__", []);

endfunction

## Update legend item because underlying plot line object has changed.
function cb_line_listener (h, ~, hlegend, linelength, update_name)

  ## Don't execute callbacks when legend is under construction
  legdata = getappdata (hlegend);
  if (legdata.nocallbacks)
    return;
  endif

  if (update_name)
    ## When string changes, have to rebuild legend completely
    [hplots, text_strings] = __getlegenddata__ (hlegend);
    if (isempty (hplots))
      delete (hlegend);
    else
      legend (legdata.handle(1), hplots, text_strings);
    endif
  else
    kids = get (hlegend, "children");
    kids = kids([getappdata(kids, "handle"){:}] == h);
    kids = kids(strcmp (get (kids, "type"), "line"));
    idx = strcmp (get (kids, "marker"), "none");
    ll = kids (idx);
    lm = kids (! idx);

    [linestyle, marker, displayname] = ...
      get (h, {"linestyle", "marker", "displayname"}){:};

    if (! isempty (ll))
      [xpos1, ypos1] = get (ll, {"xdata", "ydata"}){:};
      xpos2 = sum (xpos1) / 2;
      ypos2 = ypos1(1);
      delete (ll);
      if (! isempty (lm))
        delete (lm);
      endif
    else
      [xpos2, ypos2] = get (lm, {"xdata", "ydata"}){:};
      xpos1 = xpos2 + [-0.5, 0.5] * linelength;
      ypos1 = [ypos2, ypos2];
      delete (lm);
    endif

    if (! strcmp (linestyle, "none"))
      hl = __go_line__ (hlegend, "xdata", xpos1, "ydata", ypos1,
                        "color", get (h, "color"),
                        "linestyle", get (h, "linestyle"),
                        "linewidth", min (get (h, "linewidth"), 5),
                        "marker", "none");
      setappdata (hl, "handle", h);
    endif
    if (! strcmp (marker, "none"))
      hl = __go_line__ (hlegend, "xdata", xpos2, "ydata", ypos2, ...
                        "color", get (h, "color"), ...
                        "marker", marker, ...
                        "markeredgecolor", get (h, "markeredgecolor"), ...
                        "markerfacecolor", get (h, "markerfacecolor"), ...
                        "markersize", min (get (h, "markersize"), 10), ...
                        "linestyle", "none", ...
                        "linewidth", min (get (h, "linewidth"), 5));
      setappdata (hl, "handle", h);
    endif
  endif

endfunction


%!demo
%! clf;
%! plot (rand (2));
%! title ("legend called with string inputs for labels");
%! h = legend ("foo", "bar");
%! legend (h, "location", "northeastoutside");
%! set (h, "fontsize", 20);

%!demo
%! clf;
%! plot (rand (2));
%! title ("legend called with cell array of strings");
%! h = legend ({"cellfoo", "cellbar"});
%! legend (h, "location", "northeast");
%! set (h, "fontsize", 20);

%!demo
%! clf;
%! plot (rand (3));
%! title ("legend () without inputs creates default labels");
%! h = legend ();

%!demo
%! clf;
%! x = 0:1;
%! plot (x,x,";I am Blue;", x,2*x, x,3*x,";I am yellow;");
%! h = legend ("location", "northeastoutside");
%! ## Placing legend inside returns axes to original size
%! legend (h, "location", "northeast");
%! title ("Blue and Yellow keys, with Orange missing");

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("incline is blue and decline is orange");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend hide
%! legend show

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend with keys in horizontal orientation");
%! legend ({"I am blue", "I am orange"}, ...
%!         "location", "east", "orientation", "horizontal");
%! legend boxoff
%! legend boxon

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend with box off");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend boxoff

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend with text to the left of key");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend left

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ({"Use properties to place legend text to the left of key", ...
%!         "Legend text color is magenta"});
%! h = legend ({"I am blue", "I am orange"}, "location", "east");
%! legend ("right");
%! set (h, "textposition", "left");
%! set (h, "textcolor", [1 0 1]);

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend is hidden");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend hide

%!demo
%! clf;
%! x = 0:1;
%! plot (x,x,";I am Blue;", x,2*x,";I am Orange;", x,3*x,";I am Yellow;");
%! title ({"Labels are embedded in call to plot", ...
%!         "Legend is hidden and then shown"});
%! legend boxon
%! legend hide
%! legend show

%!demo
%! clf;
%! x = 0:1;
%! plot (x,x,  x,2*x, x,3*x);
%! title ("Labels with interpreted Greek text");
%! h = legend ('\alpha', '\beta=2\alpha', '\gamma=3\alpha');
%! set (h, "interpreter", "tex");

%!demo
%! clf;
%! plot (rand (2));
%! title ("Labels with TeX interpreter turned off");
%! h = legend ("Hello_World", "foo^bar");
%! set (h, "interpreter", "none");

%!demo
%! clf;
%! labels = {};
%! colororder = get (gca, "colororder");
%! for i = 1:5
%!   h = plot (1:100, i + rand (100,1)); hold on;
%!   set (h, "color", colororder(i,:));
%!   labels = {labels{:}, ["Signal ", num2str(i)]};
%! endfor
%! hold off;
%! title ({"Signals with random offset and uniform noise";
%!         "Legend shown below and outside of plot"});
%! xlabel ("Sample Nr [k]"); ylabel ("Amplitude [V]");
%! legend (labels, "location", "southoutside");

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x);
%! hold on;
%! stem (x, x.^2, "g");
%! title ("First created object gets first label");
%! legend ("linear");
%! hold off;

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! title ("First created object gets first label");
%! legend ("linear");

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! title ("Labels are applied in order of object creation");
%! legend ("linear", "quadratic");

%!demo
%! clf;
%! subplot (2,1,1);
%! rand_2x3_data1 = [0.341447, 0.171220, 0.284370; 0.039773, 0.731725, 0.779382];
%! bar (rand_2x3_data1);
%! ylim ([0 1.0]);
%! title ("legend() works for bar graphs (hggroups)");
%! legend ({"1st Bar", "2nd Bar", "3rd Bar"}, "location", "northwest");
%! subplot (2,1,2);
%! x = linspace (0, 10, 20);
%! stem (x, 0.5+x.*rand (size (x))/max (x), "markeredgecolor", [0 0.7 0]);
%! hold on;
%! stem (x+10/(2*20), x.*(1.0+rand (size (x)))/max (x));
%! xlim ([0 10+10/(2*20)]);
%! title ("legend() works for stem plots (hggroups)");
%! legend ({"Multicolor", "Unicolor"}, "location", "northwest");

%!demo
%! clf;
%! colormap (cool (64));
%! surf (peaks ());
%! legend ("peaks()");
%! title ("legend() works for surface objects too");

%!demo
%! clf reset;  # needed to undo colormap assignment in previous demo
%! rand_2x3_data2 = [0.44804, 0.84368, 0.23012; 0.72311, 0.58335, 0.90531];
%! bar (rand_2x3_data2);
%! ylim ([0 1.2]);
%! title ('"left" option places colors to the left of text label');
%! legend ("1st Bar", "2nd Bar", "3rd Bar");
%! legend left;

%!demo
%! clf;
%! x = 0:0.1:7;
%! h = plot (x,sin (x), x,cos (x), x,sin (x.^2/10), x,cos (x.^2/10));
%! title ("Only the sin() objects have keylabels");
%! legend (h([1, 3]), {"sin (x)", "sin (x^2/10)"}, "location", "southwest");

%!demo
%! clf;
%! x = 0:0.1:10;
%! plot (x, sin (x), ";sin (x);");
%! hold on;
%! plot (x, cos (x), ";cos (x);");
%! hold off;
%! title ("legend constructed from multiple plot calls");

%!demo
%! clf;
%! x = 0:0.1:10;
%! plot (x, sin (x), ";sin (x);");
%! hold on;
%! plot (x, cos (x), ";cos (x);");
%! hold off;
%! title ("Specified label text overrides previous labels");
%! legend ({"Sine", "Cosine"}, "location", "northeastoutside");

%!demo
%! clf;
%! x = 0:10;
%! plot (x, rand (11));
%! axis ([0, 10, 0, 1]);
%! xlabel ("Indices");
%! ylabel ("Random Values");
%! title ('Legend "off" deletes the legend');
%! legend (cellstr (num2str ((0:10)')), "location", "northeastoutside");
%! pause (1);
%! legend off;

%!demo
%! clf;
%! x = (1:5)';
%! subplot (2,2,1);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "northwestoutside");
%! subplot (2,2,2);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "northeastoutside");
%! subplot (2,2,3);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "southwestoutside");
%! subplot (2,2,4);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "southeastoutside");
%! ## Legend works on a per axes basis for each subplot

%!demo
%! clf;
%! plot (rand (2));
%! title ("legend() will warn if extra labels are specified");
%! legend ("Hello", "World", "interpreter", "foobar");

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! title ({"plotyy legend test #1", "Blue label to left axis, Orange label to right axis"});
%! drawnow ();
%! legend ("Blue", "Orange", "location", "south");

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! ylabel (ax(1), {"Blue", "Y", "Axis"});
%! title ('plotyy legend test #2: "westoutside" adjusts to ylabel');
%! drawnow ();
%! legend ([h1, h2], {"Blue", "Orange"}, "location", "westoutside");

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! ylabel (ax(2), {"Orange", "Y", "Axis"});
%! title ('plotyy legend test #3: "eastoutside" adjusts to ylabel');
%! drawnow ();
%! legend ([h1, h2], {"Blue", "Orange"}, "location", "eastoutside");

%!demo
%! clf;
%! plot (1:10, 1:10);
%! title ("a very long label can sometimes cause problems");
%! legend ("hello very big world", "location", "northeastoutside");

%!demo  # bug 36408
%! clf;
%! option = "right";
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northeastoutside");
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  legend ({"1234567890"}, "location", "eastoutside");
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  legend ({"12345678901234567890"}, "location", "southeastoutside");
%!  legend (option);

%!demo  # bug 36408
%! clf;
%! option = "right";
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northwestoutside");
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  legend ({"1234567890"}, "location", "westoutside");
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  legend ({"12345678901234567890"}, "location", "southwestoutside");
%!  legend (option);

%!demo  # bug 36408
%! clf;
%! option = "right";
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northeastoutside");
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"1234567890"}, "location", "eastoutside");
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"12345678901234567890"}, "location", "southeastoutside");
%!  legend (option);

%!demo  # bug 36408
%! clf;
%! option = "right";
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northwestoutside");
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"1234567890"}, "location", "westoutside");
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"12345678901234567890"}, "location", "southwestoutside");
%!  legend (option);

%!demo  # bug 36408;
%! clf;
%! option = "right";
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), "xaxislocation", "top");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northwestoutside");
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), "xaxislocation", "top");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"1234567890"}, "location", "westoutside");
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), "xaxislocation", "top");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"12345678901234567890"}, "location", "southwestoutside");
%!  legend (option);

%!demo  # bug 39697
%! clf;
%! plot (1:10);
%! legend ("Legend Text");
%! title ({"Multi-line", "titles", "are a", "problem", "See bug #39697"});

%!testif ; any (strcmp ("gnuplot", available_graphics_toolkits ()))
%! toolkit = graphics_toolkit ("gnuplot");
%! h = figure ("visible", "off");
%! unwind_protect
%!   position = get (h, "position");
%!   plot (rand (3));
%!   legend ();
%!   filename = sprintf ("%s.eps", tempname ());
%!   print (filename);
%!   unlink (filename);
%!   assert (get (h, "position"), position);
%! unwind_protect_cleanup
%!   close (h);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

%!test <*42035>
%! h = figure ("visible", "off");
%! unwind_protect
%!   hax1 = subplot (1,2,1);
%!   plot (1:10);
%!   hax2 = subplot (1,2,2);
%!   plot (1:10);
%!   hleg1 = legend (hax1, "foo");
%!   assert (getappdata (hleg1, "__axes_handle__"), hax1);
%!   assert (gca (), hax2);
%!   hleg2 = legend ("bar");
%!   assert (getappdata (hleg2, "__axes_handle__"), gca ());
%! unwind_protect_cleanup
%!   close (h);
%! end_unwind_protect

%!test
%! ## Difficult example from plotyy demo #1
%! hf = figure ("visible", "off");
%! unwind_protect
%!   x = 0:0.1:2*pi;
%!   y1 = sin (x);
%!   y2 = exp (x - 1);
%!   hax = plotyy (x,y1, x-1,y2, @plot, @semilogy);
%!   text (0.5, 0.5, "Left Axis", "parent", hax(1));
%!   text (4.5, 80, "Right Axis", "parent", hax(2));
%!   hleg = legend ("show");
%!   assert (get (hleg, "string"), {"data1", "data2"});
%!   fail ("legend ('foo', 'bar', 'baz')", "warning", "ignoring extra labels");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test warnings about objects to label
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("legend ('foobar')", "warning", "plot data is empty");
%!   ht = text (0.5, 0.5, "Hello World");
%!   fail ("legend ('foobar')", "warning", "plot data is empty");
%!   lastwarn ("");   # clear warning
%!   hleg = legend ();
%!   assert (isempty (hleg) && isempty (lastwarn ()));
%!   fail ("legend ('foobar')", "warning", "plot data is empty");
%!   hln = line ([0 1], [0 1]);
%!   fail ("legend ('foo', 'bar')", "warning", "ignoring extra labels");
%!   plot (rand (2, 21));
%!   fail ("legend ()", "warning", "labeling only first 20 data objects");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test warnings about unsupported features
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   fail ("legend ('location','best')", "warning", "'best' not yet implemented");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
