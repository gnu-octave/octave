########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
## @deftypefn  {} {} yline (@var{y})
## @deftypefnx {} {} yline (@var{y}, @var{linespec})
## @deftypefnx {} {} yline (@var{y}, @var{linespec}, @var{labels})
## @deftypefnx {} {} yline (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} yline (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} yline (@dots{})
## Create horizontal lines at y-coordinates specified by @var{y}.
##
## @var{y} is a scalar or vector of y-coordinates where horizontal lines
## are to be drawn.
##
## The optional @var{linespec} argument specifies the line style and color
## using the same format as @code{plot}.  For example, @qcode{"r"} for a red
## solid line, @qcode{"--b"} for a blue dashed line, or @qcode{":k"} for a
## black dotted line.  If not specified, a solid black line is used.
##
## The optional @var{labels} argument specifies labels for the lines.  It can
## be a single string, a cell array of strings for a multiline label (when
## @var{y} is scalar), or a cell array with one element per line (when @var{y}
## is a vector).
##
## Additional property/value pairs are passed directly to the underlying line
## object.  The full list of line properties is documented at
## @ref{Line Properties}.  Commonly used properties include:
##
## @table @code
## @item Color
## Line color specified as an RGB triplet, a color name, or a short name.
##
## @item LineStyle
## Line style: @qcode{"-"} (solid, default), @qcode{"--"} (dashed),
## @qcode{":"} (dotted), or @qcode{"-."} (dash-dot).
##
## @item LineWidth
## Width of the line (default is 0.5).
##
## @item Alpha
## Line transparency (0 = fully transparent, 1 = fully opaque).
##
## @item Label
## Text label to display on the line.
##
## @item LabelHorizontalAlignment
## Horizontal alignment of label: @qcode{"left"}, @qcode{"center"}, or
## @qcode{"right"} (default).
##
## @item LabelVerticalAlignment
## Vertical alignment of label relative to the line: @qcode{"top"} (label
## above line, default), @qcode{"middle"}, or @qcode{"bottom"} (label below
## line).
##
## @item LabelOrientation
## Orientation of label text: @qcode{"aligned"} or @qcode{"horizontal"}
## (default).
## @end table
##
## If the first argument @var{hax} is an axes handle, then draw the lines
## in this axes, rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle (or vector of
## handles for multiple lines) to the created constant line objects.
##
## Example:
##
## @example
## @group
## plot (1:10, rand (1, 10));
## yline (0.5, "--r", "Threshold");
## @end group
## @end example
##
## @example
## @group
## plot (1:10, rand (1, 10) * 100);
## yline ([25, 50, 75], ":", @{"Q1", "Median", "Q3"@});
## @end group
## @end example
##
## @seealso{xline, line, plot}
## @end deftypefn

function h = yline (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("yline", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  ## Get y values
  yval = varargin{1};
  if (! isnumeric (yval) || ! isreal (yval))
    error ("yline: Y must be a real numeric scalar or vector");
  endif
  if (! isvector (yval) && ! isempty (yval))
    error ("yline: Y must be a scalar or vector, not a matrix");
  endif
  if (isempty (yval))
    ## Nothing to plot
    if (nargout > 0)
      h = zeros (0, 1);
    endif
    return;
  endif
  yval = yval(:).';  # ensure row vector

  ## Filter out NaN and Inf values
  valid_idx = isfinite (yval);
  if (! all (valid_idx))
    warning ("yline: ignoring non-finite values in Y");
    yval = yval(valid_idx);
    if (isempty (yval))
      if (nargout > 0)
        h = zeros (0, 1);
      endif
      return;
    endif
  endif

  ## Parse remaining arguments
  linespec = "";
  labels = {};
  propargs = {};

  idx = 2;
  if (nargin >= 2 && ischar (varargin{2}))
    ## Check if it's a linespec or a property name
    [lstyle, valid] = __pltopt__ ("yline", varargin{2}, false);
    if (valid)
      linespec = varargin{2};
      idx = 3;
    endif
  endif

  ## Check for labels
  if (nargin >= idx)
    arg = varargin{idx};
    if (ischar (arg) || iscellstr (arg))
      ## Could be labels or property name
      ## If next arg exists and is not a string, this is likely a property name
      if (nargin > idx && ! ischar (varargin{idx + 1}))
        ## This is a property name (like "Color" followed by a value)
        propargs = varargin(idx:end);
      elseif (ischar (arg) && nargin > idx && ischar (varargin{idx + 1}))
        ## Two consecutive strings - first could be labels, or could be prop/val
        ## Check if it looks like a known property
        if (__is_graphics_property__ (arg))
          propargs = varargin(idx:end);
        else
          ## Treat as label
          labels = {arg};
          idx += 1;
          propargs = varargin(idx:end);
        endif
      elseif (iscellstr (arg))
        labels = arg;
        idx += 1;
        propargs = varargin(idx:end);
      elseif (ischar (arg))
        if (__is_graphics_property__ (arg))
          propargs = varargin(idx:end);
        else
          labels = {arg};
          idx += 1;
          propargs = varargin(idx:end);
        endif
      endif
    else
      propargs = varargin(idx:end);
    endif
  endif

  ## Process property/value pairs to extract yline-specific properties
  ## Standard line properties will be passed to the line object
  labelhalign = "right";
  labelvalign = "top";
  labelorient = "horizontal";
  alpha = 1;

  i = 1;
  lineprops = {};
  while (i <= numel (propargs))
    prop = propargs{i};
    if (! ischar (prop))
      error ("yline: property name must be a string");
    endif

    if (i == numel (propargs))
      error ("yline: property '%s' requires a value", prop);
    endif
    val = propargs{i + 1};

    switch (lower (prop))
      case "label"
        if (ischar (val))
          labels = {val};
        elseif (iscellstr (val))
          labels = val;
        else
          error ("yline: Label must be a string or cell array of strings");
        endif
      case "labelhorizontalalignment"
        val = lower (val);
        if (strcmp (val, "middle"))
          val = "center";  # accept "middle" as alias for "center"
        endif
        if (! any (strcmp (val, {"left", "center", "right"})))
          error ("yline: LabelHorizontalAlignment must be 'left', 'center', or 'right'");
        endif
        labelhalign = val;
      case "labelverticalalignment"
        val = lower (val);
        if (strcmp (val, "center"))
          val = "middle";  # accept "center" as alias for "middle"
        endif
        if (! any (strcmp (val, {"top", "middle", "bottom"})))
          error ("yline: LabelVerticalAlignment must be 'top', 'middle', or 'bottom'");
        endif
        labelvalign = val;
      case "labelorientation"
        labelorient = val;
      case "alpha"
        alpha = val;
      otherwise
        lineprops{end+1} = prop;
        lineprops{end+1} = val;
    endswitch
    i += 2;
  endwhile

  ## Parse linespec for color and linestyle
  linecolor = [0, 0, 0];  # default black
  linestyle = "-";        # default solid

  if (! isempty (linespec))
    [lstyle, ~] = __pltopt__ ("yline", linespec, false);
    if (! isempty (lstyle.color))
      linecolor = lstyle.color;
    endif
    if (! isempty (lstyle.linestyle))
      linestyle = lstyle.linestyle;
    endif
  endif

  ## Override with explicit property values
  for i = 1:2:numel (lineprops)
    switch (lower (lineprops{i}))
      case "color"
        linecolor = lineprops{i + 1};
      case "linestyle"
        linestyle = lineprops{i + 1};
    endswitch
  endfor

  ## Validate labels
  ## For a single y-value, a cell array with multiple elements is a multiline label
  ## For multiple y-values, cell array elements map to each line
  if (! isempty (labels))
    if (numel (yval) == 1)
      ## Single y-value
      if (numel (labels) > 1)
        ## Multiple labels means multiline - wrap for consistent indexing
        labels = {labels};
      endif
      ## Single label - already in correct format {"Label"}, don't wrap again
    elseif (numel (labels) == 1)
      ## Single label for multiple y-values: replicate
      labels = repmat (labels, 1, numel (yval));
    elseif (numel (labels) != numel (yval))
      error ("yline: number of labels must match number of y-values");
    endif
  endif

  ## Get or create axes
  if (isempty (hax))
    hax = gca ();
  endif

  ## Get current x-axis limits
  xlim = get (hax, "xlim");

  ## Store current hold state and set hold on
  holdstate = ishold (hax);

  ## Create lines
  htmp = zeros (numel (yval), 1);

  unwind_protect
    hold (hax, "on");

    for i = 1:numel (yval)
      ## Create an hggroup to hold the line and optional label
      hg = hggroup ("parent", hax, ...
                    "__appdata__", struct ("__creator__", "yline"));
      htmp(i) = hg;

      ## Calculate label position and create text first (to get extent for centering)
      ht = [];
      text_extent = [];
      labelx = [];
      if (! isempty (labels))
        ## Calculate padding based on font size
        fontsize = get (hax, "fontsize");  # in points
        axpos = get (hax, "position");     # normalized units
        figpos = get (ancestor (hax, "figure"), "position");  # pixels

        ## Horizontal padding: ~1 character width
        axes_width_pixels = axpos(3) * figpos(3);
        xrange = diff (xlim);
        char_width_data = (0.6 * fontsize / axes_width_pixels) * xrange;

        ## Vertical padding: ~1/4 character height
        ylim_ax = get (hax, "ylim");
        yrange = diff (ylim_ax);
        axes_height_pixels = axpos(4) * figpos(4);
        char_height_data = (fontsize / axes_height_pixels) * yrange;
        ypadding = char_height_data / 4;

        ## Determine label y-position with vertical offset
        ## and map to text object's verticalalignment
        switch (lower (labelvalign))
          case "top"
            labely = yval(i) + ypadding;  # label above line
            text_valign = "bottom";       # text anchored at bottom
          case "middle"
            labely = yval(i);
            text_valign = "middle";
          case "bottom"
            labely = yval(i) - ypadding;  # label below line
            text_valign = "top";          # text anchored at top
          otherwise
            labely = yval(i) + ypadding;  # default: above line
            text_valign = "bottom";
        endswitch

        ## Determine label x-position with horizontal offset
        switch (lower (labelhalign))
          case "left"
            labelx = xlim(1) + char_width_data;
          case "center"
            labelx = mean (xlim);
          case "right"
            labelx = xlim(2) - char_width_data;
          otherwise
            labelx = xlim(2) - char_width_data;
        endswitch

        ht = text ("parent", hg, ...
                   "position", [labelx, labely, 0], ...
                   "string", labels{i}, ...
                   "horizontalalignment", labelhalign, ...
                   "verticalalignment", text_valign, ...
                   "color", linecolor, ...
                   "clipping", "off");

        if (strcmp (lower (labelorient), "aligned"))
          set (ht, "rotation", 0);  # horizontal line, so 0 rotation
        endif
      endif

      ## Create the horizontal line(s)
      ## Split line if label is vertically centered on the line
      if (! isempty (labels) && strcmp (lower (labelvalign), "middle"))
        ## Calculate gap from font metrics (not text_extent for consistency)
        str = labels{i};
        if (iscell (str))
          max_len = max (cellfun (@numel, str));
        else
          max_len = numel (str);
        endif

        ## Text width
        text_width = max_len * char_width_data;

        ## Gap is centered at label x-position (accounting for alignment)
        switch (lower (labelhalign))
          case "left"
            text_center_x = labelx + text_width / 2;
          case "center"
            text_center_x = labelx;
          case "right"
            text_center_x = labelx - text_width / 2;
          otherwise
            text_center_x = labelx;
        endswitch

        gap_left = text_center_x - text_width / 2 - char_width_data / 2;
        gap_right = text_center_x + text_width / 2 + char_width_data / 2;

        ## Left segment
        if (xlim(1) < gap_left)
          hl1 = __go_line__ (hg, "xdata", [xlim(1), gap_left], ...
                                 "ydata", [yval(i), yval(i)], ...
                                 "color", linecolor, "linestyle", linestyle);
          set (hl1, "xliminclude", "off");
          for j = 1:2:numel (lineprops)
            try
              set (hl1, lineprops{j}, lineprops{j + 1});
            catch
            end_try_catch
          endfor
        endif

        ## Right segment
        if (gap_right < xlim(2))
          hl2 = __go_line__ (hg, "xdata", [gap_right, xlim(2)], ...
                                 "ydata", [yval(i), yval(i)], ...
                                 "color", linecolor, "linestyle", linestyle);
          set (hl2, "xliminclude", "off");
          for j = 1:2:numel (lineprops)
            try
              set (hl2, lineprops{j}, lineprops{j + 1});
            catch
            end_try_catch
          endfor
        endif
      else
        ## Single continuous line
        hl = __go_line__ (hg, "xdata", xlim, "ydata", [yval(i), yval(i)], ...
                              "color", linecolor, "linestyle", linestyle);
        set (hl, "xliminclude", "off");

        for j = 1:2:numel (lineprops)
          try
            set (hl, lineprops{j}, lineprops{j + 1});
          catch
            ## Ignore properties that don't apply to line objects
          end_try_catch
        endfor
      endif

      ## Setup properties for the hggroup
      ## Get linewidth from first line child
      line_kids = findobj (hg, "type", "line");
      if (! isempty (line_kids))
        lw = get (line_kids(1), "linewidth");
      else
        lw = 0.5;  # default
      endif

      addproperty ("value", hg, "data", yval(i));
      addproperty ("color", hg, "linecolor", linecolor);
      addproperty ("linestyle", hg, "linelinestyle", linestyle);
      addproperty ("linewidth", hg, "linelinewidth", lw);
      addproperty ("alpha", hg, "data", alpha);

      if (! isempty (labels))
        addproperty ("label", hg, "any", labels{i});
      else
        addproperty ("label", hg, "string", "");
      endif

      addproperty ("labelhorizontalalignment", hg, "string", labelhalign);
      addproperty ("labelverticalalignment", hg, "string", labelvalign);
      addproperty ("labelorientation", hg, "string", labelorient);

      ## Add listeners for property updates
      addlistener (hg, "color", @update_line_props);
      addlistener (hg, "linestyle", @update_line_props);
      addlistener (hg, "linewidth", @update_line_props);
      addlistener (hg, "value", @update_line_data);

      ## Add listener for xlim changes to update line extent
      xlim_listener = {@update_xlim, hg};
      addlistener (hax, "xlim", xlim_listener);

      ## Add delete function to clean up listener
      set (hg, "deletefcn", {@cleanup_listeners, hax, xlim_listener});

    endfor

  unwind_protect_cleanup
    if (! holdstate)
      hold (hax, "off");
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


## Check if a string is a known graphics property
function retval = __is_graphics_property__ (str)
  known_props = {"color", "linestyle", "linewidth", "alpha", "label", ...
                 "labelhorizontalalignment", "labelverticalalignment", ...
                 "labelorientation", "parent", "visible", "handlevisibility", ...
                 "displayname", "tag", "userdata"};
  retval = any (strcmpi (str, known_props));
endfunction


## Callback to update line properties
function update_line_props (hg, ~)
  kids = get (hg, "children");
  for i = 1:numel (kids)
    if (strcmp (get (kids(i), "type"), "line"))
      set (kids(i), "color", get (hg, "color"), ...
                    "linestyle", get (hg, "linestyle"), ...
                    "linewidth", get (hg, "linewidth"));
    elseif (strcmp (get (kids(i), "type"), "text"))
      set (kids(i), "color", get (hg, "color"));
    endif
  endfor
endfunction


## Callback to update line y-coordinate
function update_line_data (hg, ~)
  yval = get (hg, "value");
  kids = get (hg, "children");
  for i = 1:numel (kids)
    if (strcmp (get (kids(i), "type"), "line"))
      set (kids(i), "ydata", [yval, yval]);
    elseif (strcmp (get (kids(i), "type"), "text"))
      pos = get (kids(i), "position");
      pos(2) = yval;
      set (kids(i), "position", pos);
    endif
  endfor
endfunction


## Callback to update line x-extent when axes limits change
function update_xlim (hax, ~, hg)
  if (! ishghandle (hg))
    return;
  endif
  xlim = get (hax, "xlim");
  kids = get (hg, "children");
  labelhalign = get (hg, "labelhorizontalalignment");

  ## Calculate horizontal padding based on font size
  fontsize = get (hax, "fontsize");
  axpos = get (hax, "position");
  figpos = get (ancestor (hax, "figure"), "position");
  axes_width_pixels = axpos(3) * figpos(3);
  xrange = diff (xlim);
  char_width_data = (0.6 * fontsize / axes_width_pixels) * xrange;

  ## Find text and line children
  text_kid = [];
  line_kids = [];
  for i = 1:numel (kids)
    if (strcmp (get (kids(i), "type"), "text"))
      text_kid = kids(i);
    elseif (strcmp (get (kids(i), "type"), "line"))
      line_kids(end+1) = kids(i);
    endif
  endfor

  ## Update text position
  if (! isempty (text_kid))
    pos = get (text_kid, "position");
    switch (lower (labelhalign))
      case "left"
        pos(1) = xlim(1) + char_width_data;
      case "center"
        pos(1) = mean (xlim);
      case "right"
        pos(1) = xlim(2) - char_width_data;
    endswitch
    set (text_kid, "position", pos);
  endif

  ## Update line(s)
  labelvalign = get (hg, "labelverticalalignment");
  if (strcmp (lower (labelvalign), "middle") && ! isempty (text_kid) ...
      && numel (line_kids) >= 2)
    ## Split line case - calculate gap around text from font metrics
    ## Estimate text width based on longest line
    str = get (text_kid, "string");
    if (iscell (str))
      max_len = max (cellfun (@numel, str));
    else
      max_len = numel (str);
    endif

    ## Text width
    text_width = max_len * char_width_data;

    ## Gap is centered at text position (accounting for alignment)
    text_pos = get (text_kid, "position");
    switch (lower (labelhalign))
      case "left"
        text_center_x = text_pos(1) + text_width / 2;
      case "center"
        text_center_x = text_pos(1);
      case "right"
        text_center_x = text_pos(1) - text_width / 2;
      otherwise
        text_center_x = text_pos(1);
    endswitch

    gap_left = text_center_x - text_width / 2 - char_width_data / 2;
    gap_right = text_center_x + text_width / 2 + char_width_data / 2;

    ## Identify which segment is left and which is right by current position
    for i = 1:numel (line_kids)
      xdata = get (line_kids(i), "xdata");
      xmid = mean (xdata);
      if (xmid < mean (xlim))
        ## This is the left segment
        set (line_kids(i), "xdata", [xlim(1), gap_left]);
      else
        ## This is the right segment
        set (line_kids(i), "xdata", [gap_right, xlim(2)]);
      endif
    endfor
  else
    ## Single continuous line or non-middle label
    for i = 1:numel (line_kids)
      set (line_kids(i), "xdata", xlim);
    endfor
  endif
endfunction


## Cleanup function to remove listeners when line is deleted
function cleanup_listeners (hg, ~, hax, xlim_listener)
  try
    dellistener (hax, "xlim", xlim_listener);
  catch
    ## Ignore errors if axes no longer exists
  end_try_catch
endfunction


%!demo
%! clf;
%! plot (1:10, rand (1, 10));
%! yline (0.5, "--r", "Threshold");
%! title ("yline() with label");

%!demo
%! clf;
%! plot (1:10, rand (1, 10) * 100);
%! yline ([25, 50, 75], ":", {"Q1", "Median", "Q3"});
%! title ("yline() with multiple lines and labels");

%!demo
%! clf;
%! x = linspace (0, 2*pi, 100);
%! plot (x, sin (x));
%! yline (0, "-k");
%! yline ([-1, 1], "--b", {"Min", "Max"}, "LabelHorizontalAlignment", "left");
%! axis padded;
%! title ("yline() showing sine wave bounds");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = yline (5);
%!   assert (ishghandle (h));
%!   assert (get (h, "value"), 5);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = yline ([2, 5, 8]);
%!   assert (numel (h), 3);
%!   assert (get (h(1), "value"), 2);
%!   assert (get (h(2), "value"), 5);
%!   assert (get (h(3), "value"), 8);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = yline (5, "--r");
%!   kids = get (h, "children");
%!   hline = kids(strcmp (get (kids, "type"), "line"));
%!   assert (get (hline, "linestyle"), "--");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test with label
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = yline (5, "-b", "Test Label");
%!   assert (get (h, "label"), "Test Label");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test empty y returns empty handle
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = yline ([]);
%!   assert (isempty (h));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test multiline label
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = yline (5, "-", {"Line 1", "Line 2"});
%!   assert (iscell (get (h, "label")));
%!   assert (numel (get (h, "label")), 2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <Y must be a real numeric> yline ("invalid")
%!error <Y must be a real numeric> yline (1+i)
%!error <Y must be a scalar or vector> yline ([1 2; 3 4])
