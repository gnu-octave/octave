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
## @deftypefn  {} {} xline (@var{x})
## @deftypefnx {} {} xline (@var{x}, @var{linespec})
## @deftypefnx {} {} xline (@var{x}, @var{linespec}, @var{labels})
## @deftypefnx {} {} xline (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} xline (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} xline (@dots{})
## Create vertical lines at x-coordinates specified by @var{x}.
##
## @var{x} is a scalar or vector of x-coordinates where vertical lines
## are to be drawn.
##
## The optional @var{linespec} argument specifies the line style and color
## using the same format as @code{plot}.  For example, @qcode{"r"} for a red
## solid line, @qcode{"--b"} for a blue dashed line, or @qcode{":k"} for a
## black dotted line.  If not specified, a solid black line is used.
##
## The optional @var{labels} argument specifies labels for the lines.  It can
## be a single string, a cell array of strings for a multiline label (when
## @var{x} is scalar), or a cell array with one element per line (when @var{x}
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
## Horizontal alignment of label relative to the line: @qcode{"left"} (label
## left of line), @qcode{"center"}, or @qcode{"right"} (label right of line,
## default).
##
## @item LabelVerticalAlignment
## Vertical alignment of label: @qcode{"top"} (default), @qcode{"middle"}, or
## @qcode{"bottom"}.
##
## @item LabelOrientation
## Orientation of label text: @qcode{"aligned"} (rotated 90@textdegree{}, default)
## or @qcode{"horizontal"}.
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
## plot (rand (1, 10) * 10, 1:10);
## xline (5, "--r", "Threshold");
## @end group
## @end example
##
## @example
## @group
## plot (1:100);
## xline ([25, 50, 75], ":", @{"Q1", "Median", "Q3"@});
## @end group
## @end example
##
## @seealso{yline, line, plot}
## @end deftypefn

function h = xline (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("xline", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  ## Get x values
  xval = varargin{1};
  if (! isnumeric (xval) || ! isreal (xval))
    error ("xline: X must be a real numeric scalar or vector");
  endif
  if (! isvector (xval) && ! isempty (xval))
    error ("xline: X must be a scalar or vector, not a matrix");
  endif
  if (isempty (xval))
    ## Nothing to plot
    if (nargout > 0)
      h = zeros (0, 1);
    endif
    return;
  endif
  xval = xval(:).';  # ensure row vector

  ## Filter out NaN and Inf values
  valid_idx = isfinite (xval);
  if (! all (valid_idx))
    warning ("xline: ignoring non-finite values in X");
    xval = xval(valid_idx);
    if (isempty (xval))
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
    [lstyle, valid] = __pltopt__ ("xline", varargin{2}, false);
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

  ## Process property/value pairs to extract xline-specific properties
  ## Standard line properties will be passed to the line object
  labelhalign = "right";
  labelvalign = "top";
  labelorient = "aligned";
  alpha = 1;

  i = 1;
  lineprops = {};
  while (i <= numel (propargs))
    prop = propargs{i};
    if (! ischar (prop))
      error ("xline: property name must be a string");
    endif

    if (i == numel (propargs))
      error ("xline: property '%s' requires a value", prop);
    endif
    val = propargs{i + 1};

    switch (lower (prop))
      case "label"
        if (ischar (val))
          labels = {val};
        elseif (iscellstr (val))
          labels = val;
        else
          error ("xline: Label must be a string or cell array of strings");
        endif
      case "labelhorizontalalignment"
        val = lower (val);
        if (strcmp (val, "middle"))
          val = "center";  # accept "middle" as alias for "center"
        endif
        if (! any (strcmp (val, {"left", "center", "right"})))
          error ("xline: LabelHorizontalAlignment must be 'left', 'center', or 'right'");
        endif
        labelhalign = val;
      case "labelverticalalignment"
        val = lower (val);
        if (strcmp (val, "center"))
          val = "middle";  # accept "center" as alias for "middle"
        endif
        if (! any (strcmp (val, {"top", "middle", "bottom"})))
          error ("xline: LabelVerticalAlignment must be 'top', 'middle', or 'bottom'");
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
    [lstyle, ~] = __pltopt__ ("xline", linespec, false);
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
  ## For a single x-value, a cell array with multiple elements is a multiline label
  ## For multiple x-values, cell array elements map to each line
  if (! isempty (labels))
    if (numel (xval) == 1)
      ## Single x-value
      if (numel (labels) > 1)
        ## Multiple labels means multiline - wrap for consistent indexing
        labels = {labels};
      endif
      ## Single label - already in correct format {"Label"}, don't wrap again
    elseif (numel (labels) == 1)
      ## Single label for multiple x-values: replicate
      labels = repmat (labels, 1, numel (xval));
    elseif (numel (labels) != numel (xval))
      error ("xline: number of labels must match number of x-values");
    endif
  endif

  ## Get or create axes
  if (isempty (hax))
    hax = gca ();
  endif

  ## Get current y-axis limits
  ylim = get (hax, "ylim");

  ## Store current hold state and set hold on
  holdstate = ishold (hax);

  ## Create lines
  htmp = zeros (numel (xval), 1);

  unwind_protect
    hold (hax, "on");

    for i = 1:numel (xval)
      ## Create an hggroup to hold the line and optional label
      hg = hggroup ("parent", hax, ...
                    "__appdata__", struct ("__creator__", "xline"));
      htmp(i) = hg;

      ## Calculate label position and create text first (to get extent for centering)
      ht = [];
      text_extent = [];
      if (! isempty (labels))
        ## Calculate padding based on font size
        fontsize = get (hax, "fontsize");  # in points
        axpos = get (hax, "position");     # normalized units
        figpos = get (ancestor (hax, "figure"), "position");  # pixels

        ## Horizontal padding: ~1 character width
        axes_width_pixels = axpos(3) * figpos(3);
        xlim = get (hax, "xlim");
        xrange = diff (xlim);
        char_width_data = (0.6 * fontsize / axes_width_pixels) * xrange;

        ## Vertical padding: ~1/3 character height
        yrange = diff (ylim);
        axes_height_pixels = axpos(4) * figpos(4);
        char_height_data = (fontsize / axes_height_pixels) * yrange;
        ypadding = char_height_data / 3;

        ## Determine label x-position with horizontal offset from line
        switch (lower (labelhalign))
          case "left"
            labelx = xval(i) - char_width_data;
            text_halign = "right";  # text right-aligned so it appears left of line
          case "center"
            labelx = xval(i);
            text_halign = "center";
          case "right"
            labelx = xval(i) + char_width_data;
            text_halign = "left";   # text left-aligned so it appears right of line
          otherwise
            labelx = xval(i) + char_width_data;
            text_halign = "left";
        endswitch

        ## Determine label y-position and alignment based on labelvalign
        ## For rotated text, horizontalalignment controls vertical extent
        switch (lower (labelvalign))
          case "top"
            labely = ylim(2) - ypadding;
          case "middle"
            labely = mean (ylim);
          case "bottom"
            labely = ylim(1) + ypadding;
          otherwise
            labely = ylim(2) - ypadding;
        endswitch

        ## Set text alignment based on rotation
        if (strcmp (lower (labelorient), "aligned"))
          ## For 90° rotated text, alignments swap roles:
          ## - horizontalalignment controls vertical extent
          ## - verticalalignment controls horizontal extent
          switch (lower (labelvalign))
            case "top"
              rot_text_halign = "right";  # anchor at top, text extends down
            case "middle"
              rot_text_halign = "center";
            case "bottom"
              rot_text_halign = "left";   # anchor at bottom, text extends up
            otherwise
              rot_text_halign = "right";
          endswitch

          switch (lower (labelhalign))
            case "left"
              rot_text_valign = "bottom";  # anchor at right, text extends left
            case "center"
              rot_text_valign = "middle";
            case "right"
              rot_text_valign = "top";     # anchor at left, text extends right
            otherwise
              rot_text_valign = "top";
          endswitch

          ht = text ("parent", hg, ...
                     "position", [labelx, labely, 0], ...
                     "string", labels{i}, ...
                     "horizontalalignment", rot_text_halign, ...
                     "verticalalignment", rot_text_valign, ...
                     "rotation", 90, ...
                     "color", linecolor, ...
                     "clipping", "off");
        else
          ## Non-rotated text uses standard alignment
          ht = text ("parent", hg, ...
                     "position", [labelx, labely, 0], ...
                     "string", labels{i}, ...
                     "horizontalalignment", text_halign, ...
                     "verticalalignment", labelvalign, ...
                     "color", linecolor, ...
                     "clipping", "off");
        endif
      endif

      ## Create the vertical line(s)
      ## Split line if label is horizontally centered on the line
      if (! isempty (labels) && strcmp (lower (labelhalign), "center"))
        ## Calculate gap from font metrics
        str = labels{i};
        if (iscell (str))
          num_lines = numel (str);
          max_len = max (cellfun (@numel, str));
        else
          num_lines = 1;
          max_len = numel (str);
        endif

        ## For rotated text (aligned), width becomes height and vice versa
        if (strcmp (lower (labelorient), "aligned"))
          ## Text is rotated 90 degrees, so character width contributes to vertical extent
          text_visual_height = max_len * char_width_data * 1.2;
        else
          ## Text is horizontal
          text_visual_height = num_lines * char_height_data * 1.2;
        endif

        ## Gap is centered at label y-position
        gap_bottom = labely - text_visual_height / 2 - ypadding;
        gap_top = labely + text_visual_height / 2 + ypadding;

        ## Bottom segment
        if (ylim(1) < gap_bottom)
          hl1 = __go_line__ (hg, "xdata", [xval(i), xval(i)], ...
                                 "ydata", [ylim(1), gap_bottom], ...
                                 "color", linecolor, "linestyle", linestyle);
          set (hl1, "yliminclude", "off");
          for j = 1:2:numel (lineprops)
            try
              set (hl1, lineprops{j}, lineprops{j + 1});
            catch
            end_try_catch
          endfor
        endif

        ## Top segment
        if (gap_top < ylim(2))
          hl2 = __go_line__ (hg, "xdata", [xval(i), xval(i)], ...
                                 "ydata", [gap_top, ylim(2)], ...
                                 "color", linecolor, "linestyle", linestyle);
          set (hl2, "yliminclude", "off");
          for j = 1:2:numel (lineprops)
            try
              set (hl2, lineprops{j}, lineprops{j + 1});
            catch
            end_try_catch
          endfor
        endif
      else
        ## Single continuous line
        hl = __go_line__ (hg, "xdata", [xval(i), xval(i)], "ydata", ylim, ...
                              "color", linecolor, "linestyle", linestyle);
        set (hl, "yliminclude", "off");

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

      addproperty ("value", hg, "data", xval(i));
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

      ## Add listener for ylim changes to update line extent
      ylim_listener = {@update_ylim, hg};
      addlistener (hax, "ylim", ylim_listener);

      ## Add delete function to clean up listener
      set (hg, "deletefcn", {@cleanup_listeners, hax, ylim_listener});

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


## Callback to update line x-coordinate
function update_line_data (hg, ~)
  xval = get (hg, "value");
  kids = get (hg, "children");
  for i = 1:numel (kids)
    if (strcmp (get (kids(i), "type"), "line"))
      set (kids(i), "xdata", [xval, xval]);
    elseif (strcmp (get (kids(i), "type"), "text"))
      pos = get (kids(i), "position");
      pos(1) = xval;
      set (kids(i), "position", pos);
    endif
  endfor
endfunction


## Callback to update line y-extent when axes limits change
function update_ylim (hax, ~, hg)
  if (! ishghandle (hg))
    return;
  endif
  ylim = get (hax, "ylim");
  kids = get (hg, "children");
  labelvalign = get (hg, "labelverticalalignment");

  ## Calculate vertical padding based on font size
  fontsize = get (hax, "fontsize");
  axpos = get (hax, "position");
  figpos = get (ancestor (hax, "figure"), "position");
  axes_height_pixels = axpos(4) * figpos(4);
  yrange = diff (ylim);
  char_height_data = (fontsize / axes_height_pixels) * yrange;
  ypadding = char_height_data / 4;

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
    switch (lower (labelvalign))
      case "top"
        pos(2) = ylim(2) - ypadding;
      case "middle"
        pos(2) = mean (ylim);
      case "bottom"
        pos(2) = ylim(1) + ypadding;
    endswitch
    set (text_kid, "position", pos);
  endif

  ## Update line(s)
  labelhalign = get (hg, "labelhorizontalalignment");
  if (strcmp (lower (labelhalign), "center") && ! isempty (text_kid) ...
      && numel (line_kids) >= 2)
    ## Split line case - calculate gap around text from font metrics
    str = get (text_kid, "string");
    if (iscell (str))
      num_lines = numel (str);
      max_len = max (cellfun (@numel, str));
    else
      num_lines = 1;
      max_len = numel (str);
    endif

    ## Calculate char width in data units
    axes_width_pixels = axpos(3) * figpos(3);
    xrange = diff (get (hax, "xlim"));
    char_width_data = (0.6 * fontsize / axes_width_pixels) * xrange;

    ## Check if text is rotated
    rotation = get (text_kid, "rotation");
    if (abs (rotation - 90) < 1 || abs (rotation - 270) < 1)
      ## Text is rotated 90 degrees, so character width contributes to vertical extent
      text_visual_height = max_len * char_width_data * 1.2;
    else
      ## Text is horizontal
      text_visual_height = num_lines * char_height_data * 1.2;
    endif

    ## Gap is centered at text position
    text_pos = get (text_kid, "position");
    gap_bottom = text_pos(2) - text_visual_height / 2 - ypadding;
    gap_top = text_pos(2) + text_visual_height / 2 + ypadding;

    ## Identify which segment is bottom and which is top by current position
    for i = 1:numel (line_kids)
      ydata = get (line_kids(i), "ydata");
      ymid = mean (ydata);
      if (ymid < mean (ylim))
        ## This is the bottom segment
        set (line_kids(i), "ydata", [ylim(1), gap_bottom]);
      else
        ## This is the top segment
        set (line_kids(i), "ydata", [gap_top, ylim(2)]);
      endif
    endfor
  else
    ## Single continuous line or non-centered label
    for i = 1:numel (line_kids)
      set (line_kids(i), "ydata", ylim);
    endfor
  endif
endfunction


## Cleanup function to remove listeners when line is deleted
function cleanup_listeners (hg, ~, hax, ylim_listener)
  try
    dellistener (hax, "ylim", ylim_listener);
  catch
    ## Ignore errors if axes no longer exists
  end_try_catch
endfunction


%!demo
%! clf;
%! plot (rand (1, 10) * 10, 1:10);
%! xline (5, "--r", "Threshold");
%! title ("xline() with label");

%!demo
%! clf;
%! plot (1:100);
%! xline ([25, 50, 75], ":", {"Q1", "Median", "Q3"});
%! title ("xline() with multiple lines and labels");

%!demo
%! clf;
%! x = linspace (0, 2*pi, 100);
%! plot (x, sin (x));
%! xline (pi, "-k", "pi", "LabelVerticalAlignment", "middle");
%! xline ([pi/2, 3*pi/2], "--b", {"pi/2", "3pi/2"}, ...
%!        "LabelVerticalAlignment", "bottom");
%! title ("xline() showing sine wave key points");

%!demo
%! clf;
%! plot (1:10);
%! xline (5, "-r", {"Horizontal", "Label"}, "LabelOrientation", "horizontal");
%! title ("xline() with horizontal (non-rotated) label");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = xline (5);
%!   assert (ishghandle (h));
%!   assert (get (h, "value"), 5);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = xline ([2, 5, 8]);
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
%!   h = xline (5, "--r");
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
%!   h = xline (5, "-b", "Test Label");
%!   assert (get (h, "label"), "Test Label");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test empty x returns empty handle
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = xline ([]);
%!   assert (isempty (h));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test multiline label
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   h = xline (5, "-", {"Line 1", "Line 2"});
%!   assert (iscell (get (h, "label")));
%!   assert (numel (get (h, "label")), 2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <X must be a real numeric> xline ("invalid")
%!error <X must be a real numeric> xline (1+i)
%!error <X must be a scalar or vector> xline ([1 2; 3 4])
