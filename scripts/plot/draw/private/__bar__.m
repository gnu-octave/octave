########################################################################
##
## Copyright (C) 1996-2024 The Octave Project Developers
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
## @deftypefn {} {@var{varargout} =} __bar__ (@var{fcn}, @var{vertical}, @dots{})
## Internal function with common code to implement @code{bar} and @code{barh}
## plots.
## @seealso{bar, barh}
## @end deftypefn

function varargout = __bar__ (fcn, vertical, varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ (fcn, varargin{:});

  if (! isnumeric (varargin{1}))
    error ("%s: Y must be numeric", fcn);
  endif

  width = 0.8;
  group = true;
  stacked = false;
  histc = NA;
  ## BaseValue
  if (strcmp (get (hax, "yscale"), "log"))
    bv = 1;
  else
    bv = 0;
  endif

  if (nargin > 1 && isnumeric (varargin{2}))
    x = varargin{1};
    if (isvector (x))
      x = x(:);
    endif
    y = varargin{2};
    if (isvector (y))
      y = y(:);
    endif
    if (isscalar (y) && ! isscalar (x))
      ## "y" is actually "width" argument
      y = x;
      x = [1:rows(y)]';
      idx = 2;
    else
      if (! isvector (x))
        error ("%s: X must be a vector", fcn);
      elseif (numel (unique (x)) != numel (x))
        error ("%s: X vector values must be unique", fcn);
      endif
      idx = 3;
    endif
  else
    y = varargin{1};
    if (isvector (y))
      y = y(:);
    endif
    x = [1:rows(y)]';
    idx = 2;
  endif

  newargs = {};
  have_line_spec = false;
  while (idx <= nargin)
    if (ischar (varargin{idx}) && strcmpi (varargin{idx}, "grouped"))
      group = true;
      idx += 1;
    elseif (ischar (varargin{idx}) && strcmpi (varargin{idx}, "stacked"))
      stacked = true;
      group = false;
      idx += 1;
    elseif (ischar (varargin{idx}) && strcmpi (varargin{idx}, "histc"))
      group = true;
      histc = true;
      idx += 1;
    elseif (ischar (varargin{idx}) && strcmpi (varargin{idx}, "hist"))
      group = true;
      histc = false;
      idx += 1;
    else
      if ((ischar (varargin{idx}) || iscellstr (varargin{idx}))
          && ! have_line_spec)
        [linespec, valid] = __pltopt__ (fcn, varargin{idx}, false);
        if (valid)
          have_line_spec = true;
          ## FIXME: strange parse error requires semicolon to be spaced
          ##        away from closing ']' on next line.
          newargs = [{"facecolor", linespec.color}, newargs] ;
          idx += 1;
          continue;
        endif
      endif
      if (isscalar (varargin{idx}))
        width = varargin{idx++};
      elseif (idx == nargin)
        newargs = [newargs, varargin(idx++)];
      elseif (ischar (varargin{idx})
              && strcmpi (varargin{idx}, "basevalue")
              && isscalar (varargin{idx+1}))
        bv = varargin{idx+1};
        idx += 2;
      else
        newargs = [newargs, varargin(idx:idx+1)];
        idx += 2;
      endif
    endif
  endwhile

  ishist = islogical (histc);
  ngrp = rows (x);

  if (isvector (y) && ngrp != rows (y))
    y = y.';
  endif
  if (ngrp != rows (y))
    error ("%s: length of X and Y must be equal", fcn);
  endif

  nbars = columns (y);

  ## Column width is 1 for 'hist*' styles (bars touch).
  if (ishist)
    cwidth = 1;
    if (nbars == 1)
      gwidth = 1;
    else
      gwidth = width^2;
    endif
  elseif (nbars == 1)
    cwidth = 1;
    gwidth = width;
  else
    cwidth = gwidth = width;
  endif

  ## Complicated algorithm sizes bars with unitless parameter width.
  ## If width is 1.0, adjacent bars in a group are touching.
  ## Otherwise, bar size is cwidth and the remaining space is split evenly on
  ## either side of the bar.  For the default 0.8, spacing is [0.1 0.8 0.1].
  ## Groups of bars are spaced by gwidth.  If gwidth is 1.0 then adjacent
  ## groups will just touch.
  if (numel (x) > 1)
    cutoff = min (diff (double (x))) / 2;
  else
    cutoff = 1;
  endif
  if (group)
    gdelta = cutoff * gwidth / nbars;
    cdelta = repmat ((1 - ((1 - cwidth) / 2)) * gdelta, size (x));
  else
    cdelta = repmat (cutoff * gwidth, size (x));
  endif
  x1 = (x - cdelta)(:)';
  x2 = (x + cdelta)(:)';
  xb = repmat ([x1; x1; x2; x2](:), 1, nbars);

  if (group)
    if (ishist && histc)
      offset = 2*cdelta * [0:(nbars-1)] + cdelta(1);  # not centered
    else
      offset = 2*cdelta * [-(nbars - 1) / 2 : (nbars - 1) / 2];
    endif

    xb(1:4:4*ngrp,:) += offset + (1-cwidth) / 2 * (2 * gdelta);
    xb(2:4:4*ngrp,:) += offset + (1-cwidth) / 2 * (2 * gdelta);
    xb(3:4:4*ngrp,:) += offset - (1-cwidth) / 2 * (2 * gdelta);
    xb(4:4:4*ngrp,:) += offset - (1-cwidth) / 2 * (2 * gdelta);

    y0 = zeros (size (y)) + bv;
    y1 = y;
  else
    if (stacked && any (y(:) < 0))
      ypos = (y >= 0);
      yneg = (y <  0);

      y1p =  cumsum (y .* ypos, 2);
      y1n =  cumsum (y .* yneg, 2);
      y1 = y1p .* ypos + y1n .* yneg;

      y0p = [zeros(ngrp,1)+bv, y1p(:,1:end-1)];
      y0n = [zeros(ngrp,1)+bv, y1n(:,1:end-1)];
      y0 = y0p .* ypos + y0n .* yneg;

    else
      y1 = cumsum (y,2);
      y0 = [zeros(ngrp,1)+bv, y1(:,1:end-1)];
    endif
  endif

  yb = zeros (4*ngrp, nbars);
  yb(1:4:4*ngrp,:) = y0;
  yb(2:4:4*ngrp,:) = y1;
  yb(3:4:4*ngrp,:) = y1;
  yb(4:4:4*ngrp,:) = y0;

  xb = reshape (xb, [4, ngrp, nbars]);
  yb = reshape (yb, [4, ngrp, nbars]);

  if (nargout < 2)
    oldfig = [];
    if (! isempty (hax))
      oldfig = get (0, "currentfigure");
    endif
    unwind_protect
      hax = newplot (hax);
      htmp = bars (hax, ishist, vertical, x, y, xb, yb, gwidth, group,
                   have_line_spec, bv, newargs{:});
      if (! ishold ())
        update_axes_limits (hax, x, vertical, ishist && histc);
      endif

    unwind_protect_cleanup
      if (! isempty (oldfig))
        set (0, "currentfigure", oldfig);
      endif
    end_unwind_protect
    if (nargout == 1)
      varargout{1} = htmp;
    endif
  else
    if (vertical)
      varargout{1} = xb;
      varargout{2} = yb;
    else
      varargout{1} = yb;
      varargout{2} = xb;
    endif
  endif

endfunction

function hglist = bars (hax, ishist, vertical, x, y, xb, yb, width, group,
                        have_color_spec, base_value, varargin)

  hglist = [];
  nbars = columns (y);

  if (ishist)
    ## Special case for Matlab compatibility.  For 'hist', 'histc' arguments,
    ## return Patch objects rather than hggroup Bar object.
    for i = 1:nbars

      if (vertical)
        h = patch (hax, xb(:,:,i), yb(:,:,i),
                   "cdata", i, "FaceColor", "flat");
      else
        h = patch (hax, yb(:,:,i), xb(:,:,i),
                   "cdata", i, "FaceColor", "flat");
      endif

      if (! isempty (varargin))
        set (h, varargin{:});
      endif

      hglist = [hglist; h];
    endfor
    return;  # return immediately, rest of function creates Bar object.
  endif

  ## Code to create hggroup Bar object
  for i = 1:nbars
    hg = hggroup ();
    hglist = [hglist; hg];
    args = __add_datasource__ ("bar", hg, {"x", "y"}, varargin{:});

    if (vertical)
      if (! have_color_spec)
        color = __next_line_color__ ();
        h = patch (hax, xb(:,:,i), yb(:,:,i), "FaceColor", color, ...
                   "parent", hg);
      else
        h = patch (hax, xb(:,:,i), yb(:,:,i), "cdata", i, "parent", hg);
      endif
    else
      if (! have_color_spec)
        color = __next_line_color__ ();
        h = patch (hax, yb(:,:,i), xb(:,:,i), "FaceColor", color, ...
                  "parent", hg);
      else
        h = patch (hax, yb(:,:,i), xb(:,:,i), "cdata", i, "parent", hg);
      endif
    endif

    if (i == 1)
      ## Add baseline object the first time through loop
      ## Undocumented x/yliminclude property prevents baseline from affecting
      ## x/ylim perpendicular to bar direction, but x/ylim parallel to bar
      ## direction must be sensitive to baseline position.
      if (vertical)
        x_axis_range = get (hax, "xlim");
        baseline = __go_line__ (hax, "xdata", x_axis_range,
                                       "ydata", [base_value, base_value],
                                       "color", [0, 0, 0]);
        set (baseline, "handlevisibility", "off", "xliminclude", "off",
                         "parent", hax);
      else
        y_axis_range = get (hax, "ylim");
        baseline = __go_line__ (hax, "ydata", y_axis_range,
                                       "xdata", [base_value, base_value],
                                       "color", [0, 0, 0]);
        set (baseline, "handlevisibility", "off", "yliminclude", "off",
                         "parent", hax);
      endif
    endif

    ## Setup the hggroup and listeners
    addproperty ("showbaseline", hg, "radio", "{on}|off");
    addproperty ("basevalue", hg, "data", base_value);
    addproperty ("baseline", hg, "data", baseline);

    addlistener (hg, "showbaseline", {@show_baseline, "showbl"});
    addlistener (hg, "visible", {@show_baseline, "visib"});
    addlistener (hg, "basevalue", @move_baseline);

    addproperty ("barwidth", hg, "data", width);
    if (group)
      addproperty ("barlayout", hg, "radio", "stacked|{grouped}", "grouped");
    else
      addproperty ("barlayout", hg, "radio", "{stacked}|grouped", "stacked");
    endif

    if (vertical)
      addproperty ("horizontal", hg, "radio", "on|{off}", "off");
    else
      addproperty ("horizontal", hg, "radio", "{on}|off", "on");
    endif

    addlistener (hg, "barwidth", @update_group);
    addlistener (hg, "barlayout", @update_group);
    addlistener (hg, "horizontal", @update_group);

    addproperty ("edgecolor", hg, "patchedgecolor", get (h, "edgecolor"));
    addproperty ("facecolor", hg, "patchfacecolor", get (h, "facecolor"));
    addproperty ("linestyle", hg, "patchlinestyle", get (h, "linestyle"));
    addproperty ("linewidth", hg, "patchlinewidth", get (h, "linewidth"));

    addlistener (hg, "edgecolor", @update_props);
    addlistener (hg, "facecolor", @update_props);
    addlistener (hg, "linestyle", @update_props);
    addlistener (hg, "linewidth", @update_props);

    addproperty ("xdata", hg, "data", x);
    addproperty ("ydata", hg, "data", y(:, i));

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);

    addproperty ("bargroup", hg, "data");
    set (hglist, "bargroup", hglist);

    ## Matlab property, although Octave does not implement it.
    addproperty ("hittestarea", hg, "radio", "on|{off}", "off");

    if (! isempty (args))
      set (hg, args{:});
    endif
  endfor

  update_baseline_lim (hax, []);

  ## Add listeners outside of for loop to prevent constant updating during
  ## creation of plot when patch objects are added.
  addlistener (hax, "xlim", @update_baseline_lim);
  addlistener (hax, "yscale", {@update_basevalue_logscale, hg});
  addlistener (baseline, "ydata", @update_baseline);

  addlistener (hax, "ylim", @update_baseline_lim);
  addlistener (hax, "xscale", {@update_basevalue_logscale, hg});
  addlistener (baseline, "xdata", @update_baseline);

  addlistener (baseline, "visible", @update_baseline);

endfunction

function update_baseline_lim (hax, ~)

  ## set baseline extents to match current axes limits
  [kids, xlim, ylim] = get (hax, {"children", "xlim", "ylim"}){:};

  for i = 1 : length (kids)
    obj = get (kids(i));

    if (strcmp (obj.type, "hggroup") && isfield (obj, "baseline"))

      if (strcmp (obj.horizontal, "off"))  # if (vertical)
        if (any (get (obj.baseline, "xdata") != xlim))
          set (obj.baseline, "xdata", xlim);
        endif
      else
        if (any (get (obj.baseline, "ydata") != ylim))
          set (obj.baseline, "ydata", ylim);
        endif
      endif

    endif
  endfor

endfunction


function update_basevalue_logscale (hax, ~, hg)

  if (strcmp (get (hg, "horizontal"), "off"))
    axisscale = "yscale";
  else
    axisscale = "xscale";
  endif

  if (strcmp (get (hax, axisscale), "log"))
    warning ("off", "Octave:negative-data-log-axis", "local");
    if (get (hg, "basevalue") == 0)
      set (hg, "basevalue", 1);
    endif
  else
    if (get (hg, "basevalue") == 1)
      set (hg, "basevalue", 0);
    endif
  endif

endfunction


function update_baseline (hl, ~)
  visible = get (hl, "visible");

  ## Search axis for a bargroup that contains this baseline handle
  kids = get (get (hl, "parent"), "children");
  for i = 1 : length (kids)
    obj = get (kids(i));
    if (strcmp (obj.type, "hggroup") && isfield (obj, "baseline")
        && obj.baseline == hl)

      if (strcmp (obj.horizontal, "off"))
        data = get (hl, "ydata")(1);
      else
        data = get (hl, "xdata")(1);
      endif

      set (obj.bargroup, "showbaseline", visible, "basevalue", data);
      break;

    endif
  endfor

endfunction


function show_baseline (hg, ~, prop = "")

  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (hg, "bargroup");
      if (strcmp (prop, "showbl"))
        showbaseline = get (hg, "showbaseline");
        hlist = hlist(hlist != hg);  # remove current handle being updated
        set (hlist, "showbaseline", showbaseline);
      elseif (strcmp (prop, "visib"))
        showbaseline = "on";
        if (all (strcmp (get (hlist, "visible"), "off")))
          showbaseline = "off";
        endif
      endif
      set (get (hg, "baseline"), "visible", showbaseline);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif

endfunction


function move_baseline (hg, ~)

  persistent recursion = false;
  ## Don't allow recursion
  if (! recursion)

    recursion = true;
    unwind_protect
      [b0, bl] = get (hg, {"basevalue", "baseline"}){:};

      if (strcmp (get (hg, "horizontal"), "off"))  # if (vertical)
        set (bl, "ydata", [b0, b0]);
      else
        set (bl, "xdata", [b0, b0]);
      endif

      if (strcmp (get (hg, "barlayout"), "grouped"))
        update_data (hg);
      endif
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif

endfunction


function update_props (hg, ~)
  kids = get (hg, "children");
  set (kids, {"edgecolor", "linewidth", "linestyle", "facecolor"},
       get (hg, {"edgecolor", "linewidth", "linestyle", "facecolor"}));
endfunction


function update_data (hg, ~)

  persistent recursion = false;
  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (hg, "bargroup");
      x = get (hg, "xdata");
      if (! isvector (x))
        x = x(:);
      endif
      ydat = get (hlist, "ydata");
      if (iscell (ydat))
        y = cell2mat (ydat.');
      elseif (isvector (ydat))
        y = ydat(:);
      else
        y = ydat;
      endif

      [b0, bl, bw, blo, horiz, hax] = get (hg, ...
                                   {"basevalue", "baseline", "barwidth", ...
                                   "barlayout",  "horizontal", "parent"}){:};
      [xb, yb] = bar (x, y, bw, blo, "basevalue", b0);

      vertical = strcmp (horiz, "off");
      for i = 1:columns (y)
        hp = get (hlist(i), "children");
        if (vertical)
          set (hp, "xdata", xb(:,:,i), "ydata", yb(:,:,i));
        else
          set (hp, "xdata", yb(:,:,i), "ydata", xb(:,:,i));
        endif
      endfor

      ## Update baseline properties affecting x/ylim
      if (vertical)
        set (bl, {"xliminclude", "yliminclude"}, {"off", "on"});
        set (bl, "ydata", [b0, b0]);
      else
        set (bl, {"xliminclude", "yliminclude"}, {"on", "off"});
        set (bl, "xdata", [b0, b0]);
      endif

      if (! ishold ())
        update_axes_limits (hax, x, vertical, strcmpi (blo, "histc"));
      endif

      ## Update baseline extents to x/ylim.
      if (vertical)
        set (bl, "xdata", get (hax, "xlim"));
      else
        set (bl, "ydata", get (hax, "ylim"));
      endif

    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif

endfunction


function update_group (hg, ~)

  persistent recursion = false;
  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (hg, "bargroup");
      barwidth = get (hg, "barwidth");
      barlayout = get (hg, "barlayout");
      horizontal = get (hg, "horizontal");

      hlist = hlist(hlist != hg);  # remove current handle being updated
      set (hlist, "barwidth", barwidth, "barlayout", barlayout,
                  "horizontal", horizontal);
      update_data (hg);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif

endfunction


function update_axes_limits (hax, x, vertical, ishistc)

  if (numel (x(:,1)) <= 15 && all (x(:,1) == fix (x(:,1))))
    ## Set manual ticks, rather than relying on autoselection, when ticks are
    ## a small number of integers.
    ## Then temporarily set to auto to reset limits around patch elements and
    ## baseline component parallel to bars.
    if (vertical)
      set (hax, "xtick", x(:,1));
      set (hax, "xlimmode", "auto");
      set (hax, "ytickmode", "auto");
    else
      set (hax, "ytick", x(:,1));
      set (hax, "ylimmode", "auto");
      set (hax, "xtickmode", "auto");
    endif
  endif

  if (ishistc)
    set (hax, "climmode", "auto");
  endif

  set (hax, "box", "on", "layer", "top");

endfunction
