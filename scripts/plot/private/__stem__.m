## Copyright (C) 2006-2012 Michel D. Schmid
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
## @deftypefn {Function File} {@var{h} =} __stem__ (@var{have_z}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

## Author: Michel D. Schmid <michaelschmid@users.sourceforge.net>
## Adapted-by: jwe

function h = __stem__ (have_z, varargin)

  if (have_z)
    caller = "stem3";
  else
    caller = "stem";
  endif

  [ax, varargin, nargin] = __plt_get_axis_arg__ (caller, varargin{:});

  [x, y, z, dofill, llc, ls, mmc, ms, varargin] = ...
      check_stem_arg (have_z, varargin{:});

  oldax = gca ();
  unwind_protect
    axes (ax);
    hold_state = get (ax, "nextplot");
    newplot ();
    h = [];

    nx = rows (x);
    for i = 1: columns (x)
      if (have_z)
        xt = x(:)';
        xt = [xt; xt; NaN(1, nx)](:);
        yt = y(:)';
        yt = [yt; yt; NaN(1, nx)](:);
        zt = z(:)';
        zt = [zeros(1, nx); zt; NaN(1, nx)](:);
      else
        xt = x(:, i)';
        xt = [xt; xt; NaN(1, nx)](:);
        yt = y(:, i)';
        yt = [zeros(1, nx); yt; NaN(1, nx)](:);
      endif

      hg  = hggroup ();
      h = [h; hg];
      args = __add_datasource__ (caller, hg, {"x", "y", "z"}, varargin{:});

      if (i == 1)
        set (ax, "nextplot", "add");
      endif

      if (isempty (llc))
        lc = __next_line_color__ ();
      else
        lc = llc;
      endif

      if (isempty (mmc))
        mc = lc;
      else
        mc = mmc;
      endif

      if (dofill)
        fc = mc;
      else
        fc = "none";
      endif

      if (have_z)
        h_stems = plot3 (xt, yt, zt, "color", lc, "linestyle", ls,
                         "parent", hg, x, y, z, "color", mc,
                         "marker",  ms, "linestyle", "none",
                         "markerfacecolor", fc, "parent", hg);

        h_baseline = [];
      else
        h_stems = plot (xt, yt, "color", lc, "linestyle", ls,
                        "parent", hg, x(:,i), y(:, i), "color", mc, "marker",
                        ms, "linestyle", "none", "markerfacecolor",
                        fc, "parent", hg);

        x_axis_range = get (ax, "xlim");
        h_baseline = line (x_axis_range, [0, 0], "color", [0, 0, 0]);
        set (h_baseline, "handlevisibility", "off");
        set (h_baseline, "xliminclude", "off");
        addlistener (ax, "xlim", @update_xlim);
        addlistener (h_baseline, "ydata", @update_baseline);
        addlistener (h_baseline, "visible", @update_baseline);
      endif

      ## Setup the hggroup and listeners.
      addproperty ("showbaseline", hg, "radio", "{on}|off");
      addproperty ("basevalue", hg, "data", 0);
      addproperty ("baseline", hg, "data", h_baseline);

      if (!have_z)
        addlistener (hg, "showbaseline", @show_baseline);
        addlistener (hg, "basevalue", @move_baseline);
      endif

      addproperty ("color", hg, "linecolor", lc);
      addproperty ("linewidth", hg, "linelinewidth", 0.5);
      addproperty ("linestyle", hg, "linelinestyle", ls);
      addproperty ("marker", hg, "linemarker", ms);
      addproperty ("markerfacecolor", hg, "linemarkerfacecolor", fc);
      addproperty ("markersize", hg, "linemarkersize", 6);

      addlistener (hg, "color", @update_props);
      addlistener (hg, "linewidth", @update_props);
      addlistener (hg, "linestyle", @update_props);
      addlistener (hg, "marker", @update_props);
      addlistener (hg, "markerfacecolor", @update_props);
      addlistener (hg, "markersize", @update_props);

      addproperty ("xdata", hg, "data", x(:, i));
      addproperty ("ydata", hg, "data", y(:, i));
      if (have_z)
        addproperty ("zdata", hg, "data", z(:, i));
      else
        addproperty ("zdata", hg, "data", []);
      endif

      addlistener (hg, "xdata", @update_data);
      addlistener (hg, "ydata", @update_data);
      addlistener (hg, "zdata", @update_data);

      if (! isempty (args))
        set (hg, args{:});
      endif
      if (i == 1 && !isempty(h_baseline))
        set (h_baseline, "parent", get (hg, "parent"));
      endif
    endfor

  unwind_protect_cleanup
    set (ax, "nextplot", hold_state);
    axes (oldax);
  end_unwind_protect
endfunction

function [x, y, z, dofill, lc, ls, mc, ms, newargs] = check_stem_arg (have_z, varargin)

  ## FIXME -- there seems to be a lot of duplicated code in this
  ## function.  It seems like it should be possible to simplify things
  ## by combining some of the nearly identical code sections into
  ## additional subfunctions.

  if (have_z)
    caller = "stem3";
  else
    caller = "stem";
  endif

  ## Remove prop/val pairs from data to consider.
  i = 2;
  newargs = {};
  while (i < length (varargin))
    if (ischar (varargin{i}) && !(strcmpi ("fill", varargin{i})
                                  || strcmpi ("filled", varargin{i})))
      newargs{end + 1} = varargin{i};
      newargs{end + 1} = varargin{i + 1};
      nargin = nargin - 2;
      varargin(i:i+1) = [];
    else
      i++;
    endif
  endwhile

  ## set specifiers to default values.
  [lc, ls, mc, ms] = set_default_values ();
  dofill = 0;
  fill_2 = 0;
  linespec_2 = 0;
  z = [];

  ## Check input arguments.
  if (nargin == 2)
    if (have_z)
      z = varargin{1};
      x = 1:rows (z);
      y = 1:columns (z);
    else
      y = varargin{1};
      if (isvector (y))
        x = 1:length (y);
      elseif (ismatrix (y))
        x = 1:rows (y);
      else
        error ("stem: Y must be a matrix");
      endif # in each case, x & y will be defined
    endif
  elseif (nargin == 3)
    ## Several possibilities
    ##
    ## 1. the real y data
    ## 2. 'filled'
    ## 3. line spec
    if (ischar (varargin{2}))
      ## Only 2. or 3. possible.
      if (strcmpi ("fill", varargin{2}) || strcmpi ("filled", varargin{2}))
        dofill = 1;
      else
        ## Parse the linespec.
        [lc, ls, mc, ms] = stem_line_spec (caller, varargin{2});
      endif
      if (have_z)
        z = varargin{1};
        x = 1:rows (z);
        y = 1:columns (z);
      else
        y = varargin{1};
        if (isvector (y))
          x = 1:length (y);
        elseif (ismatrix (y))
          x = 1:rows (y);
        else
          error ("stem: Y must be a matrix");
        endif # in each case, x & y will be defined
      endif
    else
      if (have_z)
        error ("stem3: must define X, Y and Z");
      else
        ## Must be the real y data.
        x = varargin{1};
        y = varargin{2};
        if (! (ismatrix (x) && ismatrix (y)))
          error ("stem: X and Y must be matrices");
        endif
      endif
    endif
  elseif (nargin == 4)
    ## Again, several possibilities:
    ##
    ## arg2 1. real y
    ## arg2 2. 'filled' or linespec
    ## arg3 1. real z
    ## arg3 2. 'filled' or linespec
    if (ischar (varargin{2}))
      ## Only arg2 2. / arg3 1. & arg3 3. are possible.
      if (strcmpi ("fill", varargin{2}) || strcmpi ("filled", varargin{2}))
        dofill = 1;
        fill_2 = 1; # Be sure, no second "fill" is in the arguments.
      else
        ## Must be a linespec.
        [lc, ls, mc, ms] = stem_line_spec (caller, varargin{2});
        linespec_2 = 1;
      endif
      if (have_z)
        z = varargin{1};
        x = 1:rows (z);
        y = 1:columns (z);
      else
        y = varargin{1};
        if (isvector (y))
          x = 1:length (y);
        elseif (ismatrix (y))
          x = 1:rows (y);
        else
          error ("stem: Y must be a matrix");
        endif # in each case, x & y will be defined
      endif
    else
      if (have_z)
        x = varargin{1};
        y = varargin{2};
        z = varargin{3};
        if (! (ismatrix (x) && ismatrix (y) && ismatrix (z)))
          error ("stem3: X, Y and Z must be matrices");
        endif
      else
        ## must be the real y data.
        x = varargin{1};
        y = varargin{2};
        if (! (ismatrix (x) && ismatrix (y)))
          error ("stem: X and Y must be matrices");
        endif
      endif
    endif # if ischar(varargin{2})
    if (! have_z)
      ## varargin{3} must be char.
      ## Check for "fill.
      if ((strcmpi (varargin{3}, "fill") || strcmpi (varargin{3}, "filled"))
          && fill_2)
        error ("stem: duplicate fill argument");
      elseif (strcmpi ("fill", varargin{3}) && linespec_2)
        ## Must be "fill".
        dofill = 1;
        fill_2 = 1;
      elseif ((strcmpi (varargin{3}, "fill") || strcmpi (varargin{3}, "filled"))
          && !linespec_2)
        ## Must be "fill".
        dofill = 1;
        fill_2 = 1;
      elseif (! linespec_2)
        ## Must be linespec.
        [lc, ls, mc, ms] = stem_line_spec (caller, varargin{3});
        linespec_2 = 1;
      endif
    endif
  elseif (nargin == 5)
    if (have_z)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      if (! (ismatrix (x) && ismatrix (y) && ismatrix (z)))
        error ("stem3: X, Y and Z must be matrices");
      endif
    else
      x = varargin{1};
      y = varargin{2};
      if (! (ismatrix (x) && ismatrix (y)))
        error ("stem: X and Y must be matrices");
      endif
    endif

    if (! have_z)
      if (strcmpi (varargin{3}, "fill") || strcmpi (varargin{3}, "filled"))
        dofill = 1;
        fill_2 = 1; # Be sure, no second "fill" is in the arguments.
      else
        ## Must be a linespec.
        [lc, ls, mc, ms] = stem_line_spec (caller, varargin{3});
        linespec_2 = 1;
      endif
    endif

    ## Check for "fill".
    if ((strcmpi (varargin{4}, "fill") || strcmpi (varargin{4}, "filled"))
        && fill_2)
      error ("%s: duplicate fill argument", caller);
    elseif ((strcmpi (varargin{4}, "fill") || strcmpi (varargin{4}, "filled"))
        && linespec_2)
      ## Must be "fill".
      dofill = 1;
      fill_2 = 1;
    elseif (!strcmpi (varargin{4}, "fill") && !strcmpi (varargin{4}, "filled")
        && !linespec_2)
      ## Must be linespec.
      [lc, ls, mc, ms] = stem_line_spec (caller, varargin{4});
      linespec_2 = 1;
    endif
  elseif (nargin == 6 && have_z)
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    if (! (ismatrix (x) && ismatrix (y) && ismatrix (z)))
      error ("stem3: X, Y and Z must be matrices");
    endif

    if (strcmpi (varargin{4}, "fill") || strcmpi (varargin{4}, "filled"))
      dofill = 1;
      fill_2 = 1; # be sure, no second "fill" is in the arguments
    else
      ## Must be a linespec.
      [lc, ls, mc, ms] = stem_line_spec (caller, varargin{4});
      linespec_2 = 1;
    endif

    ## check for "fill" ..
    if ((strcmpi (varargin{5}, "fill") || strcmpi (varargin{5}, "filled"))
        && fill_2)
      error ("stem3: duplicate fill argument");
    elseif ((strcmpi (varargin{5}, "fill") || strcmpi (varargin{5}, "filled"))
        && linespec_2)
      ## Must be "fill".
      dofill = 1;
      fill_2 = 1;
    elseif (!strcmpi (varargin{5}, "fill") && !strcmpi (varargin{5}, "filled")
            && !linespec_2)
      ## Must be linespec.
      [lc, ls, mc, ms] = stem_line_spec (caller, varargin{5});
      linespec_2 = 1;
    endif
  else
    error ("%s: incorrect number of arguments", caller);
  endif

  ## Check sizes of x, y and z.
  if (have_z)
    if (!size_equal (x, y, z))
      error ("stem3: inconsistent size of x, y and z");
    else
      x = x(:);
      y = y(:);
      z = z(:);
    endif
  else
    if (isvector (x))
      x = x(:);
      if (isvector (y))
        if (length (x) != length (y))
          error ("stem: inconsistent size of x and y");
        else
          y = y(:);
        endif
      else
        if (length (x) == rows (y))
          x = repmat (x(:), 1, columns (y));
        else
          error ("stem: inconsistent size of x and y");
        endif
      endif
    elseif (!size_equal (x, y))
      error ("stem: inconsistent size of x and y");
    endif
  endif

endfunction

function [lc, ls, mc, ms] = stem_line_spec (caller, str)
  if (! ischar (str))
    error ("%s: expecting argument to be \"fill\" or a string of specifiers",
           caller);
  endif
  [lc, ls, mc, ms] = set_default_values ();
  ## Parse the line specifier string.
  cur_props = __pltopt__ ("stem", str, false);
  for i = 1:length(cur_props)
    if (isfield (cur_props(i), "color") && ! isempty (cur_props(i).color)); # means line color
      mc = lc = cur_props(i).color;
    elseif (isfield (cur_props(i), "linestyle"))
      ls = cur_props(i).linestyle;
      if (isempty (ls))
        ls = __next_line_style__ ();
      endif
    elseif (isfield (cur_props(i), "marker") && ! strcmpi (cur_props(i).marker, "none"))
      ms = cur_props(i).marker;
      if (isempty (ms))
        [dummy, ms] = __next_line_style__ ();
      endif
    endif
  endfor
endfunction

function [lc, ls, mc, ms] = set_default_values ()
  ## set default values
  mc = [];
  lc = [];
  ls = "-";
  ms = "o";
endfunction

function update_xlim (h, d)
  kids = get (h, "children");
  xlim = get (h, "xlim");

  for i = 1 : length (kids)
    obj = get (kids (i));
    if (strcmp (obj.type, "hggroup") && isfield (obj, "baseline"))
      if (any (get (obj.baseline, "xdata") != xlim))
        set (obj.baseline, "xdata", xlim);
      endif
    endif
  endfor
endfunction

function update_baseline (h, d)
  visible = get (h, "visible");
  ydata = get (h, "ydata")(1);

  kids = get (get (h, "parent"), "children");
  for i = 1 : length (kids)
    obj = get (kids (i));
    if (strcmp (obj.type, "hggroup") && isfield (obj, "baseline")
        && obj.baseline == h)
      ## Only alter if changed to avoid recursion of the listener functions
      if (! strcmpi (get (kids(i), "showbaseline"), visible))
        set (kids (i), "showbaseline", visible);
      endif
      if (! strcmpi (get (kids(i), "basevalue"), visible))
        set (kids (i), "basevalue", ydata);
      endif
    endif
  endfor
endfunction

function show_baseline (h, d)
  set (get (h, "baseline"), "visible", get (h, "showbaseline"));
endfunction

function move_baseline (h, d)
  b0 = get (h, "basevalue");
  bl = get (h, "baseline");

  if (get (bl, "ydata") != [b0, b0])
    set (bl, "ydata", [b0, b0]);
  endif

  kids = get (h, "children");
  yt = get(h, "ydata")(:)';
  ny = length (yt);
  yt = [b0 * ones(1, ny); yt; NaN(1, ny)](:);
  set (kids(2), "ydata", yt);
endfunction

function update_props (h, d)
  kids = get (h, "children");
  set (kids(2), "color", get (h, "color"),
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"));
  set (kids(1), "color", get (h, "color"),
       "marker", get (h, "marker"),
       "markerfacecolor", get (h, "markerfacecolor"),
       "markersize", get (h, "markersize"));
endfunction

function update_data (h, d)
  x = get (h, "xdata");
  y = get (h, "ydata");
  z = get (h, "zdata");

  if (!isempty (z) && size_equal (x, y, z))
    error ("stem3: inconsistent size of x, y and z");
  elseif (numel(x) != numel (y))
    error ("stem: inconsistent size of x and y");
  else
    bl = get (h, "basevalue");
    nx = numel (x);
    x = x(:)';
    xt = [x; x; NaN(1, nx)](:);
    if (! isempty (z))
      y = y(:)';
      yt = [y; y; NaN(1, nx)](:);
      z = z(:)';
      zt = [bl * ones(1, nx); z; NaN(1, nx)](:);
    else
      y = y(:)';
      yt = [bl * ones(1, nx); y; NaN(1, nx)](:);
      zt = [];
    endif

    kids = get (h, "children");
    set (kids(2), "xdata", xt, "ydata", yt, "zdata", zt);
    set (kids(1), "xdata", x, "ydata", y, "zdata", z);
  endif
endfunction
