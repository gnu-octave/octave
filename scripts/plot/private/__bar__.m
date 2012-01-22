## Copyright (C) 1996-2012 John W. Eaton
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
## @deftypefn {Function File} {} __bar__ (@var{vertical}, @var{func}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function varargout = __bar__ (vertical, func, varargin)

  [h, varargin] = __plt_get_axis_arg__ ((nargout > 1), func, varargin{:});

  ## Slightly smaller than 0.8 to avoid clipping issue in gnuplot 4.0
  width = 0.8 - 10 * eps;
  group = true;
  bv = 0;

  if (nargin < 3)
    print_usage ();
  endif

  if (nargin > 3 && isnumeric (varargin{2}))
    x = varargin{1};
    if (isvector (x))
      x = x(:);
    endif
    y = varargin{2};
    if (isvector (y))
      y = y(:);
    endif
    if (size (x, 1) != size (y, 1))
      y = varargin{1};
      if (isvector (y))
        y = y(:);
      endif
      x = [1:size(y,1)]';
      idx = 2;
    else
      if (! isvector (x))
        error ("%s: x must be a vector", func);
      endif
      idx = 3;
    endif
  else
    y = varargin{1};
    if (isvector (y))
      y = y(:);
    endif
    x = [1:size(y,1)]';
    idx = 2;
  endif

  newargs = {};
  have_line_spec = false;
  while (idx <= nargin - 2)
    if (ischar (varargin{idx}) && strcmpi (varargin{idx}, "grouped"))
      group = true;
      idx++;
    elseif (ischar (varargin{idx}) && strcmpi (varargin{idx}, "stacked"))
      group = false;
      idx++;
    else
      if ((ischar (varargin{idx}) || iscell (varargin{idx}))
          && ! have_line_spec)
        [linespec, valid] = __pltopt__ (func, varargin{idx}, false);
        if (valid)
          have_line_spec = true;
          newargs = [{"facecolor", linespec.color}, newargs]
          idx++;
          continue;
        endif
      endif
      if (isscalar(varargin{idx}))
        width = varargin{idx++};
      elseif (idx == nargin - 2)
        newargs = [newargs,varargin(idx++)];
      elseif (ischar (varargin{idx})
              && strcmpi (varargin{idx}, "basevalue")
              && isscalar (varargin{idx+1}))
        bv = varargin{idx+1};
        idx += 2;
      else
        newargs = [newargs,varargin(idx:idx+1)];
        idx += 2;
      endif
    endif
  endwhile

  xlen = size (x, 1);
  ylen = size (y, 1);

  if (xlen != ylen)
    error ("%s: length of x and y must be equal", func);
  endif
  if (any (x(2:end) < x(1:end-1)))
    error ("%s: x vector values must be in ascending order", func);
  endif

  ycols = size (y, 2);
  if (numel (x) > 1)
    cutoff = min (diff (double(x))) / 2;
  else
    cutoff = 1;
  endif
  if (group)
    delta_p = delta_m = repmat (cutoff * width / ycols, size (x));
  else
    delta_p = delta_m = repmat (cutoff * width, size (x));
  endif
  x1 = (x - delta_m)(:)';
  x2 = (x + delta_p)(:)';
  xb = repmat ([x1; x1; x2; x2](:), 1, ycols);

  if (group)
    offset = ((delta_p + delta_m) * [-(ycols - 1) / 2 : (ycols - 1) / 2]);
    xb(1:4:4*ylen,:) += offset;
    xb(2:4:4*ylen,:) += offset;
    xb(3:4:4*ylen,:) += offset;
    xb(4:4:4*ylen,:) += offset;
    y0 = zeros (size (y)) + bv;
    y1 = y;
  else
    y1 = cumsum(y,2);
    y0 = [zeros(ylen,1)+bv, y1(:,1:end-1)];
  endif

  yb = zeros (4*ylen, ycols);
  yb(1:4:4*ylen,:) = y0;
  yb(2:4:4*ylen,:) = y1;
  yb(3:4:4*ylen,:) = y1;
  yb(4:4:4*ylen,:) = y0;

  xb = reshape (xb, [4, numel(xb) / 4 / ycols, ycols]);
  yb = reshape (yb, [4, numel(yb) / 4 / ycols, ycols]);

  if (nargout < 2)
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();

      tmp = bars (h, vertical, x, y, xb, yb, width, group,
                  have_line_spec, bv, newargs{:});
      if (nargout == 1)
        varargout{1} = tmp;
      endif
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
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

function tmp = bars (ax, vertical, x, y, xb, yb, width, group, have_color_spec, base_value, varargin)

  ycols = columns (y);
  clim = get (ax, "clim");
  tmp = [];

  for i = 1:ycols
    hg = hggroup ();
    tmp = [tmp; hg];
    args = __add_datasource__ ("bar", hg, {"x", "y"}, varargin{:});

    if (vertical)
      if (! have_color_spec)
        if (ycols == 1)
          lev = clim(1);
        else
          lev = (i - 1) * (clim(2) - clim(1)) / (ycols - 1) - clim(1);
        endif
        h = patch(xb(:,:,i), yb(:,:,i), "FaceColor", "flat",
                  "cdata", lev, "parent", hg);
      else
        h = patch(xb(:,:,i), yb(:,:,i), "parent", hg);
      endif
    else
      if (! have_color_spec)
        if (ycols == 1)
          lev = clim(1);
        else
          lev = (i - 1) * (clim(2) - clim(1)) / (ycols - 1) - clim(1);
        endif
        h = patch(yb(:,:,i), xb(:,:,i), "FaceColor", "flat",
                  "cdata", lev, "parent", hg);
      else
        h = patch(yb(:,:,i), xb(:,:,i), "parent", hg);
      endif
    endif

    if (i == 1)
      x_axis_range = get (ax, "xlim");
      h_baseline = line (x_axis_range, [0, 0], "color", [0, 0, 0]);
      set (h_baseline, "handlevisibility", "off");
      set (h_baseline, "xliminclude", "off");
      addlistener (ax, "xlim", @update_xlim);
      addlistener (h_baseline, "ydata", @update_baseline);
      addlistener (h_baseline, "visible", @update_baseline);
    endif

    ## Setup the hggroup and listeners
    addproperty ("showbaseline", hg, "radio", "{on}|off");
    addproperty ("basevalue", hg, "data", base_value);
    addproperty ("baseline", hg, "data", h_baseline);

    addlistener (hg, "showbaseline", @show_baseline);
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
    addproperty ("linewidth", hg, "patchlinewidth", get (h, "linewidth"));
    addproperty ("linestyle", hg, "patchlinestyle", get (h, "linestyle"));
    addproperty ("facecolor", hg, "patchfacecolor", get (h, "facecolor"));

    addlistener (hg, "edgecolor", @update_props);
    addlistener (hg, "linewidth", @update_props);
    addlistener (hg, "linestyle", @update_props);
    addlistener (hg, "facecolor", @update_props);

    if (isvector (x))
      addproperty ("xdata", hg, "data", x);
    else
      addproperty ("xdata", hg, "data", x(:, i));
    endif
    addproperty ("ydata", hg, "data", y(:, i));

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);

    addproperty ("bargroup", hg, "data");
    set (tmp, "bargroup", tmp);
    if (! isempty (args))
      set (hg, args{:});
    endif
    if (i == 1)
      set (h_baseline, "parent", get (hg, "parent"));
    endif
  endfor

  update_xlim (ax, []);
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
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (h, "bargroup");
      showbaseline = get (h, "showbaseline");
      for hh = hlist(:)'
        if (hh != h)
          set (hh, "showbaseline", showbaseline);
        endif
      endfor
      set (get (h, "baseline"), "visible", showbaseline);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function move_baseline (h, d)
  b0 = get (h, "basevalue");
  bl = get (h, "baseline");

  if (get (bl, "ydata") != [b0, b0])
    set (bl, "ydata", [b0, b0]);
  endif

  if (strcmpi (get (h, "barlayout"), "grouped"))
    update_data (h, d);
  endif
endfunction

function update_props (h, d)
  kids = get (h, "children");
  set (kids, "edgecolor", get (h, "edgecolor"),
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"),
       "facecolor", get (h, "facecolor"));
endfunction

function update_data (h, d)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (h, "bargroup");
      x = get (h, "xdata");
      if (!isvector (x))
        x = x(:);
      endif
      y = [];
      for hh = hlist(:)'
        ytmp = get (hh, "ydata");
        y = [y ytmp(:)];
      endfor

      [xb, yb] = bar (x, y, get (h, "barwidth"), get (h, "barlayout"),
                      "basevalue", get (h, "basevalue"));
      ny = columns (y);
      vert = strcmpi (get (h, "horizontal"), "off");

      for i = 1:ny
        hp = get (hlist(i), "children");
        if (vert)
          set (hp, "xdata", xb(:,:,i), "ydata", yb(:,:,i));
        else
          set (hp, "xdata", yb(:,:,i), "ydata", xb(:,:,i));
        endif
      endfor
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function update_group (h, d)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (h, "bargroup");
      barwidth = get(h, "barwidth");
      barlayout = get (h, "barlayout");
      horizontal = get (h, "horizontal");

      ## To prevent recursion, only change if modified
      for hh = hlist(:)'
        if (hh != h)
          if (get (hh, "barwidth") != barwidth)
            set (hh, "barwidth", barwidth);
          endif
          if (! strcmpi (get (hh, "barlayout"), barlayout))
            set (hh, "barlayout", barlayout);
          endif
          if (! strcmpi (get (hh, "horizontal"), horizontal))
            set (hh, "horizontal", horizontal);
          endif
        endif
      endfor
      update_data (h, d);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction
