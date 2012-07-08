## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn {Function File} {@var{hg} =} __scatter__ (@dots{})
## Undocumented internal function.
## @end deftypefn

function hg = __scatter__ (varargin)

  h = varargin{1};
  nd = varargin{2};
  fcn = varargin{3};
  x = varargin{4}(:);
  y = varargin{5}(:);
  istart = 6;

  if (nd == 3)
    z = varargin{6}(:);
    idx = isnan(x) | isnan (y) | isnan (z);
    x (idx) = [];
    y (idx) = [];
    z (idx) = [];
    istart = 7;
  else
    idx = isnan(x) | isnan (y);
    x (idx) = [];
    y (idx) = [];
    z = zeros (length (x), 0);
  endif

  firstnonnumeric = Inf;
  for i = istart:nargin
    if (! isnumeric (varargin{i}))
      firstnonnumeric = i;
      break;
    endif
  endfor

  if (istart <= nargin)
    s = varargin{istart};
    if (isempty (s) || ischar (s))
      s = 6;
    endif
    if (! ischar (varargin{istart}))
      istart++;
    endif
  else
    s = 6;
  endif

  if (istart <= nargin && firstnonnumeric > istart)
    c = varargin{istart};
    if (isvector (c))
      if (columns (c) != 3)
        c = c(:);
      endif
    endif
  elseif (firstnonnumeric == istart && ischar (varargin{istart})
          && ! strcmpi (varargin{istart}, "filled"))
    c = varargin{istart};
    firstnonnumeric++;
  else
    c = [];
  endif

  newargs = {};
  filled = false;
  have_marker = false;
  marker = "o";
  iarg = firstnonnumeric;
  while (iarg <= nargin)
    arg = varargin{iarg++};
    if (ischar (arg) && strncmpi (arg, "filled", 6))
      filled = true;
    elseif ((ischar (arg) || iscell (arg)) && ! have_marker)
      [linespec, valid] = __pltopt__ (fcn, arg, false);
      if (valid)
        have_marker = true;
        marker = linespec.marker;
        if (strncmp (marker, "none", 4))
          marker = "o";
        elseif (isempty (marker))
          have_marker = false;
          [dummy, marker] = __next_line_style__ ();
        endif
      else
        error ("%s: invalid linespec", fcn);
      endif
    else
      newargs{end+1} = arg;
      if (iarg <= nargin)
        newargs{end+1} = varargin{iarg++};
      endif
    endif
  endwhile

  if (isempty (c))
    c = __next_line_color__();
  endif

  hg = hggroup ();
  newargs = __add_datasource__ (fcn, hg, {"x", "y", "z", "c", "size"},
                             newargs{:});

  addproperty ("xdata", hg, "data", x);
  addproperty ("ydata", hg, "data", y);
  addproperty ("zdata", hg, "data", z);
  if (ischar (c))
    addproperty ("cdata", hg, "data", __color_str_rgb__ (c));
  else
    addproperty ("cdata", hg, "data", c);
  endif
  addproperty ("sizedata", hg, "data", s);
  addlistener (hg, "xdata", @update_data);
  addlistener (hg, "ydata", @update_data);
  addlistener (hg, "zdata", @update_data);
  addlistener (hg, "cdata", @update_data);
  addlistener (hg, "sizedata", @update_data);

  one_explicit_color = ischar (c) || isequal (size (c), [1, 3]);

  if (numel (x) <= 100)

    ## For small number of points, we'll construct an object for each point.

    if (numel (s) == 1)
      s = repmat (s, numel(x), 1);
    endif

    if (one_explicit_color)
      for i = 1 : numel (x)
        if (filled)
          h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                            "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                            "facecolor", "none", "edgecolor", "none",
                            "marker", marker,  "markersize", s(i),
                            "markeredgecolor", c, "markerfacecolor", c,
                            "linestyle", "none");
        else
          h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                            "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                            "facecolor", "none", "edgecolor", "none",
                            "marker", marker,  "markersize", s(i),
                            "markeredgecolor", c, "markerfacecolor", "none",
                            "linestyle", "none");
        endif
      endfor
    else
      if (rows (c) == 1)
        c = ones (rows (x), 1) * c;
      endif
      for i = 1 : numel (x)
        if (filled)
          h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                            "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                            "facecolor", "none", "edgecolor", "none",
                            "marker", marker, "markersize", s(i),
                            "markeredgecolor", "none",
                            "markerfacecolor", "flat",
                            "cdata", c(i,:), "facevertexcdata", c(i,:),
                            "linestyle", "none");
        else
          h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                            "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                            "facecolor", "none", "edgecolor", "none",
                            "marker", marker, "markersize", s(i),
                            "markeredgecolor", "flat",
                            "markerfacecolor", "none",
                            "cdata", c(i,:), "facevertexcdata", c(i,:),
                            "linestyle", "none");

        endif
      endfor
    endif

  else

    ## For larger numbers of points, we split the points by common color.

    vert = [x, y, z];
    if (one_explicit_color)
      h = render_size_color (hg, vert, s, c, marker, filled, true);
    else
      if (rows (c) == 1)
        c = ones (rows (x), 1) * c;
      endif
      ## We want to group points by colour. So first get all the unique colours
      [cc, ~, c_to_cc] = unique (c, "rows");

      for i = 1:rows (cc)
        ## Now for each possible unique colour, get the logical index of
        ## points that correspond to that colour
        idx = (i == c_to_cc);
        if (isscalar (s))
          h = render_size_color (hg, vert(idx, :), s, c(idx,:),
                                 marker, filled, true);
        else
          h = render_size_color (hg, vert(idx, :), s(idx), c(idx,:),
                                 marker, filled, true);
        endif
      endfor

    endif
  endif

  if (! ischar (c) && rows (c) > 1)
    ax = get (hg, "parent");
    clim = get (ax, "clim");
    if (min(c(:)) < clim(1))
      clim(1) = min(c(:));
      set (ax, "clim", clim);
    endif
    if (max(c(:)) > clim(2))
      set (ax, "clim", [clim(1), max(c(:))]);
    endif
  endif

  addproperty ("linewidth", hg, "patchlinewidth", 0.5);
  addproperty ("marker", hg, "patchmarker", marker);
  if (filled)
    addproperty ("markeredgecolor", hg, "patchmarkeredgecolor", "none");
    if (one_explicit_color)
      addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", c);
    else
      addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", "flat");
    endif
  else
    addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", "none");
    if (one_explicit_color)
      addproperty ("markeredgecolor", hg, "patchmarkeredgecolor", c);
    else
      addproperty ("markeredgecolor", hg, "patchmarkeredgecolor", "flat");
    endif
  endif
  addlistener (hg, "linewidth", @update_props);
  addlistener (hg, "marker", @update_props);
  addlistener (hg, "markerfacecolor", @update_props);
  addlistener (hg, "markeredgecolor", @update_props);

  if (! isempty (newargs))
    set (hg, newargs{:});
  endif

endfunction

function h = render_size_color(hg, vert, s, c, marker, filled, isflat)
  if (isscalar (s))
    x = vert(:,1);
    y = vert(:,2);
    z = vert(:,3:end);
    toolkit = get (ancestor (hg, "figure"), "__graphics_toolkit__");
    ## Does gnuplot only support triangles with different vertex colors ?
    ## TODO - Verify gnuplot can only support one color. If RGB triplets
    ##        can be assigned to each vertex, then fix __go_draw_axe__.m
    gnuplot_hack = (numel (x) > 1 && columns (c) == 3
                    && strcmp (toolkit, "gnuplot"));
    if (ischar (c) || ! isflat || gnuplot_hack)
      if (filled)
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel(x), "vertices", vert,
                          "facecolor", "none", "edgecolor", "none",
                          "marker", marker,
                          "markeredgecolor", "none",
                          "markerfacecolor", c(1,:),
                          "markersize", s, "linestyle", "none");
      else
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel(x), "vertices", vert,
                          "facecolor", "none", "edgecolor", "none",
                          "marker", marker,
                          "markeredgecolor", c(1,:),
                          "markerfacecolor", "none",
                          "markersize", s, "linestyle", "none");
      endif
    else
      if (filled)
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel(x), "vertices", vert,
                          "facecolor", "none", "edgecolor", "none",
                          "marker", marker, "markersize", s,
                          "markeredgecolor", "none",
                          "markerfacecolor", "flat",
                          "cdata", c, "facevertexcdata", c,
                          "linestyle", "none");
      else
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel(x), "vertices", vert,
                          "facecolor", "none", "edgecolor", "none",
                          "marker", marker, "markersize", s,
                          "markeredgecolor", "flat",
                          "markerfacecolor", "none",
                          "cdata", c, "facevertexcdata", c,
                          "linestyle", "none");
      endif
    endif
  else
    ## FIXME: round the size to one decimal place. It's not quite right, though.
    [ss, ~, s_to_ss] = unique (ceil (s*10) / 10);
    for i = 1:rows (ss)
      idx = (i == s_to_ss);
      h = render_size_color (hg, vert(idx,:), ss(i), c,
                             marker, filled, isflat);
    endfor
  endif
endfunction

function update_props (h, d)
  lw = get (h, "linewidth");
  m = get (h, "marker");
  fc = get (h, "markerfacecolor");
  ec = get (h, "markeredgecolor");
  kids = get (h, "children");

  for i = 1 : numel (kids)
    set (kids (i), "linewidth", lw, "marker", m, "markerfacecolor", fc,
         "edgecolor", ec);
  endfor
endfunction

function update_data (h, d)
  x1 = get (h, "xdata");
  y1 = get (h, "ydata");
  z1 = get (h, "zdata");
  c1 = get (h, "cdata");
  if (!ischar (c1) && rows (c1) == 1)
    c1 = repmat (c1, numel (x1), 1);
  endif
  size1 = get (h, "sizedata");
  if (numel (size1) == 1)
    size1 = repmat (size1, numel (x1), 1);
  endif
  hlist = get (h, "children");
  if (ischar (c1))
    if (isempty (z1))
      for i = 1 : length (hlist)
        set (hlist(i), "vertices", [x1(i), y1(i)], "cdata", c1,
             "markersize", size1(i));
      endfor
    else
      for i = 1 : length (hlist)
        set (hlist(i), "vertices", [x1(i), y1(i), z1(i)], "cdata", c1,
             "markersize", size1(i));
      endfor
    endif
  else
    if (isempty (z1))
      for i = 1 : length (hlist)
        set (hlist(i), "vertices", [x1(i), y1(i)], "cdata",
             reshape(c1(i,:),[1, size(c1)(2:end)]),
             "facevertexcdata", c1(i,:),
             "markersize", size1(i));
      endfor
    else
      for i = 1 : length (hlist)
        set (hlist(i), "vertices", [x1(i), y1(i), z1(i)], "cdata",
             reshape(c1(i,:),[1, size(c1)(2:end)]),
             "facevertexcdata", c1(i,:),
             "markersize", size1(i));
      endfor
    endif
  endif
endfunction
