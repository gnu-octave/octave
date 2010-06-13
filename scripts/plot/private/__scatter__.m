## Copyright (C) 2007, 2008, 2009 David Bateman
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
  elseif (firstnonnumeric == istart && ischar (varargin{istart}))
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

  if (numel (x) <= 100)

    ## For small number of points, we'll construct an object for each point.

    if (numel (s) == 1)
      s = repmat (s, numel(x), 1);
    endif

    if (ischar (c) || rows(c) == 1)
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
      for i = 1 : numel (x)
        if (filled)
          h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                            "faces", 1, "vertices", [x(i), y(i), z(i,:)], 
                            "facecolor", "none", "edgecolor", "none", 
                            "marker", marker, "markersize", s(i), 
                            "markeredgecolor", "none", 
                            "markerfacecolor", "flat",
                            "cdata", c(i,:), "linestyle", "none");
        else
          h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                            "faces", 1, "vertices", [x(i), y(i), z(i,:)], 
                            "facecolor", "none", "edgecolor", "none", 
                            "marker", marker, "markersize", s(i), 
                            "markeredgecolor", "flat", 
                            "markerfacecolor", "none",
                            "cdata", c(i,:), "linestyle", "none");

        endif
      endfor
    endif

  else

    ## For larger numbers of points, we split the points by common color.

    vert = [x, y, z];

    if (ischar (c) || rows (c) == 1)
      h = render_size_color (hg, vert, s, c, marker, filled, false); 
    elseif (columns (c) == 1)
      h = render_size_color (hg, vert, s, c, marker, filled, true); 
    else
      [cc, idx] = unique_idx (c, "rows");
      if (isscalar (s))
        for i = 1:rows (x)
          h = render_size_color (hg, vert(idx{i},:), s, cc(i,:), 
                                 marker, filled, true);
        endfor
      else
        for i = 1:rows (x)
          h = render_size_color (hg, vert(idx{i},:), s(idx{i}), cc(i,:), 
                                 marker, filled, true);
        endfor
      endif
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
    if (ischar (c) || rows (c) == 1)
      addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", c);
    else
      addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", "flat");
    endif
  else
    addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", "none");
    if (ischar (c) || rows (c) == 1)
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
    set (hg, newargs{:})
  endif

endfunction

function [y, idx] =  unique_idx (x, byrows)
  if (nargin == 2)
    [xx, idx] = sortrows (x);
    n = rows (x);
    jdx = find (any (xx(1:n-1,:) != xx(2:n,:), 2));
    jdx(end+1) = n;
    y = xx(jdx,:);
  else
    [xx, idx] = sort (x);
    n = length (x);
    jdx = find (xx(1:n-1,:) != xx(2:n,:));
    jdx(end+1) = n;
    y = xx(jdx);
  endif

  if (nargin == 2 || columns (x) == 1)
    idx = mat2cell (idx, diff ([0; jdx]), 1);
  else
    idx = mat2cell (idx, 1, diff ([0, jdx]));
  endif
endfunction

function h = render_size_color(hg, vert, s, c, marker, filled, isflat)
  if (isscalar (s))
    x = vert(:,1);
    y = vert(:,2);
    z = vert(:,3:end);
    if (ischar (c) || !isflat)
      if (filled)
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1, "vertices", vert, 
                          "facecolor", "none", "edgecolor", "none", 
                          "marker", marker, 
                          "markeredgecolor", "none", 
                          "markerfacecolor", c,
                          "markersize", s, "linestyle", "none");
      else
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1, "vertices", vert, 
                          "facecolor", "none", "edgecolor", "none", 
                          "marker", marker, 
                          "markeredgecolor", c, 
                          "markerfacecolor", "none",
                          "markersize", s, "linestyle", "none");
      endif
    else
      if (filled)
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1, "vertices", vert,
                          "facecolor", "none", "edgecolor", "none", 
                          "marker", marker, "markersize", s, 
                          "markeredgecolor", "none", 
                          "markerfacecolor", "flat",
                          "cdata", c, "linestyle", "none");
      else
        h = __go_patch__ (hg, "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1, "vertices", vert,
                          "facecolor", "none", "edgecolor", "none", 
                          "marker", marker, "markersize", s, 
                          "markeredgecolor", "flat", 
                          "markerfacecolor", "none",
                          "cdata", c, "linestyle", "none");
      endif
    endif
  else
    ## FIXME: round the size to one decimal place. It's not quite right, though.
    [ss, idx] = unique_idx (ceil (s*10) / 10);
    for i = 1:rows (ss)
      h = render_size_color (hg, vert(idx{i},:), ss(i), c, 
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
         "edgecolor", ec)
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
             reshape(c1(i,:),[1, size(c1)(2:end)]), "markersize", size1(i));
      endfor
    else
      for i = 1 : length (hlist)
        set (hlist(i), "vertices", [x1(i), y1(i), z1(i)], "cdata", 
             reshape(c1(i,:),[1, size(c1)(2:end)]), "markersize", size1(i));
      endfor
    endif
  endif
endfunction
