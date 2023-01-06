########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn {} {@var{hg} =} __gnuplot_scatter__ (@dots{})
## Undocumented internal function.
## @end deftypefn

function hg = __gnuplot_scatter__ (hax, fcn, x, y, z, c, s, marker, filled, newargs)

if (isempty (c))
  c = __next_line_color__ ();
endif

## Must occur after __next_line_color__ in order to work correctly.
hg = hggroup (hax, "__appdata__", struct ("__creator__", "__scatter__"));
newargs = __add_datasource__ (fcn, hg, {"x", "y", "z", "c", "size"},
                              newargs{:});

addproperty ("xdata", hg, "data", x);
addproperty ("ydata", hg, "data", y);
addproperty ("zdata", hg, "data", z);
if (ischar (c))
  ## For single explicit color, cdata is unused
  addproperty ("cdata", hg, "data", []);
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
s = sqrt (s);  # size adjustment for visual compatibility w/Matlab

if (numel (x) <= 100)

  ## For small number of points, we'll construct an object for each point.

  if (numel (s) == 1)
    s = repmat (s, numel (x), 1);
  endif

  if (one_explicit_color)
    for i = 1 : numel (x)
      if (filled)
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                      "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                      "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                      "marker", marker,  "markersize", s(i),
                      "markeredgecolor", c, "markerfacecolor", c,
                      "linestyle", "none");
      else
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                      "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                      "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                      "marker", marker,  "markersize", s(i),
                      "markeredgecolor", c, "markerfacecolor", "none",
                      "linestyle", "none");
      endif
    endfor
  else
    if (rows (c) == 1)
      c = repmat (c, rows (x), 1);
    endif
    for i = 1 : numel (x)
      if (filled)
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                      "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                      "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                      "marker", marker, "markersize", s(i),
                      "markeredgecolor", "none",
                      "markerfacecolor", "flat",
                      "cdata", c(i,:), "facevertexcdata", c(i,:),
                      "linestyle", "none");
      else
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                      "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
                      "faces", 1, "vertices", [x(i), y(i), z(i,:)],
                      "marker", marker, "markersize", s(i),
                      "markeredgecolor", "flat",
                      "markerfacecolor", "none",
                      "cdata", c(i,:), "facevertexcdata", c(i,:),
                      "linestyle", "none");
      endif
    endfor
  endif

else

  ## For larger numbers of points, we use one single object.
  vert = [x, y, z];
  render_size_color (hg, vert, s, c, marker, filled, true);

endif

if (! ischar (c) && rows (c) > 1)
  ax = get (hg, "parent");
  clim = get (ax, "clim");
  if (min (c(:)) < clim(1))
    clim(1) = min (c(:));
    set (ax, "clim", clim);
  endif
  if (max (c(:)) > clim(2))
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

## Matlab property, although Octave does not implement it.
addproperty ("hittestarea", hg, "radio", "on|{off}", "off");

if (! isempty (newargs))
  set (hg, newargs{:});
endif

endfunction

function render_size_color (hg, vert, s, c, marker, filled, isflat)

  if (isempty (c))
    c = __next_line_color__ ();
  endif

  if (isscalar (s))
    x = vert(:,1);
    y = vert(:,2);
    z = vert(:,3:end);
    toolkit = get (ancestor (hg, "figure"), "__graphics_toolkit__");
    ## Does gnuplot only support triangles with different vertex colors ?
    ## FIXME: Verify gnuplot can only support one color.  If RGB triplets
    ##        can be assigned to each vertex, then fix __gnuplot_draw_axes__.m
    gnuplot_hack = (numel (x) > 1 && columns (c) == 3
                    && strcmp (toolkit, "gnuplot"));
    if (ischar (c) || ! isflat || gnuplot_hack)
      if (filled)
        ## "facecolor" and "edgecolor" must be set before any other properties
        ## to skip co-planarity check (see bug #55751).
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                          "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel (x), "vertices", vert,
                          "marker", marker,
                          "markeredgecolor", "none",
                          "markerfacecolor", c(1,:),
                          "markersize", s, "linestyle", "none");
      else
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                          "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel (x), "vertices", vert,
                          "marker", marker,
                          "markeredgecolor", c(1,:),
                          "markerfacecolor", "none",
                          "markersize", s, "linestyle", "none");
      endif
    else
      if (filled)
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                          "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel (x), "vertices", vert,
                          "marker", marker, "markersize", s,
                          "markeredgecolor", "none",
                          "markerfacecolor", "flat",
                          "cdata", c, "facevertexcdata", c,
                          "linestyle", "none");
      else
        __go_patch__ (hg, "facecolor", "none", "edgecolor", "none",
                          "xdata", x, "ydata", y, "zdata", z,
                          "faces", 1:numel (x), "vertices", vert,
                          "marker", marker, "markersize", s,
                          "markeredgecolor", "flat",
                          "markerfacecolor", "none",
                          "cdata", c, "facevertexcdata", c,
                          "linestyle", "none");
      endif
    endif
  else
    ## Round size to one decimal place.
    [ss, ~, s_to_ss] = unique (ceil (s*10) / 10);
    for i = 1:rows (ss)
      idx = (i == s_to_ss);
      render_size_color (hg, vert(idx,:), ss(i), c,
                             marker, filled, isflat);
    endfor
  endif

endfunction

function update_props (h, ~)

  lw = get (h, "linewidth");
  m  = get (h, "marker");
  fc = get (h, "markerfacecolor");
  ec = get (h, "markeredgecolor");
  kids = get (h, "children");

  set (kids, "linewidth", lw, "marker", m,
             "markerfacecolor", fc, "markeredgecolor", ec);

endfunction

## FIXME: This callback routine doesn't handle the case where N > 100.
function update_data (h, ~)

  x = get (h, "xdata");
  y = get (h, "ydata");
  z = get (h, "zdata");
  if (numel (x) > 100)
    error ("scatter: cannot update data with more than 100 points.  Call scatter (x, y, ...) with new data instead.");
  endif
  c = get (h, "cdata");
  one_explicit_color = ischar (c) || isequal (size (c), [1, 3]);
  if (! one_explicit_color)
    if (rows (c) == 1)
      c = repmat (c, numel (x), 1);
    endif
  endif
  filled = ! strcmp (get (h, "markerfacecolor"), "none");
  s = get (h, "sizedata");
  ## Size adjustment for visual compatibility with Matlab.
  s = sqrt (s);
  if (numel (s) == 1)
    s = repmat (s, numel (x), 1);
  endif
  hlist = get (h, "children");

  if (one_explicit_color)
    if (filled)
      if (isempty (z))
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i)],
                         "markersize", s(i),
                         "markeredgecolor", c, "markerfacecolor", c);

        endfor
      else
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i), z(i)],
                         "markersize", s(i),
                         "markeredgecolor", c, "markerfacecolor", c);
        endfor
      endif
    else
      if (isempty (z))
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i)],
                         "markersize", s(i),
                         "markeredgecolor", c, "markerfacecolor", "none");

        endfor
      else
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i), z(i)],
                         "markersize", s(i),
                         "markeredgecolor", c, "markerfacecolor", "none");
        endfor
      endif
    endif
  else
    if (filled)
      if (isempty (z))
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i)],
                         "markersize", s(i),
                         "markeredgecolor", "none", "markerfacecolor", "flat",
                         "cdata", reshape (c(i,:),[1, size(c)(2:end)]),
                         "facevertexcdata", c(i,:));
        endfor
      else
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i), z(i)],
                         "markersize", s(i),
                         "markeredgecolor", "none", "markerfacecolor", "flat",
                         "cdata", reshape (c(i,:),[1, size(c)(2:end)]),
                         "facevertexcdata", c(i,:));
        endfor
      endif
    else
      if (isempty (z))
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i)],
                         "markersize", s(i),
                         "markeredgecolor", "flat", "markerfacecolor", "none",
                         "cdata", reshape (c(i,:),[1, size(c)(2:end)]),
                         "facevertexcdata", c(i,:));
        endfor
      else
        for i = 1 : length (hlist)
          set (hlist(i), "vertices", [x(i), y(i), z(i)],
                         "markersize", s(i),
                         "markeredgecolor", "flat", "markerfacecolor", "none",
                         "cdata", reshape (c(i,:),[1, size(c)(2:end)]),
                         "facevertexcdata", c(i,:));
        endfor
      endif
    endif
  endif

endfunction

