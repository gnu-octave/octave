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
## @deftypefn {Function File} {@var{hg} =} __quiver__ (@dots{})
## Undocumented internal function.
## @end deftypefn

function hg = __quiver__ (varargin)

  h = varargin{1};
  is3d = varargin{2};

  autoscale = 0.9;
  arrowsize = 0.2;

  firstnonnumeric = Inf;
  for i = 3:nargin
    if (! isnumeric (varargin{i}))
      firstnonnumeric = i;
      break;
    endif
  endfor

  ioff = 3;
  if (nargin < (6 + is3d) || firstnonnumeric < (6 + is3d))
    u = varargin{ioff++};
    v = varargin{ioff++};
    if (is3d)
      w = varargin{ioff++};
      [x, y, z] = meshgrid (1:size(u,2), 1:size(u,1), 1:max(size(w)));
    else
      [x, y] = meshgrid (1:size(u,2), 1:size(u,1));
    endif
    if (nargin >= ioff && isnumeric (varargin{ioff})
        && isscalar (varargin{ioff}))
      autoscale = varargin{ioff++};
    endif
  else
    x = varargin{ioff++};
    y = varargin{ioff++};
    if (is3d)
      z = varargin{ioff++};
    endif
    u = varargin{ioff++};
    v = varargin{ioff++};
    if (is3d)
      w = varargin{ioff++};
      if (isvector (x) && isvector (y) && isvector (z)
          && (! isvector (u) || ! isvector (v) || ! isvector(w)))
        [x, y, z] = meshgrid (x, y, z);
      endif
    else
      if (isvector (x) && isvector (y) && (! isvector (u) || ! isvector (v)))
        [x, y] = meshgrid (x, y);
      endif
    endif
    if (nargin >= ioff && isnumeric (varargin{ioff})
        && isscalar (varargin{ioff}))
      autoscale = varargin{ioff++};
    endif
  endif

  have_filled = false;
  have_line_spec = false;
  args = {};
  while (ioff <= nargin)
    arg = varargin{ioff++};
    if (ischar (arg) && strncmpi (arg, "filled", 6))
      have_filled = true;
    elseif ((ischar (arg) || iscell (arg))
            && ! have_line_spec)
      [linespec, valid] = __pltopt__ ("quiver", arg, false);
      if (valid)
        have_line_spec = true;
        if (strncmp (linespec.linestyle, "none", 4))
          linespec.linestyle = "-";
        endif
      else
        args {end + 1} = arg;
        if (ioff <= nargin)
          args {end + 1} = varargin{ioff++};
        endif
      endif
    else
      args {end + 1} = arg;
      if (ioff <= nargin)
        args {end + 1} = varargin{ioff++};
      endif
    endif
  endwhile

  if (autoscale && numel (u) > 1)
    ## Scale the arrows to fit in the grid
    if (isvector (x))
      ny = nx = length (x);
    else
      [nx, ny] = size (x);
    endif
    dx = (max(x(:)) - min(x(:))) ./ nx;
    dy = (max(y(:)) - min(y(:))) ./ ny;
    if (is3d)
      dz = (max(z(:)) - min(z(:))) ./ max (size (z));
      len = max (sqrt (u(:).^2 + v(:).^2 + w(:).^2));
    else
      dz = 0;
      len = max (sqrt (u(:).^2 + v(:).^2));
    endif
    if (len > 0)
      sd = sqrt (dx.^2 + dy.^2 + dz.^2) / len;
      if (sd != 0)
        s = sqrt(2) * autoscale * sd;
      else # special case of identical points with multiple vectors
        s = autoscale;
      endif
      uu = s * u;
      vv = s * v;
      if (is3d)
        ww = s*w;
      endif
    endif
  else
    uu = u;
    vv = v;
    if (is3d)
      ww = w;
    endif
  endif

  hstate = get (h, "nextplot");
  unwind_protect
    hg = hggroup ();
    if (is3d)
      args = __add_datasource__ ("quiver3", hg,
                                 {"x", "y", "z", "u", "v", "w"}, args{:});
    else
      args = __add_datasource__ ("quiver", hg,
                                 {"x", "y", "z", "u", "v", "w"}, args{:});
    endif
    hold on;

    addproperty ("xdata", hg, "data", x);
    addproperty ("ydata", hg, "data", y);

    addproperty ("udata", hg, "data", u);
    addproperty ("vdata", hg, "data", v);
    if (is3d)
      addproperty ("zdata", hg, "data", z);
      addproperty ("wdata", hg, "data", w);
    else
      addproperty ("zdata", hg, "data", []);
      addproperty ("wdata", hg, "data", []);
    endif

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);
    addlistener (hg, "zdata", @update_data);
    addlistener (hg, "udata", @update_data);
    addlistener (hg, "vdata", @update_data);
    addlistener (hg, "wdata", @update_data);

    x = x(:);
    y = y(:);
    xend = x + uu(:);
    yend = y + vv(:);
    if (is3d)
      z = z(:);
      zend = z + ww(:);
    endif

    if (have_line_spec)
      if (is3d)
        h1 = plot3 ([x.'; xend.'; NaN(1, length (x))](:),
                    [y.'; yend.'; NaN(1, length (y))](:),
                    [z.'; zend.'; NaN(1, length (z))](:),
                    "linestyle", linespec.linestyle,
                    "color", linespec.color, "parent", hg);
      else
        h1 = plot ([x.'; xend.'; NaN(1, length (x))](:),
                   [y.'; yend.'; NaN(1, length (y))](:),
                   "linestyle", linespec.linestyle,
                    "color", linespec.color, "parent", hg);
      endif
    else
      if (is3d)
        h1 = plot3 ([x.'; xend.'; NaN(1, length (x))](:),
                    [y.'; yend.'; NaN(1, length (y))](:),
                    [z.'; zend.'; NaN(1, length (z))](:),
                    "color", "black", "parent", hg);
      else
        h1 = plot ([x.'; xend.'; NaN(1, length (x))](:),
                   [y.'; yend.'; NaN(1, length (y))](:),
                   "parent", hg);
      endif
    endif

    xtmp = x + uu(:) .* (1 - arrowsize);
    ytmp = y + vv(:) .* (1 - arrowsize);
    xarrw1 = xtmp + (y - yend) * arrowsize / 3;
    xarrw2 = xtmp - (y - yend) * arrowsize / 3;
    yarrw1 = ytmp - (x - xend) * arrowsize / 3;
    yarrw2 = ytmp + (x - xend) * arrowsize / 3;
    if (is3d)
      zarrw1 = zarrw2 = zend - ww(:) * arrowsize;
    endif

    if (have_line_spec)
      if (isfield (linespec, "marker")
          && ! strncmp (linespec.marker, "none", 4))
        if (is3d)
          h2 = plot3 ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                      [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                      [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:),
                      "linestyle", "none", "parent", hg);
        else
          h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                     [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                     "linestyle", "none", "parent", hg);
        endif
      else
        if (is3d)
          h2 = plot3 ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                      [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                      [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:),
                      "linestyle", linespec.linestyle,
                      "color", linespec.color, "parent", hg);
        else
          h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                     [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                     "linestyle", linespec.linestyle,
                      "color", linespec.color, "parent", hg);
        endif
      endif
    elseif (is3d)
      h2 = plot3 ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                  [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                  [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:),
                  "color", "black", "parent", hg);
    else
      h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                 [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                 "parent", hg);
    endif

    if (! have_line_spec
        || (isfield (linespec, "marker")
            && strncmp (linespec.marker, "none", 4)))
      if (is3d)
        h3 = plot3 (x, y, z, "linestyle", "none", "marker", "none",
                    "parent", hg);
      else
        h3 = plot (x, y, "linestyle", "none", "marker", "none", "parent", hg);
      endif
    else
      if (is3d)
        h3 = plot3 (x, y, z, "linestyle", "none", "marker", linespec.marker,
                    "parent", hg);
      else

        h3 = plot (x, y, "linestyle", "none", "marker", linespec.marker,
                   "parent", hg);
      endif
    endif
    if (have_filled)
      ## FIXME gnuplot doesn't respect the markerfacecolor field
      set (h3, "markerfacecolor", get (h1, "color"));
    endif

    ## Set up the hggroup properties and listeners
    if (autoscale)
      addproperty ("autoscale", hg, "radio", "{on}|off", "on");
      addproperty ("autoscalefactor", hg, "data", autoscale);
    else
      addproperty ("autoscale", hg, "radio", "{on}|off", "off");
      addproperty ("autoscalefactor", hg, "data", 1.0);
    endif
    addlistener (hg, "autoscale", @update_data);
    addlistener (hg, "autoscalefactor", @update_data);

    addproperty ("maxheadsize", hg, "data", arrowsize);
    addlistener (hg, "maxheadsize", @update_data);

    addproperty ("showarrowhead", hg, "radio", "{on}|off", "on");
    addlistener (hg, "showarrowhead", @update_props);

    addproperty ("color", hg, "linecolor", get (h1, "color"));
    addproperty ("linewidth", hg, "linelinewidth", get (h1, "linewidth"));
    addproperty ("linestyle", hg, "linelinestyle", get (h1, "linestyle"));
    addproperty ("marker", hg, "linemarker", get (h3, "marker"));
    addproperty ("markerfacecolor", hg, "linemarkerfacecolor",
                 get (h3, "markerfacecolor"));
    addproperty ("markersize", hg, "linemarkersize", get (h3, "markersize"));

    addlistener (hg, "color", @update_props);
    addlistener (hg, "linewidth", @update_props);
    addlistener (hg, "linestyle", @update_props);
    addlistener (hg, "marker", @update_props);
    addlistener (hg, "markerfacecolor", @update_props);
    addlistener (hg, "markersize", @update_props);

    if (! isempty (args))
      set (hg, args{:});
    endif
  unwind_protect_cleanup
    set (h, "nextplot", hstate);
  end_unwind_protect

endfunction

function update_data (h, d)
  x = get (h, "xdata");
  y = get (h, "ydata");
  z = get (h, "zdata");

  u = get (h, "udata");
  v = get (h, "vdata");
  w = get (h, "wdata");

  s = get (h, "autoscalefactor");
  arrowsize = get (h, "maxheadsize");

  kids = get (h, "children");

  if (isempty (z) || isempty (w))
    is3d = false;
  else
    is3d = true;
  endif

  if (strcmpi (get (h, "autoscale"), "on") && s != 0)
    ## Scale the arrows to fit in the grid
    if (isvector (x))
      ny = nx = length (x);
    else
      [nx, ny] = size (x);
    endif
    dx = (max(x(:)) - min(x(:))) ./ nx;
    dy = (max(y(:)) - min(y(:))) ./ ny;
    if (is3d)
      dz = (max(z(:)) - min(z(:))) ./ max (size (z));
      len = max (sqrt (u(:).^2 + v(:).^2 + w(:).^2));
    else
      dz = 0;
      len = max (sqrt (u(:).^2 + v(:).^2));
    endif
    if (len > 0)
      sd = sqrt (dx.^2 + dy.^2 + dz.^2) / len;
      if (sd != 0)
        s *= sqrt(2) * sd;
      endif
      u = s * u;
      v = s * v;
      if (is3d)
        w = s*w;
      endif
    endif
  endif

  x = x(:);
  y = y(:);
  xend = x + u(:);
  yend = y + v(:);
  if (is3d)
    z = z(:);
    zend = z + w(:);
  endif

  set (kids (3), "xdata", [x.'; xend.'; NaN(1, length (x))](:));
  set (kids (3), "ydata", [y.'; yend.'; NaN(1, length (y))](:));
  if (is3d)
    set (kids (3), "zdata", [z.'; zend.'; NaN(1, length (z))](:));
  endif

  xtmp = x + u(:) .* (1 - arrowsize);
  ytmp = y + v(:) .* (1 - arrowsize);
  xarrw1 = xtmp + (y - yend) * arrowsize / 3;
  xarrw2 = xtmp - (y - yend) * arrowsize / 3;
  yarrw1 = ytmp - (x - xend) * arrowsize / 3;
  yarrw2 = ytmp + (x - xend) * arrowsize / 3;
  if (is3d)
    zarrw1 = zarrw2 = zend - w(:) * arrowsize;
  endif

  set (kids (2), "xdata", [x.'; xend.'; NaN(1, length (x))](:));
  set (kids (2), "ydata", [y.'; yend.'; NaN(1, length (y))](:));
  if (is3d)
    set (kids (2), "zdata", [z.'; zend.'; NaN(1, length (z))](:));
  endif

  set (kids (2), "xdata", [xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:));
  set (kids (2), "ydata", [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:));
  if (is3d)
    set (kids (2), "zdata", [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:));
  endif

  set (kids (1), "xdata", x);
  set (kids (1), "ydata", y);
  if (is3d)
    set (kids (1), "zdata", z);
  endif

endfunction

function update_props (h, d)
  kids = get (h, "children");

  set (kids(3), "color", get (h, "color"),
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"));
  set (kids(2), "color", get (h, "color"),
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"));
  if (strcmpi (get (h, "showarrowhead"), "on"))
    set (kids (2), "visible", "on");
  else
    set (kids (2), "visible", "off");
  endif
  set (kids(1), "color", get (h, "color"),
       "marker", get (h, "marker"),
       "markerfacecolor", get (h, "markerfacecolor"),
       "markersize", get (h, "markersize"));
endfunction
