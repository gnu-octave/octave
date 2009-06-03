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

  if (istart < nargin && firstnonnumeric > istart)
    s = varargin{istart};
    if (isempty (s))
      s = 6;
    endif
  else
    s = 6;
  endif
  if (numel (s) == 1)
    ss = s;
    s = repmat (s, numel(x), 1);
  endif

  if (istart < nargin && firstnonnumeric > istart + 1)
    c = varargin{istart + 1};
    if (isvector (c))
      if (columns (c) == 3)
	cc = c;
	c = repmat (c, numel(x), 1);
      else
	c = c(:);
      endif
    elseif (isempty (c))
      cc = __next_line_color__();
      c = repmat (cc, numel(x), 1);
    endif
  elseif (firstnonnumeric == istart + 1 && ischar (varargin{istart + 1}))
    c = varargin{istart + 1};
    firstnonnumeric++;
  else
    cc = __next_line_color__();
    c = repmat (cc, numel(x), 1);
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
	endif
      else
	error ("%s: invalid linespec", fcn);
      endif
    else
      newargs{end+1} = arg;
      if (iarg <= nargin)
	newargs{end+1} = varagin{iarg++};
      endif
    endif
  endwhile

  hg = hggroup ();
  newargs = __add_datasource__ (fcn, hg, {"x", "y", "z", "c", "size"}, 
			     newargs{:});

  addproperty ("xdata", hg, "data", x);
  addproperty ("ydata", hg, "data", y);
  addproperty ("zdata", hg, "data", z);
  if (exist ("cc", "var"))
    addproperty ("cdata", hg, "data", cc);
  else
    addproperty ("cdata", hg, "data", c);
  endif
  if (exist ("ss", "var"))
    addproperty ("sizedata", hg, "data", ss);
  else
    addproperty ("sizedata", hg, "data", s);
  endif
  addlistener (hg, "xdata", @update_data);
  addlistener (hg, "ydata", @update_data);
  addlistener (hg, "zdata", @update_data);
  addlistener (hg, "cdata", @update_data);
  addlistener (hg, "sizedata", @update_data);

  if (ischar (c))
    for i = 1 : numel (x)
      h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
			"faces", 1, "vertices", [x(i), y(i), z(i,:)], 
			"facecolor", "none", "edgecolor", c, "marker", marker, 
			"markersize", s(i), "linestyle", "none");
      if (filled)
	set(h, "markerfacecolor", c); 
      endif
    endfor
  else
    for i = 1 : numel (x)
      h = __go_patch__ (hg, "xdata", x(i), "ydata", y(i), "zdata", z(i,:),
			"faces", 1, "vertices", [x(i), y(i), z(i,:)], 
			"facecolor", "none", "edgecolor", "flat", 
			"cdata", reshape(c(i,:),[1,size(c)(2:end)]), 
			"marker", marker, "markersize", s(i), 
			"linestyle", "none");
      if (filled)
	set(h, "markerfacecolor", "flat"); 
      endif
    endfor
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
  if (numel (x) > 0)
    addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", "none");
    addproperty ("markeredgecolor", hg, "patchmarkeredgecolor", "none");
  else
    addproperty ("markerfacecolor", hg, "patchmarkerfacecolor", 
		 get (h, "markerfacecolor"));
    addproperty ("markeredgecolor", hg, "patchmarkeredgecolor",
		 get (h, "edgecolor"));
  endif
  addlistener (hg, "linewidth", @update_props); 
  addlistener (hg, "marker", @update_props); 
  addlistener (hg, "markerfacecolor", @update_props); 
  addlistener (hg, "markeredgecolor", @update_props);

  if (! isempty (newargs))
    set (hg, newargs{:})
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
