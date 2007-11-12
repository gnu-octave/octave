## Copyright (C) 2007 John W. Eaton, Shai Ayal, Kai Habel
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

## Undocumented internal function.

## __patch__ (p, x, y, c)
## Create patch object from x and y with color c and parent p.
## Return handle to patch object.

## Author: Kai Habel

function [h, fail] = __patch__ (p, varargin)
  fail = false;
  if (nargin < 3)
    fail = true;
    return;
  endif

  iarg = 1;
  have_x = have_z = have_c = have_faces = false;
  if (isnumeric (varargin{1}))
    if (! isnumeric (varargin{2}))
      fail = true;
      return;
    endif

    x = varargin{1};
    y = varargin{2};
    have_x = true;
    iarg += 2;

    if (nargin > 3 && ndims (varargin{3}) == 2 && ndims (x) == 2
	&& isequal (size (varargin{3}), size (x)))
      z = varargin {3};
      have_z = true;
      iarg++;
    endif
  elseif (ischar (varargin{1}) && (strcmp (tolower (varargin{1}), "faces") || 
				strcmp (tolower (varargin{1}), "vertices")))
    if (! isnumeric (varargin{2}))
      fail = true;
      return;
    endif
    
    if (strcmp (tolower (varargin{1}), "faces"))
      faces = varargin{2};
      if (strcmp (tolower (varargin{3}), "vertices"))
	vert = varargin{4};
	have_faces = true;
      endif
    elseif (strcmp (tolower (varargin{3}), "vertices"))
      vert = varargin{2};
      if (strcmp (tolower (varargin{3}), "faces"))
	faces = varargin{4};
	have_faces = true;
      endif
    endif
    if (!have_faces)
      fail = true;
      return;
    else
      iarg += 4;
    endif
  endif

  if ((have_x || have_faces) && nargin > iarg)
    if (isnumeric (varargin{iarg}))
      c = varargin{iarg};
      have_c = true;
      iarg++;

      if (ndims (c) == 3 && size (c, 2) == 1)
	c = permute (c, [1, 3, 2]);
      endif
    elseif (ischar (varargin{iarg}) && rem (nargin - iarg, 2) != 0)
      ## Assume that any additional argument over an even number is color string
      c = tolower (varargin{iarg});
      have_c = true;
      iarg++;
    endif
  endif

  if (rem (nargin - iarg, 2) != 0)
    fail = true;
    return;
  endif

  if (have_x)
    if (isvector (x))
      x = x(:);
      y = y(:);
      if (have_z)
	z = z(:);
      endif
    endif
    [nr, nc] = size (x);
    if (have_z)
      vert = [x(:), y(:), z(:)];
    else
      vert = [x(:), y(:)];
    endif
    faces = reshape (1:numel(x), rows (x), columns (x));
    faces = faces';
  elseif (have_faces)
    nr = size (faces, 2);
    nc = size (faces, 1);
    idx = faces .';
    for i = 1: nc
      t1 = isnan (idx (:,i));
      if (any (t1))
	t2 = find (t1(1:end-1) != t1(2:end))(1);
        idx(t1,i) = idx(t2,i);
      endif
    endfor
    x = vert(:,1)(idx);
    y = vert(:,2)(idx);
    if (size(vert,2) > 2)
      have_z = true;
      z = vert(:,3)(idx);
    endif
  else
    error ("patch: not supported");
  endif

  h = __go_patch__ (p);
  ax = get (h, "parent");

  cargs = {};
  if (have_c)
    if (ischar (c))
      cargs{1} = "facecolor";
      cargs{2} = c;
    elseif (isvector(c) && numel(c) == nc)
      if (isnan (c))
	cargs{1} = "facecolor";
	cargs{2} = [1, 1, 1];
	cargs{3} = "cdata";
	cargs{4} = c;
      elseif (isnumeric (c))
	cargs{1} = "facecolor";
	cargs{2} = "flat";
	cargs{3} = "cdata";
	cargs{4} = c;
	clim = get(ax, "clim");
	if (c(1) < clim(1))
          set (ax, "clim", [c(1), clim(2)])
	endif
	if (c(1) > clim(2))
          set (ax, "clim", [clim(1), c(1)])
	endif
      else
	error ("patch: color value not valid");
      endif
    elseif (size(c, ndims(c)) == 3)
      cargs{1} = "facecolor";
      cargs{2} = "flat";
      cargs{3} = "cdata";
      cargs{4} = c;
    else
      ## Color Vectors

      if (rows (c2) != rows (x) || rows (c2) != length (y))
	error ("patch: size of x, y, and c must be equal")
      else
	cargs{1} = "facecolor";
	cargs{2} = "interp";
	if (abs(max(c2(:)) - min(c2(:))) < eps)
          set (ax, "clim", [c2(1)-1, c2(1)+1])
	else
          set (ax, "clim", [min(c2(:)), max(c2(:))]);
	endif
      endif
    endif
  else
    cargs{1} = "facecolor";
    cargs{2} = [0, 1, 0];
  endif

  set (h, "xdata", x, "ydata", y, "faces", faces, "vertices", vert, ...
       cargs{:}, varargin{iarg:end});
  if (have_z)
    set (h, "zdata", z);
  endif
 
endfunction
