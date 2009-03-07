## Copyright (C) 2007, 2008, 2009 John W. Eaton, Shai Ayal, Kai Habel
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
## @deftypefn {Function File} {[@var{h}, @var{fail}] =} __patch__ (@var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

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
	&& size_equal(x, varargin{3}) && !ischar(varargin{3}))
      z = varargin{3};
      have_z = true;
      iarg++;
    endif
  elseif (ischar (varargin{1})
	  && (strcmpi (varargin{1}, "faces")
	      || strcmpi (varargin{1}, "vertices")))
    if (! isnumeric (varargin{2}))
      fail = true;
      return;
    endif
    
    if (strcmpi (varargin{1}, "faces"))
      faces = varargin{2};
      if (strcmpi (varargin{3}, "vertices"))
	vert = varargin{4};
	have_faces = true;
      endif
    elseif (strcmpi (varargin{1}, "vertices"))
      vert = varargin{2};
      if (strcmpi (varargin{3}, "faces"))
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
      ## Assume that any additional argument over an even number is
      ## color string.
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
    t1 = isnan (idx);
    if (any (t1(:)))
      t2 = find (t1 != t1([2:end,end],:));
      idx (t1) = idx (t2 (cell2mat (cellfun (@(x) x(1)*ones(1,x(2)),
		mat2cell ([1 : nc; sum(t1)], 2, ones(1,nc)), 
					     "UniformOutput", false))));
    endif
    x = reshape (vert(:,1)(idx), size (idx));
    y = reshape (vert(:,2)(idx), size (idx));
    if (size(vert,2) > 2)
      have_z = true;
      z = reshape (vert(:,3)(idx), size (idx));
    endif
  else
    error ("patch: not supported");
  endif

  cargs = {};
  if (have_c)
    if (ischar (c))
      cargs{1} = "facecolor";
      cargs{2} = c;
    elseif (isvector (c) && numel (c) == nc)
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
      else
	error ("patch: color value not valid");
      endif
    elseif (size (c, ndims (c)) == 3)
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
      endif
    endif
  else
    cargs{1} = "facecolor";
    cargs{2} = [0, 1, 0];
  endif

  h = __go_patch__ (p, "xdata", x, "ydata", y, "faces", faces, 
		    "vertices", vert, cargs{:}, varargin{iarg:end});
  if (have_z)
    set (h, "zdata", z);
  endif
 
endfunction
