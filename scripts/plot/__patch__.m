## Copyright (C) 2007 John W. Eaton, Shai Ayal, Kai Habel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## Undocumented internal function.

## __patch__ (p, x, y, c)
## Create patch object from x and y with color c and parent p.
## Return handle to patch object.

## Author: Kai Habel

function h = __patch__ (p, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  iarg = 1;
  have_x = have_z = have_c = false;
  if (isnumeric (varargin{1}))
    if (! isnumeric (varargin{2}))
      print_usage ();
    endif

    x = varargin{1};
    y = varargin{2};
    have_x = true;
    iarg += 2;

    if (nargin > 3 && ndims (varargin{3}) == 2 && ndims (x) == 2
	&& size (varargin{3}) == size (x))
      z = varargin {3};
      have_z = true;
      iarg++;
    endif
  endif

  if (have_x && nargin > iarg)
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
    print_usage ();
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

    for i = 1 : nc
      h = __go_patch__ (p);
      ax = get (h, "parent");
      if (have_x)
	set (h, "xdata", x (:, i), "ydata", y (:, i));
	if (have_z)
	  set (h, "zdata", z (:, i));
	endif
      endif

      if (have_c)
	if (ndims (c) == 2 && ((nr > 3 && size (c, 2) == nc)
			       || (size (c, 1) > 1 && size (c, 2) == nc)))
	  c2 = c (:, i);
	elseif (ndims (c) == 3)
	  c2 = permute (c(:,i,:), [1, 3, 2]);
	else
	  c2 = c;
	endif

	if (ischar (c2))
	  set (h, "facecolor", c2);
	elseif (numel (c2) == 1)
	  if (isnan (c))
	    set (h, "facecolor", [1, 1, 1]);
	    set (h, "cdata", c2);
	  elseif (isnumeric (c2))
	    ## Have color index.
	    set (h, "facecolor", "flat");
	    set (h, "cdata", c2);
	    clim = get(ax, "clim");
	    if (c2 < clim(1))
              set (ax, "clim", [c2, clim(2)])
	    endif
	    if (c2 > clim(2))
              set (ax, "clim", [clim(1), c2])
	    endif
	  else
	    ## Unknown color value.
	    error ("patch: color value not valid");
	  endif
	elseif (numel (c2) == 3)
	  ## Have rgb/rgba value.
	  set (h, "facecolor", c2);
	else
	  ## Color vector.
	  if (length (c2) != length (x) || length (c2) != length (y))
	    error ("patch: size of x, y, and c must be equal")
	  else
	    set (h, "facecolor", "interp");
	    set(h, "cdata", c2);
	    if (abs(max(c2) - min(c2)) < eps)
              set (ax, "clim", [c2(1)-1, c2(1)+1])
	    else
              set (ax, "clim", [min(c2), max(c2)]);
	    endif
	  endif
	endif
      else
	set (h, "facecolor", [0, 1, 0]);
      endif

      if (nargin > iarg + 1)
	set (h, varargin{iarg:end});
      endif
    endfor
  else
    error ("patch: not supported");
  endif

endfunction
