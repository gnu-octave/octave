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

## -*- texinfo -*-
## @deftypefn {Function File} {} __patch__ (@var{p}, @var{x}, @var{y}, @var{c})
## Create patch object from @var{x} and @var{y} with color @var{c} and parent @var{p}.
## Return handle to patch object.
## @end deftypefn

## Author: Kai Habel

function h = __patch__ (p, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  nvargs = numel (varargin);

  if (nvargs > 1 && isnumeric (varargin{1}) && isnumeric (varargin{2}))
    num_data_args = 2;
  else
    num_data_args = 0;
  endif

  if (rem (nvargs - num_data_args - 1, 2) == 0 && nvargs > 2)
  else
    print_usage ("patch");
  endif

  x = varargin{1};
  y = varargin{2};
  c = varargin{3};

  h = __go_patch__ (p);
  ax = get (h, "parent");
  if (num_data_args > 1)
    set (h, "xdata", x, "ydata", y);
  endif

  if (isstr (c))
    ## Have color string.
    set (h, "FaceColor", c);
  elseif (length (c) == 1)
    if (isnan (c))
      set (h, "FaceColor", [1, 1, 1]);
      set (h, "CData", c);
    elseif (isnumeric (c))
      ## Have color index.
      set (h, "FaceColor", "flat");
      set (h, "CData", c);

      clim = get(ax, "CLim");
      if (c < clim(1))
        set (ax, "CLim", [c, clim(2)])
      endif
      if (c > clim(2))
        set (ax, "CLim", [clim(1), c])
      end

    else
      ## Unknown color value.
      error ("color value not valid");
    end
  elseif (length (c) == 3)
    ## Have rgb/rgba value.
    set (h, "FaceColor", c);
  else
    ## Color vector.
    if (length (c) != length (x) || length (c) != length (y))
      error ("size of x, y, and c must be equal")
    else
      set (h, "FaceColor", "interp");
      set(h, "CData", c);
      if (abs(max(c) - min(c)) < eps)
        set (ax, "CLim", [c(1)-1, c(1)+1])
      else
        set (ax, "CLim", [min(c), max(c)]);
      end
    end
  end 

  if (nvargs > num_data_args + 1)
    set (h, varargin{num_data_args+2:end});
  endif

endfunction
