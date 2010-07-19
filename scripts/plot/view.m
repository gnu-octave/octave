## Copyright (C) 2007, 2008 John W. Eaton
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
## @deftypefn  {Function File} {} view (@var{azimuth}, @var{elevation})
## @deftypefnx {Function File} {} view (@var{dims})
## @deftypefnx {Function File} {[@var{azimuth}, @var{elevation}] =} view ()
## Set or get the viewpoint for the current axes.
## @end deftypefn

## Author: jwe

function [azimuth, elevation] = view (x, y, z)

  if (nargin < 4)
    if (nargin == 0)
      tmp = get (gca (), "view");
      az = tmp(1);
      el = tmp(2);
    elseif (nargin == 1)
      if (x == 2)
        az = 0;
        el = 90;
      elseif (x == 3)
        az = -37.5;
        el = 30;
      else
        error ("view: expecting single argument to be 2 or 3");
      endif
    elseif (nargin == 2)
      az = x;
      el = y;
    elseif (nargin == 3)
      error ("view: view (x, y, z) not implemented");
    endif

    if (nargin > 0)
      set (gca (), "view", [az, el]);
    endif

    if (nargout == 1)
      error ("view: T = view () not implemented");
    endif

    if (nargout == 2)
      azimuth = az;
      elevation = el;
    endif
  else
    print_usage ();
  endif

endfunction
