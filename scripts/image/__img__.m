## Copyright (C) 1996, 1997, 2006, 2007 John W. Eaton
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

## -*- texinfo -*-
## @deftypefnx {Function File} {} __img__ (@var{x}, @var{y}, @var{img}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Generic image creation.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}. If you're not using gnuplot 4.2 or later, these
## variables are ignored.

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function h = __img__ (x, y, img, varargin)

  newplot ();

  if (isempty (img))
    error ("__img__: matrix is empty");
  endif

  if (isempty (x))
    x = [1, columns(img)];
  endif

  if (isempty (y))
    y = [1, rows(img)];
  endif

  xlim = [x(1), x(end)];
  ylim = [y(1), y(end)];

  ca = gca ();

  tmp = __go_image__ (ca, "cdata", img, "xdata", xlim, "ydata", ylim, 
		      "cdatamapping", "direct", varargin {:});

  if (ndims (img) == 3)
    if (isinteger (img))
      c = class (img);
      mn = intmin (c);
      mx = intmax (c);
      set (ca, "clim", double ([mn, mx]));
    endif
  endif

  set (ca, "view", [0, 90], "xlimmode", "manual", "ylimmode", "manual",
       "xlim", xlim, "ylim", ylim);

  if (strcmp (get (ca, "nextplot"), "replace"))
    set (ca, "ydir", "reverse");
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
