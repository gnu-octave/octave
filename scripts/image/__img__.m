## Copyright (C) 1996, 1997, 2006, 2007, 2008, 2009 John W. Eaton
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

  xdata = [x(1), x(end)];
  ydata = [y(1), y(end)];

  xlim = [x(1)-0.5, x(end)+0.5];
  ylim = [y(1)-0.5, y(end)+0.5];

  ca = gca ();

  tmp = __go_image__ (ca, "cdata", img, "xdata", xdata, "ydata", ydata,
		      "cdatamapping", "direct", varargin {:});

  ## FIXME -- how can we do this and also get the {x,y}limmode
  ## properties to remain "auto"?  I suppose this adjustment should
  ## happen automatically in axes::update_axis_limits instead of
  ## explicitly setting the values here.  But then what information is
  ## available to axes::update_axis_limits to determine that the
  ## adjustment is necessary?
  set (ca, "xlim", xlim, "ylim", ylim);

  if (ndims (img) == 3)
    if (isinteger (img))
      c = class (img);
      mn = intmin (c);
      mx = intmax (c);
      set (ca, "clim", double ([mn, mx]));
    endif
  endif

  set (ca, "view", [0, 90]);

  if (strcmp (get (ca, "nextplot"), "replace"))
    set (ca, "ydir", "reverse");
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
