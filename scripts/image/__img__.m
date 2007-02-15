## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} __img__ (@var{img})
## @deftypefnx {Function File} {} __img__ (@var{x}, @var{y}, @var{img})
## Generic image creation.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}. If you're not using gnuplot 4.2 or later, these
## variables are ignored.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function h = __img__ (x, y, img)

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

  s = __uiobject_image_ctor__ (ca);

  s.cdata = img;
  s.xdata = xlim;
  s.ydata = ylim;

  tmp = __uiobject_make_handle__ (s);

  __uiobject_adopt__ (ca, tmp);

  set (ca, "view", [0, 90], "xlim", xlim, "ylim", ylim);

  if (nargout > 0)
    h = tmp;
  endif

endfunction
