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
## @deftypefn {Function File} {} __plt2mv__ (@var{h}, @var{x}, @var{y}, @var{fmt}, @var{key})
## @end deftypefn

## Author: jwe

function __plt2mv__ (h, x, y, fmt, key)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (fmt))
    fmt = {""};
  endif

  if (nargin < 5 || isempty (key))
    key = {""};
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (y_nr == 1)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  endif

  if (x_nr == y_nr)
    1;
  elseif (x_nc == y_nr)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  else
    error ("__plt2mv__: matrix dimensions must match");
  endif

  if (x_nc > 0)
    if (rows (fmt) == 1)
      fmt = repmat (fmt, x_nc, 1);
    endif
    if (rows (key) == 1)
      key = repmat (key, x_nc, 1);
    endif
    for i = 1:x_nc
      ## FIXME -- need to handle labels and line format.
      tkey = key{i};
      if (! isempty (tkey))
	set (h, "key", "on");
      endif
      line (x(:,i), y, "keylabel", tkey);
    endfor
  else
    error ("__plt2mv__: arguments must be a matrices");
  endif

endfunction
