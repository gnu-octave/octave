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
## @deftypefn {Function File} {} __plt2ss__ (@var{h}, @var{x}, @var{y}, @var{fmt}, @var{key})
## @end deftypefn

## Author: jwe

function __plt2ss__ (h, x, y, fmt, key)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (fmt))
    fmt = {""};
  endif

  if (nargin < 5 || isempty (key))
    key = {""};
  endif

  if (rows (fmt) > 1)
    fmt = fmt(1);
  endif

  if (rows (key) > 1)
    key = key(1);
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1 && x_nr == y_nr && x_nc == 1 && x_nc == y_nc)
    key = key{1};
    if (! isempty (key))
      set (h, "key", "on");
    endif
    ## FIXME -- need to handle labels and line format.
    line (x, y, "keylabel", key);
  else
    error ("__plt2ss__: arguments must be scalars");
  endif

endfunction
