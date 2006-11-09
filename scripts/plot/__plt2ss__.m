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
## @deftypefn {Function File} {[data, fmtstr] =} __plt2ss__ (@var{x}, @var{y}, @var{fmt})
## @end deftypefn

## Author: jwe

function [data, fmtstr, key] = __plt2ss__ (x, y, fmt, keystr)

  if (nargin < 2 || nargin > 4 || nargout < 2 || nargout > 3)
    print_usage ();
  endif

  if (nargin < 3)
    fmt = {""};
  endif

  if (nargin < 4)
    keystr = {""};
  endif

  if (rows (fmt) > 1)
    fmt = fmt(1);
  endif

  if (rows (keystr) > 1)
    keystr = keystr(1);
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1 && x_nr == y_nr && x_nc == 1 && x_nc == y_nc)
    data = {[x, y]};
    fmtstr = fmt;
    key = keystr;
  else
    error ("__plt2ss__: arguments must be scalars");
  endif

endfunction
