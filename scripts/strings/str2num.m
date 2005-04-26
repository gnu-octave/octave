## Copyright (C) 1996 Kurt Hornik
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
## @deftypefn {Function File} {} str2num (@var{s})
## Convert the string @var{s} to a number.
## @end deftypefn

## Author: jwe

function m = str2num (s)

  if (nargin == 1 && isstr (s))
    [nr, nc] = size (s);
    sep = ";";
    sep = sep (ones (nr, 1), 1);
    s = sprintf ("m = [%s];", reshape ([s, sep]', 1, nr * (nc + 1)));
    eval (s, "m = [];");
    if (isstr (m))
      m = [];
    endif
  else
    usage ("str2num (s)");
  endif

endfunction
