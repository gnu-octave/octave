## Copyright (C) 2005 John W. Eaton
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
## @deftypefn {Function File} {} __uiobject_axes_dtor__ (@var{obj})
## Delete an axes object @var{obj}.
## @end deftypefn

## Author: jwe

function __uiobject_axes_dtor__ (obj)

  if (nargin == 1)
    delete (obj.title);
    delete (obj.xlabel);
    delete (obj.ylabel);
    delete (obj.zlabel);

    obj.title = [];
    obj.xlabel = [];
    obj.ylabel = [];
    obj.zlabel = [];

    for child = obj.children
      delete (child);
    endfor
  else
    print_usage ();
  endif

endfunction
