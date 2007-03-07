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
## @deftypefn {Function File} {} __uiobject_adopt__ (@var{parent}, @var{child})
## Add @var{child} to the list of children in @var{parent}.
## @end deftypefn

## Author: jwe

function s = __uiobject_adopt__ (parent, child)

  if (nargin == 2)
    obj = get (parent);
    if (! isempty (obj))
      kids = obj.children;
      ## Put this child at the end of the list.  If it is already in
      ## the list, move it.
      kids(kids == child) = [];
      kids = [kids, child]
      set (parent, "children", kids);
    else
      error ("__uiobject_adopt__: expecting parent to be a handle");
    endif
  else
    print_usage ();
  endif

endfunction
