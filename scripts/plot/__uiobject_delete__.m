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
## @deftypefn {Function File} {} delete (@var{h})
## Delete the graphics object @var{h}
## @end deftypefn

## Author: jwe

function __uiobject_delete__ (h)

  if (nargin == 1)
    if (ishandle (h))
      obj = get (h);
      if (strcmp (obj.type, "figure"))
	ps = obj.__plot_stream__;
	if (any (ps == fopen ("all")))
	  pclose (ps);
	endif
      endif
      if (isfield (obj, "__dtor__"))
	feval (obj.__dtor__, obj);
      else
	for child = obj.children
	  delete (child);
	endfor
      endif
      ## Also remove this object from its parent's list of children.
      if (isfield (obj, "parent"))
	parent = obj.parent;
	kids = get (parent, "children");
	kids(kids == h) = [];
	set (parent, "children", kids);
      endif
      __uiobject_free__ (h);
    else
      error ("delete: expecting argument to be a graphics object");
    endif
  else
    print_usage ();
  endif

endfunction
