## Copyright (C) 2009 Martin Helm
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, see http://www.gnu.org/licenses/gpl.html.
##
## Author: Martin Helm <martin@mhelm.de>

## usage: NORMALS = isonormals(X,Y,Z,V,VERT)
## usage: NORMALS = isonormals(V,VERT)
## usage: NORMALS = isonormals(V,P)
## usage: NORMALS = isonormals(X,Y,Z,V,P)
## usage: NORMALS = isonormals(...,'negate')
## usage: isonormals(V,P)
## usage: isonormals(X,Y,Z,V,P)
##

function varargout = isonormals(varargin)
  na = nargin;
  negate = false;
  if (ischar (varargin{nargin}))
    na = nargin-1;
    if (strcmp (lower (varargin{nargin}), "negate"))
      negate = true;
    else
      error ("Unknown option '%s'", varargin{nargin});
    endif
  endif
  switch na
    case 2
      c = varargin{1};
      vp = varargin{2};
      x = 1:size (c, 2);
      y = 1:size (c, 1);
      z = 1:size (c, 3);
    case 5
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      c = varargin{4};
      vp = varargin{5};
    otherwise 
      print_usage ();
  endswitch
  if (ismatrix (vp) && size (vp,2) == 3)
    pa = [];
    v = vp;
  elseif (ishandle (vp))
    pa = vp;
    v = get (pa, "Vertices");
  else
    error ("Last argument is no vertex list and no patch handle");
  endif
  if (negate)
    normals = -__interp_cube__ (x, y, z, c, v, "normals");
  else
    normals = __interp_cube__ (x, y, z, c, v, "normals");
  endif
  switch nargout
    case 0
      if (!isempty (pa))
        set (pa, "VertexNormals", normals);
      endif
    case 1
      varargout = {normals};
    otherwise
      print_usage ();
  endswitch
endfunction