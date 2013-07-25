## Copyright (C) 2006-2012 Kai Habel
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

## -*- texinfo -*-
## @deftypefn  {Function File} {} shading (@var{type})
## @deftypefnx {Function File} {} shading (@var{ax}, @var{type})
## Set the shading of surface or patch graphic objects.  Valid arguments
## for @var{type} are
##
## @table @asis
## @item "flat"
## Single colored patches with invisible edges.
##
## @item "faceted"
## Single colored patches with visible edges.
##
## @item "interp"
## Color between patch vertices are interpolated and the patch edges are
## invisible.
## @end table
##
## If @var{hax} is given the shading is applied to axis @var{hax} instead
## of the current axis.
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function shading (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("shading", varargin{:});

  if (nargin != 1)
    print_usage ();
  endif

  mode = varargin{1};

  if (isempty (hax))
    hax = gca ();
  endif

  hp = findobj (hax, "type", "patch");
  hs = findobj (hax, "type", "surface");
  hall = [hp(:); hs(:)];

  switch (lower (mode))
    case "flat"
      set (hall, "facecolor", "flat");
      set (hall, "edgecolor", "none");
    case "interp"
      set (hall, "facecolor", "interp");
      set (hall, "edgecolor", "none");
    case "faceted"
      set (hall, "facecolor", "flat");
      set (hall, "edgecolor", [0 0 0]);
    otherwise
      error ('shading: Invalid MODE "%s"', mode);
  endswitch

endfunction


%!demo
%! clf;
%! colormap ('default');
%! sombrero ();
%! shading faceted;
%! title ('shading ''faceted''');

%!demo
%! clf;
%! colormap ('default');
%! sombrero ();
%! shading flat;
%! title ('shading ''flat''');

%!demo
%! clf;
%! colormap ('default');
%! sombrero ();
%! shading interp;
%! title ('shading ''interp''');

%!demo
%! clf;
%! colormap ('default');
%! pcolor (peaks ());
%! shading faceted;
%! title ('shading ''faceted''');

%!demo
%! clf;
%! colormap ('default');
%! pcolor (peaks ());
%! shading flat;
%! title ('shading ''flat''');

%!demo
%! clf;
%! colormap ('default');
%! pcolor (peaks ());
%! shading interp;
%! title ('shading ''interp''');

