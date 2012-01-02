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
## @deftypefnx {Function File} {} shading (@var{ax}, @dots{})
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
## If @var{ax} is given the shading is applied to axis @var{ax} instead
## of the current axis.
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function shading (varargin)

  [ax, varargin] = __plt_get_axis_arg__ ("shading", varargin{:});

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  mode = varargin{1};

  h1 = findobj (ax, "type", "patch");
  h2 = findobj (ax, "type", "surface");

  obj = [h1(:); h2(:)];

  for n = 1:numel(obj)
    h = obj(n);
    if (strcmpi (mode, "flat"))
      set (h, "facecolor", "flat");
      set (h, "edgecolor", "none");
    elseif (strcmpi (mode, "interp"))
      set (h, "facecolor", "interp");
      set (h, "edgecolor", "none");
    elseif (strcmpi (mode, "faceted"))
      set (h, "facecolor", "flat");
      set (h, "edgecolor", [0 0 0]);
    else
      error ("shading: unknown argument");
    endif
  endfor

endfunction


%!demo
%! clf
%! colormap (jet)
%! sombrero
%! shading faceted
%! title ('shading "faceted"')

%!demo
%! clf
%! sombrero
%! shading flat
%! title ('shading "flat"')

%!demo
%! clf
%! sombrero
%! shading interp
%! title ('shading "interp"')

%!demo
%! clf
%! pcolor (peaks ())
%! shading faceted
%! title ('shading "faceted"')

%!demo
%! clf
%! pcolor (peaks ())
%! shading flat
%! title ('shading "flat"')

%!demo
%! clf
%! pcolor (peaks ())
%! shading interp
%! title ('shading "interp"')

