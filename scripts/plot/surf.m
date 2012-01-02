## Copyright (C) 2007-2012 Kai Habel
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
## @deftypefn  {Function File} {} surf (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surf (@var{z})
## @deftypefnx {Function File} {} surf (@dots{}, @var{c})
## @deftypefnx {Function File} {} surf (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} surf (@dots{})
## Plot a surface given matrices @var{x}, and @var{y} from @code{meshgrid} and
## a matrix @var{z} corresponding to the @var{x} and @var{y} coordinates of
## the mesh.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z}
## correspond to different @var{x} values and rows of @var{z} correspond
## to different @var{y} values.
##
## The color of the surface is derived from the @code{colormap} and
## the value of @var{z}.  Optionally the color of the surface can be
## specified independent of @var{z}, by adding a fourth matrix, @var{c}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
## @seealso{colormap, contour, meshgrid, mesh}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function retval = surf (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("surf", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();
    tmp = surface (varargin{:});

    if (! ishold ())
      set (h, "view", [-37.5, 30],
           "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

  if (nargout > 0)
    retval = tmp;
  endif

endfunction


%!demo
%! clf
%! [~,~,Z] = peaks;
%! surf (Z);

%!demo
%! clf
%! [~,~,Z] = sombrero;
%! [Fx,Fy] = gradient (Z);
%! surf (Z, Fx+Fy);
%! shading interp;

%!demo
%! clf
%! [X,Y,Z] = sombrero;
%! [~,Fy] = gradient (Z);
%! surf (X, Y, Z, Fy);
%! shading interp;

