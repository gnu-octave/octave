## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Function File} {} mesh (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} mesh (@var{z})
## @deftypefnx {Function File} {} mesh (@dots{}, @var{c})
## @deftypefnx {Function File} {} mesh (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} mesh (@dots{})
## Plot a mesh given matrices @var{x}, and @var{y} from @code{meshgrid} and
## a matrix @var{z} corresponding to the @var{x} and @var{y} coordinates of
## the mesh.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z}
## correspond to different @var{x} values and rows of @var{z} correspond
## to different @var{y} values.
##
## The color of the mesh is derived from the @code{colormap}
## and the value of @var{z}.  Optionally the color of the mesh can be
## specified independent of @var{z}, by adding a fourth matrix, @var{c}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
## @seealso{colormap, contour, meshgrid, surf}
## @end deftypefn

## Author: jwe

function h = mesh (varargin)

  if (! all (cellfun ("isreal", varargin)))
    error ("mesh: X, Y, Z, C arguments must be real");
  endif

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("mesh", varargin{:});

  oldfig = ifelse (isempty (hax), [], get (0, "currentfigure"));
  unwind_protect
    hax = newplot (hax);
    htmp = surface (hax, varargin{:});

    set (htmp, "facecolor", "w");
    set (htmp, "edgecolor", "flat");

    if (! ishold ())
      set (hax, "view", [-37.5, 30],
               "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! x = logspace (0,1,11);
%! z = x'*x;
%! mesh (x, x, z, z.^2);
%! xlabel 'X-axis';
%! ylabel 'Y-axis';
%! zlabel 'linear scale';

%!demo
%! clf;
%! x = logspace (0,1,11);
%! z = x'*x;
%! mesh (x, x, z, z.^2);
%! set (gca, 'zscale', 'log');
%! xlabel 'X-axis';
%! ylabel 'Y-axis';
%! zlabel 'log scale';
%! if (strcmp (get (gcf, '__graphics_toolkit__'), 'gnuplot'))
%!   title ({'Gnuplot: mesh color is wrong', 'This is a Gnuplot bug'});
%! endif

