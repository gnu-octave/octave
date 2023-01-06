########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} waterfall (@var{x}, @var{y}, @var{z})
## @deftypefnx {} {} waterfall (@var{z})
## @deftypefnx {} {} waterfall (@dots{}, @var{c})
## @deftypefnx {} {} waterfall (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} waterfall (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} waterfall (@dots{})
## Plot a 3-D waterfall plot.
##
## A waterfall plot is similar to a @code{meshz} plot except only
## mesh lines for the rows of @var{z} (x-values) are shown.
##
## The wireframe mesh is plotted using rectangles.  The vertices of the
## rectangles [@var{x}, @var{y}] are typically the output of @code{meshgrid}.
## over a 2-D rectangular region in the x-y plane.  @var{z} determines the
## height above the plane of each vertex.  If only a single @var{z} matrix is
## given, then it is plotted over the meshgrid
## @code{@var{x} = 1:columns (@var{z}), @var{y} = 1:rows (@var{z})}.
## Thus, columns of @var{z} correspond to different @var{x} values and rows
## of @var{z} correspond to different @var{y} values.
##
## The color of the mesh is computed by linearly scaling the @var{z} values
## to fit the range of the current colormap.  Use @code{caxis} and/or
## change the colormap to control the appearance.
##
## Optionally the color of the mesh can be specified independently of @var{z}
## by supplying a color matrix, @var{c}.
##
## Any property/value pairs are passed directly to the underlying surface
## object.  The full list of properties is documented at
## @ref{Surface Properties}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
##
## @seealso{meshz, mesh, meshc, contour, surf, surface, ribbon, meshgrid,
## hidden, shading, colormap, caxis}
## @end deftypefn

function h = waterfall (varargin)

  htmp = meshz (varargin{:});

  set (htmp, "meshstyle", "row");

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! Z = peaks ();
%! waterfall (Z);
%! title ("waterfall() plot of peaks() function");

%!demo
%! clf;
%! colormap ("default");
%! Z = peaks ();
%! subplot (1,2,1)
%!  meshz (Z);
%!  daspect ([2.5, 2.5, 1]);
%!  title ("meshz() plot");
%! subplot (1,2,2)
%!  waterfall (Z);
%!  daspect ([2.5, 2.5, 1]);
%!  title ("waterfall() plot");
