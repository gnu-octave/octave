## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {} quiver3 (@var{u}, @var{v}, @var{w})
## @deftypefnx {Function File} {} quiver3 (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w})
## @deftypefnx {Function File} {} quiver3 (@dots{}, @var{s})
## @deftypefnx {Function File} {} quiver3 (@dots{}, @var{style})
## @deftypefnx {Function File} {} quiver3 (@dots{}, 'filled')
## @deftypefnx {Function File} {} quiver3 (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} quiver3 (@dots{})
##
## Plot the @code{(@var{u}, @var{v}, @var{w})} components of a vector field in
## an @code{(@var{x}, @var{y}), @var{z}} meshgrid.  If the grid is uniform, you
## can specify @var{x}, @var{y} @var{z} as vectors.
##
## If @var{x}, @var{y} and @var{z} are undefined they are assumed to be
## @code{(1:@var{m}, 1:@var{n}, 1:@var{p})} where @code{[@var{m}, @var{n}] =
## size(@var{u})} and @code{@var{p} = max (size (@var{w}))}.
##
## The variable @var{s} is a scalar defining a scaling factor to use for
##  the arrows of the field relative to the mesh spacing.  A value of 0
## disables all scaling.  The default value is 1.
##
## The style to use for the plot can be defined with a line style @var{style}
## in a similar manner to the line styles used with the @code{plot} command.
## If a marker is specified then markers at the grid points of the vectors are
## printed rather than arrows.  If the argument 'filled' is given then the
## markers as filled.
##
## The optional return value @var{h} is a graphics handle to a quiver object.
## A quiver object regroups the components of the quiver plot (body, arrow,
## and marker), and allows them to be changed together.
##
## @example
## @group
## [x, y, z] = peaks (25);
## surf (x, y, z);
## hold on;
## [u, v, w] = surfnorm (x, y, z / 10);
## h = quiver3 (x, y, z, u, v, w);
## set (h, "maxheadsize", 0.33);
## @end group
## @end example
##
## @seealso{plot}
## @end deftypefn

function retval = quiver3 (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ("quiver3", varargin{:});

  if (nargin < 2)
    print_usage ();
  else
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      tmp = __quiver__ (h, 1, varargin{:});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction

%!demo
%! clf
%! colormap (jet (64));
%! [x,y] = meshgrid (-1:0.1:1);
%! z = sin (2*pi * sqrt (x.^2+y.^2));
%! theta = 2*pi * sqrt (x.^2+y.^2) + pi/2;
%! quiver3 (x, y, z, sin (theta), cos (theta), ones (size (z)));
%! hold on;
%! mesh (x,y,z);
%! hold off;

%!demo
%! clf
%! [x, y, z] = peaks (25);
%! surf (x, y, z);
%! hold on;
%! [u, v, w] = surfnorm (x, y, z / 10);
%! h = quiver3 (x, y, z, u, v, w);
%! set (h, "maxheadsize", 0.33);
%! hold off;

%!demo
%! clf
%! [x, y, z] = peaks (25);
%! surf (x, y, z);
%! hold on;
%! [u, v, w] = surfnorm (x, y, z / 10);
%! h = quiver3 (x, y, z, u, v, w);
%! set (h, "maxheadsize", 0.33);
%! hold off;
%! shading interp

