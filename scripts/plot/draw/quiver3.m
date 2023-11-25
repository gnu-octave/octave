########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} quiver3 (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w})
## @deftypefnx {} {} quiver3 (@var{z}, @var{u}, @var{v}, @var{w})
## @deftypefnx {} {} quiver3 (@dots{}, @var{s})
## @deftypefnx {} {} quiver3 (@dots{}, @var{style})
## @deftypefnx {} {} quiver3 (@dots{}, "filled")
## @deftypefnx {} {} quiver3 (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} quiver3 (@dots{})
##
## Plot a 3-D vector field with arrows.
##
## Plot the (@var{u}, @var{v}, @var{w}) components of a vector field at the
## grid points defined by (@var{x}, @var{y}, @var{z}).  If the grid is uniform
## then @var{x}, @var{y}, and @var{z} can be specified as grid vectors and
## @code{meshgrid} is used to create the 3-D grid.
##
## If @var{x} and @var{y} are not given they are assumed to be
## @code{(1:@var{m}, 1:@var{n})} where
## @code{[@var{m}, @var{n}] = size (@var{u})}.
##
## The optional input @var{s} is a scalar defining a scaling factor to use for
## the arrows of the field relative to the mesh spacing.  A value of 1.0 will
## result in the longest vector exactly filling one grid cube.  A value of 0
## or @qcode{"off"} disables all scaling.  The default value is 0.9.
##
## The style to use for the plot can be defined with a line style @var{style}
## of the same format as the @code{plot} command.  If a marker is specified
## then the markers are drawn at the origin of the vectors (which are the grid
## points defined by @var{x}, @var{y}, @var{z}).  When a marker is specified,
## the arrowhead is not drawn.  If the argument @qcode{"filled"} is given then
## the markers are filled.  If name-value plot style properties are used, they
## must appear in pairs and follow any other plot style arguments.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
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
## @seealso{quiver, compass, feather, plot}
## @end deftypefn

function h = quiver3 (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("quiver3", varargin{:});

  if (nargin < 4)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    [hax, htmp] = __quiver__ (hax, true, varargin{:});

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
%! colormap ("default");
%! [x, y, z] = peaks (25);
%! surf (x, y, z);
%! hold on;
%! [u, v, w] = surfnorm (x, y, z / 10);
%! h = quiver3 (x, y, z, u, v, w);
%! set (h, "maxheadsize", 0.25);
%! hold off;
%! title ("quiver3() of surface normals to peaks() function");

%!demo
%! clf;
%! colormap ("default");
%! [x, y, z] = peaks (25);
%! surf (x, y, z);
%! hold on;
%! [u, v, w] = surfnorm (x, y, z / 10);
%! h = quiver3 (x, y, z, u, v, w);
%! set (h, "maxheadsize", 0.25);
%! hold off;
%! shading interp;
%! title ({"quiver3() of surface normals to peaks() function"; ...
%!         'shading "interp"'});

## Check standard inputs, single arrow.
%!test
%! hf = figure ("visible", "off");
%! hax = gca ();
%! unwind_protect
%!
%!   h = quiver3 (hax, 0, 1, 2, 3);
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   childzdata = get (children, "zdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{stemchild}(1), 1, eps);
%!   assert (childxdata{stemchild}(2), 1 + 1*0.9, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 1 + 1*0.9, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!   assert (childydata{stemchild}(1), 1, eps);
%!   assert (childydata{stemchild}(2), 1 + 2*0.9, eps);
%!   assert (isnan (childydata{stemchild}(3)));
%!   assert (childydata{arrowheadchild}(2), 1 + 2*0.9, eps);
%!   assert (isnan (childydata{arrowheadchild}(4)));
%!   assert (childzdata{stemchild}(1), 0, eps);
%!   assert (childzdata{stemchild}(2), 0 + 3*0.9, eps);
%!   assert (isnan (childzdata{stemchild}(3)));
%!   assert (childzdata{arrowheadchild}(2), 0 + 3*0.9, eps);
%!   assert (isnan (childzdata{arrowheadchild}(4)));
%!
%!   h = quiver3 (hax, 1, 1, 0, 1, 2, 3);
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   childzdata = get (children, "zdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{stemchild}(1), 1, eps);
%!   assert (childxdata{stemchild}(2), 1 + 1*0.9, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 1 + 1*0.9, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!   assert (childydata{stemchild}(1), 1, eps);
%!   assert (childydata{stemchild}(2), 1 + 2*0.9, eps);
%!   assert (isnan (childydata{stemchild}(3)));
%!   assert (childydata{arrowheadchild}(2), 1 + 2*0.9, eps);
%!   assert (isnan (childydata{arrowheadchild}(4)));
%!   assert (childzdata{stemchild}(1), 0, eps);
%!   assert (childzdata{stemchild}(2), 0 + 3*0.9, eps);
%!   assert (isnan (childzdata{stemchild}(3)));
%!   assert (childzdata{arrowheadchild}(2), 0 + 3*0.9, eps);
%!   assert (isnan (childzdata{arrowheadchild}(4)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check standard inputs, multiple arrows.
%!test
%! hf = figure ("visible", "off");
%! hax = gca ();
%! unwind_protect
%!
%!   a = reshape(1:12,2,3,2);
%!   x = 1:3; y = 1:2; z = 1:2;
%!   [xx,yy,zz] = meshgrid (x,y,z);
%!   numpts = 12;
%!   sf= sqrt(sumsq([1/3 1/2 11/6])/432); # Actual internal scale factor, z=a.
%!   sf2= sqrt(sumsq([1/3 1/2 1/6])/432); # z vector internal scale factor.
%!
%!   h = quiver3 (hax, a, a, a, a, 1); # No x,y input.
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   childzdata = get (children, "zdata");
%!   basechild = find (cellfun (@numel, childxdata) == numpts);
%!   stemchild = find (cellfun (@numel, childxdata) == numpts*3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == numpts*4);
%!   ## Check all bases.
%!   assert (childxdata{basechild}, [1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3]);
%!   assert (childydata{basechild}, [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2]);
%!   assert (childzdata{basechild}, [1:12]);
%!   ## Check first arrow.
%!   assert (childxdata{stemchild}(1), 1, eps);
%!   assert (childxdata{stemchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!   assert (childydata{stemchild}(1), 1, eps);
%!   assert (childydata{stemchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childydata{stemchild}(3)));
%!   assert (childydata{arrowheadchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childydata{arrowheadchild}(4)));
%!   assert (childzdata{stemchild}(1), 1, eps);
%!   assert (childzdata{stemchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childzdata{stemchild}(3)));
%!   assert (childzdata{arrowheadchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childzdata{arrowheadchild}(4)));
%!   ## Check last arrow.
%!   assert (childxdata{stemchild}(numpts*3-2), 3, eps);
%!   assert (childxdata{stemchild}(numpts*3-1), 3 + 12*sf, eps);
%!   assert (isnan (childxdata{stemchild}(end)));
%!   assert (childxdata{arrowheadchild}(numpts*4-2), 3 + 12*sf, eps);
%!   assert (isnan (childxdata{arrowheadchild}(end)));
%!   assert (childydata{stemchild}(numpts*3-2), 2, eps);
%!   assert (childydata{stemchild}(numpts*3-1), 2 + 12*sf, eps);
%!   assert (isnan (childydata{stemchild}(end)));
%!   assert (childydata{arrowheadchild}(numpts*4-2), 2 + 12*sf, eps);
%!   assert (isnan (childydata{arrowheadchild}(end)));
%!   assert (childzdata{stemchild}(numpts*3-2), 12, eps);
%!   assert (childzdata{stemchild}(numpts*3-1), 12 + 12*sf, eps);
%!   assert (isnan (childzdata{stemchild}(end)));
%!   assert (childzdata{arrowheadchild}(numpts*4-2), 12 + 12*sf, eps);
%!   assert (isnan (childzdata{arrowheadchild}(end)));
%!
%!   h = quiver3 (hax, xx, yy, a, a, a, a, 1); # x,y input as matrices.
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   childzdata = get (children, "zdata");
%!   basechild = find (cellfun (@numel, childxdata) == numpts);
%!   stemchild = find (cellfun (@numel, childxdata) == numpts*3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == numpts*4);
%!   ## Check all bases.
%!   assert (childxdata{basechild}, [1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3]);
%!   assert (childydata{basechild}, [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2]);
%!   assert (childzdata{basechild}, [1:12]);
%!   ## Check first arrow.
%!   assert (childxdata{stemchild}(1), 1, eps);
%!   assert (childxdata{stemchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!   assert (childydata{stemchild}(1), 1, eps);
%!   assert (childydata{stemchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childydata{stemchild}(3)));
%!   assert (childydata{arrowheadchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childydata{arrowheadchild}(4)));
%!   assert (childzdata{stemchild}(1), 1, eps);
%!   assert (childzdata{stemchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childzdata{stemchild}(3)));
%!   assert (childzdata{arrowheadchild}(2), 1 + 1*sf, eps);
%!   assert (isnan (childzdata{arrowheadchild}(4)));
%!   ## Check last arrow.
%!   assert (childxdata{stemchild}(numpts*3-2), 3, eps);
%!   assert (childxdata{stemchild}(numpts*3-1), 3 + 12*sf, eps);
%!   assert (isnan (childxdata{stemchild}(end)));
%!   assert (childxdata{arrowheadchild}(numpts*4-2), 3 + 12*sf, eps);
%!   assert (isnan (childxdata{arrowheadchild}(end)));
%!   assert (childydata{stemchild}(numpts*3-2), 2, eps);
%!   assert (childydata{stemchild}(numpts*3-1), 2 + 12*sf, eps);
%!   assert (isnan (childydata{stemchild}(end)));
%!   assert (childydata{arrowheadchild}(numpts*4-2), 2 + 12*sf, eps);
%!   assert (isnan (childydata{arrowheadchild}(end)));
%!   assert (childzdata{stemchild}(numpts*3-2), 12, eps);
%!   assert (childzdata{stemchild}(numpts*3-1), 12 + 12*sf, eps);
%!   assert (isnan (childzdata{stemchild}(end)));
%!   assert (childzdata{arrowheadchild}(numpts*4-2), 12 + 12*sf, eps);
%!   assert (isnan (childzdata{arrowheadchild}(end)));
%!
%!   h = quiver3 (hax, x, y, z, a, a, a, 1); # x,y z input as vectors.
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   childzdata = get (children, "zdata");
%!   basechild = find (cellfun (@numel, childxdata) == numpts);
%!   stemchild = find (cellfun (@numel, childxdata) == numpts*3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == numpts*4);
%!   ## Check all bases.
%!   assert (childxdata{basechild}, [1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3]);
%!   assert (childydata{basechild}, [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2]);
%!   assert (childzdata{basechild}, [1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2]);
%!   ## Check first arrow.
%!   assert (childxdata{stemchild}(1), 1, eps);
%!   assert (childxdata{stemchild}(2), 1 + 1*sf2, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 1 + 1*sf2, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!   assert (childydata{stemchild}(1), 1, eps);
%!   assert (childydata{stemchild}(2), 1 + 1*sf2, eps);
%!   assert (isnan (childydata{stemchild}(3)));
%!   assert (childydata{arrowheadchild}(2), 1 + 1*sf2, eps);
%!   assert (isnan (childydata{arrowheadchild}(4)));
%!   assert (childzdata{stemchild}(1), 1, eps);
%!   assert (childzdata{stemchild}(2), 1 + 1*sf2, eps);
%!   assert (isnan (childzdata{stemchild}(3)));
%!   assert (childzdata{arrowheadchild}(2), 1 + 1*sf2, eps);
%!   assert (isnan (childzdata{arrowheadchild}(4)));
%!   ## Check last arrow.
%!   assert (childxdata{stemchild}(numpts*3-2), 3, eps);
%!   assert (childxdata{stemchild}(numpts*3-1), 3 + 12*sf2, eps);
%!   assert (isnan (childxdata{stemchild}(end)));
%!   assert (childxdata{arrowheadchild}(numpts*4-2), 3 + 12*sf2, eps);
%!   assert (isnan (childxdata{arrowheadchild}(end)));
%!   assert (childydata{stemchild}(numpts*3-2), 2, eps);
%!   assert (childydata{stemchild}(numpts*3-1), 2 + 12*sf2, eps);
%!   assert (isnan (childydata{stemchild}(end)));
%!   assert (childydata{arrowheadchild}(numpts*4-2), 2 + 12*sf2, eps);
%!   assert (isnan (childydata{arrowheadchild}(end)));
%!   assert (childzdata{stemchild}(numpts*3-2), 2, eps);
%!   assert (childzdata{stemchild}(numpts*3-1), 2 + 12*sf2, eps);
%!   assert (isnan (childzdata{stemchild}(end)));
%!   assert (childzdata{arrowheadchild}(numpts*4-2), 2 + 12*sf2, eps);
%!   assert (isnan (childzdata{arrowheadchild}(end)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

##Test input validation
%!error <Invalid call> quiver3 ()
%!error <Invalid call> quiver3 (1.1)
%!error <Invalid call> quiver3 (1.1, 2)
%!error <Invalid call> quiver3 (1.1, 2, 3)
%!error <Invalid call> quiver3 (1.1, 2, 3, "foo")
%!error <Invalid call> quiver3 (1.1, 2, 3, 4, 5, 6, 7, 8, "foo")
%!error <U, V, and W must be the same> quiver3 (30, [40 50], 60, 70)
%!error <Z vector length must equal size of> quiver3 ([30 40], eye(3), eye(3), eye(3))
%!error <Z, U, V, and W must be the same> quiver3 ([30 40], 50, 60, 70)
%!error <Z, U, V, and W must be the same> quiver3 (eye(2), eye(3), eye(2), eye(2))
%!error <Z, U, V, and W must be the same> quiver3 (eye(2), eye(2), eye(3), eye(2))
%!error <Z, U, V, and W must be the same> quiver3 (eye(2), eye(2), eye(2), eye(3))
%!error <U, V, and W must be the same size> quiver3 ([1:2], [1:2], 1, eye(3), eye(2), eye(2))
%!error <U, V, and W must be the same size> quiver3 ([1:2], [1:2], 1, eye(2), eye(3), eye(2))
%!error <U, V, and W must be the same size> quiver3 ([1:2], [1:2], 1, eye(2), eye(2), eye(3))
%!error <X vector length must equal number of> quiver3 ([1:3], [1:2], 1, eye(2), eye(2), eye(2))
%!error <Y vector length must equal number of> quiver3 ([1:2], [1:3], 1, eye(2), eye(2), eye(2))
%!error <Z vector length must equal size of> quiver3 ([1:2], [1:2], [1:2], eye(2), eye(2), eye(2))
%!error <X, Y, Z, U, V, and W must be the same size> quiver3 (eye(3), eye(2), eye(2), eye(2), eye(2), eye(2))
%!error <X, Y, Z, U, V, and W must be the same size> quiver3 (eye(2), eye(3), eye(2), eye(2), eye(2), eye(2))
%!error <X, Y, Z, U, V, and W must be the same size> quiver3 (eye(2), eye(2), eye(3), eye(2), eye(2), eye(2))
%!error <X, Y, Z, U, V, and W must be the same size> quiver3 (eye(2), eye(2), eye(2), eye(3), eye(2), eye(2))
%!error <X, Y, Z, U, V, and W must be the same size> quiver3 (eye(2), eye(2), eye(2), eye(2), eye(3), eye(2))
%!error <X, Y, Z, U, V, and W must be the same size> quiver3 (eye(2), eye(2), eye(2), eye(2), eye(2), eye(3))
%!error <scaling factor must be> quiver3 (10, 20, 30, 40, -5)
%!error <scaling factor must be> quiver3 (10, 20, 30, 40, [1 2])
%!error <scaling factor must be> quiver3 (10, 20, 30, 40, 50, 60, -5)
%!error <scaling factor must be> quiver3 (10, 20, 30, 40, 50, 60, [1 2])
