########################################################################
##
## Copyright (C) 2007-2024 The Octave Project Developers
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
## @deftypefn  {} {} quiver (@var{u}, @var{v})
## @deftypefnx {} {} quiver (@var{x}, @var{y}, @var{u}, @var{v})
## @deftypefnx {} {} quiver (@dots{}, @var{s})
## @deftypefnx {} {} quiver (@dots{}, @var{style})
## @deftypefnx {} {} quiver (@dots{}, "filled")
## @deftypefnx {} {} quiver (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} quiver (@dots{})
##
## Plot a 2-D vector field with arrows.
##
## Plot the (@var{u}, @var{v}) components of a vector field at the grid points
## defined by (@var{x}, @var{y}).  If the grid is uniform then @var{x} and
## @var{y} can be specified as grid vectors and @code{meshgrid} is used to
## create the 2-D grid.
##
## If @var{x} and @var{y} are not given they are assumed to be
## @code{(1:@var{m}, 1:@var{n})} where
## @code{[@var{m}, @var{n}] = size (@var{u})}.
##
## The optional input @var{s} is a scalar defining a scaling factor to use for
## the arrows of the field relative to the mesh spacing.  A value of 1.0 will
## result in the longest vector exactly filling one grid square.  A value of 0
## or @qcode{"off"} disables all scaling.  The default value is 0.9.
##
## The style to use for the plot can be defined with a line style, @var{style},
## of the same format as the @code{plot} command.  If a marker is specified
## then the markers are drawn at the origin of the vectors (which are the grid
## points defined by @var{x} and @var{y}).  When a marker is specified, the
## arrowhead is not drawn.  If the argument @qcode{"filled"} is given then the
## markers are filled.  If name-value plot style properties are used, they must
## appear in pairs and follow any other plot style arguments.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to a quiver object.
## A quiver object regroups the components of the quiver plot (body, arrow,
## and marker), and allows them to be changed together.
##
## Example:
##
## @example
## @group
## [x, y] = meshgrid (1:2:20);
## h = quiver (x, y, sin (2*pi*x/10), sin (2*pi*y/10));
## set (h, "maxheadsize", 0.33);
## @end group
## @end example
##
## @seealso{quiver3, compass, feather, plot}
## @end deftypefn

function h = quiver (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("quiver", varargin{:});

  if (nargin < 2)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    [hax, htmp] = __quiver__ (hax, false, varargin{:});

    ## FIXME: This should be moved into __quiver__ when problem with
    ##        re-initialization of title object is fixed.
    if (! ishold ())
      set (hax, "box", "on");
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
%! [x,y] = meshgrid (1:2:20);
%! h = quiver (x,y, sin (2*pi*x/10), sin (2*pi*y/10));
%! title ("quiver() plot w/arrowheads (default)");

%!demo
%! clf;
%! [x,y] = meshgrid (1:2:20);
%! h = quiver (x,y, sin (2*pi*x/10), sin (2*pi*y/10), "o");
%! title ("quiver() plot w/origin markers");

%!demo
%! clf;
%! [x,y] = meshgrid (1:2:20);
%! h = quiver (x,y, sin (2*pi*x/10), sin (2*pi*y/10));
%! set (h, "marker", "o");
%! title ("quiver() plot w/origin markers and arrowheads");

%!demo
%! clf;
%! x = linspace (0, 3, 80);
%! y = sin (2*pi*x);
%! theta = 2*pi*x + pi/2;
%! quiver (x, y, sin (theta)/10, cos (theta)/10, 0.4);
%! axis equal tight;
%! hold on; plot (x,y,"r"); hold off;
%! title ("quiver() with scaled arrows");

## Check standard inputs, single arrow.
%!test
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   h = quiver (hax, 1, 2);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{stemchild}(1), 1, eps);
%!   assert (childxdata{stemchild}(2), 1 + 1*0.9, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 1 + 1*0.9, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!
%!   h = quiver (hax, 1, 2, 0.5);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{stemchild}(1), 1, eps);
%!   assert (childxdata{stemchild}(2), 1 + 1*0.5, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 1 + 1*0.5, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!
%!   h = quiver (hax, 0, 1, 2, 3);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{stemchild}(1), 0, eps);
%!   assert (childxdata{stemchild}(2), 0 + 2*0.9, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 0 + 2*0.9, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!
%!   h = quiver (hax, 0, 1, 2, 3, 0.5);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{stemchild}(1), 0, eps);
%!   assert (childxdata{stemchild}(2), 0 + 2*0.5, eps);
%!   assert (isnan (childxdata{stemchild}(3)));
%!   assert (childxdata{arrowheadchild}(2), 0 + 2*0.5, eps);
%!   assert (isnan (childxdata{arrowheadchild}(4)));
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check arrowhead size
%!test
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   h = quiver (hax, 0, 0, 0, 1, 1); # up
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{arrowheadchild}, [1/9 0 -1/9 NaN], eps);
%!   assert (childydata{arrowheadchild}, [2/3 1 2/3 NaN], eps);
%!
%!   h = quiver (hax, 0, 0, 0, -1, 1); # down
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{arrowheadchild}, [-1/9 0 1/9 NaN], eps);
%!   assert (childydata{arrowheadchild}, [-2/3 -1 -2/3 NaN], eps);
%!
%!   h = quiver (hax, 0, 0, -1, 0, 1); # left
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{arrowheadchild}, [-2/3 -1 -2/3 NaN], eps);
%!   assert (childydata{arrowheadchild}, [1/9 0 -1/9 NaN], eps);
%!
%!   h = quiver (hax, 0, 0, 1, 0, 1); # right
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{arrowheadchild}, [2/3 1 2/3 NaN], eps);
%!   assert (childydata{arrowheadchild}, [-1/9 0 1/9 NaN], eps);
%!
%!   h = quiver (hax, 0, 0, 1, 1, 1); # 45 deg - symmetric
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{arrowheadchild}, [7/9 1 5/9 NaN], eps);
%!   assert (childydata{arrowheadchild}, [5/9 1 7/9 NaN], eps);
%!
%!   h = quiver (hax, 0, 0, sqrt(3), 1, 1); # 30 deg
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4);
%!   assert (childxdata{arrowheadchild}, [(6*sqrt(3)+1)/9, sqrt(3), (6*sqrt(3)-1)/9, NaN], eps);
%!   assert (childydata{arrowheadchild}, [(6-sqrt(3))/9, 1, (6+sqrt(3))/9, NaN], eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check standard inputs, multiple arrows.
%!test
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   [x,y] = meshgrid (0:1);
%!   u = [0 1; 1 -2];
%!   v = [1 0; 1 -2];
%!   numpts = 4;
%!   h = quiver (hax, u, v, 1);  # assumes [x,y] = meshgrid (1:2)
%!   childxdata = get (get (h, "children"), "xdata");
%!   basechild = find (cellfun (@numel, childxdata) == 1*numpts);
%!   stemchild = find (cellfun (@numel, childxdata) == 3*numpts);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4*numpts);
%!   assert (childxdata{basechild}, [1 1 2 2]);
%!   assert (childxdata{stemchild}, [1,1,NaN,1,1.25,NaN,2,2.25,NaN,2,1.5,NaN], eps);
%!   assert (childxdata{arrowheadchild}([2, 6, 10, 14]), [1, 1.25, 2.25, 1.5], eps);
%!   assert (childxdata{arrowheadchild}([4, 8, 12, 16]), NaN(1,4), eps);
%!
%!   h = quiver (hax, x, y, u, v, 1);
%!   childxdata = get (get (h, "children"), "xdata");
%!   basechild = find (cellfun (@numel, childxdata) == 1*numpts);
%!   stemchild = find (cellfun (@numel, childxdata) == 3*numpts);
%!   arrowheadchild = find (cellfun (@numel, childxdata) == 4*numpts);
%!   assert (childxdata{basechild}, [0 0 1 1]);
%!   assert (childxdata{stemchild}, [0,0,NaN,0,0.25,NaN,1,1.25,NaN,1,0.5,NaN], eps);
%!   assert (childxdata{arrowheadchild}([2, 6, 10, 14]), [0, 0.25, 1.25, 0.5], eps);
%!   assert (childxdata{arrowheadchild}([4, 8, 12, 16]), NaN(1,4), eps);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check multiple arrows, vector inputs identical to array inputs.
%!test
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   [x,y] = meshgrid (0:1);
%!   u = [0 1; 1 -2];
%!   v = [1 0; 1 -2];
%!   h = quiver (hax, x, y, u, v, 1);  # arrayinput
%!   haxarray = get(hax);
%!   haxarray.children = [];
%!   haxarray.xlabel = [];
%!   haxarray.ylabel = [];
%!   haxarray.zlabel = [];
%!   haxarray.title = [];
%!   parentarray = get(h);
%!   parentarray.children = [];
%!   childrenarray = get (get (h, "children"));
%!   [childrenarray.parent] = deal ([]);
%!   h = quiver (hax, [0:1], [0:1], u, v, 1);
%!   haxvect1 = get(hax);
%!   haxvect1.children = [];
%!   haxvect1.xlabel= [];
%!   haxvect1.ylabel= [];
%!   haxvect1.zlabel= [];
%!   haxvect1.title= [];
%!   parentvect1 = get(h);
%!   parentvect1.children = [];
%!   childrenvect1 = get (get (h, "children"));
%!   [childrenvect1.parent] = deal ([]);
%!   assert (isequaln (haxarray, haxvect1));
%!   assert (isequaln (parentarray, parentvect1));
%!   assert (isequaln (childrenarray, childrenvect1));
%!   h = quiver (hax, [0:1], [0:1]', u, v, 1);
%!   haxvect2 = get(hax);
%!   haxvect2.children = [];
%!   haxvect2.xlabel= [];
%!   haxvect2.ylabel= [];
%!   haxvect2.zlabel= [];
%!   haxvect2.title= [];
%!   parentvect2 = get(h);
%!   parentvect2.children = [];
%!   childrenvect2 = get (get (h, "children"));
%!   [childrenvect2.parent] = deal ([]);
%!   assert (isequaln (haxvect1, haxvect2));
%!   assert (isequaln (parentvect1, parentvect2));
%!   assert (isequaln (childrenvect1, childrenvect2));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check scale factor "off" is identical to scale factor = 0.
%!test
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   h = quiver (hax, 1, 2, 0);
%!   haxzero = get(hax);
%!   haxzero.children = [];
%!   haxzero.xlabel = [];
%!   haxzero.ylabel = [];
%!   haxzero.zlabel = [];
%!   haxzero.title = [];
%!   parentzero = get(h);
%!   parentzero.children = [];
%!   childrenzero = get (get (h, "children"));
%!   [childrenzero.parent] = deal ([]);
%!   h = quiver (hax, 1, 2, "off");
%!   haxoff = get(hax);
%!   haxoff.children = [];
%!   haxoff.xlabel= [];
%!   haxoff.ylabel= [];
%!   haxoff.zlabel= [];
%!   haxoff.title= [];
%!   parentoff = get(h);
%!   parentoff.children = [];
%!   childrenoff = get (get (h, "children"));
%!   [childrenoff.parent] = deal ([]);
%!   assert (isequaln (haxzero, haxoff));
%!   assert (isequaln (parentzero, parentoff));
%!   assert (isequaln (childrenzero, childrenoff));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check input styles.
%!test
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   h = quiver (hax, 0, 1, 2, 3, "-o"); # Linestyle
%!   parent = get (h);
%!   assert (parent.marker, "o");
%!   assert (parent.markerfacecolor, "none");
%!   childdata = get (parent.children);
%!   basechild = find (cellfun (@numel, {childdata.xdata}) == 1);
%!   arrowheadchild = find (cellfun (@numel, {childdata.xdata}) == 4);
%!   assert (childdata(basechild).marker, "o");
%!   assert (childdata(basechild).markerfacecolor, "none");
%!   assert (childdata(basechild).linestyle, "none");
%!   assert (childdata(arrowheadchild).marker, "none");
%!   assert (childdata(arrowheadchild).markerfacecolor, "none");
%!   assert (childdata(arrowheadchild).linestyle, "none");
%!
%!   h = quiver (hax, 0, 1, 2, 3, "-o", "filled");  # Linestyle + filled.
%!   parent = get (h);
%!   assert (parent.marker, "o");
%!   assert (numel (parent.markerfacecolor), 3);
%!   childdata = get (parent.children);
%!   basechild = find (cellfun (@numel, {childdata.xdata}) == 1);
%!   arrowheadchild = find (cellfun (@numel, {childdata.xdata}) == 4);
%!   assert (childdata(basechild).marker, "o");
%!   assert (numel (childdata(basechild).markerfacecolor), 3);
%!   assert (childdata(basechild).linestyle, "none");
%!   assert (childdata(arrowheadchild).marker, "none");
%!   assert (childdata(arrowheadchild).markerfacecolor, "none");
%!   assert (childdata(arrowheadchild).linestyle, "none");
%!
%!   h = quiver (hax, 0, 1, 2, 3, "linewidth", 10); # Name/value pair.
%!   parent = get (h);
%!   assert (parent.marker, "none");
%!   assert (parent.markerfacecolor, "none");
%!   assert (parent.linestyle, "-");
%!   assert (parent.linewidth, 10);
%!   childdata = get (parent.children);
%!   basechild = find (cellfun (@numel, {childdata.xdata}) == 1);
%!   stemchild = find (cellfun (@numel, {childdata.xdata}) == 3);
%!   arrowheadchild = find (cellfun (@numel, {childdata.xdata}) == 4);
%!   assert (childdata(basechild).marker, "none");
%!   assert (childdata(basechild).markerfacecolor, "none");
%!   assert (childdata(basechild).linestyle, "none");
%!   assert (childdata(stemchild).marker, "none");
%!   assert (childdata(stemchild).markerfacecolor, "none");
%!   assert (childdata(stemchild).linestyle, "-");
%!   assert (childdata(stemchild).linewidth, 10);
%!   assert (childdata(arrowheadchild).marker, "none");
%!   assert (childdata(arrowheadchild).markerfacecolor, "none");
%!   assert (childdata(arrowheadchild).linestyle, "-");
%!   assert (childdata(arrowheadchild).linewidth, 10);
%!
%!  unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test both Linestyle with marker + name/value pair suppress arrowhead
%!test <*64143>
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   h = quiver (hax, 0, 1, 2, 3, "-o", "linewidth", 10);
%!   parent = get (h);
%!   assert (parent.marker, "o");
%!   assert (parent.markerfacecolor, "none");
%!   assert (parent.linestyle, "-");
%!   assert (parent.linewidth, 10);
%!   childdata = get (parent.children);
%!   basechild = find (cellfun (@numel, {childdata.xdata}) == 1);
%!   stemchild = find (cellfun (@numel, {childdata.xdata}) == 3);
%!   arrowheadchild = find (cellfun (@numel, {childdata.xdata}) == 4);
%!   assert (childdata(basechild).marker, "o");
%!   assert (childdata(basechild).markerfacecolor, "none");
%!   assert (childdata(basechild).linestyle, "none");
%!   assert (childdata(stemchild).marker, "none");
%!   assert (childdata(stemchild).markerfacecolor, "none");
%!   assert (childdata(stemchild).linestyle, "-");
%!   assert (childdata(stemchild).linewidth, 10);
%!   assert (childdata(arrowheadchild).marker, "none");
%!   assert (childdata(arrowheadchild).markerfacecolor, "none");
%!   assert (childdata(arrowheadchild).linestyle, "none");
%!   assert (childdata(arrowheadchild).linewidth, 10);
%!
%!  unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test that linewidth and color but not linestyle are passed to base marker
%!test <*64143>
%! hf = figure ("visible", "off");
%! hax = gca();
%! unwind_protect
%!   h = quiver (hax, 0, 1, 2, 3, "--o", "linewidth", 10, "color", [1 0 0]);
%!   parent = get (h);
%!   assert (parent.marker, "o");
%!   assert (parent.markerfacecolor, "none");
%!   assert (parent.linestyle, "--");
%!   assert (parent.linewidth, 10);
%!   childdata = get (parent.children);
%!   basechild = find (cellfun (@numel, {childdata.xdata}) == 1);
%!   stemchild = find (cellfun (@numel, {childdata.xdata}) == 3);
%!   arrowheadchild = find (cellfun (@numel, {childdata.xdata}) == 4);
%!   assert (childdata(basechild).marker, "o");
%!   assert (childdata(basechild).markerfacecolor, "none");
%!   assert (childdata(basechild).linestyle, "none");
%!   assert (childdata(basechild).color, [1 0 0]);
%!   assert (childdata(basechild).linewidth, 10);
%!   assert (childdata(stemchild).marker, "none");
%!   assert (childdata(stemchild).markerfacecolor, "none");
%!   assert (childdata(stemchild).linestyle, "--");
%!   assert (childdata(stemchild).linewidth, 10);
%!   assert (childdata(stemchild).color, [1 0 0]);
%!   assert (childdata(arrowheadchild).marker, "none");
%!   assert (childdata(arrowheadchild).markerfacecolor, "none");
%!   assert (childdata(arrowheadchild).linestyle, "none");
%!   assert (childdata(arrowheadchild).linewidth, 10);
%!   assert (childdata(arrowheadchild).color, [1 0 0]);
%!
%!  unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check arrow length, scale factor adjustment, one arrow.
%!test <*39552>
%! hf = figure ("visible", "off");
%! hax = gca ();
%! unwind_protect
%!   [x,y] = meshgrid (1:2);
%!   u = [0 1; 2 3];
%!   v = [1 2; 3 4];
%!   numpts = numel (x);
%!   sf = 0.5;
%!
%!   ## Check single arrow.
%!   h = quiver (hax, x(4), y(4), u(4), v(4), 1);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   xendpoint = childxdata{stemchild}(2);
%!   assert (xendpoint, x(4) + u(4), eps);
%!
%!   h = quiver (hax, x(4), y(4), u(4), v(4), sf);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3);
%!   xendpoint = childxdata{stemchild}(2);
%!   assert (xendpoint, x(4) + sf*u(4), eps);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check arrow length, scale factor adjustment, multiple arrows.
%!test <*39552>
%! hf = figure ("visible", "off");
%! hax = gca ();
%! unwind_protect
%!   [x,y] = meshgrid (1:2);
%!   u = [0 1; 2 3];
%!   v = [1 2; 3 4];
%!   numpts = numel (x);
%!   sf = 0.5;
%!
%!   ## Check multiple arrows.
%!   h = quiver (hax, x, y, u, v, 1);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3*numpts);
%!   xendpoint1 = childxdata{stemchild}(5);
%!   xendpoint2 = childxdata{stemchild}(11);
%!   assert (xendpoint1, x(2) + (sqrt(2)/10)*u(2), eps);
%!   assert (xendpoint2, x(4) + (sqrt(2)/10)*u(4), eps);
%!
%!   h = quiver (hax, x, y, u, v, sf);
%!   childxdata = get (get (h, "children"), "xdata");
%!   stemchild = find (cellfun (@numel, childxdata) == 3*numpts);
%!   xendpoint1 = childxdata{stemchild}(5);
%!   xendpoint2 = childxdata{stemchild}(11);
%!   assert (xendpoint1, x(2) + sf*(sqrt(2)/10)*u(2), eps);
%!   assert (xendpoint2, x(4) + sf*(sqrt(2)/10)*u(4), eps);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check for proper plotting with non-float inputs.
%!test <*59695>
%! hf = figure ("visible", "off");
%! hax = gca ();
%! unwind_protect
%!   h = quiver (int32(1), int32(1), int32(1), int32(1), double(0.5));
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   assert (all (strcmp (cellfun (...
%!                 'class', childxdata, 'UniformOutput', false), "double")));
%!   assert (all (strcmp (cellfun (...
%!                 'class', childydata, 'UniformOutput', false), "double")));
%!   assert (childxdata{2}(2) , 1.5, eps);
%!   assert (childxdata{3}(2) , 1.5, eps);
%!   assert (childydata{2}(2) , 1.5, eps);
%!   assert (childydata{3}(2) , 1.5, eps);
%!
%!   h = quiver (0.5, 0.5, 0.5, 0.5, int32(1));
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   assert (all (strcmp (cellfun (...
%!                 'class', childxdata, 'UniformOutput', false), "double")));
%!   assert (all (strcmp (cellfun (...
%!                 'class', childydata, 'UniformOutput', false), "double")));
%!   assert (childxdata{2}(2) , 1, eps);
%!   assert (childxdata{3}(2) , 1, eps);
%!   assert (childydata{2}(2) , 1, eps);
%!   assert (childydata{3}(2) , 1, eps);
%!
%!   h = quiver (false, true, false, true, true);
%!   children = get (h, "children");
%!   childxdata = get (children, "xdata");
%!   childydata = get (children, "ydata");
%!   assert (all (strcmp (cellfun (...
%!                 'class', childxdata, 'UniformOutput', false), "double")));
%!   assert (all (strcmp (cellfun (...
%!                 'class', childydata, 'UniformOutput', false), "double")));
%!   assert (childxdata{2}(2) , 0, eps);
%!   assert (childxdata{3}(2) , 0, eps);
%!   assert (childydata{2}(2) , 2, eps);
%!   assert (childydata{3}(2) , 2, eps);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> quiver()
%!error <Invalid call> quiver(1.1)
%!error <Invalid call> quiver(1.1, "foo")
%!error <Invalid call> quiver(1.1, 2, 3, 4, 5, 6, "foo")
%!error <U and V must be the same size> quiver ([1, 2], 3)
%!error <U and V must be the same size> quiver (1.1, [2, 3])
%!error <U and V must be the same size> quiver (1.1, 2, eye(2), 4)
%!error <U and V must be the same size> quiver (1.1, 2, 3, eye(2))
%!error <X vector length must equal> quiver (1.1, [2 3], eye(2), eye(2))
%!error <Y vector length must equal> quiver ([1, 2], 3, eye(2), eye(2))
%!error <X, Y, U, and V must be the same size> quiver (eye(3), eye(2), eye(2), eye(2))
%!error <X, Y, U, and V must be the same size> quiver (eye(2), eye(3), eye(2), eye(2))
%!error <X, Y, U, and V must be the same size> quiver (eye(2), eye(2), eye(3), eye(2))
%!error <X, Y, U, and V must be the same size> quiver (eye(2), eye(2), eye(2), eye(3))
%!error <scaling factor must be> quiver (10, 20, -5)
%!error <scaling factor must be> quiver (10, 20, [1 2])
%!error <scaling factor must be> quiver (10, 20, 30, 40, -5)
%!error <scaling factor must be> quiver (10, 20, 30, 40, [1 2])
