########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn  {} {} line ()
## @deftypefnx {} {} line (@var{x}, @var{y})
## @deftypefnx {} {} line (@var{x}, @var{y}, @var{z})
## @deftypefnx {} {} line ("xdata", @var{x}, "ydata", @var{y})
## @deftypefnx {} {} line ("xdata", @var{x}, "ydata", @var{y}, "zdata", @var{z})
## @deftypefnx {} {} line (@dots{}, @var{property}, @var{value})
## @deftypefnx {} {} line (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} line (@dots{})
## Create a line object from @var{x} and @var{y} (and possibly @var{z}) and
## insert it in the current axes.
##
## In the standard calling form the data @var{x}, @var{y}, and @var{z} may be
## scalars, vectors, or matrices.  In the case of matrix inputs, @code{line}
## will attempt to orient scalars and vectors so the results can be plotted.
## This requires that one of the dimensions of the vector match either the
## number of rows or the number of columns of the matrix.
##
## In the low-level calling form (50% higher performance) where the data is
## specified by name (@code{line ("xdata", @var{x}, @dots{})}) the data must be
## vectors.  If no data is specified (@code{line ()}) then
## @w{@code{@var{x} == @var{y} = [0, 1]}}.
##
## Multiple property-value pairs may be specified for the line object, but they
## must appear in pairs.
##
## If called with only @var{property}/@var{value} pairs then any unspecified
## properties use their default values as specified on the root object.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle (or vector of
## handles) to the line objects created.
##
## Programming Note: The full list of properties is documented at
## @ref{Line Properties}.
##
## The function @code{line} differs from @code{plot} in that line objects are
## inserted in to the current axes without first clearing the plot.
## @seealso{image, patch, rectangle, surface, text}
## @end deftypefn

function h = line (varargin)

  ## Get axis argument which may be in a 'parent' PROP/VAL pair
  [hax, varargin] = __plt_get_axis_arg__ ("line", varargin{:});

  if (isempty (hax))
    hax = gca ();
    oldfig = [];
  else
    hax = hax(1);
    oldfig = get (0, "currentfigure");
    set (0, "currentfigure", ancestor (hax, "figure"));
  endif

  unwind_protect
    htmp = __line__ (hax, varargin{:});
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
%! line ([0 1], [0.8 0.8], "linestyle", "-", "color", "b");
%! line ([0 1], [0.6 0.6], "linestyle", "--", "color", "g");
%! line ([0 1], [0.4 0.4], "linestyle", ":", "color", "r");
%! line ([0 1], [0.2 0.2], "linestyle", "-.", "color", "k");
%! ylim ([0 1]);
%! title ("line() with various linestyles");
%! legend ('"-"', '"--"', '":"', '"-."', 'location', 'eastoutside');

%!demo
%! clf;
%! x = 0:0.3:10;
%! y1 = cos (x);
%! y2 = sin (x);
%! subplot (3,1,1);
%!  args = {"color", "b", "marker", "s"};
%!  line ([x(:), x(:)], [y1(:), y2(:)], args{:});
%!  title ("Test broadcasting for line()");
%! subplot (3,1,2);
%!  line (x(:), [y1(:), y2(:)], args{:});
%! subplot (3,1,3);
%!  line ([x(:), x(:)+pi/2], y1(:), args{:});
%!  xlim ([0 10]);

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = line ();
%!   assert (findobj (hf, "type", "line"), h);
%!   assert (get (h, "xdata"), [0 1], eps);
%!   assert (get (h, "ydata"), [0 1], eps);
%!   assert (get (h, "type"), "line");
%!   assert (get (h, "color"), get (0, "defaultlinecolor"));
%!   assert (get (h, "linestyle"), get (0, "defaultlinelinestyle"));
%!   assert (get (h, "linewidth"), get (0, "defaultlinelinewidth"), eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
