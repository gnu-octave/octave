########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn  {} {} bar (@var{y})
## @deftypefnx {} {} bar (@var{x}, @var{y})
## @deftypefnx {} {} bar (@dots{}, @var{w})
## @deftypefnx {} {} bar (@dots{}, @var{style})
## @deftypefnx {} {} bar (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} bar (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} bar (@dots{}, @var{prop}, @var{val}, @dots{})
## Produce a bar graph from two vectors of X-Y data.
##
## If only one argument is given, @var{y}, it is taken as a vector of Y values
## and the X coordinates are the range @code{1:numel (@var{y})}.
##
## The optional input @var{w} controls the width of the bars.  A value of
## 1.0 will cause each bar to exactly touch any adjacent bars.
## The default width is 0.8.
##
## If @var{y} is a matrix, then each column of @var{y} is taken to be a
## separate bar graph plotted on the same graph.  By default the columns
## are plotted side-by-side.  This behavior can be changed by the @var{style}
## argument which can take the following values:
##
## @table @asis
## @item @qcode{"grouped"} (default)
## Side-by-side bars with a gap between bars and centered over the
## X-coordinate.
##
## @item  @qcode{"stacked"}
## Bars are stacked so that each X value has a single bar composed of
## multiple segments.
##
## @item @qcode{"hist"}
## Side-by-side bars with no gap between bars and centered over the
## X-coordinate.
##
## @item @qcode{"histc"}
## Side-by-side bars with no gap between bars and left-aligned to the
## X-coordinate.
## @end table
##
## Optional property/value pairs are passed directly to the underlying patch
## objects.  The full list of properties is documented at
## @ref{Patch Properties}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of handles to the created
## "bar series" hggroups with one handle per column of the variable @var{y}.
## This series makes it possible to change a common element in one bar series
## object and have the change reflected in the other "bar series".
## For example,
##
## @example
## @group
## h = bar (rand (5, 10));
## set (h(1), "basevalue", 0.5);
## @end group
## @end example
##
## @noindent
## changes the position on the base of all of the bar series.
##
## The following example modifies the face and edge colors using
## property/value pairs.
##
## @example
## bar (randn (1, 100), "facecolor", "r", "edgecolor", "b");
## @end example
##
## @noindent
## The default color for bars is taken from the axes' @qcode{"ColorOrder"}
## property.  The default color for bars when a histogram option
## (@qcode{"hist"}, @qcode{"histc"} is used is the @qcode{"Colormap"} property
## of either the axes or figure.  The color of bars can also be set manually
## using the @qcode{"facecolor"} property as shown below.
##
## @example
## @group
## h = bar (rand (10, 3));
## set (h(1), "facecolor", "r")
## set (h(2), "facecolor", "g")
## set (h(3), "facecolor", "b")
## @end group
## @end example
##
## @seealso{barh, hist, pie, plot, patch}
## @end deftypefn

function varargout = bar (varargin)
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ ("bar", true, varargin{:});
endfunction


%!demo
%! clf;
%! y = rand (11, 1);
%! h = bar (y);
%! set (h, "ydata", sort (rand (11, 1)));
%! title ("bar() graph");

%!demo
%! clf;
%! h = bar (rand (5, 3));
%! set (h(1), "facecolor", "r");
%! set (h(2), "facecolor", "g");
%! set (h(3), "facecolor", "b");
%! title ("bar() graph w/multiple bars");

%!demo
%! clf;
%! h = bar (rand (5, 3), "stacked");
%! title ("bar() graph with stacked style");

%!demo
%! clf;
%! y = -rand (3) .* eye (3) + rand (3) .* (! eye (3));
%! h = bar (y, "stacked");
%! title ("stacked bar() graph including intermingled negative values");

%% Test input validation
%!error bar ()
%!error <Y must be numeric> bar ("foo")
%!error <X must be a vector> bar ([1 2; 3 4], [1 2 3 4])
%!error <X vector values must be unique> bar ([1 2 3 3], [1 2 3 4])
%!error <length of X and Y must be equal> bar ([1 2 3], [1 2 3 4])
%!error <length of X and Y must be equal> bar ([1 2 3 4], [1 2 3])
