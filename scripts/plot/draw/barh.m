########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
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
## @deftypefn  {} {} barh (@var{y})
## @deftypefnx {} {} barh (@var{x}, @var{y})
## @deftypefnx {} {} barh (@dots{}, @var{w})
## @deftypefnx {} {} barh (@dots{}, @var{style})
## @deftypefnx {} {} barh (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} barh (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} barh (@dots{}, @var{prop}, @var{val}, @dots{})
## Produce a horizontal bar graph from two vectors of X-Y data.
##
## If only one argument is given, it is taken as a vector of Y values
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
## Y-coordinate.
##
## @item  @qcode{"stacked"}
## Bars are stacked so that each Y value has a single bar composed of
## multiple segments.
##
## @item @qcode{"hist"}
## Side-by-side bars with no gap between bars and centered over the
## Y-coordinate.
##
## @item @qcode{"histc"}
## Side-by-side bars with no gap between bars and left-aligned to the
## Y-coordinate.
## @end table
##
## Optional property/value pairs are passed directly to the underlying patch
## objects.  The full list of properties is documented at
## @ref{Patch Properties}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## bar series hggroup.  For a description of the use of the
## bar series, @pxref{XREFbar,,@code{bar}}.
## @seealso{bar, hist, pie, plot, patch}
## @end deftypefn

function varargout = barh (varargin)
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ ("barh", false, varargin{:});
endfunction


%!demo
%! clf;
%! x = rand (10, 1);
%! barh (x);
%! title ("barh() graph");

%!demo
%! clf;
%! h = barh (rand (5, 3));
%! set (h(1), "facecolor", "r");
%! set (h(2), "facecolor", "g");
%! set (h(3), "facecolor", "b");
%! title ("barh() graph w/multiple bars");

%!demo
%! clf;
%! x = -rand (3) .* eye (3) + rand (3) .* (! eye (3));
%! h = barh (x, "stacked");
%! title ("stacked barh() graph including intermingled negative values");

%% Test input validation
%!error barh ()
%!error <Y must be numeric> barh ("foo")
%!error <X must be a vector> barh ([1 2; 3 4], [1 2 3 4])
%!error <X vector values must be unique> barh ([1 2 3 3], [1 2 3 4])
%!error <length of X and Y must be equal> barh ([1 2 3], [1 2 3 4])
%!error <length of X and Y must be equal> barh ([1 2 3 4], [1 2 3])
