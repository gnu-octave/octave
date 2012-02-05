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
## @deftypefn  {Function File} {} bar (@var{x}, @var{y})
## @deftypefnx {Function File} {} bar (@var{y})
## @deftypefnx {Function File} {} bar (@var{x}, @var{y}, @var{w})
## @deftypefnx {Function File} {} bar (@var{x}, @var{y}, @var{w}, @var{style})
## @deftypefnx {Function File} {@var{h} =} bar (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} bar (@var{h}, @dots{})
## Produce a bar graph from two vectors of x-y data.
##
## If only one argument is given, @var{y}, it is taken as a vector of y-values
## and the x coordinates are taken to be the indices of the elements.
##
## The default width of 0.8 for the bars can be changed using @var{w}.
##
## If @var{y} is a matrix, then each column of @var{y} is taken to be a
## separate bar graph plotted on the same graph.  By default the columns
## are plotted side-by-side.  This behavior can be changed by the @var{style}
## argument, which can take the values @code{"grouped"} (the default),
## or @code{"stacked"}.
##
## The optional return value @var{h} is a handle to the created "bar series"
## object with one handle per column of the variable @var{y}.  This
## series allows common elements of the group of bar series objects to
## be changed in a single bar series and the same properties are changed
## in the other "bar series".  For example,
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
## The optional input handle @var{h} allows an axis handle to be passed.
##
## The bar graph's appearance may be modified by specifying property/value
## pairs.  The following example modifies the face and edge colors.
##
## @example
## bar (randn (1, 100), "facecolor", "r", "edgecolor", "b")
## @end example
##
## @noindent
## The color of the bars is taken from the figure's colormap, such that
##
## @example
## @group
## bar (rand (10, 3));
## colormap (summer (64));
## @end group
## @end example
##
## @noindent
## will change the colors used for the bars.  The color of bars can also be set
## manually using the "facecolor" property as shown below.
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
## @seealso{barh, plot}
## @end deftypefn

## Author: jwe

function varargout = bar (varargin)
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ (true, "bar", varargin{:});
endfunction


%% FIXME: Need demo or test for function

