## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007, 2008, 2009 John W. Eaton
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
## If only one argument is given, it is taken as a vector of y-values
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
## The optional return value @var{h} provides a handle to the "bar series"
## object with one handle per column of the variable @var{y}.  This
## series allows common elements of the group of bar series objects to
## be changed in a single bar series and the same properties are changed
## in the other "bar series".  For example
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
## Properties of the patch graphics object can be changed using
## @var{prop}, @var{val} pairs.
##
## The color of the bars is taken from the figure's colormap, such that
##
## @example
## @group
## bar (rand(10,3))
## colormap (summer ())
## @end group
## @end example
##
## will change the colors used for the bars.  If you wish to force the bars to
## particular colors, this can be achieved like
##
## @example
## @group
## h = bar (rand(10,3))
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
