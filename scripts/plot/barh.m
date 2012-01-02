## Copyright (C) 1996-2012 John W. Eaton
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
## @deftypefn  {Function File} {} barh (@var{x}, @var{y})
## @deftypefnx {Function File} {} barh (@var{y})
## @deftypefnx {Function File} {} barh (@var{x}, @var{y}, @var{w})
## @deftypefnx {Function File} {} barh (@var{x}, @var{y}, @var{w}, @var{style})
## @deftypefnx {Function File} {@var{h} =} barh (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} barh (@var{h}, @dots{})
## Produce a horizontal bar graph from two vectors of x-y data.
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
## The optional input handle @var{h} allows an axis handle to be passed.
## Properties of the patch graphics object can be changed using
## @var{prop}, @var{val} pairs.
##
## The optional return value @var{h} is a graphics handle to the created
## bar series object.  See @code{bar} for a description of the use of the
## bar series.
## @seealso{bar, plot}
## @end deftypefn

## Author: jwe

function varargout = barh (varargin)
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ (false, "barh", varargin{:});
endfunction


%% FIXME: Need demo or test for function

