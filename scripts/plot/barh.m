## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {@var{h} =} barh (@var{x}, @var{y}, @var{style})
## @deftypefnx {Function File} {[@var{xb}, @var{yb}] =} barh (@dots{})
## Given two vectors of x-y data, @code{bar} produce a horizontal bar graph.
##
## If only one argument is given, it is taken as a vector of y-values
## and the x coordinates are taken to be the indices of the elements.
##
## If @var{y} is a matrix, then each column of @var{y} is taken to be a
## separate bar graph plotted on the same graph. By default the columns
## are plotted side-by-side. This behavior can be changed by the @var{style}
## argument, which can take the values 'group' (the default), or 'stack'.
##
## If two output arguments are specified, the data are generated but
## not plotted.  For example,
##
## @example
## barh (x, y);
## @end example
##
## @noindent
## and
##
## @example
## [xb, yb] = barh (x, y);
## plot (xb, yb);
## @end example
##
## @noindent
## are equivalent.
## @seealso{bar, plot}
## @end deftypefn

## Author: jwe

function varargout = barh (varargin)
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ (false, "barh", varargin{:});
endfunction
