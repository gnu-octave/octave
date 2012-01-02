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
## @deftypefn {Function File} {@var{h} =} stem3 (@var{x}, @var{y}, @var{z}, @var{linespec})
## Plot a three-dimensional stem graph and return the handles of the line
## and marker objects used to draw the stems as "stem series" object.
## The default color is @code{"r"} (red).  The default line style is
## @code{"-"} and the default marker is @code{"o"}.
##
## For example,
##
## @example
## @group
## theta = 0:0.2:6;
## stem3 (cos (theta), sin (theta), theta)
## @end group
## @end example
##
## @noindent
## plots 31 stems with heights from 0 to 6 lying on a circle.  Color
## definitions with RGB-triples are not valid!
## @seealso{bar, barh, stem, plot}
## @end deftypefn

function h = stem3 (varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  tmp = __stem__ (true, varargin{:});

  if (nargout > 0)
    h = tmp;
  endif

endfunction

%!demo
%! clf
%! theta = 0:0.2:6;
%! stem3 (cos (theta), sin (theta), theta)
