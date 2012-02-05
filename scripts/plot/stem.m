## Copyright (C) 2006-2012 Michel D. Schmid
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
## @deftypefn  {Function File} {} stem (@var{x})
## @deftypefnx {Function File} {} stem (@var{x}, @var{y})
## @deftypefnx {Function File} {} stem (@var{x}, @var{y}, @var{linespec})
## @deftypefnx {Function File} {} stem (@dots{}, "filled")
## @deftypefnx {Function File} {@var{h} =} stem (@dots{})
## Plot a stem graph from two vectors of x-y data.  If only one argument
## is given, it is taken as the y-values and the x coordinates are taken
## from the indices of the elements.
##
## If @var{y} is a matrix, then each column of the matrix is plotted as
## a separate stem graph.  In this case @var{x} can either be a vector,
## the same length as the number of rows in @var{y}, or it can be a
## matrix of the same size as @var{y}.
##
## The default color is @code{"b"} (blue).  The default line style is
## @code{"-"} and the default marker is @code{"o"}.  The line style can
## be altered by the @code{linespec} argument in the same manner as the
## @code{plot} command.  For example,
##
## @example
## @group
## x = 1:10;
## y = 2*x;
## stem (x, y, "r");
## @end group
## @end example
##
## @noindent
## plots 10 stems with heights from 2 to 20 in red;
##
## The optional return value @var{h} is a vector of "stem series" graphics
## handles with one handle per column of the variable @var{y}.  The
## handle regroups the elements of the stem graph together as the
## children of the "stem series" handle, allowing them to be altered
## together.  For example,
##
## @example
## @group
## x = [0:10]';
## y = [sin(x), cos(x)]
## h = stem (x, y);
## set (h(2), "color", "g");
## set (h(1), "basevalue", -1)
## @end group
## @end example
##
## @noindent
## changes the color of the second "stem series" and moves the base line
## of the first.
## @seealso{bar, barh, plot}
## @end deftypefn

## Author: Michel D. Schmid <michaelschmid@users.sourceforge.net>
## Adapted-by: jwe

function h = stem (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  tmp = __stem__ (false, varargin{:});

  if (nargout > 0)
    h = tmp;
  endif

endfunction


%!demo
%! clf
%! x = 1:10;
%! stem (x);

%!demo
%! clf
%! x = 1:10;
%! y = 2*x;
%! stem (x, y);

%!demo
%! clf
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, "r");

%!demo
%! clf
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, "-.k");

%!demo
%! clf
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, "-.k.");

%!demo
%! clf
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, "filled");

%!demo
%! clf
%! x = [0 : 10]';
%! y = [sin(x), cos(x)];
%! h = stem (x, y);
%! set (h(2), "color", "g");
%! set (h(1), "basevalue", -1)

