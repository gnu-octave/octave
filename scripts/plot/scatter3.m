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
## @deftypefn  {Function File} {} scatter3 (@var{x}, @var{y}, @var{z}, @var{s}, @var{c})
## @deftypefnx {Function File} {} scatter3 (@dots{}, 'filled')
## @deftypefnx {Function File} {} scatter3 (@dots{}, @var{style})
## @deftypefnx {Function File} {} scatter3 (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} scatter3 (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} scatter3 (@dots{})
##
## Plot a scatter plot of the data in 3D@.  A marker is plotted at each point
## defined by the points in the vectors @var{x}, @var{y} and @var{z}.  The size
## of the markers used is determined by @var{s}, which can be a scalar or
## a vector of the same length of @var{x}, @var{y} and @var{z}.  If @var{s} is
## not given or is an empty matrix, then the default value of 8 points is used.
##
## The color of the markers is determined by @var{c}, which can be a string
## defining a fixed color; a 3-element vector giving the red, green, and blue
## components of the color; a vector of the same length as @var{x} that gives
## a scaled index into the current colormap; or a @var{n}-by-3 matrix defining
## the colors of each of the markers individually.
##
## The marker to use can be changed with the @var{style} argument, that is a
## string defining a marker in the same manner as the @code{plot} command.
## If the argument 'filled' is given then the markers as filled.  All
## additional arguments are passed to the underlying patch command.
##
## The optional return value @var{h} is a graphics handle to the hggroup
## object representing the points.
##
## @example
## @group
## [x, y, z] = peaks (20);
## scatter3 (x(:), y(:), z(:), [], z(:));
## @end group
## @end example
##
## @seealso{plot, patch, scatter}
## @end deftypefn

function retval = scatter3 (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ("scatter3", varargin{:});

  if (nargin < 2)
    print_usage ();
  else
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      tmp = __scatter__ (h, 3, "scatter3", varargin{:});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  endif

  if (! ishold ())
    set (h, "view", [-37.5, 30],
         "xgrid", "on", "ygrid", "on", "zgrid", "on");
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction


%!demo
%! clf
%! [x, y, z] = peaks (20);
%! scatter3 (x(:), y(:), z(:), [], z(:));

%!demo
%! clf
%! x = rand (20,1);
%! y = rand (20,1);
%! z = rand (20,1);
%! scatter3 (x(:), y(:), z(:), 10, z(:), "s");

%!demo
%! clf
%! x = rand (20,1);
%! y = rand (20,1);
%! z = rand (20,1);
%! scatter3 (x(:), y(:), z(:), 20*z(:), z(:), "s");

%!demo
%! clf
%! x = rand (20,1);
%! y = rand (20,1);
%! z = rand (20,1);
%! scatter3 (x(:), y(:), z(:), 20*z(:), [], "s");

