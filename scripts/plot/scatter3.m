## Copyright (C) 2007 David Bateman
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
## @deftypefn {Function File} {} scatter3 (@var{x}, @var{y}, @var{s}, @var{c})
## @deftypefnx {Function File} {} scatter3 (@dots{}, 'filled')
## @deftypefnx {Function File} {} scatter3 (@dots{}, @var{style})
## @deftypefnx {Function File} {} scatter3 (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} scatter3 (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} scatter3 (@dots{})
##
## Plot a scatter plot of the data in 3D. A marker is ploted at each point 
## defined by the points in the vectors @var{x} and @var{y}. The size of
## the markers used is determined by the @var{s}, which can be a scalar, 
## a vector of the same length of @var{x} and @var{y}. If @var{s} is not 
## given or is an empty matrix, then the default value of 8 points is used.
##
## The color of the markers is determined by @var{c}, which can be a string
## defining a fixed color, a 3 element vector giving the red, green and blue 
## components of the color, a vector of the same length as @var{x} that gives
## a scaled index into the current colormap, or a @var{n}-by-3 matrix defining
## the colors of each of the markers individually.
##
## The marker to use can be changed with the @var{style} argument, that is a 
## string defining a marker in the same manner as the @code{plot} command. 
## If the argument 'filled' is given then the markers as filled. All 
## additional arguments are passed to the underlying patch command.
##
## The optional return value @var{h} provides a handle to the patch object
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

  if (nargin < 2)
    print_usage ();
  elseif (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin {1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("scatter3: expecting first argument to be an axes object");
    endif
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      tmp = __scatter__ (h, 3, "scatter3", varargin{2:end});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    newplot ();
    tmp = __scatter__ (gca (), 3, "scatter3", varargin{:});
  endif

  if (! ishold ())
    set (get (tmp, "parent"), "view", [-37.5, 30]);
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction
