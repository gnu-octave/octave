## Copyright (C) 1999-2012 Kai Habel
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
## @deftypefn  {Function File} {} delaunay (@var{x}, @var{y})
## @deftypefnx {Function File} {} delaunay (@var{x})
## @deftypefnx {Function File} {} delaunay (@dots{}, @var{options})
## @deftypefnx {Function File} {@var{tri} =} delaunay (@dots{})
## Compute the Delaunay triangulation for a 2-D set of points.
## The return value @var{tri} is a set of triangles which satisfies the
## Delaunay circum-circle criterion, i.e., only a single data point from
## [@var{x}, @var{y}] is within the circum-circle of the defining triangle.
## The input @var{x} may also be a matrix with two columns where the first
## column contains x-data and the second y-data.
##
## The set of triangles @var{tri} is a matrix of size [n, 3].  Each
## row defines a triangle and the three columns are the three vertices
## of the triangle.  The value of @code{@var{tri}(i,j)} is an index into
## @var{x} and @var{y} for the location of the j-th vertex of the i-th
## triangle.
##
## The optional last argument, which must be a string or cell array of strings,
## contains options passed to the underlying qhull command.
## See the documentation for the Qhull library for details
## @url{http://www.qhull.org/html/qh-quick.htm#options}.
## The default options are @code{@{"Qt", "Qbb", "Qc", "Qz"@}}.
##
## If @var{options} is not present or @code{[]} then the default arguments are
## used.  Otherwise, @var{options} replaces the default argument list. 
## To append user options to the defaults it is necessary to repeat the 
## default arguments in @var{options}.  Use a null string to pass no arguments.
##
## If no output argument is specified the resulting Delaunay triangulation 
## is plotted along with the original set of points.
##
## @example
## @group
## x = rand (1, 10);
## y = rand (1, 10);
## T = delaunay (x, y);
## VX = [ x(T(:,1)); x(T(:,2)); x(T(:,3)); x(T(:,1)) ];
## VY = [ y(T(:,1)); y(T(:,2)); y(T(:,3)); y(T(:,1)) ];
## axis ([0,1,0,1]);
## plot (VX, VY, "b", x, y, "r*");
## @end group
## @end example
## @seealso{delaunay3, delaunayn, convhull, voronoi, triplot, trimesh, trisurf}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function tri = delaunay (varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  options = [];

  switch (nargin)

  case 1
    if (! ismatrix (varargin{1}) && columns (varargin{1}) != 2)
        error ("delaunay: X must be a matrix with 2 columns");
    else
      x = varargin{1}(:,1);
      y = varargin{1}(:,2);
    endif
  
  case 2
    if (isnumeric (varargin{2}))
      x = varargin{1};
      y = varargin{2};
    elseif (ischar (varargin{2}) || iscellstr (varargin{2}))
      options = varargin{2};
      if (! ismatrix (varargin{1}) && columns (varargin{1}) != 2)
          error ("delaunay: X must be a matrix with 2 columns");
      else
        x = varargin{1}(:,1);
        y = varargin{1}(:,2);
      endif
    else
      error ("delaunay: OPTIONS must be a string or cell array of strings");
    endif

  case 3
    x = varargin{1};
    y = varargin{2};
    options = varargin{3};

    if (! (ischar (options) || iscellstr (options)))
      error ("delaunay: OPTIONS must be a string or cell array of strings");
    endif

  endswitch

  if (! (isvector (x) && isvector (y) && length (x) == length (y)))
    error ("delaunay: X and Y must be the same size");
  endif

  T = delaunayn ([x(:), y(:)], options);

  if (nargout == 0)
    x = x(:).';
    y = y(:).';
    VX = [ x(T(:,1)); x(T(:,2)); x(T(:,3)); x(T(:,1)) ];
    VY = [ y(T(:,1)); y(T(:,2)); y(T(:,3)); y(T(:,1)) ];
    plot (VX, VY, "b", x, y, "r*");
  else
    tri = T;
  endif

endfunction


%!demo
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 1);
%! x = rand (1,10);
%! y = rand (1,10);
%! T = delaunay (x,y);
%! VX = [ x(T(:,1)); x(T(:,2)); x(T(:,3)); x(T(:,1)) ];
%! VY = [ y(T(:,1)); y(T(:,2)); y(T(:,3)); y(T(:,1)) ];
%! clf;
%! plot (VX,VY,"b", x,y,"r*");
%! axis ([0,1,0,1]);

%!testif HAVE_QHULL
%! x = [-1, 0, 1, 0];
%! y = [0, 1, 0, -1];
%! assert (sortrows (sort (delaunay (x, y), 2)), [1,2,4;2,3,4]);

%!testif HAVE_QHULL
%! x = [-1, 0, 1, 0];
%! y = [0, 1, 0, -1];
%! assert (sortrows (sort (delaunay ([x(:) y(:)]), 2)), [1,2,4;2,3,4]);

%!testif HAVE_QHULL
%! x = [-1, 0, 1, 0, 0];
%! y = [0, 1, 0, -1, 0];
%! assert (sortrows (sort (delaunay (x, y), 2)), [1,2,5;1,4,5;2,3,5;3,4,5]);

%% FIXME: Need input validation tests

