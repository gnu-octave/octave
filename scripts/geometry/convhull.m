########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{H} =} convhull (@var{x}, @var{y})
## @deftypefnx {} {@var{H} =} convhull (@var{x}, @var{y}, @var{z})
## @deftypefnx {} {@var{H} =} convhull (@var{x})
## @deftypefnx {} {@var{H} =} convhull (@dots{}, @var{options})
## @deftypefnx {} {[@var{H}, @var{V}] =} convhull (@dots{})
## Compute the convex hull of a 2-D or 3-D set of points.
##
## The hull @var{H} is a linear index vector into the original set of points
## that specifies which points form the enclosing hull.  For 2-D inputs only,
## the output is ordered in a counterclockwise manner around the hull.
##
## The input @var{x} may also be a matrix with two or three columns where the
## first column contains x-data, the second y-data, and the optional third
## column contains z-data.
##
## An optional final argument, which must be a string or cell array of strings,
## contains options passed to the underlying qhull command.
## See the documentation for the Qhull library for details
## @url{http://www.qhull.org/html/qh-quick.htm#options}.
## The default option is @code{@{"Qt"@}}.
##
## If @var{options} is not present or @code{[]} then the default arguments are
## used.  Otherwise, @var{options} replaces the default argument list.
## To append user options to the defaults it is necessary to repeat the
## default arguments in @var{options}.  Use a null string to pass no arguments.
##
## If the second output @var{V} is requested the volume of the enclosing
## convex hull is calculated.
##
## @seealso{convhulln, delaunay, voronoi}
## @end deftypefn

function [H, V] = convhull (varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  z = [];
  options = [];

  switch (nargin)

    case 1
      if (! ismatrix (varargin{1})
          || (columns (varargin{1}) != 2 && columns (varargin{1}) != 3))
          error ("convhull: X must be a matrix with 2 or 3 columns");
      else
        x = varargin{1}(:,1);
        y = varargin{1}(:,2);
        if (columns (varargin{1}) == 3)
          z = varargin{1}(:,3);
        endif
      endif

    case 2
      if (isnumeric (varargin{2}))
        x = varargin{1};
        y = varargin{2};
      elseif (! (ischar (varargin{2}) || iscellstr (varargin{2})))
        error ("convhull: OPTIONS must be a string or cell array of strings");
      else
        options = varargin{2};
        ncols = columns (varargin{1});

        if (! ismatrix (varargin{1}) || (ncols != 2 && ncols != 3))
          error ("convhull: X must be a matrix with 2 or 3 columns");
        else
          x = varargin{1}(:,1);
          y = varargin{1}(:,2);
          if (ncols == 3)
            z = varargin{1}(:,3);
          endif
        endif
      endif

    case 3
      if (isnumeric (varargin{3}))
        x = varargin{1};
        y = varargin{2};
        z = varargin{3};
      elseif (! (ischar (varargin{3}) || iscellstr (varargin{3})))
        error ("convhull: OPTIONS must be a string or cell array of strings");
      else
        x = varargin{1};
        y = varargin{2};
        options = varargin{3};
      endif

    case 4
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      options = varargin{4};

      if (! (ischar (options) || iscellstr (options)))
        error ("convhull: OPTIONS must be a string or cell array of strings");
      endif

  endswitch

  if (isempty (z))
    x = x(:);  y = y(:);
    if (! size_equal (x, y))
      error ("convhull: X and Y must be the same size");
    endif
    if (nargout > 1)
      [Htmp, V] = convhulln ([x, y], options);
    else
      Htmp = convhulln ([x, y], options);
    endif
  else
    x = x(:);  y = y(:);  z = z(:);
    if (! size_equal (x, y, z))
      error ("convhull: X, Y, and Z must be the same size");
    endif
    if (nargout > 1)
      [H, V] = convhulln ([x, y, z], options);
    else
      H = convhulln ([x, y, z], options);
    endif
  endif

  if (isempty (z))
    ## Order 2-D convex hull in a counter-clockwise manner.
    n = rows (Htmp);
    Htmp = Htmp.'(:);
    H = zeros (n + 1, 1);

    H(1) = Htmp(1);
    next_pt = Htmp(2);
    Htmp(2) = 0;
    for k = 2:n
      next_idx = find (Htmp == next_pt);
      H(k) = Htmp(next_idx);

      if (rem (next_idx, 2) == 0)
        next_pt =  Htmp(next_idx - 1);
         Htmp(next_idx - 1) = 0;
      else
        next_pt = Htmp(next_idx + 1);
         Htmp(next_idx + 1) = 0;
      endif
    endfor

    H(n + 1) = H(1);
  endif

endfunction


%!demo
%! clf;
%! x = -3:0.05:3;
%! y = abs (sin (x));
%! k = convhull (x, y);
%! plot (x(k),y(k),"r-;convex hull;", x,y,"b+;points;");
%! axis ([-3.05, 3.05, -0.05, 1.05]);

%!testif HAVE_QHULL
%! x = -3:0.5:3;
%! y = abs (sin (x));
%! assert (convhull (x, y), [1;7;13;12;11;10;4;3;2;1]);

%!testif HAVE_QHULL
%! [~, V] = convhull ([0,2,2,0], [0,0,1,1]);
%! assert (V == 2);

## Input validation tests
%!error <Invalid call> convhull ()
%!error <Invalid call> convhull (1,2,3,4,5)
%!error <X must be a matrix with 2 or 3 columns> convhull (ones (2,4))
%!error <OPTIONS must be a string or cell array> convhull (ones (2,2), struct ())
%!error <X must be a matrix with 2 or 3 columns> convhull (ones (2,4), "")
%!error <OPTIONS must be a string or cell array> convhull (ones (2,2), ones (2,2), struct ())
%!error <OPTIONS must be a string or cell array> convhull (ones (2,2), ones (2,2), ones (2,2), struct ())
%!error <X and Y must be the same size> convhull (1, [1 2])
%!error <X, Y, and Z must be the same size> convhull (1, [1 2], [1 2])
