## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} stairs (@var{x}, @var{y})
## @deftypefnx {Function File} {} stairs (@dots{}, @var{style})
## @deftypefnx {Function File} {} stairs (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} stairs (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} stairs (@dots{})
## Produce a stairstep plot.  The arguments may be vectors or matrices.
##
## If only one argument is given, it is taken as a vector of y-values
## and the x coordinates are taken to be the indices of the elements.
##
## If two output arguments are specified, the data are generated but
## not plotted.  For example,
##
## @example
## stairs (x, y);
## @end example
##
## @noindent
## and
##
## @example
## @group
## [xs, ys] = stairs (x, y);
## plot (xs, ys);
## @end group
## @end example
##
## @noindent
## are equivalent.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, xlabel, ylabel, title}
## @end deftypefn

## Author: jwe

function [xs, ys] = stairs (varargin)

  [ax, varargin, nargin] = __plt_get_axis_arg__ ("stairs", varargin{:});

  if (nargin < 1)
    print_usage ();
  else
    if (nargout > 1)
      [h, xs, ys] = __stairs__ (false, varargin{:});
    else
      oldax = gca ();
      unwind_protect
	axes (ax);
	newplot ();
	[h, xxs, yys] = __stairs__ (true, varargin{:});
      unwind_protect_cleanup
	axes (oldax);
      end_unwind_protect
    endif
    if (nargout == 1)
      xs = h;
    endif
  endif
endfunction

function [h, xs, ys] = __stairs__ (doplot, varargin)

  if (nargin == 1 || ischar (varargin{2}))
    idx = 1;
    y = varargin {1};
    if (ismatrix (y))
      if (isvector (y))
	y = y(:);
      endif
      x = 1:rows (y);
    endif
  else
    idx = 2;
    x = varargin{1};
    y = varargin{2};
  endif

  if (ndims (x) > 2 || ndims (y) > 2)
    error ("stairs: expecting 2-d arguments");
  endif

  vec_x = isvector (x);

  if (vec_x)
    x = x(:);
  endif

  if (isvector (y))
    y = y(:);
  endif

  if (ismatrix (y))
    [nr, nc] = size (y);
    if (vec_x)
      x = repmat (x, [1, nc]);
    else
      [x_nr, x_nc] = size (x);
      if (x_nr != nr || x_nc != nc)
	error ("stairs: argument size mismatch");
      endif
    endif
  endif

  len = 2*nr - 1;

  xs = ys = zeros (len, nc);

  xs(1,:) = x(1,:);
  ys(1,:) = y(1,:);

  x = x(2:nr,:);
  ridx = 2:2:len-1;
  xs(ridx,:) = x;
  ys(ridx,:) = y(1:nr-1,:);

  ridx = 3:2:len;
  xs(ridx,:) = x;
  ys(ridx,:) = y(2:nr,:);

  if (doplot)
    h = plot (xs, ys, varargin{idx+1:end});
  else
    h = 0;
  endif

endfunction

%!demo
%! x = 1:10;
%! y = rand (1, 10);
## stairs (x, y);

%!demo
%! x = 1:10;
%! y = rand (1, 10);
%! [xs, ys] = stairs (x, y);
%! plot (xs, ys);
