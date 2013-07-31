## Copyright (C) 2003-2012 Shai Ayal
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
## @deftypefn  {Function File} {[@var{c}, @var{lev}] =} contourc (@var{z})
## @deftypefnx {Function File} {[@var{c}, @var{lev}] =} contourc (@var{z}, @var{vn})
## @deftypefnx {Function File} {[@var{c}, @var{lev}] =} contourc (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{c}, @var{lev}] =} contourc (@var{x}, @var{y}, @var{z}, @var{vn})
## Compute contour lines (isolines of constant Z value).
##
## The matrix @var{z} contains height values above the rectangular grid
## determined by @var{x} and @var{y}.  If only a single input @var{z} is
## provided then @var{x} is taken to be @code{1:rows (@var{z})} and @var{y} is
## taken to be @code{1:columns (@var{z})}.
##
## The optional input @var{vn} is either a scalar denoting the number of
## contour lines to compute or a vector containing the Z values where lines
## will be computed.  When @var{vn} is a vector the number of contour lines
## is @code{numel (@var{vn})}.  However, to compute a single contour line
## at a given value use @code{@var{vn} = [val, val]}.  If @var{vn} is omitted
## it defaults to 10.
##
## The return value @var{c} is a 2x@var{n} matrix containing the
## contour lines in the following format
##
## @example
## @group
## @var{c} = [lev1, x1, x2, @dots{}, levn, x1, x2, ...
##      len1, y1, y2, @dots{}, lenn, y1, y2, @dots{}]
## @end group
## @end example
##
## @noindent
## in which contour line @var{n} has a level (height) of @var{levn} and
## length of @var{lenn}.
##
## The optional return value @var{lev} is a vector with the Z values of
## of the contour levels.
##
## Example:
##
## @example
## @group
## x = 0:2;
## y = x;
## z = x' * y;
## contourc (x, y, z, 2:3)
##    @result{}   2.0000   2.0000   1.0000   3.0000   1.5000   2.0000
##         2.0000   1.0000   2.0000   2.0000   2.0000   1.5000
## @end group
## @end example
## @seealso{contour, contourf, contour3, clabel}
## @end deftypefn

## Author: Shai Ayal <shaiay@users.sourceforge.net>

function [cout, lev] = contourc (varargin)

  if (nargin == 1)
    vn = 10;
    z = varargin{1};
    [nr, nc] = size (z);
    x = 1:nc;
    y = 1:nr;
  elseif (nargin == 2)
    vn = varargin{2};
    z = varargin{1};
    [nr, nc] = size (z);
    x = 1:nc;
    y = 1:nr;
  elseif (nargin == 3)
    vn = 10;
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
  elseif (nargin == 4)
    vn = varargin{4};
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
  else
    print_usage ();
  endif

  if (!ismatrix (z) || isvector (z) || isscalar (z))
    error ("contourc: Z argument must be a matrix");
  endif

  if (isscalar (vn))
    vv = linspace (min (z(:)), max (z(:)), vn+2)(2:end-1);
  else
    vv = unique (sort (vn));
  endif

  if (isvector (x) && isvector (y))
    c = __contourc__ (x(:)', y(:)', z, vv);
  else
    ## Indexes x,y for the purpose of __contourc__.
    ii = 1:columns (z);
    jj = 1:rows (z);

    ## Now call __contourc__ for the real work...
    c = __contourc__ (ii, jj, z, vv);

    ## Map the contour lines from index space (i,j) back
    ## to the original grid (x,y)
    i = 1;

    while (i < columns (c))
      clen = c(2, i);
      ind = i + [1 : clen];

      ci = c(1, ind);
      cj = c(2,ind);

      ## due to rounding errors some elements of ci and cj
      ## can fall out of the range of ii and jj and interp2 would
      ## return NA for those values.
      ## The permitted range is enforced here:

      ci = max (ci, 1); ci = min (ci, columns (z));
      cj = max (cj, 1); cj = min (cj, rows (z));

      c(1, ind) = interp2 (ii, jj, x, ci, cj);
      c(2, ind) = interp2 (ii, jj, y, ci, cj);

      i = i + clen + 1;
    endwhile
  endif

  if (nargout > 0)
    cout = c;
    lev = vv;
  endif

endfunction


%!test
%! x = 0:2;
%! y = x;
%! z = x' * y;
%! [c_actual, lev_actual]= contourc (x, y, z, 2:3);
%! c_expected = [2, 1, 1, 2, 2, 3, 1.5, 2; 4, 2, 2, 1, 1, 2, 2, 1.5];
%! lev_expected = [2 3];
%! assert (c_actual, c_expected, eps);
%! assert (lev_actual, lev_expected, eps);

