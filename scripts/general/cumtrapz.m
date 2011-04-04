## Copyright (C) 2000-2011 Kai Habel
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
## @deftypefn  {Function File} {@var{z} =} cumtrapz (@var{y})
## @deftypefnx {Function File} {@var{z} =} cumtrapz (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{z} =} cumtrapz (@dots{}, @var{dim})
##
## Cumulative numerical integration using trapezoidal method.
## @code{cumtrapz (@var{y})} computes the cumulative integral of the
## @var{y} along the first non-singleton dimension.  If the argument
## @var{x} is omitted an equally spaced vector is assumed.  @code{cumtrapz
## (@var{x}, @var{y})} evaluates the cumulative integral with respect
## to @var{x}.
##
## @seealso{trapz, cumsum}
## @end deftypefn

## Author:      Kai Habel <kai.habel@gmx.de>
##
## also: June 2000 Paul Kienzle (fixes,suggestions)
## 2006-05-12 David Bateman - Modified for NDArrays

function z = cumtrapz (x, y, dim)

  if (nargin < 1) || (nargin > 3)
    print_usage ();
  endif

  nd = ndims (x);
  sz = size (x);

  have_x = false;
  have_dim = false;
  if (nargin == 3)
    have_x = true;
    have_dim = true;
  elseif (nargin == 2)
    if (! size_equal (x, y) && isscalar (y))
      dim = y;
      have_dim = true;
    else
      have_x = true;
    endif
  endif

  if (! have_dim)
    ## Find the first non-singleton dimension.
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  else
    dim = floor (dim);
    if (! (isscalar (dim) && 1 <= dim && dim <= nd))
      error ("cumtrapz: invalid dimension DIM");
    endif
  endif

  n = sz(dim);
  idx1 = cell ();
  for i = 1:nd
    idx1{i} = 1:sz(i);
  endfor
  idx2 = idx1;
  idx1{dim} = 2 : n;
  idx2{dim} = 1 : (n - 1);

  if (! have_x)
    z = 0.5 * cumsum (x(idx1{:}) + x(idx2{:}), dim);
  else
    if (! size_equal (x, y))
      error ("cumtrapz: X and Y must have the same shape");
    endif
    z = 0.5 * cumsum ((x(idx1{:}) - x(idx2{:})) .*
                      (y(idx1{:}) + y(idx2{:})), dim);
  endif

  sz(dim) = 1;
  z = cat (dim, zeros (sz), z);

endfunction

%!shared x1,x2,y
%! x1 = [0,0,0;2,2,2];
%! x2 = [0,2,4;0,2,4];
%! y = [1,2,3;4,5,6];
%!assert (cumtrapz(y),[0,0,0;2.5,3.5,4.5])
%!assert (cumtrapz(x1,y),[0,0,0;5,7,9])
%!assert (cumtrapz(y,1),[0,0,0;2.5,3.5,4.5])
%!assert (cumtrapz(x1,y,1),[0,0,0;5,7,9])
%!assert (cumtrapz(y,2),[0,1.5,4;0,4.5,10])
%!assert (cumtrapz(x2,y,2),[0,3,8;0,9,20])
