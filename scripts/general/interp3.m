## Copyright (C) 2007 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{vi} =} interp3 (@var{x}, @var{y},@var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v}, @var{m})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@dots{}, @var{method}, @var{extrapval})
##
## Perform 3-dimensional interpolation. Each element of then 3-dimensional 
## array @var{v} represents a value at a location given by the parameters 
## @var{x}, @var{y}, and @var{z}. The parameters @var{x}, @var{x}, and 
## @var{z} are either 3-dimensional arrays of the same size as the array 
## @var{v} in the 'meshgrid' format or vectors. The parameters @var{xi}, etc 
## respect a similar format to @var{x}, etc, and they represent the points 
## at which the array @var{vi} is interpolated.
##
## If @var{x}, @var{y}, @var{z} are ommitted, they are assumed to be 
## @code{x = 1 : size (@var{v}, 2)}, @code{y = 1 : size (@var{v}, 1)} and
## @code{z = 1 : size (@var{v}, 3)}. If @var{m} is specified, then
## the interpolation adds a point half way between each of the interplation 
## points. This process is performed @var{m} times. If only @var{v} is 
## specified, then @var{m} is assumed to be @code{1}.
##
## Method is one of:
##
## @table @asis
## @item 'nearest'
## Return the nearest neighbour.
## @item 'linear'
## Linear interpolation from nearest neighbours.
## @item 'cubic'
## Cubic interpolation from four nearest neighbours (not implemented yet).
## @item 'spline'
## Cubic spline interpolation--smooth first and second derivatives
## throughout the curve.
## @end table
##
## The default method is 'linear'.
##
## If @var{extrap} is the string 'extrap', then extrapolate values beyond
## the endpoints.  If @var{extrap} is a number, replace values beyond the
## endpoints with that number.  If @var{extrap} is missing, assume NaN.
## @seealso{interp1, interp2, spline, meshgrid}
## @end deftypefn

function vi = interp3 (varargin)
  method = "linear";
  extrapval = NaN;
  nargs = nargin;

  if (nargin < 1)
    print_usage ();
  endif

  if (ischar (varargin {end}))
    method = varargin {end};
    nargs = nargs - 1;
  elseif (ischar (varargin {end - 1}))
    if (! isnumeric (vargin {end}) || ! isscalar (vargin {end}))
      error ("extrapal is expected to be a numeric scalar");
    endif
    method = varargin {end - 1};
    nargs = nargs - 2;
  endif

  if (nargs < 3 || (nargs == 4 && ! isvector (varargin {1}) && 
      nargs == (ndims (varargin {1}) + 1)))
    v = varargin {1};
    if (ndims (v) != 3)
      error ("expect 3-dimensional array of values");
    endif
    vi = ipermute (interpn (permute(varargin, [1, 3, 2, 4]){:}), [2, 1, 3]);
  elseif (nargs == 7 && nargs == (2 * ndims (varargin {ceil (nargs / 2)})) + 1)
    v = varargin {4};
    if (ndims (v) != 3)
      error ("expect 3-dimensional array of values");
    endif
    vi = ipermute (interpn (permute(varargin, [2, 1, 3, 4, 6, 5, 7]){:}), 
		   [2, 1, 3]);
  else
    error ("wrong number or incorrectly formatted input arguments");
  endif
endfunction

%!test
%! x = y = z = -1:1;
%! f = @(x,y,z) x.^2 - y - z.^2;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! v = f (xx,yy,zz);
%! xi = yi = zi = -1:0.5:1;
%! [xxi, yyi, zzi] = meshgrid (xi, yi, zi);
%! vi = interp3(x, y, z, v, xxi, yyi, zzi);
%! [xxi, yyi, zzi] = ndgrid (xi, yi, zi);
%! vi2 = interpn(x, y, z, v, xxi, yyi, zzi);
%! assert (vi, vi2);
