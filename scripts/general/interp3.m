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
## @deftypefn  {Function File} {@var{vi} =} interp3 (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v}, @var{m})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@dots{}, @var{method}, @var{extrapval})
##
## Perform 3-dimensional interpolation.  Each element of the 3-dimensional
## array @var{v} represents a value at a location given by the parameters
## @var{x}, @var{y}, and @var{z}.  The parameters @var{x}, @var{x}, and
## @var{z} are either 3-dimensional arrays of the same size as the array
## @var{v} in the 'meshgrid' format or vectors.  The parameters @var{xi}, etc.
## respect a similar format to @var{x}, etc., and they represent the points
## at which the array @var{vi} is interpolated.
##
## If @var{x}, @var{y}, @var{z} are omitted, they are assumed to be
## @code{x = 1 : size (@var{v}, 2)}, @code{y = 1 : size (@var{v}, 1)} and
## @code{z = 1 : size (@var{v}, 3)}.  If @var{m} is specified, then
## the interpolation adds a point half way between each of the interpolation
## points.  This process is performed @var{m} times.  If only @var{v} is
## specified, then @var{m} is assumed to be @code{1}.
##
## Method is one of:
##
## @table @asis
## @item 'nearest'
## Return the nearest neighbor.
##
## @item 'linear'
## Linear interpolation from nearest neighbors.
##
## @item 'cubic'
## Cubic interpolation from four nearest neighbors (not implemented yet).
##
## @item 'spline'
## Cubic spline interpolation---smooth first and second derivatives
## throughout the curve.
## @end table
##
## The default method is 'linear'.
##
## If @var{extrap} is the string 'extrap', then extrapolate values beyond
## the endpoints.  If @var{extrap} is a number, replace values beyond the
## endpoints with that number.  If @var{extrap} is missing, assume NA.
## @seealso{interp1, interp2, spline, meshgrid}
## @end deftypefn

function vi = interp3 (varargin)
  method = "linear";
  extrapval = NA;
  nargs = nargin;

  if (nargin < 1 || ! isnumeric (varargin{1}))
    print_usage ();
  endif

  if (ischar (varargin{end}))
    method = varargin{end};
    nargs = nargs - 1;
  elseif (nargs > 1 && ischar (varargin{end - 1}))
    if (! isnumeric (varargin{end}) || ! isscalar (varargin{end}))
      error ("interp3: extrapal is expected to be a numeric scalar");
    endif
    extrapval = varargin{end};
    method = varargin{end-1};
    nargs = nargs - 2;
  endif

  if (nargs < 3 || (nargs == 4 && ! isvector (varargin{1})
                    && nargs == (ndims (varargin{1}) + 1)))
    v = varargin{1};
    if (ndims (v) != 3)
      error ("interp3: expect 3-dimensional array of values");
    endif
    x = varargin (2:end);
    if (any (! cellfun (@isvector, x)))
      for i = 2 : 3
        if (! size_equal (x{1}, x{i}))
          error ("interp3: dimensional mismatch");
        endif
        x{i} = permute (x{i}, [2, 1, 3]);
      endfor
      x{1} = permute (x{1}, [2, 1, 3]);
    endif
    v = permute (v, [2, 1, 3]);
    vi = ipermute (interpn (v, x{:}, method, extrapval), [2, 1, 3]);
  elseif (nargs == 7 && nargs == (2 * ndims (varargin{ceil (nargs / 2)})) + 1)
    v = varargin{4};
    if (ndims (v) != 3)
      error ("interp3: expect 3-dimensional array of values");
    endif
    x = varargin (1:3);
    if (any (! cellfun (@isvector, x)))
      for i = 2 : 3
        if (! size_equal (x{1}, x{i}) || ! size_equal (x{i}, v))
          error ("interp3: dimensional mismatch");
        endif
        x{i} = permute (x{i}, [2, 1, 3]);
      endfor
      x{1} = permute (x{1}, [2, 1, 3]);
    endif
    y = varargin (5:7);
    if (any (! cellfun (@isvector, y)))
      for i = 2 : 3
        if (! size_equal (y{1}, y{i}))
          error ("interp3: dimensional mismatch");
        endif
        y{i} = permute (y{i}, [2, 1, 3]);
      endfor
      y{1} = permute (y{1}, [2, 1, 3]);
    endif
    v = permute (v, [2, 1, 3]);
    vi = ipermute (interpn (x{:}, v, y{:}, method, extrapval), [2, 1, 3]);
  else
    error ("interp3: wrong number or incorrectly formatted input arguments");
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

%!shared z, zout, tol
%! z = zeros (3, 3, 3);
%! zout = zeros (5, 5, 5);
%! z(:,:,1) = [1 3 5; 3 5 7; 5 7 9];
%! z(:,:,2) = z(:,:,1) + 2;
%! z(:,:,3) = z(:,:,2) + 2;
%! for n = 1:5
%!   zout(:,:,n) = [1 2 3 4 5;
%!                  2 3 4 5 6; 
%!                  3 4 5 6 7;
%!                  4 5 6 7 8;
%!                  5 6 7 8 9] + (n-1);
%! end
%! tol = 10 * eps;
%!assert (interp3 (z), zout, tol)
%!assert (interp3 (z, "linear"), zout, tol)
%!assert (interp3 (z, "spline"), zout, tol)
