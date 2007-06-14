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
## @deftypefn {Function File} {@var{vi} =} interpn (@var{x1}, @var{x2}, @dots{}, @var{v}, @var{y1}, @var{y2}, @dots{})
## @deftypefnx {Function File} {@var{vi} =} interpn (@var{v}, @var{y1}, @var{y2}, @dots{})
## @deftypefnx {Function File} {@var{vi} =} interpn (@var{v}, @var{m})
## @deftypefnx {Function File} {@var{vi} =} interpn (@var{v})
## @deftypefnx {Function File} {@var{vi} =} interpn (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{vi} =} interpn (@dots{}, @var{method}, @var{extrapval})
##
## Perform @var{n}-dimensional interpolation, where @var{n} is at least two. 
## Each element of then @var{n}-dimensional array @var{v} represents a value 
## at a location given by the parameters @var{x1}, @var{x2}, @dots{}, @var{xn}. 
## The parameters @var{x1}, @var{x2}, @dots{}, @var{xn} are either 
## @var{n}-dimensional arrays of the same size as the array @var{v} in 
## the 'ndgrid' format or vectors. The parameters @var{y1}, etc respect a 
## similar format to @var{x1}, etc, and they represent the points at which
## the array @var{vi} is interpolated.
##
## If @var{x1}, @dots{}, @var{xn} are ommitted, they are assumed to be 
## @code{x1 = 1 : size (@var{v}, 1)}, etc. If @var{m} is specified, then
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
## @seealso{interp1, interp2, spline, ndgrid}
## @end deftypefn

function vi = interpn (varargin)

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

  if (nargs < 3)
    v = varargin {1};
    m = 1;
    if (nargs == 2)
      m = varargin {2};
      if (! isnumeric (m) || ! isscalar (m) || floor (m) != m)
	error ("m is expected to be a integer scalar");
      endif
    endif
    sz = size (v);
    nd = ndims (v);
    x = cell (1, nd);
    y = cell (1, nd);
    for i = 1 : nd;
      x{i} = 1 : sz(i);
      y{i} = 1 : (1 / (2 ^ m)) : sz(i);
    endfor
  elseif (! isvector (varargin {1}) && nargs == (ndims (varargin {1}) + 1))
    v = varargin {1};
    sz = size (v);
    nd = ndims (v);
    x = cell (1, nd);
    y = varargin (2 : nargs);
    for i = 1 : nd;
      x{i} = 1 : sz(i);
    endfor
  elseif (rem (nargs, 2) == 1 && nargs ==  
	  (2 * ndims (varargin {ceil (nargs / 2)})) + 1)
    nv = ceil (nargs / 2);
    v = varargin {nv};
    sz = size (v);
    nd = ndims (v);
    x = varargin (1 : (nv - 1));
    y = varargin ((nv + 1) : nargs);
  else
    error ("wrong number or incorrectly formatted input arguments");
  endif

  if (any (! cellfun (@isvector, x)))
    for i = 2 : nd
      if (! size_equal (x{1}, x{i}) || ! size_equal (x{i}, v))
	error ("dimensional mismatch");
      endif
      idx (1 : nd) = {1};
      idx (i) = ":";
      x{i} = x{i}(idx{:})(:);
    endfor
    idx (1 : nd) = {1};
    idx (1) = ":";
    x{1} = x{1}(idx{:})(:);
  endif

  if (strcmp (method, "linear") || strcmp (method, "nearest"))
    if (all (cellfun (@isvector, y)))
      [y{:}] = ndgrid (y{:});
    endif
  elseif (any (! cellfun (@isvector, y)))
    for i = 1 : nd
      idx (1 : nd) = {1};
      idx (i) = ":";
      y{i} = y{i}(idx{:})(:).';
    endfor
  endif

  method = tolower (method);
  if (strcmp (method, "linear"))
    vi = __lin_interpn__ (x{:}, v, y{:});
    vi (vi == NaN) = extrapval;
  elseif (strcmp (method, "nearest"))
    yshape = size (y{1});
    yidx = cell (1, nd);
    for i = 1 : nd
      y{i} = y{i}(:);
      yidx{i} = lookup (x{i}(2:end-1), y{i}) + 1;
    endfor
    idx = cell (1,nd);
    for i = 1 : nd
      idx {i} = yidx{i} + (y{i} - x{i}(yidx{i}).' > ...
			   x{i}(yidx{i} + 1).' - y{i});
    endfor
    vi = v (sub2ind (sz, idx{:}));
    idx = zeros (prod(yshape),1);
    for i = 1 : nd
      idx |= y{i} < min (x{i}(:)) | y{i} > max (x{i}(:));
    endfor
    vi(idx) = extrapval;
    vi = reshape (vi, yshape); 
  elseif (strcmp (method, "spline"))
    vi = __splinen__ (x, v, y, extrapval, "interpn");
  elseif (strcmp (method, "cubic")) 
    error ("cubic interpolation not yet implemented");
  else
    error ("unrecognized interpolation method");
  endif

endfunction

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,4]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interpn(x,y,A.',xi,yi,"linear").');
%! [x,y] = meshgrid(x,y); 
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,4]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interpn(x,y,A.',xi,yi,"nearest").');
%! [x,y] = meshgrid(x,y); 
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!#demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,2]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interpn(x,y,A.',xi,yi,"cubic").');
%! [x,y] = meshgrid(x,y); 
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,2]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interpn(x,y,A.',xi,yi,"spline").');
%! [x,y] = meshgrid(x,y); 
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;


%!demo
%! x = y = z = -1:1;
%! f = @(x,y,z) x.^2 - y - z.^2;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! v = f (xx,yy,zz);
%! xi = yi = zi = -1:0.1:1;
%! [xxi, yyi, zzi] = ndgrid (xi, yi, zi);
%! vi = interpn(x, y, z, v, xxi, yyi, zzi, 'spline');
%! mesh (yi, zi, squeeze (vi(1,:,:)));

