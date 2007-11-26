## Copyright (C) 2007 Kai Habel, David Bateman
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
## @deftypefn {Function File}   {} slice (@var{X}, @var{Y}, @var{Z}, @var{V}, @var{SX}, @var{SY}, @var{SZ})
## @deftypefnx {Function File}  {} slice (@var{X}, @var{Y}, @var{Z}, @var{V}, @var{XI}, @var{YI}, @var{ZI})
## @deftypefnx {Function File}  {} slice (@var{V}, @var{SX}, @var{SY}, @var{SZ})
## @deftypefnx {Function File}  {} slice (@var{V}, @var{XI}, @var{YI}, @var{ZI})
## @deftypefnx {Function File}  {@var{H} =} slice (...)
## @deftypefnx {Function File}  {@var{H} =} slice (...,@var{METHOD})
## Plots slice(s) of 3D data/scalar fields. Each element of then 3-dimensional 
## array @var{v} represents a scalar value at a location given by the parameters 
## @var{x}, @var{y}, and @var{z}. The parameters @var{x}, @var{x}, and 
## @var{z} are either 3-dimensional arrays of the same size as the array 
## @var{v} in the 'meshgrid' format or vectors. The parameters @var{xi}, etc 
## respect a similar format to @var{x}, etc, and they represent the points 
## at which the array @var{vi} is interpolated using interp3. The vectors
## @var{sx}, @var{sy}, and @var{sz} contain points of orthogonal slices of
## the respective axes.
##
## If @var{x}, @var{y}, @var{z} are omitted, they are assumed to be 
## @code{x = 1 : size (@var{v}, 2)}, @code{y = 1 : size (@var{v}, 1)} and
## @code{z = 1 : size (@var{v}, 3)}. 
##
## @var{Method} is one of:
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
## The optional return value @var{H} is a vector of handles to the surface graphic 
## objects.
##
## Examples:
## @example
## [X,Y,Z] = meshgrid(linspace(-8,8,32));
## V = sin (sqrt (X.^2 + Y.^2 + Z.^2)) ./ (sqrt (X.^2 + Y.^2 + Z.^2))
## slice(X,Y,Z,V,[],0,[])
## [XI,YI]=meshgrid(linspace(-7,7));
## ZI=XI+YI;
## slice(X,Y,Z,V,XI,YI,ZI)
## @end example
## @seealso{interp3, surface, pcolor}
## @end deftypefn

## Author: Kai Habel <kai.habel at gmx.de>

function h = slice(varargin)

  method = "linear";
  extrapval = NA;
  nargs = nargin;

  if (ischar (varargin{end}))
    method = varargin{end};
    nargs -= 1;
  endif

  if (nargs == 4)
    V = varargin{1};
    if (ndims (V) != 3)
      error ("slice: expect 3-dimensional array of values");
    endif
    [nx, ny, nz] = size(V);
    [X,Y,Z] = meshgrid(1:nx,1:ny,1:nz);
    sx = varargin{2};
    sy = varargin{3};
    sz = varargin{4};
  elseif (nargs == 7)
    V = varargin{4};
    if (ndims (V) != 3)
      error ("slice: expect 3-dimensional array of values");
    endif
    X = varargin{1};
    Y = varargin{2};
    Z = varargin{3};
    if (all([isvector(X) isvector(Y) isvector(Z)]))
      [X,Y,Z] = meshgrid(X,Y,Z);
    elseif ((ndims(X) == 3) && size_equal(X,Y) && size_equal(X,Z))
      ##do nothing
    else
      error("slice: X,Y,Z size mismatch")
    endif
    sx = varargin{5};
    sy = varargin{6};
    sz = varargin{7};
  else
    print_usage();
  endif

  if (any([isvector(sx), isvector(sy), isvector(sz)]))
    have_sval = true();
  elseif ((ndims(sx) == 2) && size_equal(sx,sy) && size_equal(sx,sz))
    have_sval = false();
  else
    error ("slice: dimensional mismatch for (XI,YI,ZI) or (sx,sy,sz)");
  endif

  newplot ();
  ax = gca;
  sidx = 1;
  maxv = max(V(:));
  minv = min(V(:));
  set(ax, "CLim", [minv, maxv]);

  if (have_sval)
    ns = length(sx) + length(sy) + length(sz);
    hs = zeros(ns,1);
    [ny, nx, nz] = size(V);
    if (length(sz) > 0)
      for i=1:length(sz)
        [XI,YI,ZI] = meshgrid(squeeze(X(1,:,1)),squeeze(Y(:,1,1)),sz(i));
        Vz = squeeze(interp3(X,Y,Z,V,XI,YI,ZI,method));
        tmp(sidx++) = surface(XI,YI,sz(i)*ones(size(YI)),Vz);
      endfor
    endif

    if (length(sy) > 0)
      for i=length(sy):-1:1
	[XI,YI,ZI] = meshgrid(squeeze(X(1,:,1)),sy(i),squeeze(Z(1,1,:)));
        Vy = squeeze(interp3(X,Y,Z,V,XI,YI,ZI,method));
        tmp(sidx++) = surface(squeeze(XI),squeeze(sy(i)*ones(size(ZI))),squeeze(ZI),Vy);
      endfor
    endif

    if (length(sx) > 0)
      for i=length(sx):-1:1
        [XI,YI,ZI] = meshgrid(sx(i),squeeze(Y(:,1,1)),squeeze(Z(1,1,:)));
        Vx = squeeze(interp3(X,Y,Z,V,XI,YI,ZI,method));
        tmp(sidx++) = surface(squeeze(sx(i)*ones(size(ZI))),squeeze(YI),squeeze(ZI),Vx);
      endfor
    endif
  else
    VI = interp3(X,Y,Z,V,sx,sy,sz);
    tmp(sidx++) = surface(sx,sy,sz,VI);
  endif

  if (! ishold ())
    set (ax, "view", [-37.5, 30.0]);
  endif

  if (nargout > 0)
    h = tmp;
  endif

end
