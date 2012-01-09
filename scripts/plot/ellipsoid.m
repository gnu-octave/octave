## Copyright (C) 2007-2012 Sylvain Pelissier
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
## @deftypefn  {Function File} {[@var{x}, @var{y}, @var{z}] =} ellipsoid (@var{xc}, @var{yc}, @var{zc}, @var{xr}, @var{yr}, @var{zr}, @var{n})
## @deftypefnx {Function File} {} ellipsoid (@var{h}, @dots{})
## Generate three matrices in @code{meshgrid} format that define an
## ellipsoid.  Called with no return arguments, @code{ellipsoid} calls
## directly @code{surf (@var{x}, @var{y}, @var{z})}.  If an axes handle
## is passed as the first argument, the surface is plotted to this
## set of axes.
## @seealso{sphere}
## @end deftypefn

## Author: Sylvain Pelissier <sylvain.pelissier@gmail.com>

function [xx, yy, zz] = ellipsoid (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ((nargout > 0), "ellipsoid",
                                                varargin{:});

  if (nargin != 6 && nargin != 7)
    print_usage ();
  endif

  xc = varargin{1};
  yc = varargin{2};
  zc = varargin{3};
  xr = varargin{4};
  yr = varargin{5};
  zr = varargin{6};

  if (nargin == 6)
    n = 20;
  else
    n = varargin{7};
  endif

  theta = linspace (0, 2 * pi, n + 1);
  phi = linspace (-pi / 2, pi / 2, n + 1);
  [theta, phi] = meshgrid (theta, phi);

  x = xr .* cos (phi) .* cos (theta) + xc;
  y = yr .* cos (phi) .* sin (theta) + yc;
  z = zr .* sin (phi) + zc;

  if (nargout > 0)
    xx = x;
    yy = y;
    zz = z;
  else
    surf (h, x, y, z);
  endif

endfunction

%!demo
%! clf
%! ellipsoid (0, 0, 1, 2, 3, 4, 20);
