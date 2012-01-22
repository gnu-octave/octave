## Copyright (C) 2007-2012 Michael Goffioul
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
## @deftypefn  {Function File} {[@var{x}, @var{y}, @var{z}] =} sphere (@var{n})
## @deftypefnx {Function File} {} sphere (@var{h}, @dots{})
## Generate three matrices in @code{meshgrid} format, such that
## @code{surf (@var{x}, @var{y}, @var{z})} generates a unit sphere.
## The matrices of @code{@var{n}+1}-by-@code{@var{n}+1}.  If @var{n} is
## omitted then a default value of 20 is assumed.
##
## Called with no return arguments, @code{sphere} call directly
## @code{surf (@var{x}, @var{y}, @var{z})}.  If an axes handle is passed
## as the first argument, the surface is plotted to this set of axes.
## @seealso{peaks}
## @end deftypefn

function [xx, yy, zz] = sphere (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ((nargout > 0), "sphere",
                                                varargin{:});
  if (nargin > 1)
    print_usage ();
  elseif (nargin == 1)
    n = varargin{1};
  else
    n = 20;
  endif

  theta = linspace (0, 2*pi, n+1);
  phi = linspace (-pi/2, pi/2, n+1);
  [theta,phi] = meshgrid (theta, phi);

  x = cos (phi) .* cos (theta);
  y = cos (phi) .* sin (theta);
  z = sin (phi);

  if (nargout > 0)
    xx = x;
    yy = y;
    zz = z;
  else
    surf (h, x, y, z);
  endif

endfunction
