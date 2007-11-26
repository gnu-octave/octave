## Copyright (C) 2007 Michael Goffioul
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{z}] =} sphere (@var{n})
## @deftypefnx {Function File} {} sphere (@var{h}, @dots{})
## Generates three matrices in @code{meshgrid} format, such that 
## @code{surf (@var{x}, @var{y}, @var{z})} generates a unit sphere. 
## The matrices of @code{@var{n}+1}-by-@code{@var{n}+1}. If @var{n} is 
## omitted then a default value of 20 is assumed.
##
## Called with no return arguments, @code{sphere} call directly 
## @code{surf (@var{x}, @var{y}, @var{z})}. If an axes handle is passed
## as the first argument, the the surface is plotted to this set of axes.
## @seealso{peaks}
## @end deftypefn

function [xx, yy, zz] = sphere (h, n)

  have_h = false;
  if (nargin > 1 && isscalar (h) && ishandle (h))
    if (! strcmp (get (h, "type"), "axes"))
      error ("sphere: expecting first argument to be an axes object");
    endif
    if (nargin == 1)
      n = 20;
    endif
    have_h = true;
  elseif (nargin == 1)
    n = h;
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
    if (have_h)
      oldh = gca ();
      unwind_protect
	axes (h);
	surf (x, y, z);
      unwind_protect_cleanup
	axes (oldh);
      end_unwind_protect
    else
      surf (x, y, z);
    endif
  endif

endfunction
