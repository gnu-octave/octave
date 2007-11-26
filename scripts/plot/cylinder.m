## Copyright (C) 2007 Michael Goffioul, Kai Habel
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
## @deftypefn {Function File} {} cylinder
## @deftypefnx {Function File} {} cylinder (@var{r})
## @deftypefnx {Function File} {} cylinder (@var{r}, @var{n})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} cylinder (@dots{})
## @deftypefnx {Function File} {} cylinder (@var{ax}, @dots{})
## Generates three matrices in @code{meshgrid} format, such that
## @code{surf (@var{x}, @var{y}, @var{z})} generates a unit cylinder.
## The matrices are of size @code{@var{n}+1}-by-@code{@var{n}+1}. 
## @var{r} is a vector containing the radius along the z-axis.
## If @var{n} or @var{r} are omitted then default values of 20 or [1 1]
## are assumed.
##
## Called with no return arguments, @code{cylinder} calls directly
## @code{surf (@var{x}, @var{y}, @var{z})}. If an axes handle @var{ax}
## is passed as the first argument, the surface is plotted to this set
## of axes.
##
## Examples:
## @example
## disp ("plotting a cone")
## [x, y, z] = cylinder (10:-1:0,50);
## surf (x, y, z);
## @end example
## @seealso{sphere}
## @end deftypefn

function [xx, yy, zz] = cylinder (varargin)

  nargs = nargin;
  args = varargin;

  if (nargs > 1 && isscalar (varargin{1}) && ishandle (varargin{1}))
    if (! strcmp (get (varargin{1}, "type"), "axes"))
      error ("cylinder: expecting first argument to be an axes object");
    endif
    ax = h;
    nargs--;
    args(1) = [];
  else
    ax = gca ();
  endif

  if (nargs == 0)
    n = 20;
    r = [1, 1];
  elseif (nargs == 1)
    n = 20;
    r = args{1};
  elseif (nargs == 2)
    r = args{1};
    n = args{2};
  else
    print_usage ();
  endif

  if (length (r) < 2)
    error ("cylinder: length(r) must be larger than 2")
  endif

  phi = linspace (0, 2*pi, n+1);
  idx = 1:length(r);
  [phi, idx] = meshgrid(phi, idx);
  z = (idx - 1) / (length(r) - 1);
  r = r(idx);
  [x, y] = pol2cart (phi, r);

  if (nargout > 0)
    xx = x;
    yy = y;
    zz = z;
  else
    axes(ax);
    surf (x, y, z);
  endif

endfunction
