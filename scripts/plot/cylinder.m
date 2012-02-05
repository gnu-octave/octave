## Copyright (C) 2007-2012 Michael Goffioul and Kai Habel
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
## @deftypefn  {Function File} {} cylinder
## @deftypefnx {Function File} {} cylinder (@var{r})
## @deftypefnx {Function File} {} cylinder (@var{r}, @var{n})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} cylinder (@dots{})
## @deftypefnx {Function File} {} cylinder (@var{ax}, @dots{})
## Generate three matrices in @code{meshgrid} format, such that
## @code{surf (@var{x}, @var{y}, @var{z})} generates a unit cylinder.
## The matrices are of size @code{@var{n}+1}-by-@code{@var{n}+1}.
## @var{r} is a vector containing the radius along the z-axis.
## If @var{n} or @var{r} are omitted then default values of 20 or [1 1]
## are assumed.
##
## Called with no return arguments, @code{cylinder} calls directly
## @code{surf (@var{x}, @var{y}, @var{z})}.  If an axes handle @var{ax}
## is passed as the first argument, the surface is plotted to this set
## of axes.
##
## Examples:
##
## @example
## @group
## [x, y, z] = cylinder (10:-1:0, 50);
## surf (x, y, z);
## title ("a cone");
## @end group
## @end example
## @seealso{sphere}
## @end deftypefn

function [xx, yy, zz] = cylinder (varargin)

  [ax, args, nargs] = __plt_get_axis_arg__ ((nargout > 0), "cylinder",
                                            varargin{:});

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
    error ("cylinder: length(R) must be larger than 2");
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
    surf (ax, x, y, z);
  endif

endfunction

%!demo
%! clf
%! [x, y, z] = cylinder (10:-1:0,50);
%! surf (x, y, z);
%! title ("a cone")
