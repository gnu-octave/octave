########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} sphere ()
## @deftypefnx {} {} sphere (@var{n})
## @deftypefnx {} {} sphere (@var{hax}, @dots{})
## @deftypefnx {} {[@var{x}, @var{y}, @var{z}] =} sphere (@dots{})
## Plot a 3-D unit sphere.
##
## The optional input @var{n} determines the number of faces around the
## circumference of the sphere.  The default value is 20.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## If outputs are requested @code{sphere} returns three matrices in
## @code{meshgrid} format such that @code{surf (@var{x}, @var{y}, @var{z})}
## generates a unit sphere.
##
## Example:
##
## @example
## @group
## [x, y, z] = sphere (40);
## surf (3*x, 3*y, 3*z);
## axis equal;
## title ("sphere of radius 3");
## @end group
## @end example
## @seealso{cylinder, ellipsoid, rectangle}
## @end deftypefn

function [x, y, z] = sphere (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("sphere", varargin{:});

  if (nargin > 1)
    print_usage ();
  elseif (nargin == 1)
    n = varargin{1};
    if (! (isreal (n) && isscalar (n) && n > 0))
      error ("sphere: N must be a real scalar > 0");
    endif
  else
    n = 20;
  endif

  theta = linspace (0, 2*pi, n+1);
  phi = linspace (-pi/2, pi/2, n+1);
  [theta, phi] = meshgrid (theta, phi);

  xx = cos (phi) .* cos (theta);
  yy = cos (phi) .* sin (theta);
  zz = sin (phi);

  if (nargout > 0)
    x = xx;
    y = yy;
    z = zz;
  else
    oldfig = [];
    if (! isempty (hax))
      oldfig = get (0, "currentfigure");
    endif
    unwind_protect
      hax = newplot (hax);

      surf (xx, yy, zz);
    unwind_protect_cleanup
      if (! isempty (oldfig))
        set (0, "currentfigure", oldfig);
      endif
    end_unwind_protect
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! [x, y, z] = sphere (40);
%! surf (3*x, 3*y, 3*z);
%! axis equal;
%! title ("sphere of radius 3");

## Test input validation
%!error <Invalid call> sphere (-1,1)
%!error <N must be a real scalar> sphere (2i)
%!error <N must be a real scalar> sphere ([])
%!error <N must be a real scalar> sphere (ones (2,2))
%!error <N must be a real scalar . 0> sphere (-1)
