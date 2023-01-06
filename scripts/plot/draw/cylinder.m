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
## @deftypefn  {} {} cylinder
## @deftypefnx {} {} cylinder (@var{r})
## @deftypefnx {} {} cylinder (@var{r}, @var{n})
## @deftypefnx {} {} cylinder (@var{hax}, @dots{})
## @deftypefnx {} {[@var{x}, @var{y}, @var{z}] =} cylinder (@dots{})
## Plot a 3-D unit cylinder.
##
## The optional input @var{r} is a vector specifying the radius along the
## unit z-axis.  The default is [1 1] indicating radius 1 at @code{Z == 0}
## and at @code{Z == 1}.
##
## The optional input @var{n} determines the number of faces around the
## circumference of the cylinder.  The default value is 20.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## If outputs are requested @code{cylinder} returns three matrices in
## @code{meshgrid} format, such that @code{surf (@var{x}, @var{y}, @var{z})}
## generates a unit cylinder.
##
## Example:
##
## @example
## @group
## [x, y, z] = cylinder (10:-1:0, 50);
## surf (x, y, z);
## title ("a cone");
## @end group
## @end example
## @seealso{ellipsoid, rectangle, sphere}
## @end deftypefn

function [x, y, z] = cylinder (varargin)

  [hax, args, nargs] = __plt_get_axis_arg__ ("cylinder", varargin{:});

  if (nargs > 2)
    print_usage ();
  elseif (nargs == 0)
    r = [1, 1];
    n = 20;
  elseif (nargs == 1)
    r = args{1};
    n = 20;
  else
    r = args{1};
    n = args{2};
  endif

  if (! isvector (r))
    error ("cylinder: R must be a scalar or vector");
  endif
  if (isscalar (r))
    r .*= [1 1];  # expand single radius specification to required 2-term form
  endif

  phi = linspace (0, 2*pi, n+1);
  idx = 1:length (r);
  [phi, idx] = meshgrid (phi, idx);
  zz = (idx - 1) / (length (r) - 1);
  r = r(idx);
  [xx, yy] = pol2cart (phi, r);

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
%! [x, y, z] = cylinder (10:-1:0, 50);
%! surf (x, y, z);
%! title ("cylinder() with linearly shrinking radius produces a cone");

## Test input validation
%!error <Invalid call> cylinder (1,2,3)
%!error <R must be a scalar> cylinder ([])
%!error <R must be a scalar or vector> cylinder (ones (2,2))
