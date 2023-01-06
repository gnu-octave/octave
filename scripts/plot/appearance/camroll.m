########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {} camroll (@var{theta})
## @deftypefnx {} {} camroll (@var{hax}, @var{theta})
## Roll the camera.
##
## Roll the camera clockwise by @var{theta} degrees.
## For example, the following command will roll the camera by
## 30 degrees clockwise (to the right); this will cause the scene
## to appear to roll by 30 degrees to the left:
##
## @example
## @group
## @c doctest: +SKIP
## peaks ()
## camroll (30)
## @end group
## @end example
##
## Roll the camera back:
##
## @example
## @group
## @c doctest: +SKIP
## camroll (-30)
## @end group
## @end example
##
## The following command restores the default camera roll:
##
## @example
## @group
## @c doctest: +SKIP
## camup ("auto")
## @end group
## @end example
##
## By default, these commands affect the current axis; alternatively, an axis
## can be specified by the optional argument @var{hax}.
##
## @seealso{camzoom, camorbit, camlookat, camup}
## @end deftypefn


function camroll (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("camroll", varargin{:});

  if (nargin != 1)
    print_usage ();
  endif

  a = varargin{1};

  if (! (isnumeric (a) && isscalar (a) ))
    print_usage ();
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  dar = get (hax, "dataaspectratio");

  view_ax = (camtarget (hax) - campos (hax)) ./ dar;
  view_ax /= norm (view_ax);
  ## orthogonalize the camup vector
  up = camup (hax) ./ dar;
  up = up - view_ax * dot (up, view_ax);
  up /= norm (up);

  ## rotate the modified camup vector around the view axis
  up = num2cell (up);
  [up{:}] = __rotate_around_axis__ (up{:}, a, view_ax, [0 0 0]);
  up = [up{:}] .* dar;
  camup (hax, up / norm (up))

endfunction


%!demo
%! clf;
%! peaks ();
%! camroll (30);
%! title ("camroll() demo #1");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere (10);
%!   campos ([10 0 0]);
%!   camroll (30);
%!   p = camup ();
%!   assert (p, [0 1/2 sqrt(3)/2], eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## test rolling, then rolling back
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   peaks ();
%!   p = camup (hax);
%!   assert (p, [0 0 1], eps);
%!   camroll (hax, 30);
%!   p = camup (hax);
%!   ## from Matlab R2017b
%!   q = [0.33969638129660373 0.02014238382998192 0.94031944194919104];
%!   assert (p, q, 10*eps);
%!   camroll (hax, -30);
%!   ## Note: It does not go back to [0 0 1]: instead orthog to camera view:
%!   p = camup (hax);
%!   ## The "cameraupvector" is now perpendicular to the viewing vector
%!   dar = get (hax, "dataaspectratio");
%!   ## FIXME: looser tolerance needed on i386 for assert below
%!   assert (dot (p./dar, (camtarget (hax) - campos (hax))./dar), 0, 32*eps);
%!   ## from Matlab R2017b
%!   q = [0.14033891839365262 0.18289323924769943 0.97306477226420207];
%!   assert (p, q, 10*eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## test hax input by creating another axis
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax1 = subplot (1, 2, 1);
%!   sphere (hax1);
%!   hax2 = subplot (1, 2, 2);
%!   sphere (hax2);
%!   camroll (hax1, 30);
%!   x = camup (hax1);
%!   y = camup (hax2);
%!   ## from Matlab R2016a
%!   assert (x, [0.66027810132845211 0.03915135893036471 0.75000000000000022],
%!           -1e-5);
%!   assert (y, [0 0 1]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> camroll (1, 2, 3)
%!error <Invalid call> camroll ("mod")
%!error <Invalid call> camroll (1, [1 2])
