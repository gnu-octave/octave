## Copyright (C) 2016 Colin B. Macdonald
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

## -*- texinfo -*-
## @deftypefn  {} {} camroll (@var{theta})
## @deftypefnx {} {} camroll (@var{ax}, @var{theta})
## Roll the camera.
##
## Roll the camera clockwise by @var{theta} degrees.
## For example, the following command will roll the camera by
## 30 degrees clockwise (to the right); this will cause the scene
## to appear to roll by 30 degrees to the left:
## @example
## @group
## @c doctest: +SKIP
## peaks ()
## camroll (30)
## @end group
## @end example
##
## Roll the camera back:
## @example
## @group
## @c doctest: +SKIP
## camroll (-30)
## @end group
## @end example
##
## The following command restores the default camera roll:
## @example
## @group
## @c doctest: +SKIP
## camup ("auto")
## @end group
## @end example
##
## By default, these commands affect the current axis; alternatively, an axis
## can be specified by the optional argument @var{ax}.
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

  view_ax = camtarget (hax) - campos (hax);
  view_ax /= norm (view_ax);
  ## orthogonalize the camup vector
  up = camup (hax) - view_ax*dot (camup (hax), view_ax);
  up /= norm (up);

  ## rotate the modified camup vector around the view axis
  up = num2cell (up);
  [up{:}] = __rotate_around_axis__ (up{:}, a, view_ax, [0 0 0]);
  up = [up{:}];
  camup (hax, up)

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
%!   peaks ();
%!   p = camup ();
%!   assert (p, [0 0 1], eps);
%!   camroll (30);
%!   p = camup ();
%!   ## from Matlab R2014a
%!   q = [0.826398839602911  0.255644120004753  0.50170812412194];
%!   assert (p, q, 10*eps);
%!   camroll (-30);
%!   ## note it does not go back to [0 0 1]: instead orthog to camera view:
%!   p = camup ();
%!   assert (dot (p, camtarget () - campos ()), 0, eps);
%!   q = [0.496200420425837  0.646660977913424  0.57932264103285];
%!   assert (p, q, 10*eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## test ax input by creating another axis
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
%!   assert (x, [0.660278 0.039151 0.750000], -1e-5)
%!   assert (y, [0 0 1])
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> camroll (1, 2, 3)
%!error <Invalid call> camroll ("mod")
%!error <Invalid call> camroll (1, [1 2])
