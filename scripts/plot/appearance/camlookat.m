## Copyright (C) 2016 Colin B. Macdonald
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## This software is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty
## of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with this software; see the file COPYING.
## If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} camlookat ()
## @deftypefnx {} {} camlookat (@var{h})
## @deftypefnx {} {} camlookat (@var{handle_list})
## @deftypefnx {} {} camlookat (@var{hax})
## Move the camera and adjust its properties to look at objects.
##
## When the input is a handle @var{h}, the camera is set to point toward the
## center of the bounding box of @var{h}.  The camera's position is adjusted so
## the bounding box approximately fills the field of view.
##
## This command fixes the camera's viewing direction
## (@code{camtarget() - campos()}), camera up vector (@pxref{XREFcamup,,camup})
## and viewing angle (@pxref{XREFcamva,,camva}).  The camera target
## (@pxref{XREFcamtarget,,camtarget}) and camera position
## (@pxref{XREFcampos,,campos}) are changed.
##
##
## If the argument is a list @var{handle_list}, then a single bounding box for
## all the objects is computed and the camera is then adjusted as above.
##
## If the argument is an axis object @var{hax}, then the children of the axis
## are used as @var{handle_list}.  When called with no inputs, it uses the
## current axis (@pxref{XREFgca,,gca}).
##
## @seealso{camorbit, camzoom, camroll}
## @end deftypefn


function camlookat (hh)

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    hh = get (gca (), "children");
  elseif (nargin == 1)
    if (isaxes (hh))
      hh = get (hh, "children");
    endif
  endif

  if (isempty (hh))
    return
  end

  x0 = x1 = y0 = y1 = z0 = z1 = [];
  for i = 1:numel (hh)
    h = hh(i);

    if (! ishandle (h))
      error ("camlookat: Inputs must be handles.")
    end

    x0_ = min (get (h, "xdata")(:));
    x1_ = max (get (h, "xdata")(:));
    y0_ = min (get (h, "ydata")(:));
    y1_ = max (get (h, "ydata")(:));
    z0_ = min (get (h, "zdata")(:));
    z1_ = max (get (h, "zdata")(:));

    if (i == 1)
      x0 = x0_;  x1 = x1_;
      y0 = y0_;  y1 = y1_;
      z0 = z0_;  z1 = z1_;
    else
      x0 = min (x0, x0_);  x1 = max (x1, x1_);
      y0 = min (y0, y0_);  y1 = max (y1, y1_);
      z0 = min (z0, z0_);  z1 = max (z1, z1_);
    endif
  endfor

  ## current view direction and projection operator
  curdir = camtarget () - campos ();
  curdir /= norm (curdir);
  P = eye (3) - (curdir.' * curdir);

  ## target to middle of bounding box
  mid = [x0+x1; y0+y1; z0+z1]/2;

  ## vertices of the bounding box
  bb = [x0 x0 x0 x0 x1 x1 x1 x1;
        y0 y0 y1 y1 y0 y0 y1 y1;
        z0 z1 z0 z1 z0 z1 z0 z1];

  ## project bounding box onto view plane
  Pbb = P*(bb - mid) + mid;

  ## estimate size based on projected bb, choose distance for campos
  ## (XXX: only matches Matlab to about 1 digit, see xtests)
  sz = max (norm (Pbb - mid, 2, "cols"));
  dist = 2*sz / tand (camva ());

  ## avoid auto-adjusting
  camva ("manual")

  camtarget (mid.')

  campos (mid.' - dist*curdir)

endfunction


%!demo
%! [x, y, z] = peaks ();
%! surf(x, y, z/5);
%! hold on
%! [x, y, z] = sphere ();
%! s1 = surf(x/2, y/2+1.5, z/2+2);
%! s2 = surf(x/5+0.2, y/5-2, z/5+1);
%! axis equal
%! axis tight
%! pause
%! camlookat (s1)
%! pause
%! camlookat (s2)
%! pause
%! camlookat ([s1 s2])


%!test
%! ## not an error (does nothing)
%! camlookat ([])

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   camlookat ();
%!   assert (camva ("mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## direction is preserved
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   [x y z] = sphere ();
%!   h1 = surf (x + 1, y, z);
%!   hold on
%!   h2 = surf (x - 1, y + 2, z + 1);
%!   dir = camtarget () - campos ();
%!   dir /= norm (dir);
%!   camlookat (h1);
%!   dir2 = camtarget () - campos ();
%!   dir2 /= norm (dir2);
%!   assert (dir, dir2, -2*eps);
%!   camlookat (h2);
%!   dir2 = camtarget () - campos ();
%!   dir2 /= norm (dir2);
%!   assert (dir, dir2, -2*eps)
%!   camlookat ([h1 h2]);
%!   dir2 = camtarget () - campos ();
%!   dir2 /= norm (dir2);
%!   assert (dir, dir2, -2*eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## look at axes not same as default auto view
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   zlim ([0 10]);
%!   xlim ([0 5]);
%!   A = camtarget ();
%!   assert (A, [2.5 0 5]);
%!   camlookat ();
%!   B = camtarget ();
%!   assert (B, [0 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## list, empty and hax input give same results
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   [x y z] = sphere ();
%!   h1 = surf (x + 1, y, z);
%!   hold on
%!   h2 = surf (x - 1, y + 2, z + 1);
%!   camlookat ();
%!   T1 = camtarget ();
%!   P1 = campos ();
%!   camtarget ("auto");
%!   campos ("auto");
%!   camlookat ([h1 h2]);
%!   T2 = camtarget ();
%!   P2 = campos ();
%!   assert (T1, T2, -10*eps);
%!   assert (P1, P2, -10*eps);
%!   camtarget ("auto");
%!   campos ("auto");
%!   camlookat (gca ());
%!   T3 = camtarget ();
%!   P3 = campos ();
%!   assert (T1, T3, -10*eps);
%!   assert (P1, P3, -10*eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## compare to matlab2014a output
%!xtest
%! hf = figure ("visible", "off");
%! unwind_protect
%!   [x, y, z] = peaks ();
%!   s3 = surf(x, y, z/5);
%!   hold on
%!   [x, y, z] = sphere ();
%!   s2 = surf(x/2, y/2+1.5, z/2+2);
%!   s1 = mesh(x/2-4, 3*y, z/2 - 1);
%!   axis equal
%!   axis tight
%!   camlookat (s1)
%!   assert (camtarget (), [-4 0 -1], -eps)
%!   assert (campos (), [-22.806319527016 -24.5088727773662 16.8359421671461], -1e-7)
%!   camlookat (s2)
%!   assert (camtarget (), [0 1.5 2], -eps)
%!   assert (campos (), [-5.82093528266174 -6.08599055403138 7.52058391388657], -1e-7)
%!   camlookat (s3)
%!   assert (camtarget (), [0 0 0.1528529020838], 1e-10)
%!   assert (campos (), [-30.3728392082653 -39.5826547014375 28.9585000034444], -1e-7)
%!   camlookat ()
%!   assert (camtarget (), [-0.75 0 0.5], -eps)
%!   assert (campos (), [-35.7955620339723 -45.6722656481532 33.7372645671114], -1e-7)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> camlookat (1, 2)
%!error <must be handle> camlookat ("a")
