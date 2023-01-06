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
## (@code{camtarget() - campos()}), camera up vector
## (@pxref{XREFcamup,,@code{camup}}) and viewing angle
## (@pxref{XREFcamva,,@code{camva}}).  The camera target
## (@pxref{XREFcamtarget,,@code{camtarget}}) and camera position
## (@pxref{XREFcampos,,@code{campos}}) are changed.
##
## If the argument is a list @var{handle_list}, then a single bounding box for
## all the objects is computed and the camera is then adjusted as above.
##
## If the argument is an axis object @var{hax}, then the children of the axis
## are used as @var{handle_list}.  When called with no inputs, it uses the
## current axis (@pxref{XREFgca,,@code{gca}}).
##
## @seealso{camorbit, camzoom, camroll}
## @end deftypefn


function camlookat (h)

  if (nargin == 0)
    hax = gca ();
    h = get (hax, "children");
  elseif (nargin == 1)
    if (isaxes (h))
      hax = h;
      h = get (hax, "children");
    elseif (all (ishghandle (h)))
      hax = ancestor (h, "axes");
      if (numel (hax) > 1)
        hax = unique ([hax{:}]);
      endif
      if (numel (hax) > 1)
        error ("camlookat: HANDLE_LIST must be children of the same axes");
      endif
    endif
  endif

  if (isempty (h))
    return;
  endif

  x0 = x1 = y0 = y1 = z0 = z1 = [];
  for i = 1:numel (h)
    hi = h(i);

    if (! ishghandle (hi))
      error ("camlookat: Inputs must be handles");
    endif

    x0_ = min (get (hi, "xdata")(:));
    x1_ = max (get (hi, "xdata")(:));
    y0_ = min (get (hi, "ydata")(:));
    y1_ = max (get (hi, "ydata")(:));
    z0_ = min (get (hi, "zdata")(:));
    z1_ = max (get (hi, "zdata")(:));

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

  dar = daspect (hax);

  ## current view direction
  curdir = (camtarget (hax) - campos (hax)) ./ dar;
  curdir /= norm (curdir);

  ## target to middle of bounding box
  mid = [x0+x1, y0+y1, z0+z1]/2 ./ dar;

  ## vertices of the bounding box
  bb = [x0 y0 z0;
        x0 y0 z1;
        x0 y1 z0;
        x0 y1 z1;
        x1 y0 z0;
        x1 y0 z1;
        x1 y1 z0;
        x1 y1 z1] ./ dar;

  ## Find corner of bounding box with maximum opening angle.
  ## Make sure temporary pov is well outside boundary of bounding box.
  bb_diag = norm ([x0-x1, y0-y1, z0-z1] ./ dar);
  cp_test = mid - 2*bb_diag*curdir;
  bb_cp = bb - cp_test;
  bb_cp ./= norm (bb_cp, 2, "rows");
  aperture = norm (curdir .* bb_cp, 2, "rows");
  max_corner = find (aperture == max (aperture), 1, "first");

  ## projection of corner on line of sight
  sz = curdir * (bb(max_corner,:) - mid)';
  bb_proj = mid + sz * curdir;

  ## Calculate distance for which that corner appears at camva/2
  dist = norm (bb(max_corner,:) - bb_proj) / tand (camva () / 2);

  ## Is bb_proj in front of or behind mid?
  if (curdir * (mid - bb_proj)' > 0)
    cp = bb_proj - dist * curdir;
  else
    cp = 2*mid - bb_proj - dist * curdir;
  endif

  ## set camera properties
  camva (hax, "manual");  # avoid auto-adjusting
  camtarget (hax, mid .* dar);
  campos (hax, cp .* dar);

endfunction


%!demo
%! clf;
%! [x, y, z] = peaks ();
%! surf (x, y, z/5);
%! hold on
%! [x, y, z] = sphere ();
%! s1 = surf (x/2, y/2+1.5, z/2+2);
%! s2 = surf (x/5+0.2, y/5-2, z/5+1);
%! axis equal
%! axis tight
%! title ("camlookat() demo #1");
%! pause (1);
%! camlookat (s1);
%! pause (1);
%! camlookat (s2);
%! pause (1);
%! camlookat ([s1 s2]);


%!test
%! ## not an error (does nothing)
%! camlookat ([]);

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
%!   assert (dir, dir2, -4*eps);
%!   camlookat (h2);
%!   dir2 = camtarget () - campos ();
%!   dir2 /= norm (dir2);
%!   assert (dir, dir2, -4*eps);
%!   camlookat ([h1 h2]);
%!   dir2 = camtarget () - campos ();
%!   dir2 /= norm (dir2);
%!   assert (dir, dir2, -4*eps);
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

## act on given axes
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   [x, y, z] = sphere ();
%!   hax1 = subplot (1, 2, 1);
%!   hs11 = surf (hax1, x, y, z);
%!   hold on
%!   hs12 = surf (hax1, x, y+2, z+3);
%!   hax2 = subplot (1, 2, 2);
%!   hs21 = surf (hax2, x, y, z);
%!   hold on
%!   hs22 = surf (hax2, x, y+2, z+3);
%!   ct2 = camtarget (hax2);
%!   camlookat (hs11);
%!   assert (camtarget (hax1), [0 0 0]);
%!   assert (camtarget (hax2), ct2);
%!   camlookat (hs22);
%!   assert (camtarget (hax1), [0 0 0]);
%!   assert (camtarget (hax2), [0 2 3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## compare to Matlab R2016a output
%!test <61532>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   [x, y, z] = peaks ();
%!   s3 = surf (x, y, z/5);
%!   hold on
%!   [x, y, z] = sphere ();
%!   s2 = surf (x/2, y/2+1.5, z/2+2);
%!   s1 = mesh (x/2-4, 3*y, z/2 - 1);
%!   axis equal
%!   axis tight
%!   camlookat (s1);
%!   assert (camtarget (), [-4 0 -1], -eps);
%!   assert (campos (), [-22.806319527015962 -24.508872777366225 16.835942167146133], -1e-7);
%!   camlookat (s2);
%!   assert (camtarget (), [0 1.5 2], -eps);
%!   assert (campos (), [-5.8209352826617424 -6.0859905540313779 7.5205839138865720], -1e-7);
%!   camlookat (s3);
%!   assert (camtarget (), [0 0 0.15285290208388014], 1e-10);
%!   assert (campos (), [-30.372839208265287 -39.582654701437512 28.958500003444449], -1e-7);
%!   camlookat ();
%!   assert (camtarget (), [-0.75 0 0.5], -eps);
%!   assert (campos (), [-35.795562033972338 -45.672265648153193 33.737264567111389], -1e-7);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <called with too many inputs> camlookat (1, 2)
%!error <must be handle> camlookat ("a")
%!error <children of the same axes>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   [x, y, z] = sphere ();
%!   hax1 = subplot (1, 2, 1);
%!   hs1 = surf (hax1, x, y, z);
%!   hax2 = subplot (1, 2, 2);
%!   hs2 = surf (hax2, x, y, z);
%!   camlookat ([hs1 hs2]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
