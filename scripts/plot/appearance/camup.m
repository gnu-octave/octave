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
## @deftypefn  {} {@var{up} =} camup ()
## @deftypefnx {} {} camup ([@var{x} @var{y} @var{z}])
## @deftypefnx {} {@var{mode} =} camup ("mode")
## @deftypefnx {} {} camup (@var{mode})
## @deftypefnx {} {} camup (@var{hax}, @dots{})
## Get or set the camera up vector.
##
## By default, the camera is oriented so that ``up'' corresponds to the
## positive z-axis:
##
## @example
## @group
## hf = figure ();
## sphere (36)
## v = camup ()
##   @result{} v =
##       0   0   1
## @end group
## @end example
##
## Specifying a new ``up vector'' rolls the camera and sets the mode to manual:
##
## @example
## @group
## camup ([1 1 0])
## camup ()
##   @result{}   1   1   0
## camup ("mode")
##   @result{} manual
## @end group
## @end example
##
## Modifying the up vector does not modify the camera target
## (@pxref{XREFcamtarget,,@code{camtarget}}).  Thus, the camera up vector might
## not be orthogonal to the direction of the camera's view:
##
## @example
## @group
## camup ([1 2 3])
## dot (camup (), camtarget () - campos ())
##   @result{} 6...
## @end group
## @end example
##
## A consequence is that ``pulling back'' on the up vector does not pitch the
## camera view (as that would require changing the target).  Setting the up
## vector is thus typically used only to roll the camera.  A more intuitive
## command for this purpose is @ref{XREFcamroll,,@code{camroll}}.
##
## Finally, we can reset the up vector to automatic mode:
##
## @example
## @group
## camup ("auto")
## camup ()
##   @result{}   0   0   1
## close (hf)
## @end group
## @end example
##
## By default, these commands affect the current axis; alternatively, an axis
## can be specified by the optional argument @var{hax}.
##
## @seealso{campos, camtarget, camva}
## @end deftypefn


function up = camup (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("camup", varargin{:});

  if (nargin > 1)
    print_usage ();
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  prop = "cameraupvector";
  if (nargin == 0)
    up = get (hax, prop);
  elseif (nargin == 1 && isnumeric (varargin{1}) && numel (varargin{1}) == 3)
    set (hax, prop, varargin{1});
  elseif (nargin == 1 && ischar (varargin{1}))
    s = varargin{1};
    if (strcmp (s, "mode"))
      up = get (hax, [prop "mode"]);
    else
      set (hax, [prop "mode"], s);
    endif
  else
    print_usage ();
  endif

endfunction


%!demo
%! clf;
%! sphere ();
%! title ("camup() demo #1");
%! ## what direction is "up" for the camera?
%! x1 = camup ()
%! ## re-orient the camera with a new up-vector
%! camup ([1 0 0]);
%! x2 = camup ()

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   x = camup ();
%!   camup ([1 2 3]);
%!   y = camup ();
%!   assert (y, [1 2 3]);
%!   camup (x);
%!   x2 = camup ();
%!   assert (x, x2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   p_orig = camup ();
%!   m = camup ("mode");
%!   assert (strcmp (m, "auto"));
%!
%!   camup ([1 2 3]);
%!   m = camup ("mode");
%!   assert (strcmp (m, "manual"));
%!
%!   camup ("auto");
%!   p = camup ();
%!   assert (p, p_orig);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## test hax input by creating another axis
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   subplot (1, 2, 1); sphere (); hax1 = gca ();
%!   subplot (1, 2, 2); peaks (); hax2 = gca ();
%!   camup (hax1, [1 0 0]);
%!   subplot (1, 2, 1);
%!   x = camup ();
%!   z = camup (hax2);
%!   subplot (1, 2, 2);
%!   y = camup ();
%!   assert (x, [1 0 0]);
%!   assert (norm (y - [1 0 0]) > 0.1);
%!   assert (y, z);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> camup (1, 2)
%!error <invalid value>
%! hf = figure ("visible", "off");
%! unwind_protect
%!  camup ("mod")
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
