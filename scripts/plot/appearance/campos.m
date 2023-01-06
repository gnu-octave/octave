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
## @deftypefn  {} {@var{p} =} campos ()
## @deftypefnx {} {} campos ([@var{x} @var{y} @var{z}])
## @deftypefnx {} {@var{mode} =} campos ("mode")
## @deftypefnx {} {} campos (@var{mode})
## @deftypefnx {} {} campos (@var{hax}, @dots{})
## Get or set the camera position.
##
## The default camera position is determined automatically based on the scene.
## For example, to get the camera position:
##
## @example
## @group
## hf = figure();
## peaks()
## p = campos ()
##   @result{} p =
##       -27.394  -35.701   64.079
## @end group
## @end example
##
## We can then move the camera further up the z-axis:
##
## @example
## @group
## campos (p + [0 0 10])
## campos ()
##   @result{} ans =
##       -27.394  -35.701   74.079
## @end group
## @end example
##
## Having made that change, the camera position @var{mode} is now manual:
##
## @example
## @group
## campos ("mode")
##   @result{} manual
## @end group
## @end example
##
## We can set it back to automatic:
##
## @example
## @group
## campos ("auto")
## campos ()
##   @result{} ans =
##       -27.394  -35.701   64.079
## close (hf)
## @end group
## @end example
##
## By default, these commands affect the current axis; alternatively, an axis
## can be specified by the optional argument @var{hax}.
##
## @seealso{camup, camtarget, camva}
## @end deftypefn


function p = campos (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("campos", varargin{:});

  if (nargin > 1)
    print_usage ();
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  prop = "cameraposition";
  if (nargin == 0)
    p = get (hax, prop);
  elseif (nargin == 1 && isnumeric (varargin{1}) && numel (varargin{1}) == 3)
    set (hax, prop, varargin{1});
  elseif (nargin == 1 && ischar (varargin{1}))
    s = varargin{1};
    if (strcmp (s, "mode"))
      p = get (hax, [prop "mode"]);
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
%! title ("campos() demo #1");
%! ## where is camera located?
%! x1 = campos ()
%! ## move the camera upwards
%! campos (x1 + [0 0 2]);
%! x2 = campos ()

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   sphere (hax);
%!   x = campos ();
%!   campos ([1 2 3]);
%!   y = campos ();
%!   assert (y, [1 2 3]);
%!   campos (x);
%!   x2 = campos ();
%!   assert (x, x2);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   p_orig = campos ();
%!   m = campos ("mode");
%!   assert (strcmp (m, "auto"));
%!
%!   campos ([1 2 3]);
%!   m = campos ("mode");
%!   assert (strcmp (m, "manual"));
%!
%!   campos ("auto");
%!   p = campos ();
%!   assert (p, p_orig);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## test hax input by creating another axis
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!  subplot (1, 2, 1); sphere (); hax1 = gca ();
%!  subplot (1, 2, 2); peaks (); hax2 = gca ();
%!  campos (hax1, [20 0 0]);
%!  subplot (1, 2, 1);
%!  x = campos ();
%!  z = campos (hax2);
%!  subplot (1, 2, 2);
%!  y = campos ();
%!  assert (x, [20 0 0]);
%!  assert (norm (y - [20 0 0]) > 1);
%!  assert (y, z);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> campos (1, 2)
%!error <invalid value>
%! hf = figure ("visible", "off");
%! unwind_protect
%!  campos ("mod");
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
