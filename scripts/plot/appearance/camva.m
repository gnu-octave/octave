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
## @deftypefn  {} {@var{a} =} camva ()
## @deftypefnx {} {} camva (@var{a})
## @deftypefnx {} {@var{mode} =} camva ("mode")
## @deftypefnx {} {} camva (@var{mode})
## @deftypefnx {} {} camva (@var{hax}, @dots{})
## Get or set the camera viewing angle.
##
## The camera has a viewing angle which determines how much can be seen.  By
## default this is:
##
## @example
## @group
## hf = figure();
## sphere (36)
## a = camva ()
##   @result{} a =  10.340
## @end group
## @end example
##
## To get a wider-angle view, we could double the viewing angle.  This will
## also set the mode to manual:
##
## @example
## @group
## camva (2*a)
## camva ("mode")
##   @result{} manual
## @end group
## @end example
##
## We can set it back to automatic:
##
## @example
## @group
## camva ("auto")
## camva ("mode")
##   @result{} auto
## camva ()
##   @result{} ans =  10.340
## close (hf)
## @end group
## @end example
##
## By default, these commands affect the current axis; alternatively, an axis
## can be specified by the optional argument @var{hax}.
##
## @seealso{campos, camtarget, camup}
## @end deftypefn


function a = camva (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("camva", varargin{:});

  if (nargin > 1)
    print_usage ();
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  prop = "cameraviewangle";
  if (nargin == 0)
    a = get (hax, prop);
  elseif (nargin == 1 && isnumeric (varargin{1}) && isscalar (varargin{1}))
    set (hax, prop, varargin{1});
  elseif (nargin == 1 && ischar (varargin{1}))
    s = varargin{1};
    if (strcmp (s, "mode"))
      a = get (hax, [prop "mode"]);
    else
      set (hax, [prop "mode"], s);
    endif
  else
    print_usage ();
  endif

endfunction


%!demo
%! clf;
%! peaks ();
%! title ("camva() demo #1");
%! ## query the viewing angle
%! a1 = camva ()
%! ## get a close-up view by decreasing the viewing angle:
%! camva (0.5*a1);
%! a2 = camva ()

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   x = camva ();
%!   camva (5);
%!   y = camva ();
%!   assert (y, 5);
%!   camva (x);
%!   x2 = camva ();
%!   assert (x, x2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   a_orig = camva ();
%!   m = camva ("mode");
%!   assert (strcmp (m, "auto"));
%!
%!   camva (15);
%!   m = camva ("mode");
%!   assert (strcmp (m, "manual"));
%!
%!   camva ("auto");
%!   a = camva ();
%!   assert (a, a_orig);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## test hax input by creating another axis
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   subplot (1, 2, 1); sphere (); hax1 = gca ();
%!   subplot (1, 2, 2); sphere (); hax2 = gca ();
%!   camva (hax1, 5);
%!   subplot (1, 2, 1);
%!   x = camva ();
%!   z = camva (hax2);
%!   subplot (1, 2, 2);
%!   y = camva ();
%!   assert (x, 5);
%!   assert (abs (z - 5) > 1);
%!   assert (y, z);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> camva (1, 2)
%!error <invalid value>
%! hf = figure ("visible", "off");
%! unwind_protect
%!  camva ("mod")
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
