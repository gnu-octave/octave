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
## @deftypefn  {} {@var{t} =} camtarget ()
## @deftypefnx {} {} camtarget ([@var{x} @var{y} @var{z}])
## @deftypefnx {} {@var{mode} =} camtarget ("mode")
## @deftypefnx {} {} camtarget (@var{mode})
## @deftypefnx {} {} camtarget (@var{hax}, @dots{})
## Get or set where the camera is pointed.
##
## The camera target is a point in space where the camera is pointing.
## Usually, it is determined automatically based on the scene:
##
## @example
## @group
## hf = figure();
## sphere (36)
## v = camtarget ()
##   @result{} v =
##       0   0   0
## @end group
## @end example
##
## We can turn the camera to point at a new target:
##
## @example
## @group
## camtarget ([1 1 1])
## camtarget ()
##   @result{}   1   1   1
## @end group
## @end example
##
## Having done so, the camera target @var{mode} is manual:
##
## @example
## @group
## camtarget ("mode")
##   @result{} manual
## @end group
## @end example
##
## This means, for example, adding new objects to the scene will not retarget
## the camera:
##
## @example
## @group
## hold on;
## peaks ()
## camtarget ()
##   @result{}   1   1   1
## @end group
## @end example
##
## We can reset it to be automatic:
##
## @example
## @group
## @c doctest: +XFAIL
## @c https://savannah.gnu.org/bugs/?44503
## camtarget ("auto")
## camtarget ()
##   @result{}   0   0   0.76426
## close (hf)
## @end group
## @end example
##
## By default, these commands affect the current axis; alternatively, an axis
## can be specified by the optional argument @var{hax}.
##
## @seealso{campos, camup, camva}
## @end deftypefn


function t = camtarget (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("camtarget", varargin{:});

  if (nargin > 1)
    print_usage ();
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  prop = "cameratarget";
  if (nargin == 0)
    t = get (hax, prop);
  elseif (nargin == 1 && isnumeric (varargin{1}) && numel (varargin{1}) == 3)
    set (hax, prop, varargin{1});
  elseif (nargin == 1 && ischar (varargin{1}))
    s = varargin{1};
    if (strcmp (s, "mode"))
      t = get (hax, [prop "mode"]);
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
%! title ("camtarget() demo #1");
%! ## where is camera pointing?
%! x1 = camtarget ()
%! ## point the camera upwards
%! camtarget (x1 + [0 0 1]);
%! x2 = camtarget ()

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   x = camtarget ();
%!   camtarget ([1 2 3]);
%!   y = camtarget ();
%!   assert (y, [1 2 3]);
%!   camtarget (x);
%!   x2 = camtarget ();
%!   assert (x, x2);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   x_orig = camtarget ();
%!   m = camtarget ("mode");
%!   assert (strcmp (m, "auto"));
%!
%!   camtarget ([1 2 3]);
%!   m = camtarget ("mode");
%!   assert (strcmp (m, "manual"));
%!
%!   camtarget ("auto");
%!   x = camtarget ();
%!   assert (x, x_orig);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## test hax input by creating another axis
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   subplot (1, 2, 1); sphere (); hax1 = gca ();
%!   subplot (1, 2, 2); peaks (); hax2 = gca ();
%!   camtarget (hax1, [0.1 0.2 0.3]);
%!   subplot (1, 2, 1);
%!   x = camtarget ();
%!   z = camtarget (hax2);
%!   subplot (1, 2, 2);
%!   y = camtarget ();
%!   assert (x, [0.1 0.2 0.3]);
%!   assert (norm (y - [0.1 0.2 0.3]) > 0.1);
%!   assert (y, z);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid> camtarget (1, 2)
%!error <invalid value>
%! hf = figure ("visible", "off");
%! unwind_protect
%!  camtarget ("mod");
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
