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
## @deftypefn  {} {} camzoom (@var{zf})
## @deftypefnx {} {} camzoom (@var{hax}, @var{zf})
## Zoom the camera in or out.
##
## A value of @var{zf} larger than 1 ``zooms in'' such that the scene appears
## magnified:
##
## @example
## @group
## hf = figure ();
## sphere (36)
## camzoom (1.2)
## @end group
## @end example
##
## A value smaller than 1 ``zooms out'' so the camera can see more of the
## scene:
##
## @example
## @group
## camzoom (0.5)
## @end group
## @end example
##
## Technically speaking, zooming affects the ``viewing angle''.  The following
## command resets to the default zoom:
##
## @example
## @group
## camva ("auto")
## close (hf)
## @end group
## @end example
##
## By default, these commands affect the current axis; alternatively, an axis
## can be specified by the optional argument @var{hax}.
##
## @seealso{camroll, camorbit, camlookat, camva}
## @end deftypefn


function camzoom (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("camva", varargin{:});

  if (nargin != 1)
    print_usage ();
  endif

  zf = varargin{1};

  if (! (isnumeric (zf) && isscalar (zf) ))
    print_usage ();
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  va = 2 * atand (tand (camva (hax)/2) / zf);
  camva (hax, va);

endfunction


%!demo
%! clf;
%! peaks ();
%! camzoom (2);
%! title ("camzoom() demo #1");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   x = camva ();
%!   camzoom (5);
%!   assert (tand (x/2) / tand (camva ()/2), 5);
%!   camzoom (1/5);
%!   y = camva ();
%!   assert (x, y, eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   x = camva ();
%!   camzoom (2);
%!   y = camva ();
%!   ## Matlab 2016a
%!   xm = 10.339584907201974;
%!   ym = 5.1803362845094822;
%!   assert (tand (x/2) / tand (y/2), tand (xm/2) / tand (ym/2), 2e-14);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   peaks ();
%!   campos ([20, 30, 50]);
%!   camtarget ([0 0 1]);
%!   x = camva ();
%!   camzoom (5);
%!   y = camva ();
%!   ## Matlab 2014a
%!   xm = 13.074668029506947;
%!   ym = 2.6258806698721222;
%!   assert (tand (x/2) / tand (y/2), tand (xm/2) / tand (ym/2), 2e-14);
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
%!   camzoom (hax1, 2)
%!   x = camva (hax1);
%!   y = camva (hax2);
%!   assert (tand (y/2) / tand (x/2), 2, 2*eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> camzoom (1, 2, 3)
%!error <Invalid call> camzoom (1, [2, 3])
%!error <Invalid call> camzoom ("string")
