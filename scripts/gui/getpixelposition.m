########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{pos} =} getpixelposition (@var{h})
## @deftypefnx {} {@var{pos} =} getpixelposition (@var{h}, @var{rel_to_fig})
## Return the position of a user interface component in pixel units.
##
## The first argument @var{h} must be a handle to a valid graphics object of
## type uibuttongroup, uicontrol, uipanel, uitable, axes, or figure.  For other
## object types, the function returns zeros.
##
## By default, the position is returned relative to the object's parent.
## If the second argument @var{rel_to_fig} is logically true, the position
## is computed relative to the enclosing figure object.
##
## The return value @var{pos} is a 4-element vector with values
## @code{[ lower_left_X, lower_left_Y, width, height ]}.
##
## @seealso{get}
## @end deftypefn

function pos = getpixelposition (h, rel_to_fig = false)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isscalar (h) || ! ishghandle (h))
    error ("getpixelposition: H must be a scalar graphics handle");
  endif

  if (! any (strcmp (get (h, "type"), {"uibuttongroup", "uicontrol", ...
                                       "uitable", "uipanel", ...
                                       "axes", "figure"})))
    pos = zeros (1, 4);
    return;
  endif

  pos = __get_position__ (h, "pixels");

  if (rel_to_fig)
    while (! isfigure (h))
      h = get (h, "parent");
      pos(1:2) += __get_position__ (h, "pixels")(1:2);
    endwhile
  endif

endfunction


%!demo
%! clf ();
%! hax = axes ("position", [0.25 0.25 0.5 0.5])
%! pos = getpixelposition (hax);
%! han = annotation ("rectangle");
%! set (han, "units", "pixels", "position", pos, "color", "r")

%!demo
%! hf = clf ();
%! hbg = uibuttongroup (hf, "position", [0.2 0.7 0.2 0.2]);
%! hb1 = uicontrol (hbg, "style", "radiobutton", ...
%!                       "string", "Choice 1", ...
%!                       "units", "normalized", ...
%!                       "position", [0.01 0.5 0.98 0.5]);
%! hb2 = uicontrol (hbg, "style", "radiobutton", ...
%!                       "string", "Choice 2", ...
%!                       "units", "normalized", ...
%!                       "position", [0.01 0 0.98 0.5]);
%! pos = getpixelposition (hbg);
%! han = annotation ("rectangle");
%! set (han, "units", "pixels", "position", pos, "color", "r")


%!test
%! pos = [0 0 400 400];
%! hf = figure ("visible", "off", "menubar", "none", "position", pos);
%! unwind_protect
%!   hp = uipanel (hf, "position", [0.5 0.5 0.5 0.5]);
%!   hax = axes (hp, "position", [0.5 0.5 0.5 0.5]);
%!   assert (getpixelposition (hax), [100 100 100 100], 2);
%!   assert (getpixelposition (hax, true), [300 300 100 100], 2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Compatibility: return zeros for root figure
%! assert (getpixelposition (groot), zeros (1, 4));

%!test
%! ## Compatibility: return the same for figures regardless of rel_to_fig
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (getpixelposition (hf), getpixelposition (hf, true));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> getpixelposition ()
%!error <H must be a scalar> getpixelposition ([1, 2])
%!error <H must be a .* graphics handle> getpixelposition (-1)

