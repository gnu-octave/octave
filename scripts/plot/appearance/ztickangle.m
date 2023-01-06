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
## @deftypefn  {} {@var{angle} =} ztickangle ()
## @deftypefnx {} {@var{angle} =} ztickangle (@var{hax})
## @deftypefnx {} {} ztickangle (@var{angle})
## @deftypefnx {} {} ztickangle (@var{hax}, @var{angle})
## Query or set the rotation angle of the tick labels on the z-axis of the
## current axes.
##
## When called without an argument, return the rotation angle in degrees of the
## tick labels as specified in the axes property @qcode{"ZTickLabelRotation"}.
## When called with a numeric scalar @var{angle}, rotate the tick labels
## counterclockwise to @var{angle} degrees.
##
## If the first argument @var{hax} is an axes handle, then operate on this axes
## rather than the current axes returned by @code{gca}.
##
## Programming Note: Requesting a return value while also setting a specified
## rotation will result in an error.
##
## @seealso{xtickangle, ytickangle, get, set}
## @end deftypefn

function angle = ztickangle (hax, angle)

  switch (nargin)
    case 0
      angle = __tickangle__ (mfilename ());

    case 1
      if (nargout > 0)
        angle = __tickangle__ (mfilename (), hax);
      else
        __tickangle__ (mfilename (), hax);
      endif

    case 2
      if (nargout > 0)
        angle = __tickangle__ (mfilename (), hax, angle);
      else
        __tickangle__ (mfilename (), hax, angle);
      endif

  endswitch

endfunction


%!test
%! hf = figure ("visible", "off");
%! hax = axes (hf);
%! unwind_protect
%!   ztickangle (45);
%!   assert (ztickangle (), 45);
%!   ztickangle (hax, 90);
%!   a1 = ztickangle ();
%!   a2 = ztickangle (hax);
%!   assert (a1, a2);
%!   assert (a1, 90);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <HAX must be a handle to an axes object> ztickangle (0, 45)
%!error <ANGLE must be .* scalar> ztickangle (eye (2))
%!error <ANGLE must be .* numeric> ztickangle ({90})
%!error <ANGLE must be .* finite> ztickangle (Inf)
%!error <called with output query and input set value> ang = ztickangle (45)
