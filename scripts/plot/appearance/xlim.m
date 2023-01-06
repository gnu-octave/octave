########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{xlimits} =} xlim ()
## @deftypefnx {} {@var{xmode} =} xlim ("mode")
## @deftypefnx {} {} xlim ([@var{x_lo} @var{x_hi}])
## @deftypefnx {} {} xlim ("auto")
## @deftypefnx {} {} xlim ("manual")
## @deftypefnx {} {} xlim (@var{hax}, @dots{})
## Query or set the limits of the x-axis for the current plot.
##
## Called without arguments @code{xlim} returns the x-axis limits of the
## current plot.
##
## With the input query @qcode{"mode"}, return the current x-limit calculation
## mode which is either @qcode{"auto"} or @qcode{"manual"}.
##
## If passed a 2-element vector [@var{x_lo} @var{x_hi}], the limits of the
## x-axis are set to these values and the mode is set to @qcode{"manual"}.
## The special values -Inf and Inf can be used to indicate that either
## the lower axis limit or upper axis limit should be automatically calculated.
##
## The current plotting mode can be changed by using either @qcode{"auto"}
## or @qcode{"manual"} as the argument.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axes rather than the current axes returned by @code{gca}.
##
## Programming Note: The @code{xlim} function operates by modifying the
## @qcode{"xlim"} and @qcode{"xlimmode"} properties of an axes object.  These
## properties can be directly inspected and altered with @code{get}/@code{set}.
## @seealso{ylim, zlim, axis, set, get, gca}
## @end deftypefn

function xlimits = xlim (varargin)

  lims = __axis_limits__ ("xlim", varargin{:});

  if (! isempty (lims))
    xlimits = lims;
  endif

endfunction


%!demo
%! clf;
%! line ();
%! xlim ([0.2, 0.8]);
%! title ("xlim is [0.2, 0.8]");

%!demo
%! clf;
%! line ();
%! xlim auto;
%! title ("xlim is auto");

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! xlim ([0.2, 0.8]);
%! title ("xlim is [0.2, 0.8]");

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! xlim ("auto");
%! title ("xlim is auto");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1], [0,1]);
%!   xlim ([0, 1.1]);
%!   assert (get (gca, "xlim"), [0, 1.1], eps);
%!   assert (xlim ("mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot3 ([0,1.1], [0,1], [0, 1]);
%!   assert (get (gca, "xlim"), [0, 1.4], eps);
%!   assert (xlim ("mode"), "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! ## Test input validation, done only in xlim since it is common to ylim,zlim.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot3 ([0,1.1], [0,1], [0, 1]);
%!   fail ("xlim ({1, 2})", "LIMITS must be a 2-element vector");
%!   fail ("xlim ([1, 2, 3])", "LIMITS must be a 2-element vector");
%!   fail ("xlim ([2, 1])", "axis limits must be increasing");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect


