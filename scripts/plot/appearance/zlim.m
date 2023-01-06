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
## @deftypefn  {} {@var{zlimits} =} zlim ()
## @deftypefnx {} {@var{xmode} =} zlim ("mode")
## @deftypefnx {} {} zlim ([@var{z_lo} @var{z_hi}])
## @deftypefnx {} {} zlim ("auto")
## @deftypefnx {} {} zlim ("manual")
## @deftypefnx {} {} zlim (@var{hax}, @dots{})
## Query or set the limits of the z-axis for the current plot.
##
## Called without arguments @code{zlim} returns the z-axis limits of the
## current plot.
##
## With the input query @qcode{"mode"}, return the current z-limit calculation
## mode which is either @qcode{"auto"} or @qcode{"manual"}.
##
## If passed a 2-element vector [@var{z_lo} @var{z_hi}], the limits of the
## z-axis are set to these values and the mode is set to @qcode{"manual"}.
## The special values -Inf and Inf can be used to indicate that either
## the lower axis limit or upper axis limit should be automatically calculated.
##
## The current plotting mode can be changed by using either @qcode{"auto"}
## or @qcode{"manual"} as the argument.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axes rather than the current axes returned by @code{gca}.
##
## Programming Note: The @code{zlim} function operates by modifying the
## @qcode{"zlim"} and @qcode{"zlimmode"} properties of an axes object.  These
## properties can be directly inspected and altered with @code{get}/@code{set}.
## @seealso{xlim, ylim, axis, set, get, gca}
## @end deftypefn

function zlimits = zlim (varargin)

  lims = __axis_limits__ ("zlim", varargin{:});

  if (! isempty (lims))
    zlimits = lims;
  endif

endfunction


%!demo
%! clf;
%! line ();
%! zlim ([0.2, 0.8]);
%! title ("zlim is [0.2, 0.8]");

%!demo
%! clf;
%! line ();
%! zlim ("auto");
%! title ("zlim is auto");

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! zlim ([0.2, 0.8]);
%! title ("zlim is [0.2, 0.8]");

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! zlim auto;
%! title ("zlim is auto");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   limz = [0, 1.1];
%!   plot3 ([0,1], [0,1], [0,1]);
%!   zlim (limz);
%!   assert (get (gca, "zlim"), limz, eps);
%!   assert (zlim ("mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1], [0, 1.1]);
%!   assert (get (gca, "zlim"), [0, 1.4], eps);
%!   assert (zlim ("mode"), "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
