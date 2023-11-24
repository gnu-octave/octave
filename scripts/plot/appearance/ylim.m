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
## @deftypefn  {} {@var{ylimits} =} ylim ()
## @deftypefnx {} {@var{ymode} =} ylim ("mode")
## @deftypefnx {} {@var{ymethod} =} ylim ("method")
## @deftypefnx {} {} ylim ([@var{y_lo} @var{y_hi}])
## @deftypefnx {} {} ylim ("mode")
## @deftypefnx {} {} ylim ("method")
## @deftypefnx {} {} ylim (@var{hax}, @dots{})
## Query or set the limits of the y-axis for the current plot.
##
## Called without arguments @code{ylim} returns the y-axis limits of the
## current plot.
##
## With the input query @qcode{"mode"}, return the current y-limit calculation
## mode which is either @qcode{"auto"} or @qcode{"manual"}.
##
## With the input query @qcode{"method"}, return the current y-limit
## calculation method which is either @qcode{"tickaligned"}, @qcode{"tight"},
## or @qcode{"padded"}.
##
## If passed a 2-element vector [@var{y_lo} @var{y_hi}], the limits of the
## y-axis are set to these values and the mode is set to @qcode{"manual"}.
## The special values -Inf and Inf can be used to indicate that either
## the lower axis limit or upper axis limit should be automatically calculated.
##
## The current limit calculation @qcode{"mode"} may be one of
##
## @table @asis
## @item @qcode{"auto"} (default)
## Automatically calculate limits based on the plot data and the currently
## specified limit calculation method.
##
## @item @qcode{"manual"}
## Fix axis limits at current values.
## @end table
##
## The current limit calculation method may be one of
##
## @table @asis
## @item @qcode{"tickaligned"} (default)
## Calculate limits that encompass all of the data and extend outwards to the
## nearest tick mark.
##
## @item @qcode{"tight"}
## Calculate limits that exactly fit the data range.
##
## @item @qcode{"padded"}
## Calculate limits that leave a margin around the data of approximately 7% of
## the data range.
## @end table
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axes rather than the current axes returned by @code{gca}.
##
## Programming Note: The @code{ylim} function operates by modifying the
## @qcode{"ylim"}, @qcode{"ylimmode"}, and @qcode{"ylimitmethod"} properties of
## an axes object.  These properties can be directly inspected and altered with
## @code{get}/@code{set}.
## @seealso{xlim, zlim, axis, set, get, gca}
## @end deftypefn

function ylimits = ylim (varargin)

  lims = __axis_limits__ ("ylim", varargin{:});

  if (! isempty (lims))
    ylimits = lims;
  endif

endfunction


%!demo
%! clf;
%! line ();
%! ylim ([0.2, 0.8]);
%! title ("ylim is [0.2, 0.8]");

%!demo
%! clf;
%! line ();
%! ylim auto;
%! title ("ylim is auto");

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! ylim ([0.2, 0.8]);
%! title ("ylim is [0.2, 0.8]");

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! ylim ("auto");
%! title ("ylim is auto");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   limy = [0, 1.1];
%!   plot3 ([0,1], [0,1], [0,1]);
%!   ylim (limy);
%!   assert (get (gca, "ylim"), limy, eps);
%!   assert (ylim ("mode"), "manual");
%!   ylim ('padded');
%!   assert (get (gca, "ylimitmethod"), 'padded');
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1.1], [0, 1]);
%!   assert (get (gca, "ylim"), [0, 1.4], eps);
%!   assert (ylim ("mode"), "auto");
%!   assert (ylim ("method"), "tickaligned");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
