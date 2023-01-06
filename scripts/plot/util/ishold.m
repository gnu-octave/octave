########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{tf} =} ishold
## @deftypefnx {} {@var{tf} =} ishold (@var{hax})
## @deftypefnx {} {@var{tf} =} ishold (@var{hfig})
## Return true if the next plot will be added to the current plot, or
## false if the plot device will be cleared before drawing the next plot.
##
## If the first argument is an axes handle @var{hax} or figure handle
## @var{hfig} then operate on this plot rather than the current one.
## @seealso{hold, newplot}
## @end deftypefn

function tf = ishold (h)

  if (nargin == 0)
    fig = gcf ();
    ax = get (fig, "currentaxes");
  else
    if (! ishghandle (h))
      error ("ishold: H must be an axes or figure graphics handle");
    endif

    switch (get (h, "type"))
      case "figure"
        fig = h;
        ax = get (fig, "currentaxes");

      case "axes"
        ax = h;
        fig = ancestor (ax, "figure");

      otherwise
        error ("ishold: H must be an axes or figure graphics handle");

    endswitch
  endif

  tf = (strcmp (get (fig, "nextplot"), "add")
            && ! isempty (ax) && strcmp (get (ax, "nextplot"), "add"));

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (! ishold);
%!   assert (isempty (get (hf, "currentaxes")));
%!   assert (get (hf, "NextPlot"), "add");
%!   l = plot ([0 1]);
%!   assert (! ishold);
%!   assert (! ishold (gca));
%!   assert (get (gca, "NextPlot"), "replace");
%!   assert (get (hf, "NextPlot"), "add");
%!   hold;
%!   assert (ishold);
%!   assert (ishold (gca));
%!   assert (get (gca, "NextPlot"), "add");
%!   assert (get (hf, "NextPlot"), "add");
%!   p = fill ([0 1 1], [0 0 1],"black");
%!   assert (length (get (hf, "children")), 1);
%!   assert (length (get (gca, "children")), 2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
