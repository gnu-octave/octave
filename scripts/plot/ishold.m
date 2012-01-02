## Copyright (C) 2005-2012 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Command} {} ishold
## @deftypefnx {Function File} {} ishold (@var{h})
## Return true if the next plot will be added to the current plot, or
## false if the plot device will be cleared before drawing the next plot.
##
## Optionally, operate on the graphics handle @var{h} rather than the current
## plot.
## @seealso{hold}
## @end deftypefn

function retval = ishold (h)

  if (nargin == 0)
    fig = gcf ();
    ax = get (fig, "currentaxes");
  elseif (nargin == 1)
    if (ishandle (h))
      if (isfigure (h))
        ax = get (h, "currentaxes");
        fig = h;
      elseif (strcmpi (get (h, "type"), "axes"))
        ax = h;
        fig = get (h, "parent");
      else
        error ("ishold: expecting argument to be axes or figure graphics handle");
      endif
    else
      error ("ishold: expecting argument to be axes or figure graphics handle");
    endif
  else
    print_usage ();
  endif

  retval = (strcmpi (get (fig, "nextplot"), "add")
            && ! isempty (ax) && strcmpi (get (ax, "nextplot"), "add"));

endfunction

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (!ishold);
%!   assert (isempty (get (hf, "currentaxes")));
%!   assert (get (hf, "NextPlot"), "add");
%!   l = plot ([0 1]);
%!   assert (!ishold);
%!   assert (!ishold (gca));
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
