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
## @deftypefn {Function File} {} newplot ()
## Prepare graphics engine to produce a new plot.  This function is
## called at the beginning of all high-level plotting functions.
## It is not normally required in user programs.
## @end deftypefn

function newplot ()

  if (nargin == 0)
    cf = gcf ();
    fnp = get (cf, "nextplot");
    switch (fnp)
      ## FIXME -- probably we should do more than validate the nextplot
      ## property value...
      case "new"
      case "add"
      case "replacechildren"
        delete (get (cf, "children"));
      case "replace"
      otherwise
        error ("newplot: unrecognized nextplot property for current figure");
    endswitch
    ca = gca ();
    anp = get (ca, "nextplot");
    if (strcmp (get (ca, "__hold_all__"), "off"))
      __next_line_color__ (true);
      __next_line_style__ (true);
    else
      __next_line_color__ (false);
      __next_line_style__ (false);
    endif
    switch (anp)
      case "new"
      case "add"
      case "replacechildren"
        delete (get (ca, "children"));
      case "replace"
        __go_axes_init__ (ca, "replace");
        __request_drawnow__ ();
      otherwise
        error ("newplot: unrecognized nextplot property for current axes");
    endswitch
  else
    print_usage ();
  endif

endfunction

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   p = plot ([0, 1]);
%!   newplot;
%!   assert (isempty (get (gca, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
