########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
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
## @deftypefn  {} {} pan
## @deftypefnx {} {} pan on
## @deftypefnx {} {} pan off
## @deftypefnx {} {} pan xon
## @deftypefnx {} {} pan yon
## @deftypefnx {} {} pan (@var{hfig}, @var{option})
## Control the interactive panning mode of a figure in the GUI.
##
## Given the option @qcode{"on"} or @qcode{"off"}, set the interactive
## pan mode on or off.
##
## With no arguments, toggle the current pan mode on or off.
##
## Given the option @qcode{"xon"} or @qcode{"yon"}, enable pan mode
## for the x or y axis only.
##
## If the first argument @var{hfig} is a figure, then operate on the given
## figure rather than the current figure as returned by @code{gcf}.
##
## @seealso{rotate3d, zoom}
## @end deftypefn

function pan (hfig, option)

  ## FIXME: Presumably should implement this for Matlab compatibility.
  if (nargin == 1 && nargout > 0 && isfigure (hfig))
    error ("pan: syntax 'handle = pan (hfig)' not implemented");
  endif

  if (nargin == 0)
    hfig = gcf ();
  else
    if (nargin == 1)
      option = hfig;
      hfig = gcf ();
    else
      if (! isfigure (hfig))
        error ("pan: invalid figure handle HFIG");
      endif
    endif

    if (! ischar (option))
      error ("pan: OPTION must be a string");
    endif
  endif

  if (nargin == 0)
    pm = get (hfig, "__pan_mode__");
    if (strcmp (pm.Enable, "on"))
      pm.Enable = "off";
    else
      pm.Enable = "on";
    endif
    set (hfig, "__pan_mode__", pm);
    update_mouse_mode (hfig, pm.Enable);
  else
    switch (option)
      case {"on", "off", "xon", "yon"}
        pm = get (hfig, "__pan_mode__");
        switch (option)
          case {"on", "off"}
            pm.Enable = option;
            pm.Motion = "both";
          case "xon"
            pm.Enable = "on";
            pm.Motion = "horizontal";
          case "yon"
            pm.Enable = "on";
            pm.Motion = "vertical";
        endswitch
        set (hfig, "__pan_mode__", pm);
        update_mouse_mode (hfig, option);
      otherwise
        error ("pan: unrecognized OPTION '%s'", option);
    endswitch
  endif

endfunction

function update_mouse_mode (hfig, arg)

  if (strcmp (arg, "off"))
    set (hfig, "__mouse_mode__", "none");
  else
    ## FIXME: Is there a better way other than calling these functions
    ##        to set the other mouse mode Enable fields to "off"?
    rotate3d (hfig, "off");
    zoom (hfig, "off");
    set (hfig, "__mouse_mode__", "pan");
  endif

endfunction
