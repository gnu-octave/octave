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
## @deftypefn  {} {} rotate3d
## @deftypefnx {} {} rotate3d on
## @deftypefnx {} {} rotate3d off
## @deftypefnx {} {} rotate3d (@var{hfig}, @var{option})
## Control the interactive 3-D rotation mode of a figure in the GUI.
##
## Given the option @qcode{"on"} or @qcode{"off"}, set the interactive
## rotate mode on or off.
##
## With no arguments, toggle the current rotate mode on or off.
##
## If the first argument @var{hfig} is a figure, then operate on the given
## figure rather than the current figure as returned by @code{gcf}.
##
## @seealso{pan, zoom}
## @end deftypefn

function rotate3d (hfig, option)

  ## FIXME: Presumably should implement this for Matlab compatibility.
  if (nargin == 1 && nargout > 0 && isfigure (hfig))
    error ("rotate3d: syntax 'handle = rotate3d (hfig)' not implemented");
  endif

  if (nargin == 0)
    hfig = gcf ();
  else
    if (nargin == 1)
      option = hfig;
      hfig = gcf ();
    else
      if (! isfigure (hfig))
        error ("rotate3d: invalid figure handle HFIG");
      endif
    endif

    if (! ischar (option))
      error ("rotate3d: OPTION must be a string");
    endif
  endif

  if (nargin == 0)
    rm = get (hfig, "__rotate_mode__");
    if (strcmp (rm.Enable, "on"))
      rm.Enable = "off";
    else
      rm.Enable = "on";
    endif
    set (hfig, "__rotate_mode__", rm);
    update_mouse_mode (hfig, rm.Enable);
  else
    switch (option)
      case {"on", "off"}
        rm = get (hfig, "__rotate_mode__");
        rm.Enable = option;
        rm.Motion = "both";
        set (hfig, "__rotate_mode__", rm);
        update_mouse_mode (hfig, option);
      otherwise
        error ("rotate3d: unrecognized OPTION '%s'", option);
    endswitch
  endif

endfunction

function update_mouse_mode (hfig, arg)

  if (strcmp (arg, "off"))
    set (hfig, "__mouse_mode__", "none");
  else
    ## FIXME: Is there a better way other than calling these functions
    ##        to set the other mouse mode Enable fields to "off"?
    pan (hfig, "off");
    zoom (hfig, "off");
    set (hfig, "__mouse_mode__", "rotate");
  endif

endfunction
