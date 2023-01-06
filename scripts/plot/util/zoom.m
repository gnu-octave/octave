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
## @deftypefn  {} {} zoom
## @deftypefnx {} {} zoom (@var{factor})
## @deftypefnx {} {} zoom on
## @deftypefnx {} {} zoom off
## @deftypefnx {} {} zoom xon
## @deftypefnx {} {} zoom yon
## @deftypefnx {} {} zoom out
## @deftypefnx {} {} zoom reset
## @deftypefnx {} {} zoom (@var{hfig}, @var{option})
## Zoom the current axes object or control the interactive zoom mode of a
## figure in the GUI.
##
## Given a numeric argument greater than zero, zoom by the given factor.  If
## the zoom factor is greater than one, zoom in on the plot.  If the factor
## is less than one, zoom out.  If the zoom factor is a two- or three-element
## vector, then the elements specify the zoom factors for the x, y, and z
## axes respectively.
##
## Given the option @qcode{"on"} or @qcode{"off"}, set the interactive zoom
## mode on or off.
##
## With no arguments, toggle the current zoom mode on or off.
##
## Given the option @qcode{"xon"} or @qcode{"yon"}, enable zoom mode for the
## x or y-axis only.
##
## Given the option @qcode{"out"}, zoom to the initial zoom setting.
##
## Given the option @qcode{"reset"}, store the current zoom setting so that
## @code{zoom out} will return to this zoom level.
##
## If the first argument @var{hfig} is a figure, then operate on the given
## figure rather than the current figure as returned by @code{gcf}.
##
## @seealso{pan, rotate3d}
## @end deftypefn

## Eventually we need to also support these features:
## @deftypefnx {} {zoom_object_handle =} zoom (@var{hfig})

function zoom (hfig, option)

  ## FIXME: Presumably should implement this for Matlab compatibility.
  if (nargin == 1 && nargout > 0 && isfigure (hfig))
    error ("zoom: syntax 'handle = zoom (hfig)' not implemented");
  endif

  if (nargin == 0)
    hfig = gcf ();
  else
    if (nargin == 1)
      option = hfig;
      hfig = gcf ();
    else
      if (! isfigure (hfig))
        error ("zoom: invalid figure handle HFIG");
      endif
    endif
  endif

  if (nargin == 0)
    zm = get (hfig, "__zoom_mode__");
    if (strcmp (zm.Enable, "on"))
      zm.Enable = "off";
    else
      zm.Enable = "on";
    endif
    set (hfig, "__zoom_mode__", zm);
    update_mouse_mode (hfig, zm.Enable);
  else
    if (isnumeric (option))
      factor = option;
      switch (numel (factor))
        case 2
          xfactor = factor(1);
          yfactor = factor(2);
        case 1
          xfactor = yfactor = factor;
        otherwise
          error ("zoom: FACTOR must be a 1- or 2-element vector");
      endswitch
      if (xfactor <= 0 || yfactor <= 0)
        error ("zoom: FACTOR must be greater than 0");
      elseif (xfactor == 1 && yfactor == 1)
        return;
      endif
      cax = get (hfig, "currentaxes");
      if (! isempty (cax))
        if (xfactor != 1)
          if (yfactor != 1)
            mode = "both";
          else
            mode = "horizontal";
          endif
        else
          if (yfactor != 1)
            mode = "vertical";
          endif
        endif
        __zoom__ (cax, mode, factor);
      endif
    elseif (ischar (option))
      switch (option)
        case {"on", "off", "xon", "yon"}
          zm = get (hfig, "__zoom_mode__");
          switch (option)
            case {"on", "off"}
              zm.Enable = option;
              zm.Motion = "both";
            case "xon"
              zm.Enable = "on";
              zm.Motion = "horizontal";
            case "yon"
              zm.Enable = "on";
              zm.Motion = "vertical";
          endswitch
          set (hfig, "__zoom_mode__", zm);
          update_mouse_mode (hfig, option);
        case "out"
          cax = get (hfig, "currentaxes");
          if (! isempty (cax))
            __zoom__ (cax, "out");
          endif
        case "reset"
          cax = get (hfig, "currentaxes");
          if (! isempty (cax))
            __zoom__ (cax, "reset");
          endif
        otherwise
          error ("zoom: unrecognized OPTION '%s'", option);
      endswitch
    else
      error ("zoom: OPTION must be a number or a string");
    endif
  endif

endfunction

function update_mouse_mode (hfig, arg)

  if (strcmp (arg, "off"))
    set (hfig, "__mouse_mode__", "none");
  else
    ## FIXME: Is there a better way other than calling these functions
    ##        to set the other mouse mode Enable fields to "off"?
    pan (hfig, "off");
    rotate3d (hfig, "off");
    set (hfig, "__mouse_mode__", "zoom");
  endif

endfunction


%!demo
%! clf;
%! sombrero ();
%! title ("zoom() demo #1");
%! pause (1);
%! ## zoom in by a factor of 2
%! zoom (2);
%! pause (1);
%! ## return to original zoom level
%! zoom out;
%! pause (1);
%! ## zoom in by a factor of 2
%! zoom (2);
%! pause (1);
%! ## set this zoom level as the "initial zoom level"
%! ## and zoom in some more
%! zoom reset;
%! zoom (2);
%! pause (1);
%! ## return to zoom level set by last call to "zoom reset"
%! zoom out;
