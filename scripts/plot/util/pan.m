## Copyright (C) 2014 Andreas Weber
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
## @deftypefn  {Command} {} pan
## @deftypefnx {Command} {} pan on
## @deftypefnx {Command} {} pan xon
## @deftypefnx {Command} {} pan yon
## @deftypefnx {Command} {} pan off
## @deftypefnx {Function File} {} pan (@var{hax}, @dots{})
## Control panning mode of interactive graph in GUI.
##
## The function state input may be either @qcode{"on"}, @qcode{"xon"},
## @qcode{"yon"} or @qcode{"off"}.
##
## If it is omitted the current state is toggled (@qcode{"xon"} and
## @qcode{"yon"} are treated as @qcode{"on"}).
##
## @qcode{"xon"} limits panning to the x-axis, @qcode{"yon"} to the
## y-axis.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## To query the current mode use the @code{get}
## function.  For example:
## @example
## mode = get (gca, "pan");
## @end example
## @seealso{rotate3d, zoom}
## @end deftypefn

function pan (varargin)

  if (numel (varargin) > 0 && isaxes (varargin{1}))
    hax = varargin{1};
    varargin(1) = [];
  else
    hax = gca ();
  endif

  toolkit = get (ancestor (hax, "figure"), "__graphics_toolkit__");
  if (! strcmp (toolkit, "fltk"))
    warning ("pan: Only implemented for graphics_toolkit FLTK");
  endif

  if (numel (varargin) > 1)
    print_usage ();
  elseif (numel (varargin) == 0)
    # toggle
    m = get (hax, "pan");
    if (findstr (m, "on") > 0)
      set (hax, "pan", "off");
    else
      set (hax, "pan", "on");
    endif
  elseif (numel (varargin) == 1)
    set (hax, "pan", varargin{1});
  endif

endfunction

