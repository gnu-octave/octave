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
## @deftypefn  {Command} {} rotate3d
## @deftypefnx {Command} {} rotate3d on
## @deftypefnx {Command} {} rotate3d off
## @deftypefnx {Function File} {} rotate3d (@var{hax}, @dots{})
## Control 3D rotation mode of interactive graph in GUI.
##
## The function state input may be either @qcode{"on"} or @qcode{"off"}
## and can only be set for 3D plots.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## To query the current mode use the @code{get} function.  For example:
## @example
## mode = get (gca, "rotate3d");
## @end example
## @seealso{pan}
## @end deftypefn

function rotate3d (varargin)

  if (numel (varargin) > 0 && isaxes (varargin{1}))
    hax = varargin{1};
    varargin(1) = [];
  else
    hax = gca ();
  endif

  toolkit = get (ancestor (hax, "figure"), "__graphics_toolkit__");
  if (! strcmp (toolkit, "fltk"))
    warning ("rotate3d: Only implemented for graphics_toolkit FLTK");
  endif

  ndims = __calc_dimensions__ (hax);
  if (ndims == 2)
    warning ("rotate3d: Only available for 3D plots");
  else
    if (numel (varargin) > 1)
      print_usage ();
    elseif (numel (varargin) == 0)
      # toggle
      m = get (hax, "pan");
      if (strcmp (get (hax, "rotate3d"), "on"))
        set (hax, "rotate3d", "off");
      else
        set (hax, "rotate3d", "on");
      endif
    elseif (numel (varargin) == 1)
      set (hax, "rotate3d", varargin{1});
    endif
  endif

endfunction

