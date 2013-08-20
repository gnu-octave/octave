## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Command} {} grid
## @deftypefnx {Command} {} grid on
## @deftypefnx {Command} {} grid off
## @deftypefnx {Command} {} grid minor
## @deftypefnx {Command} {} grid minor on
## @deftypefnx {Command} {} grid minor off
## @deftypefnx {Function File} {} grid (@var{hax}, @dots{})
## Control the display of plot grid lines.
##
## The function state input may be either @qcode{"on"} or @qcode{"off"}.
## If it is omitted, the current grid state is toggled.
##
## When the first argument is @qcode{"minor"} all subsequent commands
## modify the minor grid rather than the major grid.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## To control the grid lines for an individual axis use the @code{set}
## function.  For example:
##
## @example
## set (gca, "ygrid", "on");
## @end example
## @seealso{axis, box}
## @end deftypefn

## Author: jwe

function grid (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("grid", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif
  
  if (nargs > 2)
    print_usage ();
  endif

  grid_on = (   strcmp (get (hax, "xgrid"), "on")
             && strcmp (get (hax, "ygrid"), "on")
             && strcmp (get (hax, "zgrid"), "on"));

  minor_on = (   strcmp (get (hax, "xminorgrid"), "on")
              && strcmp (get (hax, "yminorgrid"), "on")
              && strcmp (get (hax, "zminorgrid"), "on"));

  if (nargs == 0)
    grid_on = ! grid_on;
  else
    x = varargin{1};
    if (! ischar (x))
      error ("grid: argument 1 must be an axis handle or a string");
    endif
    if (strcmpi (x, "off"))
      grid_on = false;
    elseif (strcmpi (x, "on"))
      grid_on = true;
    elseif (strcmpi (x, "minor"))
      if (nargs == 2)
        x2 = varargin{2};
        if (strcmpi (x2, "on"))
          minor_on = true;
          grid_on = true;
        elseif (strcmpi (x2, "off"))
          minor_on = false;
        else
          print_usage ();
        endif
      else
        minor_on = ! minor_on;
        if (minor_on)
          grid_on = true;
        endif
      endif
    else
      print_usage ();
    endif
  endif

  if (grid_on)
    set (hax, "xgrid", "on", "ygrid", "on", "zgrid", "on");
    if (minor_on)
      set (hax, "xminorgrid", "on", "yminorgrid", "on", "zminorgrid", "on");
    else
      set (hax, "xminorgrid", "off", "yminorgrid", "off", "zminorgrid", "off");
    endif
  else
    set (hax, "xgrid", "off", "ygrid", "off", "zgrid", "off",
              "xminorgrid", "off", "yminorgrid", "off", "zminorgrid", "off");
  endif

endfunction


%!demo
%! clf;
%! subplot (2,2,1);
%!  plot (1:100);
%!  grid off;
%!  title ('no grid');
%! subplot (2,2,2);
%!  plot (1:100);
%!  grid on;
%!  title ('grid on');
%! subplot (2,2,3);
%!  plot (1:100);
%!  grid minor;
%!  title ('grid minor');
%! subplot (2,2,4);
%!  semilogy (1:100);
%!  grid minor;
%!  title ('grid minor');

