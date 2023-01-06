########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn  {} {} grid
## @deftypefnx {} {} grid on
## @deftypefnx {} {} grid off
## @deftypefnx {} {} grid minor
## @deftypefnx {} {} grid minor on
## @deftypefnx {} {} grid minor off
## @deftypefnx {} {} grid (@var{hax}, @dots{})
## Control the display of plot grid lines.
##
## The function state input may be either @qcode{"on"} or @qcode{"off"}.
## If it is omitted, the current grid state is toggled.
##
## When the first argument is @qcode{"minor"} all subsequent commands
## modify the minor grid rather than the major grid.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axes rather than the current axes returned by @code{gca}.
##
## To control the grid lines for an individual axes use the @code{set}
## function.  For example:
##
## @example
## set (gca, "ygrid", "on");
## @end example
## @seealso{axis, box}
## @end deftypefn

function grid (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("grid", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  ## Must be after gca (), since grid with no plot should create one.
  if (nargs > 2)
    print_usage ();
  endif

  grid_on = any (strcmp (get (hax, {"xgrid", "ygrid", "zgrid"}), "on"));

  minor_on = any (strcmp (get (hax, {"xminorgrid", "yminorgrid", "zminorgrid"}),
                         "on") &
                  ! strcmp (get (hax, {"xscale", "yscale", "zscale"}), "log"));

  minor_auto = true;
  if (nargs == 0)
    grid_on = ! grid_on;
    if (! grid_on)
      minor_auto = false;
    endif
  else
    arg1 = varargin{1};
    if (! ischar (arg1))
      error ("grid: argument 1 must be an axis handle or a string");
    endif
    if (strcmpi (arg1, "off"))
      grid_on = false;
      minor_on = false;
      minor_auto = false;
    elseif (strcmpi (arg1, "on"))
      grid_on = true;
      minor_auto = true;
    elseif (strcmpi (arg1, "minor"))
      minor_auto = false;
      if (nargs == 2)
        arg2 = varargin{2};
        if (strcmpi (arg2, "on"))
          minor_on = true;
        elseif (strcmpi (arg2, "off"))
          minor_on = false;
        else
          print_usage ();
        endif
      else
        minor_on = ! minor_on;
      endif
    else
      print_usage ();
    endif
  endif

  if (grid_on)
    set (hax, "xgrid", "on", "ygrid", "on", "zgrid", "on");
  else
    set (hax, "xgrid", "off", "ygrid", "off", "zgrid", "off");
  endif

  if (minor_on)
    set (hax, "xminorgrid", "on", "yminorgrid", "on", "zminorgrid", "on");
  elseif (minor_auto)
    xmg = ifelse (strcmp (get (hax, "xscale"), "log"), "on", "off");
    ymg = ifelse (strcmp (get (hax, "yscale"), "log"), "on", "off");
    zmg = ifelse (strcmp (get (hax, "zscale"), "log"), "on", "off");
    set (hax, "xminorgrid", xmg, "yminorgrid", ymg, "zminorgrid", zmg);
  else
    set (hax, "xminorgrid", "off", "yminorgrid", "off", "zminorgrid", "off");
  endif

endfunction


%!demo
%! clf;
%! subplot (3, 2, 1);
%!  plot (1:100);
%!  grid off;
%!  title ("grid off");
%! subplot (3, 2, 2);
%!  plot (1:100);
%!  grid on;
%!  title ("grid on");
%! subplot (3, 2, 3);
%!  plot (1:100);
%!  set (gca, "xgrid", "on");
%!  title ("xgrid on");
%! subplot (3, 2, 4);
%!  plot (1:100);
%!  set (gca, "ygrid", "on");
%!  title ("ygrid on");
%! subplot (3, 2, 5);
%!  plot (1:100);
%!  grid minor;
%!  title ("grid minor");
%! subplot (3, 2, 6);
%!  plot (1:100);
%!  set (gca, "yminorgrid", "on");
%!  title ("yminorgrid on");

%!demo
%! clf;
%! subplot (2,2,1);
%!  semilogy (1:100);
%!  grid off;
%!  title ("grid off");
%! subplot (2,2,2);
%!  semilogy (1:100);
%!  grid on;
%!  title ("grid on");
%! subplot (2,2,3);
%!  semilogy (1:100);
%!  grid off;
%!  title ("no grid");
%! subplot (2,2,4);
%!  semilogy (1:100);
%!  grid minor;
%!  title ("grid minor");

%!demo
%! ## Display minor grid lines at major ticks
%! clf;
%! subplot (1,2,1)
%!  plot (1:10);
%!  set (gca, "xminorgrid", "on");
%!  set (gca, "yminorgrid", "on");
%!  title ({"major grid disabled",
%!          "minor grid displayed at major ticks"});
%! subplot (1,2,2)
%!  semilogy (1:100);
%!  set (gca, "xminorgrid", "on");
%!  set (gca, "yminorgrid", "on");
%!  title ({"major grid disabled",
%!          "minor grid displayed at major ticks"});

%!demo
%! clf;
%! plot3 (1:10, 1:10, 1:10);
%! set (gca, "xtick", [0, pi/2, 4.7, 8, 10]);
%! set (gca, "ytick", [0, 1, pi, 7.3, 10]);
%! set (gca, "ztick", [0, exp(1), 5, 9.1, 10]);
%! set (gca, "xminorgrid", "on");
%! set (gca, "yminorgrid", "on");
%! set (gca, "zminorgrid", "on");
%! view (3);
%! title ("Minor grid adapts to xticks (bug #45850)")

## linear scaling
%!test <*48533>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   plot (1:10);
%!   grid on
%!   assert (get (hax, "xgrid"), "on");
%!   assert (get (hax, "ygrid"), "on");
%!   assert (get (hax, "zgrid"), "on");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid minor
%!   assert (get (hax, "xgrid"), "on");
%!   assert (get (hax, "ygrid"), "on");
%!   assert (get (hax, "zgrid"), "on");
%!   assert (get (hax, "xminorgrid"), "on");
%!   assert (get (hax, "yminorgrid"), "on");
%!   assert (get (hax, "zminorgrid"), "on");
%!   grid off
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid minor
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "on");
%!   assert (get (hax, "yminorgrid"), "on");
%!   assert (get (hax, "zminorgrid"), "on");
%!   grid minor
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid
%!   assert (get (hax, "xgrid"), "on");
%!   assert (get (hax, "ygrid"), "on");
%!   assert (get (hax, "zgrid"), "on");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## semilog scaling
%!test <*48533>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   semilogy (1:100);
%!   grid on
%!   assert (get (hax, "xgrid"), "on");
%!   assert (get (hax, "ygrid"), "on");
%!   assert (get (hax, "zgrid"), "on");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "on");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid on
%!   assert (get (hax, "xgrid"), "on");
%!   assert (get (hax, "ygrid"), "on");
%!   assert (get (hax, "zgrid"), "on");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "on");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid minor
%!   assert (get (hax, "xgrid"), "on");
%!   assert (get (hax, "ygrid"), "on");
%!   assert (get (hax, "zgrid"), "on");
%!   assert (get (hax, "xminorgrid"), "on");
%!   assert (get (hax, "yminorgrid"), "on");
%!   assert (get (hax, "zminorgrid"), "on");
%!   grid off
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid minor
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "on");
%!   assert (get (hax, "yminorgrid"), "on");
%!   assert (get (hax, "zminorgrid"), "on");
%!   grid minor
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid
%!   assert (get (hax, "xgrid"), "on");
%!   assert (get (hax, "ygrid"), "on");
%!   assert (get (hax, "zgrid"), "on");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "on");
%!   assert (get (hax, "zminorgrid"), "off");
%!   grid
%!   assert (get (hax, "xgrid"), "off");
%!   assert (get (hax, "ygrid"), "off");
%!   assert (get (hax, "zgrid"), "off");
%!   assert (get (hax, "xminorgrid"), "off");
%!   assert (get (hax, "yminorgrid"), "off");
%!   assert (get (hax, "zminorgrid"), "off");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
