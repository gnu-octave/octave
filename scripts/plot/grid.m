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
## @deftypefn  {Function File} {} grid (@var{arg})
## @deftypefnx {Function File} {} grid ("minor", @var{arg2})
## @deftypefnx {Function File} {} grid (@var{hax}, @dots{})
## Force the display of a grid on the plot.
## The argument may be either @code{"on"}, or @code{"off"}.
## If it is omitted, the current grid state is toggled.
##
## If @var{arg} is @code{"minor"} then the minor grid is toggled.  When
## using a minor grid a second argument @var{arg2} is allowed, which can
## be either @code{"on"} or @code{"off"} to explicitly set the state of
## the minor grid.
##
## If the first argument is an axis handle, @var{hax}, operate on the
## specified axis object.
## @seealso{plot}
## @end deftypefn

## Author: jwe

function grid (varargin)

  [ax, varargin, nargs] = __plt_get_axis_arg__ ("grid", varargin{:});

  grid_on = (strcmp (get (ax, "xgrid"), "on")
             && strcmp (get (ax, "ygrid"), "on")
             && strcmp (get (ax, "zgrid"), "on"));

  minor_on = (strcmp (get (ax, "xminorgrid"), "on")
              && strcmp (get (ax, "yminorgrid"), "on")
              && strcmp (get (ax, "zminorgrid"), "on"));

  if (nargs > 2)
    print_usage ();
  elseif (nargs == 0)
    grid_on = ! grid_on;
  else
    x = varargin{1};
    if (ischar (x))
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
    else
      error ("grid: argument must be a string");
    endif
  endif

  if (grid_on)
    set (ax, "xgrid", "on", "ygrid", "on", "zgrid", "on");
    if (minor_on)
      set (ax, "xminorgrid", "on", "yminorgrid", "on", "zminorgrid", "on");
    else
      set (ax, "xminorgrid", "off", "yminorgrid", "off", "zminorgrid", "off");
    endif
  else
    set (ax, "xgrid", "off", "ygrid", "off", "zgrid", "off");
    set (ax, "xminorgrid", "off", "yminorgrid", "off", "zminorgrid", "off");
  endif

endfunction

%!demo
%! clf
%! subplot (2,2,1)
%! plot (1:100)
%! grid minor
%! grid minor
%! grid
%! title ("no grid")
%! subplot (2,2,2)
%! plot (1:100)
%! grid
%! title ("grid on")
%! subplot (2,2,3)
%! plot (1:100)
%! grid minor
%! title ("grid minor")
%! subplot (2,2,4)
%! semilogy (1:100)
%! grid minor
%! title ("grid minor")

