########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{tickval} =} rticks
## @deftypefnx {} {} rticks (@var{tickval})
## @deftypefnx {} {@dots{} =} rticks (@var{hax}, @dots{})
## Query or set the tick values on the r-axis of the current axis.
##
## When called without argument, return the current tick locations as specified
## in the @qcode{"rtick"} axes property.  These locations can be changed by
## calling @code{rticks} with a vector of tick values.  Note: ascending order
## is not required.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{rticks} to set a property value
## will result in an error.
##
## NOTE: Octave does not currently implement polaraxes objects.  It is
## therefore not possible to query or set a @qcode{"mode"} for the
## @qcode{"rtick"} property as can be done with the equivalent functions for
## @var{x}, @var{y}, and @var{z} axes.
##
## @seealso{thetaticks, xticks, yticks, zticks, polar, get, set}
## @end deftypefn

function tickval = rticks (varargin)

  hax = [];
  switch (nargin)
    case 0
      tickval = get (gca (), "rtick");  # will error if no rtick exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        tickval = get (varargin{1}, "rtick");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! isaxes (varargin{1}))
        error ("rticks: HAX must be a handle to an axes object");
      endif
      hax = varargin{1};
      arg = varargin{2};

    otherwise
      print_usage ();

  endswitch

  if (isempty (hax))
    hax = gca ();
  endif

  if (isnumeric (arg))
    if (nargout > 0)
      error ("rticks: too many output arguments requested");
    endif
    ## NOTE: Matlab errors if tick points are not in ascending order.  Octave
    ## permits out of order tick points, so no error is produced.
    set (hax, "rtick", arg);

  elseif (ischar (arg))
    error ("rticks: MODE is not yet implemented for the rtick property");

    ## FIXME: Enable mode args if/when they are available in polar/polarplot
    ## arg = tolower (arg);
    ## switch (arg)
    ##   case "mode"
    ##     tickval = get (hax, "rtickmode");
    ##
    ##   case {"auto", "manual"}
    ##     if (nargout > 0)
    ##       error (["rticks: " ...
    ##               "too many output arguments requested for arg: ", arg]);
    ##     endif
    ##     set (hax, "rtickmode", arg);
    ##
    ##   otherwise
    ##     error ("rticks: invalid option: %s", arg);
    ##
    ## endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   polar (linspace (0, pi, 20), rand (20,1));
%!   hax = gca ();
%!   ticks = rticks;
%!   assert (rticks (hax), ticks);
%!   rticks (hax, [0 0.25 0.75 1 2]);
%!   assert (rticks (hax), [0 0.25 0.75 1 2]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error rticks (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   polar (linspace (0, pi, 20), 1:20);
%!   hax = gca ();
%!   fail ("rticks (-1, [0 1])", "HAX must be a handle to an axes");
%!   fail ("tmp = rticks (hax, [0 1])", "too many output arguments");
%!   fail ("tmp = rticks (hax, 'mode')", "MODE is not yet implemented");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
