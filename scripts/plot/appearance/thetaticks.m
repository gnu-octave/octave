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
## @deftypefn  {} {@var{tickval} =} thetaticks
## @deftypefnx {} {} thetaticks (@var{tickval})
## @deftypefnx {} {@dots{} =} thetaticks (@var{hax}, @dots{})
## Query or set the tick values on the theta-axis of the current axis.
##
## When called without argument, return the current tick locations as specified
## in the @qcode{"ttick"} axes property.  These locations can be changed by
## calling @code{thetaticks} with a vector of tick values.  Note: ascending
## order is not required.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{thetaticks} to set a property
## value will result in an error.
##
## NOTE: Octave does not currently implement polaraxes objects.  It is
## therefore not possible to query or set a @qcode{"mode"} for the
## @qcode{"thetatick"} property as can be done with the equivalent functions
## for @var{x}, @var{y}, and @var{z} axes.
##
## @seealso{rticks, xticks, yticks, zticks, polar, get, set}
## @end deftypefn

function tickval = thetaticks (varargin)

  ## FIXME: Update function to work with polaraxes objects once that function
  ##        is implemented in Octave.  For compatibility with Matlab this may
  ##        need to function with both the property names ttick and thetatick.

  hax = [];
  switch (nargin)
    case 0
      tickval = get (gca , "ttick");  # will error if no ttick exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        tickval = get (varargin{1}, "ttick");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! ishghandle (varargin{1}))
        error ("thetaticks: HAX must be a handle to an axes object");
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
      error ("thetaticks: too many output arguments requested");
    endif
    ## NOTE: Matlab errors if tick points are not in ascending order.  Octave
    ## permits out of order tick points, so error is not produced.
    set (hax, "ttick", arg);

  elseif (ischar (arg))
    error ("thetaticks: MODE is not yet implemented for the ttick property");

    ## FIXME: Enable mode args if/when they are available in polar/polarplot
    ##    arg = tolower (arg);
    ## switch (arg)
    ##   case "mode"
    ##     tickval = get (hax, "ttickmode");
    ##
    ##   case {"auto", "manual"}
    ##     if (nargout > 0)
    ##       error (["thetaticks: " ...
    ##               "too many output arguments requested for arg: ", arg]);
    ##     endif
    ##     set (hax, "ttickmode", arg);
    ##
    ##   otherwise
    ##     error ("thetaticks: invalid option: %s", arg);
    ##
    ## endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%! polar (linspace (0, pi, 20), rand (20,1));
%!   hax = gca ();
%!   ticks = thetaticks;
%!   assert (thetaticks (hax), ticks);
%!   thetaticks (hax, [0 45 90 135 180]);
%!   assert (thetaticks (hax), [0 45 90 135 180]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error thetaticks (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   polar (linspace (0, pi, 20), 1:20);
%!   hax = gca ();
%!   fail ("thetaticks (-1, [0 1])", "HAX must be a handle to an axes");
%!   fail ("tmp = thetaticks (hax, [0 1])", "too many output arguments");
%!   fail ("tmp = thetaticks (hax, 'mode')", "MODE is not yet implemented");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
