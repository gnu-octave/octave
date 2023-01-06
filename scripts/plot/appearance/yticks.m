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
## @deftypefn  {} {@var{tickval} =} yticks
## @deftypefnx {} {@var{mode} =} yticks ("mode")
## @deftypefnx {} {} yticks (@var{tickval})
## @deftypefnx {} {} yticks ("auto")
## @deftypefnx {} {} yticks ("manual")
## @deftypefnx {} {@dots{} =} yticks (@var{hax}, @dots{})
## Query or set the tick values on the y-axis of the current axis.
##
## When called without an argument, return the current tick locations as
## specified in the @qcode{"ytick"} axes property.  These locations can be
## changed by calling @code{yticks} with a vector of tick values.  Note:
## ascending order is not required.
##
## When called with argument @qcode{"mode"}, @code{yticks} returns the current
## value of the axes property @qcode{"ytickmode"}.  This property can be
## changed by calling @code{yticks} with either @qcode{"auto"} (algorithm
## determines tick positions) or @qcode{"manual"} (tick values remain fixed
## regardless of axes resizing or rotation).  Note: Specifying ytick values
## will also set the property @qcode{"ytickmode"} to @qcode{"manual"}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{yticks} to set a property value
## will result in an error.
##
## @seealso{yticklabels, xticks, zticks, rticks, thetaticks, get, set}
## @end deftypefn

function tickval = yticks (varargin)

  hax = [];
  switch (nargin)
    case 0
      tickval = get (gca , "ytick");  # will error if no ytick exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        tickval = get (varargin{1}, "ytick");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! isaxes (varargin{1}))
        error ("yticks: HAX must be a handle to an axes object");
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
      error ("yticks: too many output arguments requested");
    else
     ## NOTE: Matlab errors if tick points are not in ascending order. Octave
     ## permits out of order tick points, so error is not produced.
     set (hax, "ytick", arg);

    endif

  elseif (ischar (arg))
    arg = tolower (arg);
    switch (arg)
      case "mode"
        tickval = get (hax, "ytickmode");

      case {"auto", "manual"}
        if (nargout > 0)
          error ("yticks: too many output arguments requested for arg: %s",
                                                                       arg);
        endif
        set (hax, "ytickmode", arg);

      otherwise
        error ("yticks: invalid option: %s", arg);

    endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (gca, "ytickmode", "auto");
%!   hax = gca ();
%!   vals1 = yticks;
%!   assert (yticks (hax), vals1);
%!   mode1 = yticks ("mode");
%!   assert (yticks (hax, "mode"), mode1);
%!   yticks (hax, "manual");
%!   assert (yticks (hax, "mode"), "manual");
%!   yticks (hax, "auto");
%!   assert (yticks (hax, "mode"), "auto");
%!   yticks (hax, [1 2 3 4]);
%!   assert (yticks (hax), [1 2 3 4]);
%!   assert (yticks (hax, "mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error yticks (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("yticks (-1, [0 1])", "HAX must be a handle to an axes");
%!   fail ("tmp = yticks (hax, [0 1])", "too many output arguments");
%!   fail ("tmp = yticks (hax, 'auto')", "too many .* for arg: auto");
%!   fail ("tmp = yticks (hax, 'foo')", "invalid option: foo");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
