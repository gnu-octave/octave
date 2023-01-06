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
## @deftypefn  {} {@var{tickval} =} zticks
## @deftypefnx {} {@var{mode} =} zticks ("mode")
## @deftypefnx {} {} zticks (@var{tickval})
## @deftypefnx {} {} zticks ("auto")
## @deftypefnx {} {} zticks ("manual")
## @deftypefnx {} {@dots{} =} zticks (@var{hax}, @dots{})
## Query or set the tick values on the z-axis of the current axis.
##
## When called without an argument, return the current tick locations as
## specified in the @qcode{"ztick"} axes property.  These locations can be
## changed by calling @code{zticks} with a vector of tick values.  Note:
## ascending order is not required.
##
## When called with argument @qcode{"mode"}, @code{zticks} returns the current
## value of the axes property @qcode{"ztickmode"}.  This property can be
## changed by calling @code{zticks} with either @qcode{"auto"} (algorithm
## determines tick positions) or @qcode{"manual"} (tick values remain fixed
## regardless of axes resizing or rotation).  Note: Specifying ztick values
## will also set the property @qcode{"ztickmode"} to @qcode{"manual"}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{zticks} to set a property value
## will result in an error.
##
## @seealso{zticklabels, xticks, yticks, rticks, thetaticks, get, set}
## @end deftypefn

function tickval = zticks (varargin)

  hax = [];
  switch (nargin)
    case 0
      tickval = get (gca , "ztick");  # will error if no ztick exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        tickval = get (varargin{1}, "ztick");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! isaxes (varargin{1}))
        error ("zticks: HAX must be a handle to an axes object");
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
      error ("zticks: too many output arguments requested");
    endif
    ## NOTE: Matlab errors if tick points are not in ascending order. Octave
    ## permits out of order tick points, so error is not produced.
    set (hax, "ztick", arg);

  elseif (ischar (arg))
    arg = tolower (arg);
    switch (arg)
      case "mode"
        tickval = get (hax, "ztickmode");

      case {"auto", "manual"}
        if (nargout > 0)
          error ("zticks: too many output arguments requested for arg: %s",
                                                                       arg);
        endif
        set (hax, "ztickmode", arg);

      otherwise
        error ("zticks: invalid option: %s", arg);

    endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (gca, "ztickmode", "auto");
%!   hax = gca ();
%!   vals1 = zticks;
%!   assert (zticks (hax), vals1);
%!   mode1 = zticks ("mode");
%!   assert (zticks (hax, "mode"), mode1);
%!   zticks (hax, "manual");
%!   assert (zticks (hax, "mode"), "manual");
%!   zticks (hax, "auto");
%!   assert (zticks (hax, "mode"), "auto");
%!   zticks (hax, [1 2 3 4]);
%!   assert (zticks (hax), [1 2 3 4]);
%!   assert (zticks (hax, "mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error zticks (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("zticks (-1, [0 1])", "HAX must be a handle to an axes");
%!   fail ("tmp = zticks (hax, [0 1])", "too many output arguments");
%!   fail ("tmp = zticks (hax, 'auto')", "too many .* for arg: auto");
%!   fail ("tmp = zticks (hax, 'foo')", "invalid option: foo");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
