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
## @deftypefn  {} {@var{tickval} =} xticks
## @deftypefnx {} {@var{mode} =} xticks ("mode")
## @deftypefnx {} {} xticks (@var{tickval})
## @deftypefnx {} {} xticks ("auto")
## @deftypefnx {} {} xticks ("manual")
## @deftypefnx {} {@dots{} =} xticks (@var{hax}, @dots{})
## Query or set the tick values on the x-axis of the current axis.
##
## When called without an argument, return the current tick locations as
## specified in the @qcode{"xtick"} axes property.  These locations can be
## changed by calling @code{xticks} with a vector of tick values.  Note:
## ascending order is not required.
##
## When called with argument @qcode{"mode"}, @code{xticks} returns the current
## value of the axes property @qcode{"xtickmode"}.  This property can be
## changed by calling @code{xticks} with either @qcode{"auto"} (algorithm
## determines tick positions) or @qcode{"manual"} (tick values remain fixed
## regardless of axes resizing or rotation).  Note: Specifying xtick values
## will also set the property @qcode{"xtickmode"} to @qcode{"manual"}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{xticks} to set a property value
## will result in an error.
##
## @seealso{xticklabels, yticks, zticks, rticks, thetaticks, get, set}
## @end deftypefn

function tickval = xticks (varargin)

  hax = [];
  switch (nargin)
    case 0
      tickval = get (gca, "xtick");  # will error if no xtick exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        tickval = get (varargin{1}, "xtick");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! isaxes (varargin{1}))
        error ("xticks: HAX must be a handle to an axes object");
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
      error ("xticks: too many output arguments requested");
    endif
    ## NOTE: Matlab errors if tick points are not in ascending order. Octave
    ## permits out of order tick points, so error is not produced.
    set (hax, "xtick", arg);

  elseif (ischar (arg))
    arg = tolower (arg);
    switch (arg)
      case "mode"
        tickval = get (hax, "xtickmode");

      case {"auto", "manual"}
        if (nargout > 0)
          error ("xticks: too many output arguments requested for arg: %s",
                                                                       arg);
        endif
        set (hax, "xtickmode", arg);

      otherwise
        error ("xticks: invalid option: %s", arg);

    endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (gca, "xtickmode", "auto");
%!   hax = gca ();
%!   vals1 = xticks;
%!   assert (xticks (hax), vals1);
%!   mode1 = xticks ("mode");
%!   assert (xticks (hax, "mode"), mode1);
%!   xticks (hax, "manual");
%!   assert (xticks (hax, "mode"), "manual");
%!   xticks (hax, "auto");
%!   assert (xticks (hax, "mode"), "auto");
%!   xticks (hax, [1 2 3 4]);
%!   assert (xticks (hax), [1 2 3 4]);
%!   assert (xticks (hax, "mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error xticks (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("xticks (-1, [0 1])", "HAX must be a handle to an axes");
%!   fail ("tmp = xticks (hax, [0 1])", "too many output arguments");
%!   fail ("tmp = xticks (hax, 'auto')", "too many .* for arg: auto");
%!   fail ("tmp = xticks (hax, 'foo')", "invalid option: foo");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
