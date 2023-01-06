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
## @deftypefn  {} {@var{labels} =} xticklabels
## @deftypefnx {} {@var{mode} =} xticklabels ("mode")
## @deftypefnx {} {} xticklabels (@var{tickval})
## @deftypefnx {} {} xticklabels ("auto")
## @deftypefnx {} {} xticklabels ("manual")
## @deftypefnx {} {@dots{} =} xticklabels (@var{hax}, @dots{})
## Query or set the tick labels on the x-axis of the current axis.
##
## When called without an argument, return a cell array of strings of the
## current tick labels as specified in the @qcode{"xticklabel"} axes property.
## These labels can be changed by calling @code{xticklabels} with a cell array
## of strings.  Note: a vector of numbers will be mapped to a cell array of
## strings.  If fewer labels are specified than the current number of ticks,
## blank labels will be appended to the array.
##
## When called with argument @qcode{"mode"}, @code{xticklabels} returns the
## current value of the axes property @qcode{"xticklabelmode"}.  This property
## can be changed by calling @code{xticklabels} with either @qcode{"auto"}
## (algorithm determines tick labels) or @qcode{"manual"} (tick labels remain
## fixed).  Note: Specifying xticklabel values will also set the
## @qcode{"xticklabelmode"} and @qcode{"xticks"} properties to
## @qcode{"manual"}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{xticklabels} to set a property
## value will result in an error.
##
## @seealso{xticks, yticklabels, zticklabels, get, set}
## @end deftypefn

function labels = xticklabels (varargin)

  hax = [];
  switch (nargin)
    case 0
      labels = get (gca , "xticklabel");  # will error if no xticklabel exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        labels = get (varargin{1}, "xticklabel");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! isaxes (varargin{1}))
        error ("xticklabels: HAX must be a handle to an axes object");
      endif
      hax = varargin{1};
      arg = varargin{2};

    otherwise
      print_usage ();

  endswitch

  if (isempty (hax))
    hax = gca ();
  endif

  if (iscell (arg) || isnumeric (arg))
    if (nargout > 0)
      error ("xticklabels: too many output arguments requested");
    endif

    if (isnumeric (arg))
      ## NOTE: Matlab accepts a cell array, but a non-vector numeric array will
      ## simply produce a black set of labels without error or warning.
      ## This implementation allows for a numeric array, which is handled in
      ## the same order as Matlab handles a cell array
      arg = num2cell (arg(:));

    endif

    ## Convert any numeric elements to characters, make it a 1-D cell array.
    arg = cellfun (@num2str, arg, "UniformOutput", false)(:);

    ## Pad with blank cell entries if needed.
    arg((numel (arg) + 1):(numel (get (hax, "xtick")))) = {""};

    ## Setting labels sets both ticklabel and tick mode to manual.
    set (hax, "xticklabel", arg,
              "xticklabelmode", "manual",
              "xtickmode", "manual");

  elseif (ischar (arg))
    arg = tolower (arg);
    switch (arg)
      case "mode"
        labels = get (hax, "xticklabelmode");

      case {"auto", "manual"}
        if (nargout > 0)
          error (["xticklabels: " ...
                  "too many output arguments requested for arg: ", arg]);
        endif
        set (hax, "xticklabelmode", arg);

      otherwise
        error ("xticklabels: invalid option: %s", arg);

    endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (gca, "xticklabelmode", "auto");
%!   hax = gca ();
%!   vals1 = xticklabels;
%!   assert (xticklabels (hax), vals1);
%!   mode1 = xticklabels ("mode");
%!   assert (xticklabels (hax, "mode"), mode1);
%!   xticklabels (hax, "manual");
%!   assert (xticklabels (hax, "mode"), "manual");
%!   xticklabels (hax, "auto");
%!   assert (xticks (hax, "mode"), "auto");
%!   xticklabels (hax, {"1", "2", "3", "4", "5", "6"});
%!   assert (xticklabels (hax), {"1"; "2"; "3"; "4"; "5"; "6"});
%!   assert (xticklabels (hax, "mode"), "manual");
%!   assert (xticks (hax, "mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error xticklabels (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("xticklabels (-1, {})", "HAX must be a handle to an axes");
%!   fail ("tmp = xticklabels (hax, {'A','B'})", "too many output arguments");
%!   fail ("tmp = xticklabels (hax, [0, 1])", "too many output arguments");
%!   fail ("tmp = xticklabels (hax, 'auto')", "too many .* for arg: auto");
%!   fail ("tmp = xticklabels (hax, 'foo')", "invalid option: foo");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
