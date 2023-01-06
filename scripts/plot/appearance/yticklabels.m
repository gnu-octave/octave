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
## @deftypefn  {} {@var{labels} =} yticklabels
## @deftypefnx {} {@var{mode} =} yticklabels ("mode")
## @deftypefnx {} {} yticklabels (@var{tickval})
## @deftypefnx {} {} yticklabels ("auto")
## @deftypefnx {} {} yticklabels ("manual")
## @deftypefnx {} {@dots{} =} yticklabels (@var{hax}, @dots{})
## Query or set the tick labels on the x-axis of the current axis.
##
## When called without an argument, return a cell array of strings of the
## current tick labels as specified in the @qcode{"yticklabel"} axes property.
## These labels can be changed by calling @code{yticklabels} with a cell array
## of strings.  Note: a vector of numbers will be mapped to a cell array of
## strings.  If fewer labels are specified than the current number of ticks,
## blank labels will be appended to the array.
##
## When called with argument @qcode{"mode"}, @code{yticklabels} returns the
## current value of the axes property @qcode{"yticklabelmode"}.  This property
## can be changed by calling @code{yticklabels} with either @qcode{"auto"}
## (algorithm determines tick labels) or @qcode{"manual"} (tick labels remain
## fixed).  Note: Specifying yticklabel values will also set the
## @qcode{"yticklabelmode"} and @qcode{"yticks"} properties to
## @qcode{"manual"}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{xticklabels} to set a property
## value will result in an error.
##
## @seealso{yticks, xticklabels, zticklabels, get, set}
## @end deftypefn

function labels = yticklabels (varargin)

  hax = [];
  switch (nargin)
    case 0
      labels = get (gca , "yticklabel"); # will error if no yticklabel exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        labels = get (varargin{1}, "yticklabel");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! isaxes (varargin{1}))
        error ("yticklabels: HAX must be a handle to an axes object");
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
      error ("yticklabels: too many output arguments requested");
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
    arg((numel (arg) + 1):(numel (get (hax, "ytick")))) = {""};

    ## Setting labels sets both ticklabel and tick mode to manual.
    set (hax, "yticklabel", arg,
              "yticklabelmode", "manual",
              "ytickmode", "manual");

  elseif (ischar (arg))
    arg = tolower (arg);
    switch (arg)
      case "mode"
        labels = get (hax, "yticklabelmode");

      case {"auto", "manual"}
        if (nargout > 0)
          error (["yticklabels: " ...
                  "too many output arguments requested for arg: ", arg]);
        endif
        set (hax, "yticklabelmode", arg);

      otherwise
        error ("yticklabels: invalid option: %s", arg);

    endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (gca, "yticklabelmode", "auto");
%!   hax = gca ();
%!   vals1 = yticklabels;
%!   assert (yticklabels (hax), vals1);
%!   mode1 = yticklabels ("mode");
%!   assert (yticklabels (hax, "mode"), mode1);
%!   yticklabels (hax, "manual");
%!   assert (yticklabels (hax, "mode"), "manual");
%!   yticklabels (hax, "auto");
%!   assert (yticks (hax, "mode"), "auto");
%!   yticklabels (hax, {"1", "2", "3", "4", "5", "6"});
%!   assert (yticklabels (hax), {"1"; "2"; "3"; "4"; "5"; "6"});
%!   assert (yticklabels (hax, "mode"), "manual");
%!   assert (yticks (hax, "mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error yticklabels (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("yticklabels (-1, {})", "HAX must be a handle to an axes");
%!   fail ("tmp = yticklabels (hax, {'A','B'})", "too many output arguments");
%!   fail ("tmp = yticklabels (hax, [0, 1])", "too many output arguments");
%!   fail ("tmp = yticklabels (hax, 'auto')", "too many .* for arg: auto");
%!   fail ("tmp = yticklabels (hax, 'foo')", "invalid option: foo");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
