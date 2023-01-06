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
## @deftypefn  {} {@var{labels} =} zticklabels
## @deftypefnx {} {@var{mode} =} zticklabels ("mode")
## @deftypefnx {} {} zticklabels (@var{tickval})
## @deftypefnx {} {} zticklabels ("auto")
## @deftypefnx {} {} zticklabels ("manual")
## @deftypefnx {} {@dots{} =} zticklabels (@var{hax}, @dots{})
## Query or set the tick labels on the x-axis of the current axis.
##
## When called without an argument, return a cell array of strings of the
## current tick labels as specified in the @qcode{"zticklabel"} axes property.
## These labels can be changed by calling @code{zticklabels} with a cell array
## of strings.  Note: a vector of numbers will be mapped to a cell array of
## strings.  If fewer labels are specified than the current number of ticks,
## blank labels will be appended to the array.
##
## When called with argument @qcode{"mode"}, @code{zticklabels} returns the
## current value of the axes property @qcode{"zticklabelmode"}.  This property
## can be changed by calling @code{zticklabels} with either @qcode{"auto"}
## (algorithm determines tick labels) or @qcode{"manual"} (tick labels remain
## fixed).  Note: Specifying zticklabel values will also set the
## @qcode{"zticklabelmode"} and @qcode{"zticks"} properties to
## @qcode{"manual"}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{xticklabels} to set a property
## value will result in an error.
##
## @seealso{zticks, xticklabels, zticklabels, get, set}
## @end deftypefn

function labels = zticklabels (varargin)

  hax = [];
  switch (nargin)
    case 0
      labels = get (gca , "zticklabel"); # will error if no zticklabel exists.
      return;

    case 1
      if (isaxes (varargin{1}))
        labels = get (varargin{1}, "zticklabel");
        return;
      else
        arg = varargin{1};
      endif

    case 2
      if (! isaxes (varargin{1}))
        error ("zticklabels: HAX must be a handle to an axes object");
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
      error ("zticklabels: too many output arguments requested");
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
    arg((numel (arg) + 1):(numel (get (hax, "ztick")))) = {""};

    ## Setting labels sets both ticklabel and tick mode to manual.
    set (hax, "zticklabel", arg,
              "zticklabelmode", "manual",
              "ztickmode", "manual");

  elseif (ischar (arg))
    arg = tolower (arg);
    switch (arg)
      case "mode"
        labels = get (hax, "zticklabelmode");

      case {"auto", "manual"}
        if (nargout > 0)
          error (["zticklabels: " ...
                  "too many output arguments requested for arg: ", arg]);
        endif
        set (hax, "zticklabelmode", arg);

      otherwise
        error ("zticklabels: invalid option: %s", arg);

    endswitch

  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (gca, "zticklabelmode", "auto");
%!   hax = gca ();
%!   vals1 = zticklabels;
%!   assert (zticklabels (hax), vals1);
%!   mode1 = zticklabels ("mode");
%!   assert (zticklabels (hax, "mode"), mode1);
%!   zticklabels (hax, "manual");
%!   assert (zticklabels (hax, "mode"), "manual");
%!   zticklabels (hax, "auto");
%!   assert (zticks (hax, "mode"), "auto");
%!   zticklabels (hax, {"1", "2", "3", "4", "5", "6"});
%!   assert (zticklabels (hax), {"1"; "2"; "3"; "4"; "5"; "6"});
%!   assert (zticklabels (hax, "mode"), "manual");
%!   assert (zticks (hax, "mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error zticklabels (1,2,3)
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("zticklabels (-1, {})", "HAX must be a handle to an axes");
%!   fail ("tmp = zticklabels (hax, {'A','B'})", "too many output arguments");
%!   fail ("tmp = zticklabels (hax, [0, 1])", "too many output arguments");
%!   fail ("tmp = zticklabels (hax, 'auto')", "too many .* for arg: auto");
%!   fail ("tmp = zticklabels (hax, 'foo')", "invalid option: foo");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
