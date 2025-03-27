########################################################################
##
## Copyright (C) 2017-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{labels} =} thetaticklabels
## @deftypefnx {} {} thetaticklabels (@var{tickval})
## @deftypefnx {} {@dots{} =} thetaticklabels (@var{hax}, @dots{})
## Query or set the tick labels on the theta-axis of a polar plot.
##
## When called without an argument, return a cell array of strings of the
## current ttick labels.
##
## When called with the argument @var{tickval} being a vector of numbers or
## a cell array of strings and/or numbers, the labels will be changed to
## match these new values.  Values will be applied starting with the
## zero-degree tick mark and will progress counterclockwise.
##
## If fewer labels are specified than the current number of theta tick marks,
## those labels will be applied starting at the zero-degree tick mark and
## blank labels will be appended to the remainder.  If the specified label
## count exceeds the number of tick labels, the excess labels are ignored.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{thetaticklabels} to set a
## property value will result in an error.
##
## Compatability Note: The @qcode{'mode'} property for thetaticklabels has not
## yet been implemented.
##
## @seealso{polar, thetaticks, rticklabels, xticklabels, yticklabels,
## zticklabels, get, set}
## @end deftypefn

## FIXME:  Octave's polar plot implementation does not currently create the
##         properties tticklabel, tticklabelmode, and ttickmode.  Fully
##         implemented versions of those proporties could simplify much of the
##         code below, which could then mimick much the behavior of the
##         equivalent Cartesian functions.

function labels = thetaticklabels (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("thetaticklabels", varargin{:});

  if (nargs > 1)
    print_usage;
  endif

  returnlabels = true;

  ## Check first input for axes handle and remove from argument list.
  if (nargs > 0)
    ## Single remaining input must be tick labels and should be a numeric
    ## vector or a cell vector of numbers and strings.

    ## Error if trying to request and set values simultaneously.
    if (nargout > 0)
      error ("thetaticklabels: cannot set and return labels simultaneously");
    endif

    returnlabels = false;
    arg = varargin{1};

    if (isnumeric (arg))
      ## All inputs handled the same way as cells. (:) permits nonvectors.
      cellarg_num = ones (1, numel (arg));
      cellarg_char = zeros (1, numel (arg));
      arg = num2cell (arg(:));

    elseif (iscell (arg))

      if (! all ((cellarg_num = cellfun ('isnumeric', arg))
                           | (cellarg_char = cellfun ('ischar', arg))))
        error ("thetaticklabels: TICKVAL cell must contain numbers or strings");
      endif

    else
      error (["thetaticklabels: TICKVAL must be numeric or a cell ", ...
              "containing numbers and strings"]);
    endif

  ## Finish converting TICVAL into a cellstr.

  ## Convert numeric elements to characters and make it a 1-D cell array.
  arg(cellarg_num) = cellfun ('num2str', arg(cellarg_num),
                              "UniformOutput", false);
  arg = arg(:);
  endif

  if (isempty (hax))
    hax = gca ();
  endif

  ## Error if the remaining input
  polarhandle = findall (hax, "tag", "polar_grid");

  ## Error if hax does not point to a polar plot with theta elements.
  if (isempty (polarhandle))
    error ("thetaticklabels: function can only be used on a polar plot");
  elseif (! isfield (get (hax), "ttick") )
    error ("thetaticklabels: ttick property not defined for current axes");
  endif

  ## Get theta curves count.
  nt = numel (get (hax, "ttick"));

  ## rtick and ttick object ordering:
  ##  1:nt = text handles containing theta labels (reverse order)
  ##  nt+1:2*nt = line object handles for ttick radial grid lines
  ##  2*nt+1:2*nt+nr = text handles containing nr rtick labels (reverse order)
  ##  2*nt+nr+1:2*nt+2*nr = line object handles for rtick circles
  ##  end = patch object handle for darker outside border

  ## Get theta label handles, reverse order so output is in increasing.
  tlabel_handles = get (polarhandle, "children")(nt:-1:1);

  if (nargs == 0)
    ## Just return theta labels.
    labels = get (tlabel_handles, "string");

  else
    ## Set new label values.

    if (isempty (arg))
      ## If an empty cell or array is set, set all labels to "".
      arg = cell (nt, 1);
      arg(:) = {""};

    else
      ## Pad and trim arg as needed to match number of labels.
      if (nt < numel (arg))
        arg (nt + 1 : end) = [];
      else
        arg(end + 1 : nt) = {""};
      endif
    endif

    ## Replace labels with values from arg, ensure column vector.
    set (tlabel_handles, {"string"}, arg(:));
  endif

endfunction


%!test
%! t = [0:15:180] * pi / 180;
%! r = sin (t);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hp = polar (t, r);
%!   hax = gca ();
%!   thetaticks (hax, [0:60:300]);
%!   assert (thetaticklabels (hax), {"0"; "60"; "120"; "180"; "240"; "300"});
%!   thetaticklabels (hax, [0:20:100]);
%!   assert (thetaticklabels (hax), {"0"; "20"; "40"; "60"; "80"; "100"});
%!   thetaticklabels (hax, [0:-20:-100]');
%!   assert (thetaticklabels (hax), {"0"; "-20"; "-40"; "-60"; "-80"; "-100"});
%!   thetaticklabels (hax, {1, 2, 3, 4, 5 ,6});
%!   assert (thetaticklabels (hax), {"1"; "2"; "3"; "4"; "5"; "6"});
%!   thetaticklabels (hax, {"a", "bee", "c", "d", "eeee", "f"});
%!   assert (thetaticklabels (hax), {"a"; "bee"; "c"; "d"; "eeee"; "f"});
%!   thetaticklabels (hax, {1, "two", 3, "4", 5, "six"}');
%!   assert (thetaticklabels (hax), {"1"; "two"; "3"; "4"; "5"; "six"});
%!   thetaticklabels (hax, {1, 2, 3, 4, 5 ,6, 7});
%!   assert (thetaticklabels (hax), {"1"; "2"; "3"; "4"; "5"; "6"});
%!   thetaticklabels (hax, [5, 6, 7, 8, 9, 10, 11]);
%!   assert (thetaticklabels (hax), {"5"; "6"; "7"; "8"; "9"; "10"});
%!   thetaticklabels (hax, {1});
%!   assert (thetaticklabels (hax), {"1"; ""; ""; ""; ""; ""});
%!   thetaticklabels (hax, {1, 2, 3, 4, 5 ,6});
%!   assert (thetaticklabels (hax), {"1"; "2"; "3"; "4"; "5"; "6"});
%!   thetaticklabels (hax, []);
%!   assert (thetaticklabels (hax), {""; ""; ""; ""; ""; ""});
%!   thetaticklabels (hax, {1, 2, 3, 4, 5 ,6});
%!   assert (thetaticklabels (hax), {"1"; "2"; "3"; "4"; "5"; "6"});
%!   thetaticklabels (hax, {});
%!   assert (thetaticklabels (hax), {""; ""; ""; ""; ""; ""});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect


## Test input validation
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("thetaticklabels (1,2,3)", "Invalid call");
%!   fail ("thetaticklabels (-1, 2)", "Invalid call");
%!   fail ("thetaticklabels (hax, 2, 3)", "Invalid call");
%!   fail ("thetaticklabels (hf, 2)", "Invalid call");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("tmp = thetaticklabels ([1, 2, 3])", "cannot set and return labels simultaneously");
%!   fail ("tmp = thetaticklabels (hax, [1, 2, 3])", "cannot set and return labels simultaneously");
%!   fail ("thetaticklabels (hax, {{1}, 'two', 3})", "TICKVAL cell must contain numbers or strings");
%!   fail ("thetaticklabels (hax, {1, 'two', false})", "TICKVAL cell must contain numbers or strings");
%!   fail ("thetaticklabels (hax, {1, struct()})", "TICKVAL cell must contain numbers or strings");
%!   fail ("thetaticklabels (hax, struct())", "TICKVAL must be numeric or a cell containing");
%!   fail ("thetaticklabels (hax, [true false])", "TICKVAL must be numeric or a cell containing");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! t = [0:45:180] * pi / 180;
%! r = sin (t);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hp = plot (t, r);
%!   hax = gca ();
%!   fail ("thetaticklabels (hax, 1)", "function can only be used on a polar plot");
%!   fail ("thetaticklabels ({1})", "function can only be used on a polar plot");
%!   hp = polar (t, r);
%!   hax = gca ();
%!   delete (findall (hax, "tag", "polar_grid"));
%!   fail ("thetaticklabels (hax, 1)", "function can only be used on a polar plot");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
