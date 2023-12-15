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
## @deftypefn  {} {@var{labels} =} rticklabels
## @deftypefnx {} {} rticklabels (@var{tickval})
## @deftypefnx {} {@dots{} =} rticklabels (@var{hax}, @dots{})
## Query or set the tick labels on the r-axis of a polar plot.
##
## When called without an argument, return a cell array of strings of the
## current rtick labels.
##
## When called with the argument @var{tickval} being a vector of numbers or
## a cell array of strings and/or numbers, the labels will be changed to
## match these new values. Note that the center point of the plots made by
## @code{polar} are never labeled, so the first specified label will be
## applied to the second rtick location and subesquent labels will progress
## outward.
##
## If fewer labels are specified than the current number of tick marks, those
## labels will be applied starting with the innermost tick labels, and blank
## labels will be appended to the remainder.  If the specified label count
## exceeds the number of tick labels, the excess labels are ignored.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## Requesting a return value when calling @code{rticklabels} to set a property
## value will result in an error.
##
## Compatability Note:  The 'mode' property for rticklabels has not yet been
##                      implemented.
##
## @seealso{polar, rticks, tticklabels, xticklabels, yticklabels, zticklabels,
## get, set}
## @end deftypefn

## FIXME:  Octave's polar plot implementation does not currently create the
##         properties rticklabel, rticklabelmode, and rtickmode.  Fully
##         implemented versions of those proporties could simplify much of the
##         code below, which could then mimick much the behavior of the
##         equivalent Cartesian functions.

function labels = rticklabels (varargin)
  [hax, varargin, nargs] = __plt_get_axis_arg__ ("rticklabels", varargin{:});

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
      error ("rticklabels: cannot set and return labels simultaneously");
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
        error ("rticklabels: TICKVAL cell must contain numbers or strings");
      endif

    else
      error (["rticklabels: TICKVAL must be numeric or a cell ", ...
              "containing numbers and strings"]);
    endif

  ## Finish converting TICVAL into a cellstr.

  ## Convert numeric elements to characters and make it a 1-D cell array.
  arg(cellarg_num) = cellfun (@num2str, arg(cellarg_num), ...
                                "UniformOutput", false);
  arg = arg(:);
  endif

  if (isempty (hax))
    hax = gca ();
  endif

  ## Error if the remaining input
  polarhandle = findall (hax, "tag", "polar_grid");

  ## Error if hax does not point to a polar plot with r elements.
  if (isempty (polarhandle))
    error ("rticklabels: rticklabels can only be used on a polar plot");
  elseif (! isfield (get (hax), "rtick") )
    error ("rticklabels: rtick property not defined for current axes");
  endif

  ## Get count of radius and theta curves.
  nr = numel (get (hax, "rtick")); # Must check if it includes unlabeled center.
  nt = numel (get (hax, "ttick"));
  rt_label_handles = get (polarhandle, "children");

  ## Center is never labeled in polar plots.
  ## If rtick does not include unlabled center, label_count = 2*nr+2*nt-1,
  ## otherwise label_count = 2*nr+2*nt+1 and nr should be reduced by 1
  ## to prevent attempts to get label value from nonlabel handle.
  if (2 * (nr + nt) + 1 > numel (rt_label_handles))
    nr--;
  endif

  ## rtick and ttick object ordering:
  ##  1:nt = text handles containing theta labels (reverse order)
  ##  nt+1:2*nt = line object handles for ttick radial grid lines
  ##  2*nt+1:2*nt+nr = text handles containing nr rtick labels (reverse order)
  ##  2*nt+nr+1:2*nt+2*nr = line object handles for rtick circles
  ##  end = patch object handle for darker outside border

  rlabelrange = 2*nt + [nr:-1:1]; # Flip back to increasing order.

  if (nargs == 0)
    ## Get radius labels.
    labels = get (rt_label_handles(rlabelrange), "string");

  else
    ## Set new label values.

    if isempty (arg)
      ## If an empty cell or array is set, set all labels to "".
      arg = cell (nr, 1);
      arg(:) = {""};

    else
      ## Pad and trim arg as needed to match number of labels.
      if (nr < numel (arg))
        arg (nr + 1 : end) = [];
      else
        arg(end + 1 : nr) = {""};
      endif
    endif

    ## Replace labels with values from arg, ensure column vector.
    set (rt_label_handles(rlabelrange), {"string"}, arg(:));
  endif

endfunction

%!test
%! t = [0:15:180] * pi / 180;
%! r = sin (t);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hp = polar (t, r);
%!   hax = gca ();
%!   rticks (hax, [0, 0.5, 1]);
%!   assert (rticklabels (hax), {"0.5"; "1"});
%!   rticklabels (hax, [1 2]);
%!   assert (rticklabels (hax), {"1"; "2"});
%!   rticklabels (hax, [3 4]');
%!   assert (rticklabels (hax), {"3"; "4"});
%!   rticklabels (hax, {5, 6});
%!   assert (rticklabels (hax), {"5"; "6"});
%!   rticklabels (hax, {"a", "bee"});
%!   assert (rticklabels (hax), {"a"; "bee"});
%!   rticklabels (hax, {1, "two"});
%!   assert (rticklabels (hax), {"1"; "two"});
%!   rticklabels (hax, {1, 2, 3, 4});
%!   assert (rticklabels (hax), {"1"; "2"});
%!   rticklabels (hax, [5, 6, 7, 8]);
%!   assert (rticklabels (hax), {"5"; "6"});
%!   rticklabels (hax, {1});
%!   assert (rticklabels (hax), {"1"; ""});
%!   rticklabels (hax, {1,2});
%!   assert (rticklabels (hax), {"1"; "2"});
%!   rticklabels (hax, []);
%!   assert (rticklabels (hax), {""; ""});
%!   rticklabels (hax, {1,2});
%!   assert (rticklabels (hax), {"1"; "2"});
%!   rticklabels (hax, {});
%!   assert (rticklabels (hax), {""; ""});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect


## Test input validation
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("rticklabels (1,2,3)", "Invalid call");
%!   fail ("rticklabels (-1, 2)", "Invalid call");
%!   fail ("rticklabels (hax, 2, 3)", "Invalid call");
%!   fail ("rticklabels (hf, 2)", "Invalid call");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   fail ("tmp = rticklabels ([1, 2, 3])", "cannot set and return labels simultaneously");
%!   fail ("tmp = rticklabels (hax, [1, 2, 3])", "cannot set and return labels simultaneously");
%!   fail ("rticklabels (hax, {{1}, 'two', 3})", "TICKVAL cell must contain numbers or strings");
%!   fail ("rticklabels (hax, {1, 'two', false})", "TICKVAL cell must contain numbers or strings");
%!   fail ("rticklabels (hax, {1, struct()})", "TICKVAL cell must contain numbers or strings");
%!   fail ("rticklabels (hax, struct())", "TICKVAL must be numeric or a cell containing");
%!   fail ("rticklabels (hax, [true false])", "TICKVAL must be numeric or a cell containing");
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
%!   fail ("rticklabels (hax, 1)", "rticklabels can only be used on a polar plot");
%!   fail ("rticklabels ({1})", "rticklabels can only be used on a polar plot");
%!   hp = polar (t, r);
%!   hax = gca ();
%!   delete (findall (hax, "tag", "polar_grid"));
%!   fail ("rticklabels (hax, 1)", "rticklabels can only be used on a polar plot");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
