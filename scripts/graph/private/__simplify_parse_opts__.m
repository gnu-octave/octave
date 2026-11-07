########################################################################
##
## Copyright (C) 2026 The Octave Project Developers
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
## @deftypefn {} {[@var{method}, @var{omit_loops}] =} __simplify_parse_opts__ (@var{nv_cell})
## Private helper that parses the trailing arguments of @code{simplify}.
##
## @var{nv_cell} is the @code{varargin} cell array from the public
## @code{simplify} call.  The first cell, if present, may be the
## positional aggregation method -- one of @qcode{"sum"}, @qcode{"mean"},
## @qcode{"min"}, or @qcode{"max"} (case-insensitive).  Remaining cells
## are either the bare flag @qcode{"omitselfloops"} or Name-Value pairs
## @qcode{"SelfLoops"} (@qcode{"keep"} or @qcode{"discard"}) or
## @qcode{"AggregationVariables"} (same value domain as @var{method};
## last specification wins).
##
## Returns:
## @itemize
## @item @var{method}: lower-case char row, defaulting to @qcode{"sum"}.
## @item @var{omit_loops}: logical scalar, @code{true} when self-loops
## should be dropped, defaulting to @code{false}.
## @end itemize
##
## @seealso{simplify}
## @end deftypefn

function [method, omit_loops] = __simplify_parse_opts__ (nv_cell)

  if (nargin != 1)
    print_usage ();
  endif

  method = "sum";
  omit_loops = false;

  if (! iscell (nv_cell))
    error ("Octave:invalid-input-arg", ...
           "__simplify_parse_opts__: NV_CELL must be a cell array");
  endif

  n = numel (nv_cell);
  if (n == 0)
    return;
  endif

  i = 1;

  ## First positional argument may be the method -- but only if it is a
  ## string whose lower-case matches one of the known aggregation method
  ## names.  If it does not match, we still treat it as a Name token and
  ## let the Name-Value loop validate / reject it.
  if (ischar (nv_cell{i}) && isrow (nv_cell{i}) ...
      && any (strcmpi (nv_cell{i}, {"sum", "mean", "min", "max"})))
    method = tolower (nv_cell{i});
    i++;
  endif

  while (i <= n)
    a = nv_cell{i};
    if (! (ischar (a) && isrow (a)))
      error ("Octave:invalid-input-arg", ...
             "simplify: Name-Value option name must be a string");
    endif
    if (strcmpi (a, "omitselfloops"))
      omit_loops = true;
      i++;
      continue;
    endif
    if (i + 1 > n)
      error ("Octave:invalid-input-arg", ...
             "simplify: Name-Value pairs expected pairs; '%s' is missing its value", ...
             a);
    endif
    v = nv_cell{i + 1};
    if (strcmpi (a, "SelfLoops"))
      if (! (ischar (v) && isrow (v)))
        error ("Octave:invalid-input-arg", ...
               "simplify: SelfLoops value must be a string");
      endif
      if (strcmpi (v, "keep"))
        omit_loops = false;
      elseif (strcmpi (v, "discard"))
        omit_loops = true;
      else
        error ("Octave:invalid-input-arg", ...
               "simplify: SelfLoops value must be 'keep' or 'discard'");
      endif
    elseif (strcmpi (a, "AggregationVariables"))
      if (! (ischar (v) && isrow (v)))
        error ("Octave:invalid-input-arg", ...
               "simplify: AggregationVariables value must be a string");
      endif
      if (! any (strcmpi (v, {"sum", "mean", "min", "max"})))
        error ("Octave:invalid-input-arg", ...
               "simplify: AggregationVariables value must be 'sum', 'mean', 'min', or 'max'");
      endif
      method = tolower (v);
    else
      error ("Octave:invalid-input-arg", ...
             "simplify: unknown option '%s'; valid names are 'SelfLoops', 'AggregationVariables', 'omitselfloops'", ...
             a);
    endif
    i += 2;
  endwhile

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Default: method = "sum", omit_loops = false.
%!test
%! [m, o] = __simplify_parse_opts__ ({});
%! assert (m, "sum");
%! assert (o, false);

## Positional method: sum.
%!test
%! [m, o] = __simplify_parse_opts__ ({"sum"});
%! assert (m, "sum");
%! assert (o, false);

## Positional method: mean.
%!test
%! [m, o] = __simplify_parse_opts__ ({"mean"});
%! assert (m, "mean");

## Positional method: min.
%!test
%! [m, o] = __simplify_parse_opts__ ({"min"});
%! assert (m, "min");

## Positional method: max.
%!test
%! [m, o] = __simplify_parse_opts__ ({"max"});
%! assert (m, "max");

## Case-insensitive positional method.
%!test
%! [m, o] = __simplify_parse_opts__ ({"SUM"});
%! assert (m, "sum");

%!test
%! [m, o] = __simplify_parse_opts__ ({"Mean"});
%! assert (m, "mean");

## Bare omitselfloops flag.
%!test
%! [m, o] = __simplify_parse_opts__ ({"omitselfloops"});
%! assert (m, "sum");
%! assert (o, true);

## Method + omitselfloops.
%!test
%! [m, o] = __simplify_parse_opts__ ({"max", "omitselfloops"});
%! assert (m, "max");
%! assert (o, true);

## SelfLoops=keep.
%!test
%! [m, o] = __simplify_parse_opts__ ({"SelfLoops", "keep"});
%! assert (o, false);

## SelfLoops=discard.
%!test
%! [m, o] = __simplify_parse_opts__ ({"SelfLoops", "discard"});
%! assert (o, true);

## SelfLoops is case-insensitive.
%!test
%! [m, o] = __simplify_parse_opts__ ({"selfloops", "DISCARD"});
%! assert (o, true);

## AggregationVariables sets the method.
%!test
%! [m, o] = __simplify_parse_opts__ ({"AggregationVariables", "mean"});
%! assert (m, "mean");

## AggregationVariables overrides the positional method (last wins).
%!test
%! [m, o] = __simplify_parse_opts__ ({"sum", "AggregationVariables", "max"});
%! assert (m, "max");

## Positional method followed by SelfLoops Name-Value.
%!test
%! [m, o] = __simplify_parse_opts__ ({"mean", "SelfLoops", "discard"});
%! assert (m, "mean");
%! assert (o, true);

## Unknown option name.
%!error <unknown option>
%! __simplify_parse_opts__ ({"Bogus", "stable"});

## Non-char Name.
%!error <name.*string>
%! __simplify_parse_opts__ ({7, "stable"});

## Invalid SelfLoops value.
%!error <SelfLoops>
%! __simplify_parse_opts__ ({"SelfLoops", "bogus"});

## Non-char SelfLoops value.
%!error <SelfLoops>
%! __simplify_parse_opts__ ({"SelfLoops", 7});

## Invalid AggregationVariables value.
%!error <AggregationVariables>
%! __simplify_parse_opts__ ({"AggregationVariables", "bogus"});

## Non-char AggregationVariables value.
%!error <AggregationVariables>
%! __simplify_parse_opts__ ({"AggregationVariables", 7});

## Missing value after SelfLoops.
%!error <missing|pair>
%! __simplify_parse_opts__ ({"SelfLoops"});

## Missing value after AggregationVariables.
%!error <missing|pair>
%! __simplify_parse_opts__ ({"AggregationVariables"});
