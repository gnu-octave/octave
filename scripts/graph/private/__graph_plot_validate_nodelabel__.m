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
## @deftypefn {} {@var{labels} =} __graph_plot_validate_nodelabel__ (@var{val}, @var{N})
## Validate and normalize a @code{NodeLabel} assignment for
## @code{GraphPlot}.
##
## @var{val} may be a cell array of strings, a character matrix, or a
## numeric vector.  A numeric vector is converted element-wise via
## @code{num2str}.  The result must have exactly @var{N} entries (the
## number of nodes).  The normalized return value is a column cell
## array of strings of length @var{N}.
##
## When @var{N} is @code{0}, the only accepted values are an empty
## cell, an empty numeric vector, or an empty character array.
##
## This is a private helper for @file{GraphPlot.m}.
## @end deftypefn

function labels = __graph_plot_validate_nodelabel__ (val, N)

  if (nargin < 2)
    error ("__graph_plot_validate_nodelabel__: N required");
  endif

  ## Numeric vector -> cellstr via num2str.
  if (isnumeric (val))
    if (isempty (val))
      val = cell (0, 1);
    else
      if (! isvector (val))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeLabel numeric input must be a vector");
      endif
      vv = val(:);
      tmp = cell (numel (vv), 1);
      for ii = 1:numel (vv)
        tmp{ii} = num2str (vv(ii));
      endfor
      val = tmp;
    endif
  endif

  ## Character matrix -> cellstr rows.
  if (ischar (val))
    if (isempty (val))
      val = cell (0, 1);
    else
      val = cellstr (val);
    endif
  endif

  if (! iscell (val))
    error ("Octave:invalid-input-arg", ...
           "GraphPlot: NodeLabel must be a cell array of strings");
  endif

  ## Verify every entry is a character row (or empty string).
  val = val(:);
  for ii = 1:numel (val)
    e = val{ii};
    if (! ischar (e) || (! isempty (e) && ! isrow (e)))
      error ("Octave:invalid-input-arg", ...
             "GraphPlot: NodeLabel entries must be character vectors");
    endif
  endfor

  if (numel (val) != N)
    error ("Octave:invalid-input-arg", ...
           "GraphPlot: NodeLabel must have length %d", N);
  endif

  labels = val;

endfunction


## ---------------- BIST ----------------

## Empty input for N=0.
%!assert (__graph_plot_validate_nodelabel__ ({}, 0), cell (0, 1))
%!assert (__graph_plot_validate_nodelabel__ ([], 0), cell (0, 1))

## Row cellstr becomes column.
%!test
%! r = __graph_plot_validate_nodelabel__ ({"a", "b", "c"}, 3);
%! assert (r, {"a"; "b"; "c"});
%! assert (iscolumn (r));

## Column cellstr pass-through.
%!test
%! r = __graph_plot_validate_nodelabel__ ({"a"; "b"; "c"}, 3);
%! assert (r, {"a"; "b"; "c"});

## Numeric vector converts via num2str.
%!test
%! r = __graph_plot_validate_nodelabel__ ([10 20 30], 3);
%! assert (r, {"10"; "20"; "30"});
%!test
%! r = __graph_plot_validate_nodelabel__ ([1.5; 2.5], 2);
%! assert (r, {"1.5"; "2.5"});

## Length mismatch.
%!error <NodeLabel> __graph_plot_validate_nodelabel__ ({"a", "b"}, 3)
%!error <NodeLabel> __graph_plot_validate_nodelabel__ ({"a", "b", "c"}, 2)
%!error <NodeLabel> __graph_plot_validate_nodelabel__ ([1 2 3], 4)

## Non-cellstr content rejected.
%!error <NodeLabel> __graph_plot_validate_nodelabel__ ({1, 2, 3}, 3)
%!error <NodeLabel> __graph_plot_validate_nodelabel__ ({"a", 1}, 2)

## Non-cell wrong type rejected.
%!error <NodeLabel> __graph_plot_validate_nodelabel__ (struct ("x", 1), 1)

## Character matrix converted to cellstr.
%!test
%! r = __graph_plot_validate_nodelabel__ (["aa"; "bb"], 2);
%! assert (r, {"aa"; "bb"});

## Numeric non-vector rejected.
%!error <vector> __graph_plot_validate_nodelabel__ ([1 2; 3 4], 4)
