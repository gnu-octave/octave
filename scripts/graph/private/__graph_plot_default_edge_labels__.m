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
## @deftypefn {} {@var{labels} =} __graph_plot_default_edge_labels__ (@var{G}, @var{M})
## Return the default @code{EdgeLabel} for @code{GraphPlot} when
## @code{EdgeLabelMode} is @qcode{"auto"}.
##
## When @var{G} is a weighted @code{graph} or @code{digraph}
## (@code{G.Edges.Weight} non-empty), the default labels are the
## column cellstr obtained by @code{num2str} of each weight.
## Otherwise the default is an empty column cell array.
##
## The return value is always a column cell array of strings: either
## @code{cell (0, 1)} (unweighted or edgeless) or a length-@var{M}
## cellstr (weighted graphs).
##
## This is a private helper for @file{GraphPlot.m}.
## @end deftypefn

function labels = __graph_plot_default_edge_labels__ (G, M)

  if (nargin < 2)
    error ("__graph_plot_default_edge_labels__: G and M required");
  endif

  if (M <= 0)
    labels = cell (0, 1);
    return;
  endif

  weights = [];
  if (! isempty (G))
    try
      ET = G.Edges;
      if (isstruct (ET) && isfield (ET, "Weight") && ! isempty (ET.Weight))
        weights = ET.Weight;
      endif
    catch
      weights = [];
    end_try_catch
  endif

  if (numel (weights) != M)
    labels = cell (0, 1);
    return;
  endif

  labels = cell (M, 1);
  for ii = 1:M
    labels{ii} = num2str (weights(ii));
  endfor

endfunction


## ---------------- BIST ----------------

## Unweighted digraph: default is empty column cell.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! L = __graph_plot_default_edge_labels__ (G, 3);
%! assert (L, cell (0, 1));

## Unweighted undirected graph: empty column cell.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! L = __graph_plot_default_edge_labels__ (G, 3);
%! assert (L, cell (0, 1));

## Weighted digraph: labels come from Edges.Weight via num2str.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! L = __graph_plot_default_edge_labels__ (G, 3);
%! assert (iscolumn (L));
%! assert (numel (L), 3);
%! assert (L{1}, "10");
%! assert (L{2}, "20");
%! assert (L{3}, "30");

## Weighted graph: labels come from Edges.Weight via num2str.
%!test
%! G = graph ([1 2 3], [2 3 1], [0.5 1.5 2.5]);
%! L = __graph_plot_default_edge_labels__ (G, 3);
%! assert (iscolumn (L));
%! assert (numel (L), 3);
%! assert (L{1}, num2str (0.5));

## Edgeless graph yields empty column cell.
%!test
%! G = digraph (5);
%! L = __graph_plot_default_edge_labels__ (G, 0);
%! assert (L, cell (0, 1));

## M=0 yields empty column cell even on a weighted graph.
%!test
%! G = digraph ([1 2], [2 3], [3 4]);
%! L = __graph_plot_default_edge_labels__ (G, 0);
%! assert (L, cell (0, 1));

## Output is always a column cell array.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 5 5]);
%! L = __graph_plot_default_edge_labels__ (G, 3);
%! assert (iscolumn (L));
%! assert (iscell (L));
