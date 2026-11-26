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
## @deftypefn {} {@var{tf} =} ismultigraph (@var{G})
## Return @code{true} if the graph or digraph @var{G} contains two or more
## parallel edges between the same endpoint pair, @code{false} otherwise.
##
## @var{G} must be either a @code{graph} or @code{digraph} object.  Two
## edges are @emph{parallel} when:
##
## @itemize @bullet
## @item
## For a @code{digraph}: they share the same ordered pair
## @code{(source, destination)}.
## @item
## For a @code{graph}: they share the same unordered pair of endpoints.
## @end itemize
##
## An empty graph, an edgeless N-node graph, or any graph that was built
## from inputs that contain no duplicate endpoint pairs returns
## @code{false}, even when the graph was explicitly constructed with the
## @qcode{'multigraph'} flag.  This matches MATLAB's convention: the flag
## only @emph{permits} parallel edges, and @code{ismultigraph} reports
## whether they are actually present.
##
## The undirected @code{graph} class in this Octave build does not
## currently accept a @qcode{'multigraph'} constructor flag, so
## @code{ismultigraph} always returns @code{false} for a @code{graph}
## object.
##
## @example
## @group
## G = digraph ([1 1 2], [2 2 3], "multigraph");
## ismultigraph (G)                # @result{} true
##
## H = digraph ([1 2 3], [2 3 1]);
## ismultigraph (H)                # @result{} false
##
## K = graph ([1 2 3], [2 3 1]);
## ismultigraph (K)                # @result{} false
## @end group
## @end example
##
## @seealso{graph, digraph, numedges, numnodes}
## @end deftypefn

function tf = ismultigraph (G)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{ismultigraph} method
  ## and this free-function body is not reached.  This file exists both
  ## as a canonical documentation target (so @code{help ismultigraph}
  ## works outside the context of an instance) and as a fallback that
  ## gives a helpful error for non-graph inputs.

  if (nargin != 1)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "ismultigraph: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but route through dot notation
  ## in case a future subclassing edge case skips the free function.
  tf = G.ismultigraph ();

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Digraph: true cases -----------------------------

## digraph with two parallel edges -> true.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (ismultigraph (G), true);

## digraph with three parallel edges between the same pair -> true.
%!test
%! G = digraph ([1 1 1], [2 2 2], [1 2 3], "multigraph");
%! assert (ismultigraph (G), true);

## digraph multigraph with parallel edges and weights -> true.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! assert (ismultigraph (G), true);

## digraph multigraph with parallel edges and node names -> true.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], ...
%!              {"a", "b", "c"}, "multigraph");
%! assert (ismultigraph (G), true);

## digraph multigraph via EdgeTable with duplicate EndNodes -> true.
%!test
%! ET.EndNodes = [1 2; 1 2; 2 3];
%! ET.Weight = [10; 20; 30];
%! G = digraph (ET, "multigraph");
%! assert (ismultigraph (G), true);

## digraph multigraph with parallel self-loops -> true.
%!test
%! G = digraph ([1 1 2], [1 1 2], [1 2 3], "multigraph");
%! assert (ismultigraph (G), true);

## digraph multigraph with anti-parallel (1->2 and 2->1) pairs -> false
## since (1,2) and (2,1) are different ordered pairs.
%!test
%! G = digraph ([1 2], [2 1], "multigraph");
%! assert (ismultigraph (G), false);

## ---------------- Digraph: false cases ----------------------------

## plain digraph (no multigraph flag) -> false.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (ismultigraph (G), false);

## empty digraph -> false.
%!test
%! G = digraph ();
%! assert (ismultigraph (G), false);

## edgeless N-node digraph -> false.
%!test
%! G = digraph (5);
%! assert (ismultigraph (G), false);

## digraph with 'multigraph' flag but no duplicates -> false
## (MATLAB parity: flag only permits, it does not imply).
%!test
%! G = digraph ([1 2 3], [2 3 1], "multigraph");
%! assert (ismultigraph (G), false);

## digraph from adjacency matrix even with 'multigraph' flag -> false
## (adjacency cannot express parallel edges).
%!test
%! A = [0 1 0; 0 0 1; 1 0 0];
%! G = digraph (A, "multigraph");
%! assert (ismultigraph (G), false);

## digraph from EdgeTable with unique rows even under 'multigraph' -> false.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! G = digraph (ET, "multigraph");
%! assert (ismultigraph (G), false);

## digraph with self-loops (single) and regular edges -> false.
%!test
%! G = digraph ([1 2 3], [1 3 1]);
%! assert (ismultigraph (G), false);

## digraph(N, 'multigraph') (edgeless) -> false.
%!test
%! G = digraph (5, "multigraph");
%! assert (ismultigraph (G), false);

## digraph('multigraph') alone (empty) -> false.
%!test
%! G = digraph ("multigraph");
%! assert (ismultigraph (G), false);

## digraph with 'omitselfloops' only -> false.
%!test
%! G = digraph ([1 1 2], [1 2 3], "omitselfloops");
%! assert (ismultigraph (G), false);

## ---------------- Graph: always false -----------------------------

## plain graph -> false.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (ismultigraph (G), false);

## empty graph -> false.
%!test
%! G = graph ();
%! assert (ismultigraph (G), false);

## edgeless N-node graph -> false.
%!test
%! G = graph (5);
%! assert (ismultigraph (G), false);

## weighted graph -> false.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (ismultigraph (G), false);

## graph with named nodes -> false.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (ismultigraph (G), false);

## graph from symmetric adjacency -> false.
%!test
%! A = [0 1 1; 1 0 1; 1 1 0];
%! G = graph (A);
%! assert (ismultigraph (G), false);

## graph with a self-loop -> false (a self-loop is not a parallel edge).
%!test
%! G = graph ([1 2 3], [1 3 1]);
%! assert (ismultigraph (G), false);

## graph from EdgeTable -> false.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! G = graph (ET);
%! assert (ismultigraph (G), false);

## graph built from EdgeTable with a self-loop -> false.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 3];
%! G = graph (ET);
%! assert (ismultigraph (G), false);

## ---------------- Return type and shape ---------------------------

## Result is scalar logical (class 'logical') for both branches.
%!test
%! G = digraph ([1 1], [2 2], "multigraph");
%! tf = ismultigraph (G);
%! assert (class (tf), "logical");
%! assert (isscalar (tf));

%!test
%! G = graph ();
%! tf = ismultigraph (G);
%! assert (class (tf), "logical");
%! assert (isscalar (tf));

%!test
%! G = digraph ();
%! tf = ismultigraph (G);
%! assert (class (tf), "logical");
%! assert (isscalar (tf));

## ---------------- Dot-notation method dispatch --------------------

## G.ismultigraph() via explicit method-call syntax on digraph.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (G.ismultigraph (), true);

%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (G.ismultigraph (), false);

## G.ismultigraph() via explicit method-call syntax on graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (G.ismultigraph (), false);

%!test
%! G = graph ();
%! assert (G.ismultigraph (), false);

## ---------------- Larger / realistic fixtures ---------------------

## Siever-style 9-node fixture (no duplicates) -> false.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! assert (ismultigraph (G), false);

## Siever-style 9-node fixture under 'multigraph' (still no duplicates)
## -> false.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t, "multigraph");
%! assert (ismultigraph (G), false);

## Siever-style 9-node fixture as undirected graph -> false.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! pairs = unique (sort ([s.', t.'], 2), "rows");
%! G = graph (pairs(:, 1), pairs(:, 2));
%! assert (ismultigraph (G), false);

## ---------------- General input errors ----------------------------

## Non-graph scalar first arg.
%!error <G must be a graph or digraph> ismultigraph (3)

## Non-graph numeric vector.
%!error <G must be a graph or digraph> ismultigraph ([1 2 3])

## Non-graph string.
%!error <G must be a graph or digraph> ismultigraph ("hello")

## Non-graph cell.
%!error <G must be a graph or digraph> ismultigraph ({1, 2})

## Non-graph struct.
%!error <G must be a graph or digraph> ismultigraph (struct ("a", 1))

## Non-graph sparse matrix.
%!error <G must be a graph or digraph> ismultigraph (sparse (3, 3))

## Non-graph empty double.
%!error <G must be a graph or digraph> ismultigraph ([])

## nargin == 0 -> print_usage.
%!error <Invalid call> ismultigraph ()
