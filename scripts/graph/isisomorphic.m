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
## @deftypefn {} {@var{tf} =} isisomorphic (@var{G1}, @var{G2})
## Return @code{true} if the graphs @var{G1} and @var{G2} are
## isomorphic, @code{false} otherwise.
##
## Two graphs are isomorphic when a permutation @var{P} of nodes exists
## such that @code{reordernodes (@var{G2}, @var{P})} has the same
## structure (edges and self-loops, and for multigraphs the edge
## multiplicities) as @var{G1}.  Node names and edge weights are not
## considered by this test --- only the underlying graph structure.
##
## @var{G1} and @var{G2} must both be @code{graph} objects or both be
## @code{digraph} objects; mixing the two classes is an error.  Graphs
## with different numbers of nodes, different numbers of edges, or
## different sorted degree sequences are immediately reported as
## non-isomorphic without running the full search.
##
## The test uses the VF2 algorithm of Cordella, Foggia, Sansone, and
## Vento (2004), an optimised backtracking search that prunes infeasible
## partial mappings using degree and neighbourhood consistency.  The
## algorithm runs in polynomial time for most practical graphs even
## though graph isomorphism is in general of unknown complexity.
##
## @example
## @group
## G1 = graph ([1 2 3], [2 3 1]);               # triangle
## G2 = graph ([2 3 1], [3 1 2]);               # same triangle relabelled
## isisomorphic (G1, G2)                         # @result{} true
##
## H1 = graph ([1 2 3 4], [2 3 4 1]);           # 4-cycle
## H2 = graph ([1 2 3], [2 3 4]);               # path P_4
## isisomorphic (H1, H2)                         # @result{} false
## @end group
## @end example
##
## @seealso{graph, digraph, isomorphism, reordernodes}
## @end deftypefn

function tf = isisomorphic (G1, G2)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{isisomorphic} method
  ## and this free-function body is not reached.  This file exists both
  ## as a canonical documentation target (so @code{help isisomorphic}
  ## works outside the context of an instance) and as a fallback that
  ## gives a helpful error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G1, "graph") && ! isa (G1, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "isisomorphic: G1 must be a graph or digraph object");
  endif

  if (! isa (G2, "graph") && ! isa (G2, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "isisomorphic: G2 must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but route through dot notation
  ## in case a future subclassing edge case skips the free function.
  tf = G1.isisomorphic (G2);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Digraph: isomorphic cases ----------------------

## Two identical 3-cycles -> isomorphic (identity permutation).
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3], [2 3 1]);
%! assert (isisomorphic (G1, G2), true);

## 3-cycle with nodes relabelled via permutation -> isomorphic.
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([3 1 2], [1 2 3]);
%! assert (isisomorphic (G1, G2), true);

## Directed 4-star (centre at node 1) vs same star with centre at node 4
## -> isomorphic.
%!test
%! G1 = digraph ([1 1 1], [2 3 4]);
%! G2 = digraph ([4 4 4], [1 2 3]);
%! assert (isisomorphic (G1, G2), true);

## Single self-loop at node 1 vs single self-loop at node 2 -> isomorphic.
%!test
%! G1 = digraph (1, 1);
%! G1 = addnode (G1, 1);
%! G2 = digraph (2, 2);
%! assert (isisomorphic (G1, G2), true);

## Empty digraphs -> isomorphic.
%!test
%! G1 = digraph ();
%! G2 = digraph ();
%! assert (isisomorphic (G1, G2), true);

## Edgeless N-node digraphs with same N -> isomorphic.
%!test
%! G1 = digraph (5);
%! G2 = digraph (5);
%! assert (isisomorphic (G1, G2), true);

## Single node, no edges -> isomorphic to itself.
%!test
%! G1 = digraph (1);
%! G2 = digraph (1);
%! assert (isisomorphic (G1, G2), true);

## Weights differ but structure matches -> isomorphic (weights ignored).
%!test
%! G1 = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! G2 = digraph ([1 2 3], [2 3 1], [1 1 1]);
%! assert (isisomorphic (G1, G2), true);

## Node names differ but structure matches -> isomorphic (names ignored).
%!test
%! G1 = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! G2 = digraph ([1 2 3], [2 3 1], [], {"x", "y", "z"});
%! assert (isisomorphic (G1, G2), true);

## Siever-like 9-node digraph vs itself with a deep permutation.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G1 = digraph (s, t);
%! perm = [5 2 1 8 9 3 4 7 6];       # arbitrary permutation
%! G2 = reordernodes (G1, perm);
%! assert (isisomorphic (G1, G2), true);

## ---------------- Digraph: non-isomorphic cases ------------------

## Different number of nodes -> not isomorphic.
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3 4], [2 3 4 1]);
%! assert (isisomorphic (G1, G2), false);

## Same nodes and edges but different structure -> not isomorphic.
## 3-cycle (each node has outdegree 1, indegree 1) vs in-star
## (one node gets 2 in-edges, others get 1 each) -> not iso.
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);      # 3-cycle
%! G2 = digraph ([1 2 3], [3 3 1]);      # not a cycle: duplicate 3
%! assert (isisomorphic (G1, G2), false);

## 4-cycle vs directed path P_4 -> different edge counts -> not iso.
%!test
%! G1 = digraph ([1 2 3 4], [2 3 4 1]);  # 4-cycle
%! G2 = digraph ([1 2 3], [2 3 4]);      # P_4
%! assert (isisomorphic (G1, G2), false);

## Same node count but different edge count -> not iso.
%!test
%! G1 = digraph ([1 2], [2 3]);          # 3 nodes, 2 edges
%! G2 = digraph ([1 2 3], [2 3 1]);      # 3 nodes, 3 edges
%! assert (isisomorphic (G1, G2), false);

## One has self-loop, the other doesn't, everything else matches -> not iso.
%!test
%! G1 = digraph ([1 2 3], [2 3 3]);      # has 3->3 self-loop
%! G2 = digraph ([1 2 3], [2 3 1]);      # no self-loop
%! assert (isisomorphic (G1, G2), false);

## ---------------- Graph (undirected): isomorphic -----------------

## Triangle vs same triangle with relabelled nodes -> isomorphic.
%!test
%! G1 = graph ([1 2 3], [2 3 1]);
%! G2 = graph ([2 3 1], [3 1 2]);
%! assert (isisomorphic (G1, G2), true);

## K_4 (all 6 edges) vs K_4 on permuted nodes -> isomorphic.  Use
## reordernodes to guarantee G2 is a valid simple graph with distinct
## undirected endpoint pairs.
%!test
%! G1 = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! G2 = reordernodes (G1, [3 1 4 2]);
%! assert (isisomorphic (G1, G2), true);

## Path vs same path reversed -> isomorphic.
%!test
%! G1 = graph ([1 2 3], [2 3 4]);
%! G2 = graph ([4 3 2], [3 2 1]);
%! assert (isisomorphic (G1, G2), true);

## Empty graphs -> isomorphic.
%!test
%! G1 = graph ();
%! G2 = graph ();
%! assert (isisomorphic (G1, G2), true);

## Cycle C_4 vs itself permuted.
%!test
%! G1 = graph ([1 2 3 4], [2 3 4 1]);
%! perm = [3 1 4 2];
%! G2 = reordernodes (G1, perm);
%! assert (isisomorphic (G1, G2), true);

## ---------------- Graph (undirected): non-isomorphic --------------

## K_{3,3} vs triangular prism: both are 3-regular on 6 nodes but
## not isomorphic (the prism contains triangles, K_{3,3} does not).
%!test
%! ## K_{3,3}: bipartite with parts {1,2,3} and {4,5,6}.
%! G1 = graph ([1 1 1 2 2 2 3 3 3], [4 5 6 4 5 6 4 5 6]);
%! ## Triangular prism: two triangles joined by matching.
%! G2 = graph ([1 2 3 4 5 6 1 2 3], [2 3 1 5 6 4 4 5 6]);
%! assert (isisomorphic (G1, G2), false);

## 4-cycle vs path P_4 -> same node count, different edge counts.
%!test
%! G1 = graph ([1 2 3 4], [2 3 4 1]);    # C_4 has 4 edges
%! G2 = graph ([1 2 3], [2 3 4]);        # P_4 has 3 edges
%! assert (isisomorphic (G1, G2), false);

## Two trees on 5 nodes with different degree sequences -> not iso.
## Star K_{1,4} (degrees [4,1,1,1,1]) vs path P_5 (degrees [1,2,2,2,1]).
%!test
%! G1 = graph ([1 1 1 1], [2 3 4 5]);    # K_{1,4}
%! G2 = graph ([1 2 3 4], [2 3 4 5]);    # P_5
%! assert (isisomorphic (G1, G2), false);

## Self-loop at node 1 vs no self-loop (same edges otherwise) -> not iso.
%!test
%! G1 = graph ([1 1 2], [1 2 3]);        # self-loop + P_3
%! G2 = graph ([1 2], [2 3]);            # P_3 only
%! assert (isisomorphic (G1, G2), false);

## ---------------- Cross-class and error cases --------------------

## graph vs digraph: different classes -> error.
%!error <same class>
%! G1 = graph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3], [2 3 1]);
%! isisomorphic (G1, G2);

## Non-graph arg -> error.  When G1 is a non-graph the free function
## catches it; when G2 is a non-graph the classdef-method dispatch on
## G1 catches it with the same-class check.
%!error <must be a graph or digraph>
%! isisomorphic (5, digraph ());
%!error <same class>
%! isisomorphic (digraph (), "abc");

## Wrong number of arguments -> print_usage error.
%!error isisomorphic (digraph ())
%!error isisomorphic ()
