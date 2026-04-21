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
## @deftypefn  {} {@var{D} =} distances (@var{G})
## @deftypefnx {} {@var{d} =} distances (@var{G}, @var{src})
## @deftypefnx {} {@var{d} =} distances (@var{G}, @var{src}, @var{tgt})
## @deftypefnx {} {@var{D} =} distances (@dots{}, @qcode{"Method"}, @var{method})
## Return shortest-path distances on the graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.
##
## With no additional arguments, @code{distances (@var{G})} returns the
## all-pairs @code{numnodes (@var{G})}-by-@code{numnodes (@var{G})}
## dense double matrix @var{D}.  @var{D}(i, j) is the length of a
## shortest path from node @math{i} to node @math{j} under the stored
## edge weights (every edge has weight @code{1} when @var{G} is
## unweighted), or @code{Inf} when @math{j} is not reachable from
## @math{i}.  The diagonal @var{D}(i, i) is always @code{0}.
##
## When a scalar @var{src} is given, @code{distances (@var{G}, @var{src})}
## returns a row vector of length @code{numnodes (@var{G})} giving the
## shortest-path distance from @var{src} to every node.  When @var{src}
## is a vector (numeric indices or a cell array of node names), the
## result is a @code{numel (@var{src})}-by-@code{numnodes (@var{G})}
## matrix with one row per source.
##
## When both @var{src} and @var{tgt} are given,
## @code{distances (@var{G}, @var{src}, @var{tgt})} returns a
## @code{numel (@var{src})}-by-@code{numel (@var{tgt})} submatrix
## (scalar when both arguments are scalar).  @var{src} and @var{tgt}
## may mix numeric indices and node names (character row vectors or
## cell arrays of strings) when @var{G} has node names.
##
## For the undirected @code{graph} class, edges may be traversed in
## either direction, so @var{D} is symmetric.  For the directed
## @code{digraph} class, paths must follow edge direction so
## @var{D}(i, j) is in general not the same as @var{D}(j, i).
##
## The optional @qcode{"Method"} Name-Value pair selects the algorithm
## used for the computation.  Supported values (case-insensitive) are:
##
## @table @asis
## @item @qcode{"auto"} (default)
## Pick automatically: BFS when @var{G} is unweighted, Dijkstra when
## all weights are non-negative, and Bellman-Ford when any weight is
## negative.
## @item @qcode{"unweighted"}
## Treat every edge as having unit weight and run breadth-first
## search.  Stored weights are ignored.
## @item @qcode{"positive"}
## Run Dijkstra's algorithm.  Every edge weight must be non-negative;
## a negative weight raises an error.
## @item @qcode{"mixed"}
## Run Bellman-Ford.  Negative edge weights are allowed provided no
## negative cycle is reachable from the requested sources; a negative
## cycle raises an error.  For an undirected graph, any negative
## weight is a negative cycle (u-v-u) and is always rejected.
## @item @qcode{"acyclic"}
## Run an @math{O (N + E)} topological-order relaxation; requires
## @var{G} to be a directed acyclic graph.  This method is only
## supported for the @code{digraph} class.
## @end table
##
## Self-loops do not influence shortest paths: @var{D}(i, i) is
## always @code{0}, regardless of any self-loop weight on node
## @math{i}.  For a @code{digraph} with parallel edges (multigraph),
## each parallel edge is considered independently; the shortest path
## uses the edge with the smallest weight connecting each pair of
## endpoints.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1], [5 10 15]);
## distances (G)
##          @result{}  0   5  15
##             25   0  10
##             15  20   0
## distances (G, 1)
##          @result{}  0   5  15
## distances (G, 1, 3)
##          @result{}  15
##
## H = graph ([1 2], [2 3]);
## distances (H)
##          @result{}  0  1  2
##             1  0  1
##             2  1  0
##
## ## Weighted digraph with a negative edge: 'mixed' handles it,
## ## 'positive' would error.
## J = digraph ([1 2 1], [2 3 3], [5 -3 10]);
## distances (J, "Method", "mixed")
##          @result{}  0   5   2
##            Inf  0  -3
##            Inf Inf  0
## @end group
## @end example
##
## @seealso{graph, digraph, shortestpath, shortestpathtree, adjacency}
## @end deftypefn

function D = distances (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal @code{distances}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so
  ## @code{help distances} works outside the context of an instance)
  ## and as a fallback that gives a helpful error for non-graph
  ## inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "distances: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a graph/digraph first arg, but route through dot
  ## notation just in case.
  D = G.distances (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## distances on a non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! distances (42);

## distances on a non-graph string input is an error.
%!error <must be a graph or digraph object>
%! distances ("foo");

## distances with no args is an error via print_usage.
%!error distances ()

## -------------------- return type and shape --------------------

## distances on an empty digraph returns a 0x0 double matrix.
%!test
%! G = digraph ();
%! D = distances (G);
%! assert (size (D), [0, 0]);
%! assert (isa (D, "double"));

## distances on an empty graph returns a 0x0 double matrix.
%!test
%! G = graph ();
%! D = distances (G);
%! assert (size (D), [0, 0]);
%! assert (isa (D, "double"));

## distances on a single-node digraph returns 0 (1x1).
%!test
%! G = digraph (1);
%! D = distances (G);
%! assert (D, 0);

## distances on a single-node graph returns 0 (1x1).
%!test
%! G = graph (1);
%! D = distances (G);
%! assert (D, 0);

## distances returns a dense double matrix even when adj is sparse.
%!test
%! G = digraph ([1 2], [2 3]);
%! D = distances (G);
%! assert (isa (D, "double"));
%! assert (! issparse (D));

## distances on an N-node digraph yields an NxN matrix.
%!test
%! G = digraph (5);
%! D = distances (G);
%! assert (size (D), [5, 5]);

## -------------------- diagonal is zero --------------------

## distances diagonal is 0 on an unweighted digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! D = distances (G);
%! assert (diag (D), zeros (3, 1));

## distances diagonal is 0 on a weighted digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D = distances (G);
%! assert (diag (D), zeros (3, 1));

## distances diagonal is 0 on an edgeless N-node digraph.
%!test
%! G = digraph (4);
%! D = distances (G);
%! assert (diag (D), zeros (4, 1));

## distances diagonal is 0 even when a self-loop exists.
%!test
%! G = digraph ([1 1 2], [1 2 3], [7 1 1]);
%! D = distances (G);
%! assert (D(1, 1), 0);

## distances diagonal is 0 on the undirected graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! D = distances (G);
%! assert (diag (D), zeros (3, 1));

## -------------------- edgeless graphs: Inf off-diagonal --------------------

## distances off-diagonal is Inf when no edges exist (digraph).
%!test
%! G = digraph (3);
%! D = distances (G);
%! expected = [0 Inf Inf; Inf 0 Inf; Inf Inf 0];
%! assert (D, expected);

## distances off-diagonal is Inf when no edges exist (graph).
%!test
%! G = graph (3);
%! D = distances (G);
%! expected = [0 Inf Inf; Inf 0 Inf; Inf Inf 0];
%! assert (D, expected);

## -------------------- directed single-edge --------------------

## distances on a single directed edge 1->2 (unweighted).
%!test
%! G = digraph (1, 2);
%! D = distances (G);
%! expected = [0, 1; Inf, 0];
%! assert (D, expected);

## distances on a single directed weighted edge 1->2 (weight 7).
%!test
%! G = digraph (1, 2, 7);
%! D = distances (G);
%! expected = [0, 7; Inf, 0];
%! assert (D, expected);

## distances respects direction: only 1->2 present, so D(2,1) = Inf.
%!test
%! G = digraph ([1 2], [2 3]);
%! D = distances (G);
%! assert (D(1, 2), 1);
%! assert (D(2, 3), 1);
%! assert (D(1, 3), 2);
%! assert (D(2, 1), Inf);
%! assert (D(3, 1), Inf);
%! assert (D(3, 2), Inf);

## -------------------- undirected single-edge --------------------

## distances on a single undirected edge 1--2 is symmetric.
%!test
%! G = graph (1, 2);
%! D = distances (G);
%! expected = [0, 1; 1, 0];
%! assert (D, expected);

## distances on a 3-node path 1--2--3 (undirected, unweighted).
%!test
%! G = graph ([1 2], [2 3]);
%! D = distances (G);
%! expected = [0 1 2; 1 0 1; 2 1 0];
%! assert (D, expected);

## distances on an undirected weighted edge (weight 7) is symmetric.
%!test
%! G = graph (1, 2, 7);
%! D = distances (G);
%! expected = [0, 7; 7, 0];
%! assert (D, expected);

## distances on an undirected weighted graph (symmetric result).
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! D = distances (G);
%! assert (D, D');
%! assert (D(1, 2), 5);
%! assert (D(2, 3), 10);
%! ## 1--3 direct is 15; 1--2--3 is 5+10=15; equal -> 15.
%! assert (D(1, 3), 15);

## -------------------- directed cycle --------------------

## distances on a directed 3-cycle 1->2->3->1 (unweighted).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! D = distances (G);
%! expected = [0 1 2; 2 0 1; 1 2 0];
%! assert (D, expected);

## distances on a directed 3-cycle with weights.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D = distances (G);
%! expected = [0  5 15; 25  0 10; 15 20  0];
%! assert (D, expected);

## -------------------- reachability / Inf --------------------

## distances returns Inf for unreachable pairs (isolated node).
%!test
%! G = digraph ([1], [2], [], 3);
%! D = distances (G);
%! assert (D(1, 3), Inf);
%! assert (D(2, 3), Inf);
%! assert (D(3, 1), Inf);
%! assert (D(3, 2), Inf);

## distances returns Inf on two disjoint components (digraph).
%!test
%! G = digraph ([1 3], [2 4]);
%! D = distances (G);
%! assert (D(1, 2), 1);
%! assert (D(3, 4), 1);
%! assert (D(1, 3), Inf);
%! assert (D(1, 4), Inf);
%! assert (D(3, 1), Inf);
%! assert (D(3, 2), Inf);

## distances on two disjoint components (graph).
%!test
%! G = graph ([1 3], [2 4]);
%! D = distances (G);
%! assert (D(1, 2), 1);
%! assert (D(3, 4), 1);
%! assert (D(1, 3), Inf);
%! assert (D(1, 4), Inf);
%! assert (D(3, 1), Inf);
%! assert (D(3, 2), Inf);

## -------------------- weighted cases --------------------

## distances on a weighted line 1-(2)-2-(3)-3 (digraph).
%!test
%! G = digraph ([1 2], [2 3], [2, 3]);
%! D = distances (G);
%! assert (D(1, 2), 2);
%! assert (D(2, 3), 3);
%! assert (D(1, 3), 5);

## distances picks the shorter of two paths (digraph).
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! D = distances (G);
%! ## 1->3 direct is 100; 1->2->3 is 5+1=6; shorter is 6.
%! assert (D(1, 3), 6);

## distances picks the shorter of two paths (graph).
%!test
%! G = graph ([1 1 2], [2 3 3], [5 100 1]);
%! D = distances (G);
%! ## 1--3 direct is 100; 1--2--3 is 5+1=6; shorter is 6.
%! assert (D(1, 3), 6);

## distances on a 4-node weighted digraph, small example.
%!test
%! G = digraph ([1 1 2 2 3], [2 3 3 4 4], [1 4 2 5 1]);
%! D = distances (G);
%! ## 1->3 via (1->2->3) is 1+2=3 vs direct 4; min = 3.
%! ## 1->4 via (1->2->3->4) = 1+2+1=4 vs (1->2->4)=1+5=6 vs
%! ## (1->3->4)=4+1=5; min = 4.
%! ## 2->4 via (2->3->4)=2+1=3 vs direct 5; min = 3.
%! assert (D(1, 2), 1);
%! assert (D(1, 3), 3);
%! assert (D(1, 4), 4);
%! assert (D(2, 3), 2);
%! assert (D(2, 4), 3);
%! assert (D(3, 4), 1);
%! assert (isinf (D(2, 1)));
%! assert (isinf (D(4, 1)));

## -------------------- self-loops ignored --------------------

## Self-loop on a node does not change its diagonal (digraph).
%!test
%! G = digraph ([1 1], [1 2], [3, 7]);
%! D = distances (G);
%! assert (D(1, 1), 0);
%! assert (D(1, 2), 7);

## Self-loop on a node does not change its diagonal (graph).
%!test
%! G = graph ([1 1], [1 2], [3, 7]);
%! D = distances (G);
%! assert (D(1, 1), 0);
%! assert (D(1, 2), 7);

## -------------------- negative weights error --------------------

## On a digraph with a negative edge weight, the default 'auto'
## method promotes to 'mixed' (Bellman-Ford) and succeeds provided
## no negative cycle is reachable.
%!test
%! G = digraph ([1 2], [2 3], [1, -1]);
%! D = distances (G);
%! assert (D(1, 3), 0);    ## 1->2->3 = 1 + (-1) = 0
%! assert (D(1, 2), 1);

## On an undirected graph with any negative edge weight, the default
## 'auto' method promotes to 'mixed' and errors: an undirected
## negative edge is a negative cycle by itself (u-v-u = 2*w < 0).
%!error <negative cycle>
%! G = graph ([1 2], [2 3], [1, -1]);
%! distances (G);

## -------------------- named nodes --------------------

## Distances preserves the N x N shape for a named digraph (names
## do not affect the numeric distance matrix).
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! D = distances (G);
%! assert (size (D), [3, 3]);
%! assert (D(1, 2), 1);
%! assert (D(1, 3), 2);

## Distances on a named undirected graph returns symmetric matrix.
%!test
%! G = graph ([1 2], [2 3], [], {"x", "y", "z"});
%! D = distances (G);
%! assert (D, D');

## -------------------- dot notation dispatch --------------------

## G.distances() matches distances(G) for a digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D1 = distances (G);
%! D2 = G.distances ();
%! assert (D1, D2);

## G.distances() matches distances(G) for an undirected graph.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! D1 = distances (G);
%! D2 = G.distances ();
%! assert (D1, D2);

## -------------------- multigraph handling --------------------

## Parallel edges in a multigraph: shortest path uses the min weight.
%!test
%! G = digraph ([1 1], [2 2], [3, 7], "multigraph");
%! D = distances (G);
%! assert (D(1, 2), 3);

## Parallel edges of equal weight collapse naturally.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! D = distances (G);
%! assert (D(1, 2), 1);
%! assert (D(1, 3), 2);

## -------------------- siever-style medium example --------------------

## Siever-style 9-node digraph, unweighted: edge-count distances.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! D = distances (G);
%! assert (size (D), [9, 9]);
%! assert (diag (D), zeros (9, 1));
%! ## 1->2: 1,  1->3: 2, 1->4: 3 (via 1->2->3->4), 1->5: 4,
%! ## 1->9: 5 (via 1->2->3->4->5->9).
%! assert (D(1, 2), 1);
%! assert (D(1, 3), 2);
%! assert (D(1, 4), 3);
%! assert (D(1, 5), 4);
%! assert (D(1, 9), 5);

## -------------------- larger graph reachability sanity --------------------

## Chain 1->2->...->10: distance equals index difference for reachable
## pairs, Inf for reverse direction.
%!test
%! s = 1:9;
%! t = 2:10;
%! G = digraph (s, t);
%! D = distances (G);
%! for i = 1:10
%!   for j = 1:10
%!     if (j >= i)
%!       assert (D(i, j), j - i);
%!     else
%!       assert (D(i, j), Inf);
%!     endif
%!   endfor
%! endfor

## Undirected chain 1--2--...--10: distance equals |i-j|.
%!test
%! s = 1:9;
%! t = 2:10;
%! G = graph (s, t);
%! D = distances (G);
%! for i = 1:10
%!   for j = 1:10
%!     assert (D(i, j), abs (j - i));
%!   endfor
%! endfor

## -------------------- US-P02 single-source form --------------------

## distances(G, src) on an unweighted digraph returns a 1xN row vector
## whose k-th entry is the shortest path from src to node k.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! d = distances (G, 1);
%! assert (size (d), [1, 3]);
%! assert (d, [0, 1, 2]);

## distances(G, src) on a weighted digraph picks the weighted
## shortest path.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! d = distances (G, 1);
%! assert (size (d), [1, 3]);
%! assert (d, [0, 5, 15]);

## distances(G, src) on the undirected graph.
%!test
%! G = graph ([1 2], [2 3]);
%! d = distances (G, 2);
%! assert (size (d), [1, 3]);
%! assert (d, [1, 0, 1]);

## distances(G, src) with an isolated node returns Inf in that slot.
%!test
%! G = digraph ([1], [2], [], 3);
%! d = distances (G, 1);
%! assert (d, [0, 1, Inf]);

## distances(G, src) with a string src (named digraph).  On the
## directed 3-cycle 1->2->3->1 starting at "b" (node 2), the forward
## distances are 2 (2->3->1), 0, 1.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! d = distances (G, "b");
%! assert (size (d), [1, 3]);
%! assert (d, [2, 0, 1]);

## distances(G, src) with a 1-element cellstr src.  On the directed
## 3-cycle starting at "c" (node 3), distances are 1, 2 (3->1->2), 0.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! d = distances (G, {"c"});
%! assert (d, [1, 2, 0]);

## distances(G, src) with string src on a weighted named graph.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! d = distances (G, "a");
%! assert (d, [0, 5, 15]);

## Singleton source: element (src, src) is always 0.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! d = distances (G, 2);
%! assert (d(2), 0);

## Source on a single-node graph returns 0 (1x1).
%!test
%! G = digraph (1);
%! d = distances (G, 1);
%! assert (d, 0);
%! assert (size (d), [1, 1]);

## Direction matters for digraph: distances(G, src) only considers
## forward paths.
%!test
%! G = digraph ([1 2], [2 3]);
%! d = distances (G, 3);
%! assert (d, [Inf, Inf, 0]);

## -------------------- US-P02 single-pair form --------------------

## distances(G, src, tgt) returns a scalar double.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! d = distances (G, 1, 2);
%! assert (size (d), [1, 1]);
%! assert (isa (d, "double"));
%! assert (d, 5);

## distances(G, src, tgt) diagonal case is 0.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! d = distances (G, 2, 2);
%! assert (d, 0);

## distances(G, src, tgt) picks the shorter of two paths (digraph).
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! d = distances (G, 1, 3);
%! assert (d, 6);

## distances(G, src, tgt) on the undirected graph.
%!test
%! G = graph ([1 2], [2 3]);
%! d = distances (G, 1, 3);
%! assert (d, 2);

## distances(G, src, tgt) returns Inf for unreachable pair.
%!test
%! G = digraph ([1 2], [2 3]);
%! d = distances (G, 3, 1);
%! assert (d, Inf);

## distances(G, src, tgt) respects direction (digraph).
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (distances (G, 1, 3), 2);
%! assert (distances (G, 3, 1), Inf);

## distances(G, src, tgt) with string src and tgt on a named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! assert (distances (G, "a", "c"), 15);
%! assert (distances (G, "c", "a"), 15);
%! assert (distances (G, "b", "a"), 25);

## distances(G, src, tgt) with mixed numeric and string endpoints.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (distances (G, "a", 3), 2);
%! assert (distances (G, 1, "c"), 2);

## distances(G, src, tgt) on a named undirected graph is symmetric.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! assert (distances (G, "a", "c"), 15);
%! assert (distances (G, "c", "a"), 15);

## distances(G, src, tgt) with 1-element cellstr arguments.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! assert (distances (G, {"a"}, {"c"}), 15);

## distances(G, src, tgt) agrees with all-pairs D(src, tgt).
%!test
%! G = digraph ([1 1 2 2 3], [2 3 3 4 4], [1 4 2 5 1]);
%! D = distances (G);
%! for s = 1:4
%!   for t = 1:4
%!     assert (distances (G, s, t), D(s, t));
%!   endfor
%! endfor

## Single-source siever-style distance matches all-pairs row.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! D = distances (G);
%! assert (distances (G, 1), D(1, :));
%! assert (distances (G, 4), D(4, :));
%! assert (distances (G, 1, 9), D(1, 9));

## -------------------- US-P02 vector-argument extension -----------

## distances(G, src) with vector src returns length(src) x N matrix
## (MATLAB parity).
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D = distances (G, [1; 2]);
%! assert (size (D), [2, 3]);
%! assert (D(1, :), [0, 5, 15]);
%! assert (D(2, :), [25, 0, 10]);

## distances(G, src, tgt) with vector src and tgt returns length(s) x length(t).
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D = distances (G, [1 2], [2 3]);
%! assert (size (D), [2, 2]);
%! assert (D, [5 15; 0 10]);

## -------------------- US-P02 dot notation dispatch ---------------

## G.distances(src) matches distances(G, src).
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! d1 = distances (G, 2);
%! d2 = G.distances (2);
%! assert (d1, d2);

## G.distances(src, tgt) matches distances(G, src, tgt).
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! d1 = distances (G, 2, 3);
%! d2 = G.distances (2, 3);
%! assert (d1, d2);

## -------------------- US-P02 multigraph single-source ------------

## Parallel edges: single-source distances still use the min weight.
%!test
%! G = digraph ([1 1], [2 2], [7, 3], "multigraph");
%! d = distances (G, 1);
%! assert (d(2), 3);

## -------------------- US-P02 error cases -------------------------

## Out-of-range numeric src.
%!error <invalid node index>
%! G = digraph (3);
%! distances (G, 5);

## Zero numeric src.
%!error <invalid node index>
%! G = digraph (3);
%! distances (G, 0);

## Non-integer numeric src.
%!error <invalid node index>
%! G = digraph (3);
%! distances (G, 1.5);

## Out-of-range numeric tgt.
%!error <invalid node index>
%! G = digraph (3);
%! distances (G, 1, 5);

## String src on a digraph without names.
%!error <no node names>
%! G = digraph (3);
%! distances (G, "a");

## Missing node name.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! distances (G, "z");

## Missing node name on tgt.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! distances (G, "a", "z");

## Unsupported type (logical) for src.
%!error <numeric index array>
%! G = digraph (3);
%! distances (G, true);

## Too many positional arguments.
%!error distances (digraph (3), 1, 2, 3)

## -------------------- US-P03 Method = 'unweighted' ---------------

## 'unweighted' ignores weights and returns BFS distances.  For
## digraph ([1 1 2], [2 3 3], [5 100 1]), Dijkstra gives D(1, 3) = 6
## (1->2->3 costs 5+1=6 vs direct 100).  BFS gives D(1, 3) = 1 (one
## hop via the direct edge) because every edge has weight 1.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! D = distances (G, "Method", "unweighted");
%! assert (D(1, 2), 1);
%! assert (D(1, 3), 1);
%! assert (D(2, 3), 1);

## 'unweighted' on an unweighted digraph matches default.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! D_default = distances (G);
%! D_unw = distances (G, "Method", "unweighted");
%! assert (D_default, D_unw);

## 'unweighted' on an undirected graph gives symmetric BFS hop counts.
%!test
%! G = graph ([1 2 1], [2 3 3], [5 100 1]);
%! D = distances (G, "Method", "unweighted");
%! assert (D(1, 3), 1);
%! assert (D(3, 1), 1);
%! assert (D, D');

## 'unweighted' permits negative weights without error (they are
## ignored along with all weight values).
%!test
%! G = digraph ([1 2], [2 3], [-1, -1]);
%! D = distances (G, "Method", "unweighted");
%! assert (D(1, 3), 2);

## 'unweighted' with src gives a 1xN row vector (BFS from src).
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! d = distances (G, 1, "Method", "unweighted");
%! assert (size (d), [1, 3]);
%! assert (d, [0, 1, 1]);

## 'unweighted' with src and tgt returns a scalar.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! d = distances (G, 1, 3, "Method", "unweighted");
%! assert (d, 1);

## -------------------- US-P03 Method = 'positive' -----------------

## 'positive' matches the default on a nonneg-weighted digraph.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! D_default = distances (G);
%! D_pos = distances (G, "Method", "positive");
%! assert (D_default, D_pos);

## 'positive' matches default on a nonneg-weighted graph.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! D_default = distances (G);
%! D_pos = distances (G, "Method", "positive");
%! assert (D_default, D_pos);

## 'positive' errors on a negative edge weight (digraph).
%!error <negative edge weights>
%! G = digraph ([1 2], [2 3], [1, -1]);
%! distances (G, "Method", "positive");

## 'positive' errors on a negative edge weight (graph).
%!error <negative edge weights>
%! G = graph ([1 2], [2 3], [1, -1]);
%! distances (G, "Method", "positive");

## 'positive' with src and tgt.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! assert (distances (G, 1, 3, "Method", "positive"), 15);

## -------------------- US-P03 Method = 'mixed' --------------------

## 'mixed' accepts negative weights on a digraph and finds the path.
## 1->2->3 costs 5 + (-3) = 2, vs direct 10.  Expect 2.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! D = distances (G, "Method", "mixed");
%! assert (D(1, 3), 2);

## 'mixed' on a DAG with all negative weights.
%!test
%! G = digraph ([1 2 1], [2 3 3], [-2 -3 -10]);
%! D = distances (G, "Method", "mixed");
%! assert (D(1, 3), -10);
%! assert (D(1, 2), -2);
%! assert (D(2, 3), -3);

## 'mixed' matches Dijkstra on a nonneg-weighted digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D_def = distances (G);
%! D_mixed = distances (G, "Method", "mixed");
%! assert (D_def, D_mixed);

## 'mixed' errors on a negative cycle (directed 3-cycle with sum < 0).
%!error <negative cycle>
%! G = digraph ([1 2 3], [2 3 1], [1 1 -10]);
%! distances (G, "Method", "mixed");

## 'mixed' on an undirected graph with negative weight is a negative
## cycle (u-v-u = 2*w < 0); treated as error.
%!error <negative cycle>
%! G = graph ([1 2], [2 3], [1, -1]);
%! distances (G, "Method", "mixed");

## 'mixed' on an undirected graph with nonneg weights works (same as
## default).
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! D_def = distances (G);
%! D_mixed = distances (G, "Method", "mixed");
%! assert (D_def, D_mixed);

## 'mixed' with src on a digraph with negative weights.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! d = distances (G, 1, "Method", "mixed");
%! assert (d, [0, 5, 2]);

## 'mixed' with src and tgt.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! assert (distances (G, 1, 3, "Method", "mixed"), 2);

## -------------------- US-P03 Method = 'acyclic' ------------------

## 'acyclic' handles a DAG with negative weights.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! D = distances (G, "Method", "acyclic");
%! assert (D(1, 3), 2);

## 'acyclic' matches default on a simple DAG (positive weights).
%!test
%! G = digraph ([1 1 2 2 3], [2 3 3 4 4], [1 4 2 5 1]);
%! D_def = distances (G);
%! D_acy = distances (G, "Method", "acyclic");
%! assert (D_def, D_acy);

## 'acyclic' returns Inf for unreachable pairs.
%!test
%! G = digraph ([1 3], [2 4]);
%! D = distances (G, "Method", "acyclic");
%! assert (D(1, 3), Inf);
%! assert (D(1, 2), 1);

## 'acyclic' errors when the digraph has a cycle.
%!error <acyclic|DAG|cycle>
%! G = digraph ([1 2 3], [2 3 1]);
%! distances (G, "Method", "acyclic");

## 'acyclic' errors when the digraph has a self-loop (one-node cycle).
%!error <acyclic|DAG|cycle>
%! G = digraph ([1], [1]);
%! distances (G, "Method", "acyclic");

## 'acyclic' errors on undirected graph (not a DAG).
%!error <acyclic|undirected|DAG>
%! G = graph ([1 2], [2 3]);
%! distances (G, "Method", "acyclic");

## 'acyclic' on an edgeless digraph (trivially acyclic).
%!test
%! G = digraph (3);
%! D = distances (G, "Method", "acyclic");
%! expected = [0 Inf Inf; Inf 0 Inf; Inf Inf 0];
%! assert (D, expected);

## -------------------- US-P03 Method = 'auto' (default) -----------

## 'auto' on an unweighted digraph uses BFS (matches default).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! D_auto = distances (G, "Method", "auto");
%! D_def = distances (G);
%! assert (D_auto, D_def);

## 'auto' on a nonneg-weighted digraph uses Dijkstra.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D_auto = distances (G, "Method", "auto");
%! D_def = distances (G);
%! assert (D_auto, D_def);

## 'auto' on a digraph with negative weights uses Bellman-Ford.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! D_auto = distances (G, "Method", "auto");
%! assert (D_auto(1, 3), 2);

## 'auto' is the explicit default method.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! D_default = distances (G);
%! D_auto = distances (G, "Method", "auto");
%! assert (D_default, D_auto);

## -------------------- US-P03 case-insensitive parsing ------------

## Method name matches case-insensitively.
%!test
%! G = digraph ([1 2], [2 3]);
%! D1 = distances (G, "Method", "unweighted");
%! D2 = distances (G, "METHOD", "UNWEIGHTED");
%! D3 = distances (G, "method", "Unweighted");
%! assert (D1, D2);
%! assert (D1, D3);

## Value matches case-insensitively for all methods.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! assert (distances (G, "Method", "Positive"), distances (G));
%! assert (distances (G, "Method", "POSITIVE"), distances (G));

%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! assert (distances (G, "Method", "Mixed"),
%!         distances (G, "Method", "mixed"));
%! assert (distances (G, "Method", "ACYCLIC"),
%!         distances (G, "Method", "acyclic"));

## -------------------- US-P03 error cases -------------------------

## Unknown method name.
%!error <Method|method|unknown>
%! G = digraph ([1 2], [2 3]);
%! distances (G, "Method", "bogus");

## Numeric method value.
%!error <Method.*string|string value>
%! G = digraph ([1 2], [2 3]);
%! distances (G, "Method", 7);

## Missing method value (odd NV pair).
%!error <Method|pair|missing>
%! G = digraph ([1 2], [2 3]);
%! distances (G, "Method");

## Unknown option name after 2 positional arguments.  A char row
## at position 3 cannot be src/tgt (both already consumed), so it
## must be an option name; only 'Method' is recognised.
%!error <unknown option|Method>
%! G = digraph ([1 2], [2 3]);
%! distances (G, 1, 2, "Bogus", "auto");

## -------------------- US-P03 dot notation dispatch ---------------

## G.distances('Method', 'unweighted') matches free-function call.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! D1 = distances (G, "Method", "unweighted");
%! D2 = G.distances ("Method", "unweighted");
%! assert (D1, D2);

## G.distances(src, 'Method', method) works via dot notation.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! d1 = distances (G, 1, "Method", "mixed");
%! d2 = G.distances (1, "Method", "mixed");
%! assert (d1, d2);
