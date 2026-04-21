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
## @deftypefn {} {@var{D} =} distances (@var{G})
## Return the all-pairs shortest-path distance matrix of the graph or
## digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  The result
## @var{D} is a @code{numnodes (@var{G})}-by-@code{numnodes (@var{G})}
## dense double matrix.  @var{D}(i, j) is the length of a shortest path
## from node @math{i} to node @math{j} under the stored edge weights
## (every edge has weight @code{1} when @var{G} is unweighted), or
## @code{Inf} when @math{j} is not reachable from @math{i}.  The
## diagonal @var{D}(i, i) is always @code{0}.
##
## For the undirected @code{graph} class, edges may be traversed in
## either direction, so @var{D} is symmetric.  For the directed
## @code{digraph} class, paths must follow edge direction so
## @var{D}(i, j) is in general not the same as @var{D}(j, i).
##
## The default method is Dijkstra's algorithm, which requires all edge
## weights to be non-negative.  A negative edge weight causes an error
## (future stories will add a Name-Value option to pick alternative
## methods such as Bellman-Ford).  Self-loops do not influence shortest
## paths: @var{D}(i, i) is always @code{0}, regardless of any self-loop
## weight on node @math{i}.  For a @code{digraph} with parallel edges
## (multigraph), each parallel edge is considered independently; the
## shortest path uses the edge with the smallest weight connecting each
## pair of endpoints.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1], [5 10 15]);
## distances (G)
##          @result{}  0   5  15
##             25   0  10
##             15  20   0
##
## H = graph ([1 2], [2 3]);
## distances (H)
##          @result{}  0  1  2
##             1  0  1
##             2  1  0
## @end group
## @end example
##
## @seealso{graph, digraph, shortestpath, shortestpathtree, adjacency}
## @end deftypefn

function D = distances (G)

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
  D = G.distances ();

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

## Negative weight triggers an error under the default Dijkstra method.
%!error <negative edge weights>
%! G = digraph ([1 2], [2 3], [1, -1]);
%! distances (G);

## Negative weight on an undirected graph also errors.
%!error <negative edge weights>
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
