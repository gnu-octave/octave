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
## @deftypefn {} {@var{H} =} transclosure (@var{G})
## Return the transitive closure of the digraph @var{G}.
##
## @var{G} must be a @code{digraph} object.  Transitive closure on the
## undirected @code{graph} class is not provided; calling
## @code{transclosure} on a @code{graph} raises an error.
##
## The result @var{H} is a @code{digraph} on the same node set as
## @var{G}: @var{H} has the same number of nodes, in the same order,
## and preserves @code{G.Nodes.Name} when present.  An edge
## @math{i \to j} is present in @var{H} whenever there is a directed
## path from node @math{i} to node @math{j} in @var{G} with at least
## one edge and @math{i \ne j}.  Equivalently, the adjacency matrix of
## @var{H} (as an unweighted binary relation) is the reflexive-free
## transitive closure of the adjacency matrix of @var{G}.
##
## Self-loops and parallel edges are not present in @var{H} even when
## @var{G} has them: the output is always a simple digraph.  Edge
## weights are not preserved (the transitive closure is a boolean
## relation, so weights have no canonical meaning).
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 4]);
## H = transclosure (G);
## numedges (H)              # @result{} 6  (1->2, 1->3, 1->4, 2->3,
##                           #               2->4, 3->4)
## H.Edges.EndNodes
##                    # @result{} [1 2; 1 3; 1 4; 2 3; 2 4; 3 4]
## @end group
## @end example
##
## @seealso{digraph, transreduction, condensation, conncomp}
## @end deftypefn

function H = transclosure (G)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{transclosure} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help transclosure} works outside the context of
  ## an instance) and as a fallback that gives a helpful error for
  ## non-digraph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "transclosure: not defined for an undirected graph; transclosure requires a digraph");
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "transclosure: G must be a digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a digraph first arg, but route through dot notation
  ## just in case.
  H = G.transclosure ();

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## transclosure on an undirected graph is an error.
%!error <not defined for an undirected graph>
%! G = graph ([1 2], [2 3]);
%! transclosure (G);

## transclosure on an empty undirected graph is still an error.
%!error <requires a digraph>
%! G = graph ();
%! transclosure (G);

## transclosure on a non-graph numeric input is an error.
%!error <must be a digraph object>
%! transclosure (42);

## transclosure on a non-graph string input is an error.
%!error <must be a digraph object>
%! transclosure ("foo");

## transclosure with no args is an error via print_usage.
%!error transclosure ()

## -------------------- return type --------------------

## Result is a digraph instance.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = transclosure (G);
%! assert (isa (H, "digraph"));

## Result on empty digraph is still a digraph.
%!test
%! G = digraph ();
%! H = transclosure (G);
%! assert (isa (H, "digraph"));

## Result has the same number of nodes as the input.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H = transclosure (G);
%! assert (numnodes (H), numnodes (G));

## Result has the same number of nodes as the input for an edgeless digraph.
%!test
%! G = digraph (5);
%! H = transclosure (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 0);

## -------------------- empty / trivial cases --------------------

## Empty digraph gives empty digraph.
%!test
%! G = digraph ();
%! H = transclosure (G);
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## Single node with no edge stays single node with no edge.
%!test
%! G = digraph (1);
%! H = transclosure (G);
%! assert (numnodes (H), 1);
%! assert (numedges (H), 0);

## Five isolated nodes stay five isolated nodes.
%!test
%! G = digraph (5);
%! H = transclosure (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 0);

## -------------------- DAG examples --------------------

## Single edge 1->2 stays as single edge 1->2.
%!test
%! G = digraph (1, 2);
%! H = transclosure (G);
%! assert (numnodes (H), 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1, 2]);

## Line 1->2->3: transclosure adds 1->3.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = transclosure (G);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1, 2; 1, 3; 2, 3]);

## Chain 1->2->3->4: transclosure has 6 edges (all forward pairs).
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H = transclosure (G);
%! assert (numedges (H), 6);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 1 4; 2 3; 2 4; 3 4]);

## Diamond 1->{2,3}, {2,3}->4: transclosure adds 1->4 only.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! H = transclosure (G);
%! assert (numedges (H), 5);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 1 4; 2 4; 3 4]);

## Branching tree 1->{2,3}, 2->{4,5}: transclosure adds 1->4 and 1->5.
%!test
%! G = digraph ([1 1 2 2], [2 3 4 5]);
%! H = transclosure (G);
%! assert (numedges (H), 6);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 1 4; 1 5; 2 4; 2 5]);

## Two disjoint edges 1->2 and 3->4: transclosure leaves them unchanged.
%!test
%! G = digraph ([1 3], [2 4]);
%! H = transclosure (G);
%! assert (numnodes (H), 4);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1, 2; 3, 4]);

## Isolated node plus an edge stays with its isolated node.
%!test
%! G = digraph ([1], [2], [], 3);
%! H = transclosure (G);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1, 2]);

## -------------------- cyclic cases --------------------

## Self-loop on a single node: transclosure has no edges (no self-loops).
%!test
%! G = digraph (1, 1);
%! H = transclosure (G);
%! assert (numnodes (H), 1);
%! assert (numedges (H), 0);

## Self-loop plus forward edge: transclosure has only the forward edge.
%!test
%! G = digraph ([1 1], [1 2]);
%! H = transclosure (G);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1, 2]);

## 2-cycle 1<->2: transclosure is the 2-cycle (no self-loops).
%!test
%! G = digraph ([1 2], [2 1]);
%! H = transclosure (G);
%! assert (numnodes (H), 2);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1, 2; 2, 1]);

## 3-cycle 1->2->3->1: transclosure is the complete loop-free digraph on 3
## nodes (6 edges, all ordered pairs (i, j) with i != j).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = transclosure (G);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 6);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 1; 2 3; 3 1; 3 2]);

## 3-cycle with an exit to node 4: transclosure has edges from every
## cycle member to node 4 and all cycle-member pairs.
%!test
%! G = digraph ([1 2 3 3], [2 3 1 4]);
%! H = transclosure (G);
%! assert (numnodes (H), 4);
%! ## Within-SCC edges {1,2,3}: 6 ordered pairs. Plus 1->4, 2->4, 3->4.
%! assert (numedges (H), 9);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 1 4;
%!                            2 1; 2 3; 2 4;
%!                            3 1; 3 2; 3 4]);

## Two back-to-back 2-cycles {1<->2} and {3<->4}: no cross-SCC edges.
%!test
%! G = digraph ([1 2 3 4], [2 1 4 3]);
%! H = transclosure (G);
%! assert (numnodes (H), 4);
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 2 1; 3 4; 4 3]);

## -------------------- named nodes --------------------

## Named DAG: transclosure preserves names and expands reachability.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = transclosure (G);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);

## Named digraph with cycle: names preserved, within-SCC edges emitted.
%!test
%! G = digraph ([1 2], [2 1], [], {"foo", "bar"});
%! H = transclosure (G);
%! assert (H.Nodes.Name, {"foo"; "bar"});
%! assert (numedges (H), 2);

## Isolated named nodes are preserved.
%!test
%! G = digraph ([], [], [], {"x", "y", "z"});
%! H = transclosure (G);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 0);
%! assert (H.Nodes.Name, {"x"; "y"; "z"});

## -------------------- weights are not preserved --------------------

## Weighted DAG: transclosure result is unweighted (no Weight field).
%!test
%! G = digraph ([1 2], [2 3], [10 20]);
%! H = transclosure (G);
%! assert (numedges (H), 3);
%! assert (! isfield (H.Edges, "Weight"));

## Weighted cycle: transclosure result still unweighted.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1.5 2.5 3.5]);
%! H = transclosure (G);
%! assert (! isfield (H.Edges, "Weight"));
%! assert (numedges (H), 6);

## Negative weights: transclosure result still unweighted.
%!test
%! G = digraph ([1 2], [2 3], [-1 -2]);
%! H = transclosure (G);
%! assert (! isfield (H.Edges, "Weight"));

## -------------------- output is a simple digraph --------------------

## No self-loops in transclosure output, even when G has them.
%!test
%! G = digraph ([1 1 2], [1 2 3]);
%! H = transclosure (G);
%! EN = H.Edges.EndNodes;
%! if (! isempty (EN))
%!   assert (all (EN(:, 1) != EN(:, 2)));
%! endif

## No self-loops in transclosure of any cyclic digraph.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = transclosure (G);
%! EN = H.Edges.EndNodes;
%! if (! isempty (EN))
%!   assert (all (EN(:, 1) != EN(:, 2)));
%! endif

## Output is not a multigraph (no parallel edges).
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H = transclosure (G);
%! assert (! ismultigraph (H));

## -------------------- multigraph input --------------------

## Parallel edges in G collapse in transclosure (output is simple).
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! H = transclosure (G);
%! assert (! ismultigraph (H));
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);

## Multigraph self-loop: transclosure still has no self-loop.
%!test
%! G = digraph ([1 1 1], [1 1 2], "multigraph");
%! H = transclosure (G);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1, 2]);

## -------------------- dot notation dispatch --------------------

## G.transclosure() matches transclosure(G).
%!test
%! G = digraph ([1 2], [2 3]);
%! H1 = transclosure (G);
%! H2 = G.transclosure ();
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);
%! assert (numnodes (H1), numnodes (H2));

## Dot dispatch preserves node names.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = G.transclosure ();
%! assert (H.Nodes.Name, {"a"; "b"; "c"});

## -------------------- idempotence --------------------

## transclosure is idempotent: applying it twice yields the same result.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 5]);
%! H1 = transclosure (G);
%! H2 = transclosure (H1);
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);
%! assert (numnodes (H1), numnodes (H2));

## Idempotence on a cyclic digraph.
%!test
%! G = digraph ([1 2 3 3], [2 3 1 4]);
%! H1 = transclosure (G);
%! H2 = transclosure (H1);
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);

## -------------------- larger example --------------------

## Larger DAG: every forward pair (i, j) with i < j is in transclosure.
%!test
%! s = 1:9;
%! t = 2:10;
%! G = digraph (s, t);
%! H = transclosure (G);
%! ## Expected: all ordered pairs (i, j) with 1 <= i < j <= 10.
%! assert (numnodes (H), 10);
%! assert (numedges (H), 45);
%! EN = H.Edges.EndNodes;
%! expected = [];
%! for ii = 1:10
%!   for jj = ii+1:10
%!     expected = [expected; ii, jj];
%!   endfor
%! endfor
%! assert (EN, expected);
