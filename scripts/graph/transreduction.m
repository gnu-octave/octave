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
## @deftypefn {} {@var{H} =} transreduction (@var{G})
## Return the transitive reduction of the directed acyclic graph @var{G}.
##
## @var{G} must be a @code{digraph} object that is a directed acyclic
## graph (DAG).  Transitive reduction on a cyclic digraph is not defined
## by this function and raises an error; use @code{condensation} first
## if you need a DAG from a general digraph.  Transitive reduction on
## the undirected @code{graph} class is not provided.
##
## The result @var{H} is a @code{digraph} on the same node set as
## @var{G}: @var{H} has the same number of nodes, in the same order,
## and preserves @code{G.Nodes.Name} when present.  An edge
## @math{i \to j} is present in @var{H} if and only if it is present
## in @var{G} and no other directed path from @math{i} to @math{j}
## exists in @var{G}.  Equivalently, @var{H} is the unique minimum-edge
## subgraph of @var{G} with the same reachability relation; for a DAG
## the transitive reduction is unique.
##
## Parallel edges and self-loops in @var{G} would create cycles, so a
## digraph with such structure is rejected as non-DAG.  The output
## @var{H} is therefore always a simple digraph.  Edge weights are not
## preserved (the reduction is defined on the reachability relation,
## so weights have no canonical meaning).
##
## @example
## @group
## G = digraph ([1 1 2], [2 3 3]);   # 1->2, 1->3, 2->3
## H = transreduction (G);
## H.Edges.EndNodes        # @result{} [1 2; 2 3]
## numedges (H)            # @result{} 2
## @end group
## @end example
##
## @seealso{digraph, transclosure, isdag, condensation, conncomp}
## @end deftypefn

function H = transreduction (G)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{transreduction} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help transreduction} works outside the context of
  ## an instance) and as a fallback that gives a helpful error for
  ## non-digraph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "transreduction: not defined for an undirected graph; transreduction requires a digraph");
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "transreduction: G must be a digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a digraph first arg, but route through dot notation
  ## just in case.
  H = G.transreduction ();

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## transreduction on an undirected graph is an error.
%!error <not defined for an undirected graph>
%! G = graph ([1 2], [2 3]);
%! transreduction (G);

## transreduction on an empty undirected graph is still an error.
%!error <requires a digraph>
%! G = graph ();
%! transreduction (G);

## transreduction on a non-graph numeric input is an error.
%!error <must be a digraph object>
%! transreduction (42);

## transreduction on a non-graph string input is an error.
%!error <must be a digraph object>
%! transreduction ("foo");

## transreduction with no args is an error via print_usage.
%!error transreduction ()

## -------------------- non-DAG error cases --------------------

## transreduction on a 2-cycle 1<->2 is an error.
%!error <directed acyclic graph>
%! G = digraph ([1 2], [2 1]);
%! transreduction (G);

## transreduction on a 3-cycle 1->2->3->1 is an error.
%!error <directed acyclic graph>
%! G = digraph ([1 2 3], [2 3 1]);
%! transreduction (G);

## transreduction on a digraph with a self-loop is an error.
%!error <directed acyclic graph>
%! G = digraph (1, 1);
%! transreduction (G);

## transreduction on a larger digraph with one cycle is an error.
%!error <directed acyclic graph>
%! G = digraph ([1 2 3 4 5], [2 3 4 5 3]);
%! transreduction (G);

## transreduction on a multigraph with parallel edges is an error
## (parallel edges create a length-2 cycle through the edge and back).
## Actually parallel edges alone do not form a cycle.  Confirm we
## accept a multigraph DAG:
## (this test lives in the DAG-examples section further down.)

## -------------------- return type --------------------

## Result is a digraph instance.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = transreduction (G);
%! assert (isa (H, "digraph"));

## Result on empty digraph is still a digraph.
%!test
%! G = digraph ();
%! H = transreduction (G);
%! assert (isa (H, "digraph"));

## Result has the same number of nodes as the input.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H = transreduction (G);
%! assert (numnodes (H), numnodes (G));

## Result has the same number of nodes as the input for an edgeless
## digraph.
%!test
%! G = digraph (5);
%! H = transreduction (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 0);

## -------------------- empty / trivial cases --------------------

## Empty digraph gives empty digraph.
%!test
%! G = digraph ();
%! H = transreduction (G);
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## Single node with no edge stays single node with no edge.
%!test
%! G = digraph (1);
%! H = transreduction (G);
%! assert (numnodes (H), 1);
%! assert (numedges (H), 0);

## Five isolated nodes stay five isolated nodes.
%!test
%! G = digraph (5);
%! H = transreduction (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 0);

## -------------------- DAG examples: irreducible ----------------

## Single edge 1->2 stays as single edge 1->2.
%!test
%! G = digraph (1, 2);
%! H = transreduction (G);
%! assert (numnodes (H), 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1, 2]);

## Chain 1->2->3: no reducible edge.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = transreduction (G);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Chain 1->2->3->4: irreducible, 3 edges.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H = transreduction (G);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 4]);

## Tree (2 branches off root, no redundant edges): irreducible.
%!test
%! G = digraph ([1 1 2 2], [2 3 4 5]);
%! H = transreduction (G);
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 4; 2 5]);

## Two disjoint edges 1->2, 3->4: irreducible.
%!test
%! G = digraph ([1 3], [2 4]);
%! H = transreduction (G);
%! assert (numnodes (H), 4);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 3 4]);

## Isolated node plus an edge stays unchanged.
%!test
%! G = digraph ([1], [2], [], 3);
%! H = transreduction (G);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1, 2]);

## -------------------- DAG examples: reducible ------------------

## Triangle 1->2, 1->3, 2->3: transreduction removes 1->3.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! H = transreduction (G);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Chain plus shortcut 1->2->3 with 1->3: shortcut removed.
%!test
%! G = digraph ([1 2 1], [2 3 3]);
%! H = transreduction (G);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Diamond 1->{2,3}, {2,3}->4 plus 1->4: 1->4 is redundant.
%!test
%! G = digraph ([1 1 2 3 1], [2 3 4 4 4]);
%! H = transreduction (G);
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 4; 3 4]);

## Complete DAG on 4 nodes (6 forward edges) reduces to the 3-edge chain.
%!test
%! s = [1 1 1 2 2 3];
%! t = [2 3 4 3 4 4];
%! G = digraph (s, t);
%! H = transreduction (G);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 4]);

## Complete DAG on 5 nodes (10 forward edges) reduces to the 4-edge
## chain 1->2->3->4->5.
%!test
%! EN = [];
%! for ii = 1:5
%!   for jj = ii+1:5
%!     EN = [EN; ii, jj];
%!   endfor
%! endfor
%! G = digraph (EN(:, 1), EN(:, 2));
%! H = transreduction (G);
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 4; 4 5]);

## Two disjoint triangles share no structure: each reduces to 2 edges.
%!test
%! G = digraph ([1 1 2 4 4 5], [2 3 3 5 6 6]);
%! H = transreduction (G);
%! assert (numnodes (H), 6);
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 4 5; 5 6]);

## -------------------- named nodes --------------------

## Named DAG: transreduction preserves names and reduces redundancy.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"a", "b", "c"});
%! H = transreduction (G);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Named DAG with no reducible edges preserves structure and names.
%!test
%! G = digraph ([1 2], [2 3], [], {"x", "y", "z"});
%! H = transreduction (G);
%! assert (H.Nodes.Name, {"x"; "y"; "z"});
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Isolated named nodes preserved.
%!test
%! G = digraph ([], [], [], {"p", "q", "r"});
%! H = transreduction (G);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 0);
%! assert (H.Nodes.Name, {"p"; "q"; "r"});

## -------------------- weights are not preserved --------------------

## Weighted DAG: transreduction result is unweighted (no Weight field).
%!test
%! G = digraph ([1 1 2], [2 3 3], [10 20 30]);
%! H = transreduction (G);
%! assert (numedges (H), 2);
%! assert (! isfield (H.Edges, "Weight"));

## Weighted irreducible chain: result still unweighted.
%!test
%! G = digraph ([1 2], [2 3], [1.5 2.5]);
%! H = transreduction (G);
%! assert (! isfield (H.Edges, "Weight"));
%! assert (numedges (H), 2);

## Negative weights: result still unweighted.
%!test
%! G = digraph ([1 1 2], [2 3 3], [-1 -2 -3]);
%! H = transreduction (G);
%! assert (! isfield (H.Edges, "Weight"));

## -------------------- output is a simple digraph --------------------

## No self-loops in transreduction output (DAGs have no self-loops anyway).
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H = transreduction (G);
%! EN = H.Edges.EndNodes;
%! if (! isempty (EN))
%!   assert (all (EN(:, 1) != EN(:, 2)));
%! endif

## Output is not a multigraph (no parallel edges).
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! H = transreduction (G);
%! assert (! ismultigraph (H));

## -------------------- multigraph input --------------------

## Multigraph DAG with parallel edges: output is simple DAG.
%!test
%! G = digraph ([1 1 1 2], [2 2 3 3], "multigraph");
%! H = transreduction (G);
%! assert (! ismultigraph (H));
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Multigraph DAG with no redundancy: output has one edge per distinct
## (src, dst) pair.
%!test
%! G = digraph ([1 1 2 2], [2 2 3 3], "multigraph");
%! H = transreduction (G);
%! assert (! ismultigraph (H));
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## -------------------- dot notation dispatch --------------------

## G.transreduction() matches transreduction(G).
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! H1 = transreduction (G);
%! H2 = G.transreduction ();
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);
%! assert (numnodes (H1), numnodes (H2));

## Dot dispatch preserves node names.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"a", "b", "c"});
%! H = G.transreduction ();
%! assert (H.Nodes.Name, {"a"; "b"; "c"});

## -------------------- idempotence --------------------

## transreduction is idempotent: applying it twice yields the same result.
%!test
%! G = digraph ([1 1 2 3 1], [2 3 4 4 4]);
%! H1 = transreduction (G);
%! H2 = transreduction (H1);
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);
%! assert (numnodes (H1), numnodes (H2));

## Idempotence on a complete DAG.
%!test
%! G = digraph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! H1 = transreduction (G);
%! H2 = transreduction (H1);
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);

## -------------------- round-trip with transclosure ----------------

## transreduction(transclosure(G)) == transreduction(G) for any DAG G.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H1 = transreduction (G);
%! H2 = transreduction (transclosure (G));
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);

## transclosure(transreduction(G)) == transclosure(G) for any DAG G.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! H1 = transclosure (G);
%! H2 = transclosure (transreduction (G));
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);

## -------------------- larger example --------------------

## Larger complete DAG on 8 nodes (28 forward edges) reduces to a
## 7-edge chain 1->2->...->8.
%!test
%! EN = [];
%! for ii = 1:8
%!   for jj = ii+1:8
%!     EN = [EN; ii, jj];
%!   endfor
%! endfor
%! G = digraph (EN(:, 1), EN(:, 2));
%! assert (numedges (G), 28);
%! H = transreduction (G);
%! assert (numnodes (H), 8);
%! assert (numedges (H), 7);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 4; 4 5; 5 6; 6 7; 7 8]);

## Layered DAG with cross-layer shortcuts: reduces to layer edges only.
##   Layer 0: {1}
##   Layer 1: {2, 3}
##   Layer 2: {4, 5}
##   Layer 3: {6}
## Full edges: 1->{2,3}, {2,3}->{4,5}, {4,5}->6, plus shortcuts
## 1->{4,5,6}, {2,3}->6.  Reduction keeps only the layer edges.
%!test
%! s = [1 1 2 2 3 3 4 5 1 1 1 2 3];
%! t = [2 3 4 5 4 5 6 6 4 5 6 6 6];
%! G = digraph (s, t);
%! H = transreduction (G);
%! assert (numnodes (H), 6);
%! assert (numedges (H), 8);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 4; 2 5; 3 4; 3 5; 4 6; 5 6]);
