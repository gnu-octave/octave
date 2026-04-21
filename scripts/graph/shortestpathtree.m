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
## @deftypefn  {} {@var{TR} =} shortestpathtree (@var{G}, @var{s})
## @deftypefnx {} {@var{TR} =} shortestpathtree (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {@var{TR} =} shortestpathtree (@dots{}, "OutputForm", @var{form})
## Return a single-source shortest path tree rooted at node @var{s}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{s} is a
## scalar node identifier: a positive integer node index, a character
## row vector naming a node, or a 1-element cell array of strings naming
## a node.
##
## With the two-argument form, the tree covers every node reachable from
## @var{s}.  With the three-argument form, @var{t} is a list of target
## nodes and the tree is pruned to only include edges that lie on some
## shortest path from @var{s} to a target.  @var{t} may be a numeric
## vector of node indices, a character row vector naming a single node,
## or a cell array of strings naming several nodes.  Any mix of numeric
## indices and names is accepted when @var{G} has node names.
##
## The return value @var{TR} depends on the @qcode{"OutputForm"} option:
##
## @table @asis
## @item @qcode{"tree"} (the default)
## @var{TR} is a @code{digraph} whose nodes match @var{G}'s nodes (and
## node names, when @var{G} has them) and whose edges are the
## predecessor edges of the shortest path tree.  Every edge is oriented
## from parent to child, regardless of whether @var{G} is directed or
## undirected, so @var{TR} is always a @code{digraph}.  When @var{G} is
## weighted, each edge in @var{TR} carries the weight of the
## corresponding edge in @var{G}; for a @code{digraph} with parallel
## edges (multigraph), the cheapest of the parallel edges connecting
## each pair of endpoints is used.
##
## @item @qcode{"vector"}
## @var{TR} is a @code{1}-by-@code{numnodes (@var{G})} double row
## vector of predecessor indices.  @code{@var{TR}(i)} is the predecessor
## of node @math{i} in the shortest path tree, so @code{@var{TR}(i) ==
## 0} means that node @math{i} is not reachable from @var{s} (or has
## been pruned in the three-argument form).  By convention
## @code{@var{TR}(@var{s}) = @var{s}}.
##
## @item @qcode{"cell"}
## When called with the two-argument form, @var{TR} is a @code{numnodes
## (@var{G})}-by-1 cell array in which @code{@var{TR}@{i@}} is the
## shortest path from @var{s} to node @math{i}, in forward order.  When
## called with the three-argument form, @var{TR} has one cell per target
## in the order supplied.  Each path is a numeric row vector when the
## input node identifiers were numeric, or a cellstr row when the input
## identifiers were names.  When a target is not reachable from
## @var{s}, the corresponding cell is a @code{1}-by-@code{0} empty
## vector (numeric or cellstr, matching the input type).
## @end table
##
## Self-loops in @var{G} do not influence the tree: no tree edge is ever
## a self-loop and the distance to the source itself is always
## @code{0}.
##
## @example
## @group
## G = digraph ([1 1 2], [2 3 3], [5 100 1]);
## TR = shortestpathtree (G, 1);
## TR.Edges.EndNodes
##          @result{}  1   2
##              2   3
##
## v = shortestpathtree (G, 1, "OutputForm", "vector");
## v
##          @result{}  [1 1 2]
##
## C = shortestpathtree (G, 1, "OutputForm", "cell");
## C@{3@}
##          @result{}  [1 2 3]
## @end group
## @end example
##
## @seealso{graph, digraph, shortestpath, distances, allpaths}
## @end deftypefn

function TR = shortestpathtree (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{shortestpathtree} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help shortestpathtree} works outside the context
  ## of an instance) and as a fallback that gives a helpful error for
  ## non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "shortestpathtree: G must be a graph or digraph object");
  endif

  TR = G.shortestpathtree (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## Non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! shortestpathtree (42, 1);

## Non-graph string input is an error.
%!error <must be a graph or digraph object>
%! shortestpathtree ("foo", 1);

## No-args call is an error via print_usage.
%!error shortestpathtree ()

## Missing source is an error.
%!error shortestpathtree (digraph ())

## Unknown OutputForm value errors.
%!error <OutputForm>
%! G = digraph ([1 2], [2 3]);
%! shortestpathtree (G, 1, "OutputForm", "nonsense");

## Unknown Name-Value pair errors.
%!error <unknown>
%! G = digraph ([1 2], [2 3]);
%! shortestpathtree (G, 1, "NoSuchOption", "x");

## Name-Value with odd count of remaining args errors.
%!error
%! G = digraph ([1 2], [2 3]);
%! shortestpathtree (G, 1, "OutputForm");

## -------------------- tree output (default) --------------------

## Default OutputForm is 'tree' -- returns a digraph (even on graph).
%!test
%! G = digraph ([1 2], [2 3]);
%! TR = shortestpathtree (G, 1);
%! assert (isa (TR, "digraph"));

## Graph input still produces a digraph tree.
%!test
%! G = graph ([1 2], [2 3]);
%! TR = shortestpathtree (G, 1);
%! assert (isa (TR, "digraph"));

## Tree has same node count as G.
%!test
%! G = digraph (5);
%! TR = shortestpathtree (G, 1);
%! assert (numnodes (TR), 5);

## Tree from 1 on chain 1->2->3 has exactly edges [1 2; 2 3].
%!test
%! G = digraph ([1 2], [2 3]);
%! TR = shortestpathtree (G, 1);
%! E = TR.Edges.EndNodes;
%! assert (sortrows (E), [1 2; 2 3]);

## Tree on single node is edgeless.
%!test
%! G = digraph (3);
%! TR = shortestpathtree (G, 1);
%! assert (numnodes (TR), 3);
%! assert (numedges (TR), 0);

## Tree on single-node digraph.
%!test
%! G = digraph (1);
%! TR = shortestpathtree (G, 1);
%! assert (numnodes (TR), 1);
%! assert (numedges (TR), 0);

## Tree preserves the weights of the tree edges (weighted digraph).
%!test
%! G = digraph ([1 1 2], [2 3 3], [10 100 1]);
%! TR = shortestpathtree (G, 1);
%! ## Tree: 1->2 (w=10), 2->3 (w=1).  Direct 1->3 (w=100) is pruned.
%! E = TR.Edges.EndNodes;
%! W = TR.Edges.Weight;
%! assert (sortrows ([E, W]), [1 2 10; 2 3 1]);

## Unweighted tree on unweighted digraph has no Weight column.
%!test
%! G = digraph ([1 2], [2 3]);
%! TR = shortestpathtree (G, 1);
%! assert (! isfield (TR.Edges, "Weight"));

## Tree on graph (undirected) has edges oriented away from source.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! TR = shortestpathtree (G, 1);
%! E = TR.Edges.EndNodes;
%! ## Every edge starts at a node closer to the source (in hops).
%! ## Specifically, from source 1 the tree has two edges, both with
%! ## src == 1 (since 2 and 3 are both 1-hop away in the 3-cycle).
%! assert (numedges (TR), 2);
%! assert (all (E(:, 1) == 1));

## Unreachable nodes remain in the tree as isolated nodes.
%!test
%! G = digraph ([1 3], [2 4]);
%! TR = shortestpathtree (G, 1);
%! assert (numnodes (TR), 4);
%! E = TR.Edges.EndNodes;
%! assert (sortrows (E), [1 2]);

## Two disjoint components: tree from one side only has edges reachable
## from the source.
%!test
%! G = digraph ([1 3 3], [2 4 5]);
%! TR = shortestpathtree (G, 3);
%! E = TR.Edges.EndNodes;
%! assert (sortrows (E), [3 4; 3 5]);
%! assert (numnodes (TR), 5);

## Named graph: TR preserves node names.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! TR = shortestpathtree (G, 1);
%! assert (TR.Nodes.Name, {"a"; "b"; "c"});

## Named graph: source resolved by name works and preserves names.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! TR = shortestpathtree (G, "a");
%! assert (TR.Nodes.Name, {"a"; "b"; "c"});
%! assert (numedges (TR), 2);

## -------------------- targeted form (G, s, t) --------------------

## Specific single target: tree pruned to that path only.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 4], [1 10 1 1]);
%! TR = shortestpathtree (G, 1, 3);
%! ## Shortest 1 to 3 is 1->2->3 (weight 2 vs direct 10).  Edge 3->4 is
%! ## not on the path.
%! E = TR.Edges.EndNodes;
%! assert (sortrows (E), [1 2; 2 3]);
%! assert (numnodes (TR), 4);

## Multiple targets: tree covers edges on any shortest path to any
## target.
%!test
%! G = digraph ([1 1 2 2], [2 3 3 4]);
%! TR = shortestpathtree (G, 1, [3, 4]);
%! E = TR.Edges.EndNodes;
%! ## Required tree edges: 1->2 (to reach 4 via 2), 2->4, and 1->3.
%! ## (1->3 has length 1, 1->2->3 would have length 2, so 1->3 direct
%! ## is the shortest path to 3.)
%! assert (ismember ([1 2], E, "rows"));
%! assert (ismember ([2 4], E, "rows"));
%! assert (ismember ([1 3], E, "rows"));

## Unreachable target: tree omits the unreachable part.
%!test
%! G = digraph ([1 3], [2 4]);
%! TR = shortestpathtree (G, 1, 3);
%! ## 3 is not reachable from 1, so tree has no edges along the path.
%! assert (numedges (TR), 0);

## Target == source: tree is edgeless.
%!test
%! G = digraph ([1 2], [2 3]);
%! TR = shortestpathtree (G, 1, 1);
%! assert (numedges (TR), 0);

## String target on a named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! TR = shortestpathtree (G, "a", "b");
%! E = TR.Edges.EndNodes;
%! ## Shortest a->b is direct (weight 5) because a->c->b would be 15+10.
%! assert (size (E, 1), 1);

## Cellstr target on a named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! TR = shortestpathtree (G, "a", {"b", "c"});
%! assert (numedges (TR), 2);

## -------------------- 'OutputForm', 'vector' --------------------

## Vector output shape.
%!test
%! G = digraph ([1 2], [2 3]);
%! v = shortestpathtree (G, 1, "OutputForm", "vector");
%! assert (size (v), [1, 3]);
%! assert (isa (v, "double"));

## Vector values: source is its own predecessor, chain predecessors.
%!test
%! G = digraph ([1 2], [2 3]);
%! v = shortestpathtree (G, 1, "OutputForm", "vector");
%! assert (v(1), 1);  # source is its own predecessor by convention
%! assert (v(2), 1);
%! assert (v(3), 2);

## Vector on graph (undirected) still returns a vector.
%!test
%! G = graph ([1 2], [2 3]);
%! v = shortestpathtree (G, 1, "OutputForm", "vector");
%! assert (numel (v), 3);
%! assert (v(1), 1);
%! assert (v(2), 1);
%! assert (v(3), 2);

## Vector with unreachable nodes: 0 in those positions.
%!test
%! G = digraph ([1 3], [2 4]);
%! v = shortestpathtree (G, 1, "OutputForm", "vector");
%! assert (v(1), 1);
%! assert (v(2), 1);
%! assert (v(3), 0);
%! assert (v(4), 0);

## Vector output with (G, s, t) form: only path-to-target predecessors
## are filled; other reachable but off-path nodes are 0.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 4], [1 10 1 1]);
%! v = shortestpathtree (G, 1, 3, "OutputForm", "vector");
%! assert (v(1), 1);
%! assert (v(2), 1);
%! assert (v(3), 2);
%! assert (v(4), 0);  # 4 is reachable but not on path to target 3

## Vector output with (G, s, t) on unreachable target: the path is
## empty so the only filled entry is the source itself.
%!test
%! G = digraph ([1 3], [2 4]);
%! v = shortestpathtree (G, 1, 3, "OutputForm", "vector");
%! assert (v(1), 1);
%! assert (v(2), 0);
%! assert (v(3), 0);
%! assert (v(4), 0);

## -------------------- 'OutputForm', 'cell' --------------------

## Cell shape: numnodes-by-1 for two-arg form.
%!test
%! G = digraph ([1 2], [2 3]);
%! C = shortestpathtree (G, 1, "OutputForm", "cell");
%! assert (size (C), [3, 1]);

## Cell contents: path from s to each reachable node.
%!test
%! G = digraph ([1 2], [2 3]);
%! C = shortestpathtree (G, 1, "OutputForm", "cell");
%! assert (C{1}, 1);
%! assert (C{2}, [1 2]);
%! assert (C{3}, [1 2 3]);

## Cell with unreachable entries: empty 1x0 numeric row.
%!test
%! G = digraph ([1 3], [2 4]);
%! C = shortestpathtree (G, 1, "OutputForm", "cell");
%! assert (size (C{1}), [1, 1]);
%! assert (C{1}, 1);
%! assert (C{2}, [1 2]);
%! assert (size (C{3}), [1, 0]);
%! assert (isa (C{3}, "double"));
%! assert (size (C{4}), [1, 0]);

## Cell with (G, s, t) form: numel(t)-by-1 cell.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! C = shortestpathtree (G, 1, [2, 3], "OutputForm", "cell");
%! assert (size (C), [2, 1]);
%! assert (C{1}, [1, 2]);
%! assert (C{2}, [1, 2, 3]);

## Cell with single numeric target returns 1x1 cell.
%!test
%! G = digraph ([1 2], [2 3]);
%! C = shortestpathtree (G, 1, 3, "OutputForm", "cell");
%! assert (size (C), [1, 1]);
%! assert (C{1}, [1, 2, 3]);

## Cell with unreachable target in (G, s, t) form.
%!test
%! G = digraph ([1 3], [2 4]);
%! C = shortestpathtree (G, 1, 3, "OutputForm", "cell");
%! assert (size (C), [1, 1]);
%! assert (size (C{1}), [1, 0]);

## -------------------- named-node cell output --------------------

## Named digraph with string source: cell entries are cellstr.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! C = shortestpathtree (G, "a", "OutputForm", "cell");
%! assert (iscell (C));
%! assert (size (C), [3, 1]);
%! assert (iscellstr (C{2}));
%! assert (C{1}, {"a"});
%! assert (C{2}, {"a", "b"});
%! assert (C{3}, {"a", "b", "c"});

## Named digraph with numeric source: cell entries are still cellstr
## because G has node names (MATLAB parity).  Actually MATLAB returns
## numeric indices for numeric source on a named graph, matching its
## behaviour for shortestpath.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! C = shortestpathtree (G, 1, "OutputForm", "cell");
%! assert (isa (C{2}, "double"));
%! assert (C{2}, [1, 2]);

## Named digraph, cellstr targets: cell paths are cellstr.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! C = shortestpathtree (G, "a", {"b", "c"}, "OutputForm", "cell");
%! assert (size (C), [2, 1]);
%! assert (iscellstr (C{1}));
%! assert (C{1}, {"a", "b"});
%! assert (C{2}, {"a", "b", "c"});

## Unreachable named target returns an empty cellstr 1x0.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! C = shortestpathtree (G, "c", "a", "OutputForm", "cell");
%! assert (size (C), [1, 1]);
%! assert (iscell (C{1}));
%! assert (size (C{1}), [1, 0]);

## -------------------- dot notation dispatch --------------------

## G.shortestpathtree(s) == shortestpathtree(G, s).
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! TR1 = shortestpathtree (G, 1);
%! TR2 = G.shortestpathtree (1);
%! assert (TR1.Edges.EndNodes, TR2.Edges.EndNodes);

## G.shortestpathtree(s, 'OutputForm', 'cell') == free-function version.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! C1 = shortestpathtree (G, 1, "OutputForm", "cell");
%! C2 = G.shortestpathtree (1, "OutputForm", "cell");
%! assert (C1, C2);

## -------------------- self-loops ignored --------------------

## Self-loop on source does not appear in tree.
%!test
%! G = digraph ([1 1], [1 2]);
%! TR = shortestpathtree (G, 1);
%! E = TR.Edges.EndNodes;
%! assert (all (E(:, 1) != E(:, 2)));

## -------------------- node-index validation --------------------

## Out-of-range numeric source errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpathtree (G, 5);

## Zero numeric source errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpathtree (G, 0);

## Non-integer source errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpathtree (G, 1.5);

## Non-scalar numeric source errors.
%!error <scalar node identifier>
%! G = digraph (3);
%! shortestpathtree (G, [1, 2]);

## Out-of-range numeric target errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpathtree (G, 1, 5);

## String source on a digraph without names errors.
%!error <no node names>
%! G = digraph (3);
%! shortestpathtree (G, "a");

## Missing named source errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! shortestpathtree (G, "z");

## Missing named target errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! shortestpathtree (G, "a", "z");

## -------------------- negative weights (deferred) ---------------

## Negative edge weights error on the default Dijkstra method.  US-P08
## will add 'Method','mixed' support.
%!error <negative edge weights>
%! G = digraph ([1 2], [2 3], [1, -1]);
%! shortestpathtree (G, 1);

%!error <negative edge weights>
%! G = graph ([1 2], [2 3], [1, -1]);
%! shortestpathtree (G, 1);

## -------------------- multigraph (digraph) --------------------

## Multigraph: tree carries the cheapest parallel edge's weight.
%!test
%! G = digraph ([1 1], [2 2], [7, 3], "multigraph");
%! TR = shortestpathtree (G, 1);
%! E = TR.Edges.EndNodes;
%! assert (E, [1, 2]);
%! assert (TR.Edges.Weight, 3);

## -------------------- larger graph sanity ----------------------

## Siever-style 9-node digraph: tree from node 1 covers every
## reachable node.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! TR = shortestpathtree (G, 1);
%! assert (numnodes (TR), 9);
%! ## All 9 nodes are reachable from 1, so the tree has 8 edges.
%! assert (numedges (TR), 8);

## Cell form of the same: every node has a non-empty path.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! C = shortestpathtree (G, 1, "OutputForm", "cell");
%! assert (size (C), [9, 1]);
%! for ii = 1:9
%!   assert (numel (C{ii}) >= 1);
%!   assert (C{ii}(1), 1);
%!   assert (C{ii}(end), ii);
%! endfor

## Vector form of the same: every entry is non-zero (all reachable).
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! v = shortestpathtree (G, 1, "OutputForm", "vector");
%! assert (all (v != 0));
