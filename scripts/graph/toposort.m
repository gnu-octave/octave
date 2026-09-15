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
## @deftypefn  {} {@var{n} =} toposort (@var{G})
## @deftypefnx {} {@var{n} =} toposort (@var{G}, @qcode{"Order"}, @var{order})
## Return a topological ordering of the directed acyclic graph @var{G}.
##
## @var{G} must be a @code{digraph} object that represents a directed
## acyclic graph (DAG).  Topological sorting is not defined on the
## undirected @code{graph} class; calling @code{toposort} on a
## @code{graph} raises an error, as does calling it on a digraph with
## one or more cycles (including any self-loop).
##
## The result @var{n} is a @code{1}-by-@code{numnodes (@var{G})} row
## vector of node indices such that, for every edge
## @code{@var{s} -> @var{t}} in @var{G}, the index of @var{s} in
## @var{n} is less than the index of @var{t}.  When @var{G} has zero
## nodes, @var{n} is a @code{1}-by-@code{0} row vector.
##
## The optional Name-Value option @qcode{"Order"} selects the tie-break
## rule used when the graph admits several valid topological orders
## (case-insensitive for both the name and the value):
##
## @itemize
## @item
## @qcode{"stable"} (the default): when multiple nodes have no
## remaining incoming edges, always pick the one with the smallest
## node index first.  This is MATLAB's @qcode{"stable"} semantics and
## yields the lexicographically smallest topological order.
## @item
## @qcode{"lexicographic"}: behaves identically to @qcode{"stable"}.
## Accepted as a synonym for readability when it is the ordering
## property (rather than a stability property) that the caller wants
## to emphasise.
## @end itemize
##
## @example
## @group
## G = digraph ([1 1 2 2 3 4], [2 3 3 4 4 5]);
## toposort (G)           # @result{} [1 2 3 4 5]
## @end group
## @end example
##
## @seealso{digraph, isdag, conncomp, condensation, bfsearch, dfsearch}
## @end deftypefn

function n = toposort (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal @code{toposort}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so
  ## @code{help toposort} works outside the context of an instance)
  ## and as a fallback that gives a helpful error for non-digraph
  ## inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "toposort: not defined for an undirected graph; toposort requires a digraph");
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "toposort: G must be a digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a digraph first arg, but route through dot notation
  ## just in case.
  n = G.toposort (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## toposort on an undirected graph is an error.
%!error <not defined for an undirected graph>
%! G = graph ([1 2], [2 3]);
%! toposort (G);

## toposort on an empty undirected graph is still an error.
%!error <requires a digraph>
%! G = graph ();
%! toposort (G);

## toposort on a non-graph numeric input is an error.
%!error <must be a digraph object>
%! toposort (42);

## toposort on a non-graph string input is an error.
%!error <must be a digraph object>
%! toposort ("foo");

## toposort with no args is an error via print_usage.
%!error toposort ()

## -------------------- default (no options) --------------------

## Empty digraph: empty 1x0 row vector.
%!test
%! G = digraph ();
%! n = toposort (G);
%! assert (size (n), [1, 0]);
%! assert (class (n), "double");

## Single node: the only valid topological ordering is [1].
%!test
%! G = digraph (1);
%! n = toposort (G);
%! assert (n, 1);

## Three isolated nodes: [1 2 3].
%!test
%! G = digraph (3);
%! n = toposort (G);
%! assert (n, [1, 2, 3]);

## Single edge 1->2: [1 2].
%!test
%! G = digraph ([1], [2]);
%! n = toposort (G);
%! assert (n, [1, 2]);

## Single edge 2->1: [2 1].
%!test
%! G = digraph ([2], [1]);
%! n = toposort (G);
%! assert (n, [2, 1]);

## Line 1->2->3 in forward order: [1 2 3].
%!test
%! G = digraph ([1 2], [2 3]);
%! n = toposort (G);
%! assert (n, [1, 2, 3]);

## Line 1->2->3 constructed out of order stays [1 2 3] for the stable rule.
%!test
%! G = digraph ([2 1], [3 2]);
%! n = toposort (G);
%! assert (n, [1, 2, 3]);

## Inverted line 3->2->1: unique order [3 2 1].
%!test
%! G = digraph ([3 2], [2 1]);
%! n = toposort (G);
%! assert (n, [3, 2, 1]);

## MATLAB-style small DAG with a diamond.
%!test
%! G = digraph ([1 1 2 2 3 4], [2 3 3 4 4 5]);
%! n = toposort (G);
%! assert (n, [1, 2, 3, 4, 5]);

## Tree 1->2, 1->3, 2->4, 2->5: stable/lex gives [1 2 3 4 5].
%!test
%! G = digraph ([1 1 2 2], [2 3 4 5]);
%! n = toposort (G);
%! assert (n, [1, 2, 3, 4, 5]);

## Stable order picks smallest free index first: source 3 is smallest
## indegree-0 before 1 only when 1 has an incoming edge.  Here nodes
## 1..4 with edges 3->1, 3->2, stable picks [3 4 1 2] (4 is indegree 0
## and tied with 3; smallest wins, so 3 first; then 4 tied with {1, 2}
## after 3 is removed, 1 still has indegree from 3... wait).
%!test
%! ## Edges 3->1 and 3->2; node 4 is isolated.  After removing 3 the
%! ## zero-indegree set is {1, 2, 4}, so stable picks 3, then 1, 2, 4.
%! G = digraph ([3 3], [1 2], [], 4);
%! n = toposort (G);
%! assert (n, [3, 1, 2, 4]);

## The result is always a permutation of 1:numnodes.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 5]);
%! n = toposort (G);
%! assert (sort (n), 1:numnodes (G));

## Default is a row vector of class double.
%!test
%! G = digraph ([1 2], [2 3]);
%! n = toposort (G);
%! assert (size (n, 1), 1);
%! assert (class (n), "double");

## -------------------- validity: edge precedence --------------------

## For every edge s->t, s comes before t in the output.
%!test
%! G = digraph ([1 1 2 2 3 4], [2 3 3 4 4 5]);
%! n = toposort (G);
%! E = G.Edges.EndNodes;
%! pos = zeros (1, numnodes (G));
%! pos(n) = 1:numel (n);
%! for k = 1:size (E, 1)
%!   assert (pos(E(k, 1)) < pos(E(k, 2)));
%! endfor

## Edge precedence on a larger 10-node DAG.
%!test
%! s = [1 1 2 3 3 4 4 5 6 6 7 8 8 9];
%! t = [2 3 4 4 5 6 7 7 8 9 9 10 9 10];
%! G = digraph (s, t);
%! n = toposort (G);
%! pos = zeros (1, numnodes (G));
%! pos(n) = 1:numel (n);
%! for k = 1:numel (s)
%!   assert (pos(s(k)) < pos(t(k)));
%! endfor

## Edge precedence with isolated nodes appearing somewhere in the order.
%!test
%! G = digraph ([1 3], [2 4], [], 6);
%! n = toposort (G);
%! ## All six nodes must appear
%! assert (sort (n), 1:6);
%! pos = zeros (1, 6);
%! pos(n) = 1:6;
%! assert (pos(1) < pos(2));
%! assert (pos(3) < pos(4));

## Edge precedence on disconnected components.
%!test
%! G = digraph ([1 2 10 11], [2 3 11 12], [], 12);
%! n = toposort (G);
%! assert (sort (n), 1:12);
%! pos = zeros (1, 12);
%! pos(n) = 1:12;
%! assert (pos(1) < pos(2));
%! assert (pos(2) < pos(3));
%! assert (pos(10) < pos(11));
%! assert (pos(11) < pos(12));

## -------------------- cycle detection --------------------

## A 2-cycle is not a DAG.
%!error <not a DAG>
%! G = digraph ([1 2], [2 1]);
%! toposort (G);

## A 3-cycle is not a DAG.
%!error <not a DAG>
%! G = digraph ([1 2 3], [2 3 1]);
%! toposort (G);

## A self-loop makes the digraph non-DAG.
%!error <not a DAG>
%! G = digraph (1, 1);
%! toposort (G);

## Self-loop on a multi-node digraph is still a cycle.
%!error <not a DAG>
%! G = digraph ([1 2 2], [2 2 3]);
%! toposort (G);

## Cycle in one component; other component is a DAG.  Should still error.
%!error <not a DAG>
%! G = digraph ([1 2 3 4], [2 1 4 5]);
%! toposort (G);

## MATLAB-style error identifier.
%!error <toposort: .*not a DAG>
%! G = digraph ([1 2], [2 1]);
%! toposort (G);

## -------------------- 'Order' option --------------------

## Explicit Order=stable is the same as the default.
%!test
%! G = digraph ([1 1 2 2 3 4], [2 3 3 4 4 5]);
%! n1 = toposort (G);
%! n2 = toposort (G, "Order", "stable");
%! assert (n1, n2);

## Explicit Order=lexicographic.
%!test
%! G = digraph ([1 1 2 2 3 4], [2 3 3 4 4 5]);
%! n = toposort (G, "Order", "lexicographic");
%! assert (n, [1, 2, 3, 4, 5]);

## stable and lexicographic produce the same output.
%!test
%! G = digraph ([1 3 3 2 5], [4 4 2 5 1]);
%! a = toposort (G, "Order", "stable");
%! b = toposort (G, "Order", "lexicographic");
%! assert (a, b);

## Case insensitive Order name.
%!test
%! G = digraph ([1 2], [2 3]);
%! n = toposort (G, "ORDER", "stable");
%! assert (n, [1, 2, 3]);
%! n = toposort (G, "order", "lexicographic");
%! assert (n, [1, 2, 3]);

## Case insensitive Order value.
%!test
%! G = digraph ([1 2], [2 3]);
%! n = toposort (G, "Order", "STABLE");
%! assert (n, [1, 2, 3]);
%! n = toposort (G, "Order", "Lexicographic");
%! assert (n, [1, 2, 3]);

## Stable order breaks ties by smallest index: both 1 and 3 are
## indegree 0 initially (edges 1->2, 3->2), stable picks 1 first.
%!test
%! G = digraph ([1 3], [2 2]);
%! n = toposort (G, "Order", "stable");
%! assert (n, [1, 3, 2]);

## Stable order on {1->3, 2->3, 4->3}: [1 2 4 3] (all three sources are
## tied at indegree 0; stable picks smallest index each time).
%!test
%! G = digraph ([1 2 4], [3 3 3]);
%! n = toposort (G, "Order", "stable");
%! assert (n, [1, 2, 4, 3]);

## -------------------- option error cases --------------------

## Odd number of Name-Value arguments.
%!error <toposort:|pairs|missing>
%! G = digraph ([1 2], [2 3]);
%! toposort (G, "Order");

## Unknown option name.
%!error <unknown option|Order>
%! G = digraph ([1 2], [2 3]);
%! toposort (G, "Bogus", "stable");

## Invalid Order value.
%!error <Order.*stable|lexicographic>
%! G = digraph ([1 2], [2 3]);
%! toposort (G, "Order", "fast");

## Non-char Order name.
%!error <name.*string>
%! G = digraph ([1 2], [2 3]);
%! toposort (G, 7, "stable");

## Non-char Order value.
%!error <Order.*string>
%! G = digraph ([1 2], [2 3]);
%! toposort (G, "Order", 7);

## -------------------- dot notation dispatch --------------------

## G.toposort() returns the same as toposort(G).
%!test
%! G = digraph ([1 1 2 2 3 4], [2 3 3 4 4 5]);
%! n1 = toposort (G);
%! n2 = G.toposort ();
%! assert (n1, n2);

## G.toposort('Order','stable') = toposort(G, 'Order','stable').
%!test
%! G = digraph ([1 1 2 2], [2 3 4 5]);
%! n1 = toposort (G, "Order", "stable");
%! n2 = G.toposort ("Order", "stable");
%! assert (n1, n2);

## -------------------- named nodes --------------------

## Named digraph: output is still numeric indices, not names.
%!test
%! G = digraph ([1 2 3], [2 3 4], [], {"a","b","c","d"});
%! n = toposort (G);
%! assert (class (n), "double");
%! assert (n, [1, 2, 3, 4]);

## Named digraph with a cycle still errors cleanly.
%!error <not a DAG>
%! G = digraph ({"a","b"}, {"b","a"}, [], {"a","b"});
%! toposort (G);

## -------------------- weighted edges irrelevant --------------------

## Edge weights don't affect the topological order.
%!test
%! G = digraph ([1 1 2 2 3 4], [2 3 3 4 4 5], [10 20 30 40 50 60]);
%! n = toposort (G);
%! assert (n, [1, 2, 3, 4, 5]);

## Negative weights also don't affect topological order.
%!test
%! G = digraph ([1 2], [2 3], [-1 -2]);
%! n = toposort (G);
%! assert (n, [1, 2, 3]);

## -------------------- larger DAG --------------------

## MATLAB doc-style example: 20 nodes arranged in a layered DAG.
## Layer 1: {1 2 3 4 5}; layer 2: {6 7 8 9 10}; layer 3: {11..15};
## layer 4: {16..20}.  Each layer-k node has edges to one layer-(k+1)
## node (a permutation).
%!test
%! s = [1 2 3 4 5  6 7 8 9 10  11 12 13 14 15];
%! t = [6 7 8 9 10 11 12 13 14 15  16 17 18 19 20];
%! G = digraph (s, t);
%! n = toposort (G);
%! pos = zeros (1, 20);
%! pos(n) = 1:20;
%! for k = 1:numel (s)
%!   assert (pos(s(k)) < pos(t(k)));
%! endfor
%! assert (sort (n), 1:20);

## Cycle introduced in a larger DAG still errors.
%!error <not a DAG>
%! G = digraph ([1 2 3 4 5 5], [2 3 4 5 6 1]);
%! toposort (G);
