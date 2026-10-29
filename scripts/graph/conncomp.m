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
## @deftypefn  {} {@var{bins} =} conncomp (@var{G})
## @deftypefnx {} {@var{bins} =} conncomp (@var{G}, @var{name}, @var{value}, @dots{})
## @deftypefnx {} {@var{C} =} conncomp (@dots{}, @qcode{"OutputForm"}, @qcode{"cell"})
## Compute the connected components of the graph or digraph @var{G}.
##
## With no options, return a row vector @var{bins} of length
## @code{numnodes (@var{G})} where @code{@var{bins}(i)} is the 1-based
## component label of node @var{i}.  Components are labelled in the
## order they are first discovered when scanning nodes from 1 upward.
##
## Supported Name-Value options (case-insensitive names):
##
## @itemize
## @item
## @qcode{"Type"} (only meaningful for a @code{digraph}).  Either
## @qcode{"weak"} (default) or @qcode{"strong"}.  @qcode{"weak"}
## treats the digraph as undirected for the purpose of component
## discovery, so two nodes are in the same component whenever they
## are connected by a sequence of edges regardless of direction.
## @qcode{"strong"} runs Tarjan's strongly connected components
## algorithm and two nodes are in the same component iff there is a
## directed path from each to the other.  Passing
## @qcode{"Type"} to a @code{graph} is only allowed for
## @qcode{"weak"}; any other value is an error.
## @item
## @qcode{"OutputForm"}.  Either @qcode{"vector"} (default) or
## @qcode{"cell"}.  @qcode{"vector"} returns the @var{bins} row
## vector described above.  @qcode{"cell"} returns a cell array
## @var{C} of length equal to the number of components, where each
## cell contains the sorted column vector of node indices belonging
## to that component.
## @end itemize
##
## @example
## @group
## G = digraph ([1 2 3 4], [2 3 1 5]);
## conncomp (G)
##    @result{} 1   1   1   2   2
## conncomp (G, "Type", "strong")
##    @result{} 1   1   1   2   3
## @end group
## @end example
##
## @seealso{graph, digraph, bfsearch, dfsearch}
## @end deftypefn

function bins = conncomp (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{conncomp} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help conncomp} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "conncomp: G must be a graph or digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  bins = G.conncomp (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- digraph weak (default) --------------------

## Empty digraph: conncomp returns a 1x0 row vector.
%!test
%! G = digraph ();
%! bins = conncomp (G);
%! assert (size (bins), [1, 0]);
%! assert (class (bins), "double");

## Single isolated node: one component.
%!test
%! G = digraph (1);
%! bins = conncomp (G);
%! assert (bins, 1);
%! assert (size (bins), [1, 1]);

## Three isolated nodes -> 3 components, each labelled in index order.
%!test
%! G = digraph (3);
%! bins = conncomp (G);
%! assert (bins, [1, 2, 3]);

## A 3-cycle is weakly connected: all three nodes share one label.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1]);

## Disconnected directed edge 1->2 plus isolated node 3 -> 2 components.
%!test
%! G = digraph ([1], [2], [], 3);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2]);

## Two disjoint directed edges 1->2 and 3->4 -> 2 components.
%!test
%! G = digraph ([1, 3], [2, 4]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2, 2]);

## Weak: a tree of 5 nodes with directed edges is one weak component.
%!test
%! G = digraph ([1 1 2 2], [2 3 4 5]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1, 1, 1]);

## Weak: nodes 1->2 (directed) plus nodes 3->4 (directed) plus
## isolated node 5 -> 3 components.
%!test
%! G = digraph ([1, 3], [2, 4], [], 5);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2, 2, 3]);

## Weak: back-edges and forward-edges merge components.  Nodes 1<->2
## via a double edge are weakly connected (and strongly connected).
%!test
%! G = digraph ([1, 2], [2, 1]);
%! bins = conncomp (G);
%! assert (bins, [1, 1]);

## conncomp(G) output is always a row vector.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! bins = conncomp (G);
%! assert (size (bins), [1, 3]);
%! assert (isrow (bins));

## conncomp(G) output is always class double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! bins = conncomp (G);
%! assert (class (bins), "double");

## Self-loops don't affect component labels.
%!test
%! G = digraph ([1, 1, 2], [1, 2, 2]);
%! bins = conncomp (G);
%! assert (bins, [1, 1]);

## Weak on a named digraph: names don't matter for labels.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a","b","c"});
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1]);

## -------------------- digraph Type = strong --------------------

## Strong: single node is its own SCC.
%!test
%! G = digraph (1);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, 1);

## Strong: 3-cycle is one SCC (strongly connected).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, [1, 1, 1]);

## Strong: 1->2->3 (line) is 3 SCCs.  Tarjan labels in reverse finish
## order, so the sink is discovered first; but for MATLAB parity we
## relabel so the SCC containing node 1 has label 1, node 2's SCC has
## next unused label, etc. (scan from low index up).
%!test
%! G = digraph ([1, 2], [2, 3]);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, [1, 2, 3]);

## Strong: 1->2->3->1 plus an extra sink 4.  1,2,3 are one SCC,
## 4 is its own.
%!test
%! G = digraph ([1, 2, 3, 4], [2, 3, 1, 4]);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, [1, 1, 1, 2]);

## Strong: disjoint SCCs: {1,2} back and forth; {3,4} back and forth.
## Labels assigned in discovery order (1,2 first, then 3,4).
%!test
%! G = digraph ([1 2 3 4], [2 1 4 3]);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, [1, 1, 2, 2]);

## Strong: MATLAB doc example.  Digraph with edges 1->2->3->1 and
## 4->5 (no back edge).  Weak: {1,2,3,4,5}; Strong: {1,2,3}, {4}, {5}.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! bins_weak = conncomp (G);
%! assert (bins_weak, [1, 1, 1, 2, 2]);
%! bins_strong = conncomp (G, "Type", "strong");
%! assert (bins_strong, [1, 1, 1, 2, 3]);

## Strong: two disconnected 3-cycles -> 2 SCCs of size 3 each.
%!test
%! G = digraph ([1 2 3 4 5 6], [2 3 1 5 6 4]);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, [1, 1, 1, 2, 2, 2]);

## Strong: self-loop alone is its own SCC.
%!test
%! G = digraph ([1], [1]);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, 1);

## Strong: line 1->2->3 has self-loop at 2.  Still 3 SCCs (the
## self-loop doesn't merge 1 and 3).
%!test
%! G = digraph ([1, 2, 2], [2, 2, 3]);
%! bins = conncomp (G, "Type", "strong");
%! assert (bins, [1, 2, 3]);

## Strong: empty digraph.
%!test
%! G = digraph ();
%! bins = conncomp (G, "Type", "strong");
%! assert (size (bins), [1, 0]);

## Strong: explicit "Type","weak" matches default.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! b1 = conncomp (G);
%! b2 = conncomp (G, "Type", "weak");
%! assert (b1, b2);

## Case-insensitive "Type" name and value.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! b1 = conncomp (G, "type", "strong");
%! b2 = conncomp (G, "Type", "STRONG");
%! assert (b1, b2);
%! assert (b1, [1, 1, 1]);

## -------------------- digraph OutputForm = cell --------------------

## Cell form: empty digraph -> empty cell row vector.
%!test
%! G = digraph ();
%! C = conncomp (G, "OutputForm", "cell");
%! assert (iscell (C));
%! assert (isempty (C));

## Cell form: single node -> 1-cell with [1].
%!test
%! G = digraph (1);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (iscell (C));
%! assert (numel (C), 1);
%! assert (C{1}, 1);

## Cell form: 3-cycle -> 1-cell with [1;2;3] (weak default).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (numel (C), 1);
%! assert (C{1}, [1; 2; 3]);

## Cell form: disconnected components, weak default.
%!test
%! G = digraph ([1, 3], [2, 4], [], 5);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (numel (C), 3);
%! assert (C{1}, [1; 2]);
%! assert (C{2}, [3; 4]);
%! assert (C{3}, 5);

## Cell form with Strong.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! C = conncomp (G, "OutputForm", "cell", "Type", "strong");
%! assert (numel (C), 3);
%! assert (C{1}, [1; 2; 3]);
%! assert (C{2}, 4);
%! assert (C{3}, 5);

## OutputForm option order doesn't matter.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! C1 = conncomp (G, "Type", "strong", "OutputForm", "cell");
%! C2 = conncomp (G, "OutputForm", "cell", "Type", "strong");
%! assert (numel (C1), numel (C2));
%! for k = 1:numel (C1)
%!   assert (C1{k}, C2{k});
%! endfor

## Explicit "vector" matches default.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! b1 = conncomp (G);
%! b2 = conncomp (G, "OutputForm", "vector");
%! assert (b1, b2);

## Case-insensitive "OutputForm" name and value.
%!test
%! G = digraph (3);
%! C = conncomp (G, "outputform", "CELL");
%! assert (numel (C), 3);

## -------------------- error cases --------------------

## Unknown option name.
%!error <unknown option|unknown name>
%! G = digraph (3);
%! conncomp (G, "Bogus", "weak");

## Odd number of trailing args.
%!error <Name-Value|expected pairs>
%! G = digraph (3);
%! conncomp (G, "Type");

## Unknown Type value.
%!error <Type.*must be.*weak|strong>
%! G = digraph (3);
%! conncomp (G, "Type", "bogus");

## Unknown OutputForm value.
%!error <OutputForm.*must be.*vector|cell>
%! G = digraph (3);
%! conncomp (G, "OutputForm", "matrix");

## Non-char Name argument.
%!error <must be a string>
%! G = digraph (3);
%! conncomp (G, 42, "weak");

## Non-graph input.
%!error <must be a graph or digraph object>
%! conncomp (42);

## conncomp on graph works with Type='weak' (undirected).  Type='strong'
## on a graph is an error (graphs have no notion of strong connectivity
## distinct from weak).
%!error <Type.*strong.*digraph|must be.*weak>
%! G = graph ([1 2], [2 3]);
%! conncomp (G, "Type", "strong");

## -------------------- graph dispatch --------------------

## conncomp on graph with default: weak component labels.
%!test
%! G = graph ([1, 3], [2, 4], [], 5);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2, 2, 3]);

## conncomp on graph with explicit Type=weak.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! bins = conncomp (G, "Type", "weak");
%! assert (bins, [1, 1, 1]);

## conncomp on graph with OutputForm=cell.
%!test
%! G = graph ([1, 3], [2, 4], [], 5);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (numel (C), 3);
%! assert (C{1}, [1; 2]);
%! assert (C{2}, [3; 4]);
%! assert (C{3}, 5);

## Dot notation dispatch G.conncomp().
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! bins = G.conncomp ();
%! assert (bins, [1, 1, 1, 2, 2]);

## -------------------- US-S02: graph-specific coverage --------------------

## Empty graph: conncomp returns a 1x0 row vector of class double.
%!test
%! G = graph ();
%! bins = conncomp (G);
%! assert (size (bins), [1, 0]);
%! assert (class (bins), "double");

## Single isolated node in a graph: one component.
%!test
%! G = graph (1);
%! bins = conncomp (G);
%! assert (bins, 1);
%! assert (size (bins), [1, 1]);

## Three isolated nodes -> three components labelled in index order.
%!test
%! G = graph (3);
%! bins = conncomp (G);
%! assert (bins, [1, 2, 3]);

## Single undirected edge 1-2 connects two nodes: one component.
%!test
%! G = graph ([1], [2]);
%! bins = conncomp (G);
%! assert (bins, [1, 1]);

## Undirected path 1-2-3: one component.
%!test
%! G = graph ([1 2], [2 3]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1]);

## Undirected triangle 1-2, 2-3, 3-1: one component.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1]);

## Undirected tree (star with centre 1 and leaves 2..5): one component.
%!test
%! G = graph ([1 1 1 1], [2 3 4 5]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1, 1, 1]);

## Undirected path 1-2-3-4-5: one component.
%!test
%! G = graph ([1 2 3 4], [2 3 4 5]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1, 1, 1]);

## Two disconnected undirected edges 1-2 and 3-4: two components.
%!test
%! G = graph ([1 3], [2 4]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2, 2]);

## Disconnected edges plus an isolated node: three components.
%!test
%! G = graph ([1 3], [2 4], [], 5);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2, 2, 3]);

## Multiple isolated nodes scattered between connected pairs.
%!test
%! G = graph ([1 4], [2 5], [], 6);
%! bins = conncomp (G);
%! ## Nodes 1-2 are one component, 3 isolated, 4-5 another, 6 isolated.
%! assert (bins, [1, 1, 2, 3, 3, 4]);

## Self-loop on a graph node does not affect connectivity labels.
%!test
%! G = graph ([1 1 2], [1 2 3]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1]);

## Self-loop alone (single node with a self-loop) is still one component.
%!test
%! G = graph ([1], [1]);
%! bins = conncomp (G);
%! assert (bins, 1);

## Node names do not affect component labels for a graph.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1]);
%! assert (numel (bins), numnodes (G));

## Isolated named node preserved as its own component.
%!test
%! G = graph ([1], [2], [], {"a", "b", "c"});
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2]);

## Edge weights do not affect component labels.
%!test
%! G = graph ([1 2], [2 3], [0.1 100]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1]);

## Negative edge weights do not affect component labels.
%!test
%! G = graph ([1 3], [2 4], [-5 -0.001]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 2, 2]);

## conncomp(G) on a graph returns a row vector whose length equals numnodes.
%!test
%! G = graph ([1 2 4], [2 3 5], [], 7);
%! bins = conncomp (G);
%! assert (size (bins), [1, numnodes(G)]);
%! assert (isrow (bins));

## conncomp(G) on a graph always returns class double.
%!test
%! G = graph ([1 2], [2 3]);
%! bins = conncomp (G);
%! assert (class (bins), "double");

## Every node label is in the range [1, K] where K is the number of components.
%!test
%! G = graph ([1 3 5], [2 4 6], [], 7);
%! bins = conncomp (G);
%! assert (all (bins >= 1));
%! assert (max (bins) == numel (unique (bins)));
%! assert (max (bins), 4);

## Explicit "vector" matches default on a graph.
%!test
%! G = graph ([1 2 4], [2 3 5], [], 6);
%! b1 = conncomp (G);
%! b2 = conncomp (G, "OutputForm", "vector");
%! assert (b1, b2);

## OutputForm=cell on an empty graph returns an empty cell row vector.
%!test
%! G = graph ();
%! C = conncomp (G, "OutputForm", "cell");
%! assert (iscell (C));
%! assert (size (C), [1, 0]);

## OutputForm=cell on a single-node graph returns {[1]}.
%!test
%! G = graph (1);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (iscell (C));
%! assert (numel (C), 1);
%! assert (C{1}, 1);

## OutputForm=cell on a connected triangle returns a single cell [1;2;3].
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (numel (C), 1);
%! assert (C{1}, [1; 2; 3]);
%! ## Each component cell is a column vector.
%! assert (size (C{1}, 2), 1);

## OutputForm=cell on an N-isolates graph returns N singleton column vectors.
%!test
%! G = graph (4);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (numel (C), 4);
%! for k = 1:4
%!   assert (C{k}, k);
%!   assert (size (C{k}), [1, 1]);
%! endfor

## OutputForm=cell component contents are sorted column vectors of doubles.
%!test
%! G = graph ([1 2 5], [3 4 6], [], 7);
%! C = conncomp (G, "OutputForm", "cell");
%! for k = 1:numel (C)
%!   assert (size (C{k}, 2), 1);          # column vector
%!   assert (class (C{k}), "double");
%!   assert (all (diff (C{k}) >= 0));     # sorted ascending
%! endfor

## OutputForm=cell: sum of component sizes equals numnodes.
%!test
%! G = graph ([1 3 5], [2 4 6], [], 8);
%! C = conncomp (G, "OutputForm", "cell");
%! total = 0;
%! for k = 1:numel (C)
%!   total = total + numel (C{k});
%! endfor
%! assert (total, numnodes (G));

## Case-insensitive OutputForm name and value on a graph.
%!test
%! G = graph ([1 2], [2 3]);
%! C = conncomp (G, "outputform", "CELL");
%! assert (numel (C), 1);
%! assert (C{1}, [1; 2; 3]);

## Case-insensitive "Type","weak" on a graph.
%!test
%! G = graph ([1 3], [2 4]);
%! b1 = conncomp (G, "type", "WEAK");
%! b2 = conncomp (G);
%! assert (b1, b2);

## Dot-notation dispatch G.conncomp() on a graph works.
%!test
%! G = graph ([1 3], [2 4], [], 5);
%! bins = G.conncomp ();
%! assert (bins, [1, 1, 2, 2, 3]);

## Dot-notation dispatch with OutputForm="cell" on a graph works.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! C = G.conncomp ("OutputForm", "cell");
%! assert (numel (C), 1);
%! assert (C{1}, [1; 2; 3]);

## graph: Type="strong" is rejected even with OutputForm specified.
%!error <Type.*strong.*digraph|must be.*weak>
%! G = graph ([1 2], [2 3]);
%! conncomp (G, "OutputForm", "cell", "Type", "strong");

## graph: Type="strong" is rejected with mixed case.
%!error <Type.*strong.*digraph|must be.*weak>
%! G = graph ([1 2], [2 3]);
%! conncomp (G, "Type", "STRONG");

## MATLAB-parity example: graph with 5 nodes, edges (1,2),(2,3),(4,5).
## weak components: {1,2,3} and {4,5}.
%!test
%! G = graph ([1 2 4], [2 3 5]);
%! bins = conncomp (G);
%! assert (bins, [1, 1, 1, 2, 2]);
%! C = conncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2; 3]);
%! assert (C{2}, [4; 5]);

## Scaling: 10 disconnected undirected edges -> 10 components, 20 nodes.
%!test
%! s = 1:2:19;
%! t = 2:2:20;
%! G = graph (s, t);
%! bins = conncomp (G);
%! assert (numel (bins), 20);
%! assert (max (bins), 10);
%! ## Every consecutive pair shares a label.
%! assert (all (bins(1:2:end) == bins(2:2:end)));
