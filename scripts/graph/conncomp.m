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
