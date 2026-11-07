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
## @deftypefn  {} {@var{bins} =} biconncomp (@var{G})
## @deftypefnx {} {[@var{bins}, @var{iC}] =} biconncomp (@var{G})
## @deftypefnx {} {@var{C} =} biconncomp (@var{G}, @qcode{"OutputForm"}, @qcode{"cell"})
## @deftypefnx {} {[@var{C}, @var{iC}] =} biconncomp (@var{G}, @qcode{"OutputForm"}, @qcode{"cell"})
## Compute the biconnected components of the undirected graph @var{G}.
##
## @var{G} must be a @code{graph} object.  Biconnected components are
## not defined on the directed @code{digraph} class; calling
## @code{biconncomp} on a @code{digraph} raises an error.
##
## A biconnected component (BCC) is a maximal connected subgraph that
## remains connected after the removal of any single node.  Every edge
## belongs to exactly one BCC; articulation points (@dfn{cut vertices})
## may belong to multiple BCCs.
##
## With no options, return a row vector @var{bins} of length
## @code{numedges (@var{G})} where @code{@var{bins}(i)} is the 1-based
## BCC label of edge @math{i} (in the same lexicographic order used by
## @code{G.Edges}).  Labels are assigned in the order BCCs are first
## discovered by a depth-first search starting from the smallest
## unvisited node index.
##
## When called with two outputs, @code{[@var{bins}, @var{iC}]} also
## returns @var{iC}, a row vector of node indices that are articulation
## points of @var{G}.
##
## Supported Name-Value options (case-insensitive names and values):
##
## @itemize
## @item
## @qcode{"OutputForm"}.  Either @qcode{"vector"} (default) or
## @qcode{"cell"}.  @qcode{"vector"} returns the @var{bins} row vector
## described above.  @qcode{"cell"} returns a row cell array @var{C}
## whose elements are sorted column vectors of node indices, one per
## BCC.  Isolated nodes appear as singleton cells (each trivial
## one-node subgraph is its own BCC in the cell form); self-loops
## appear together with their single incident node.  Articulation
## points appear in multiple cells.
## @end itemize
##
## @example
## @group
## G = graph ([1 2 3 3], [2 3 1 4]);   # triangle 1-2-3 plus bridge 3-4
## biconncomp (G)
##    @result{} 1   1   1   2
## [~, iC] = biconncomp (G)
##    @result{} iC = 3
## biconncomp (G, "OutputForm", "cell")
##    @result{} @{[1; 2; 3], [3; 4]@}
## @end group
## @end example
##
## @seealso{graph, conncomp, bfsearch, dfsearch}
## @end deftypefn

function [out, iC] = biconncomp (G, varargin)

  ## NOTE: When called with a graph object, Octave's classdef method
  ## dispatch runs the class-internal @code{biconncomp} method and this
  ## free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help biconncomp} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "biconncomp: not defined for a digraph; biconncomp requires an undirected graph");
  endif

  if (! isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "biconncomp: G must be a graph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph first arg, but route through dot notation just
  ## in case.
  if (nargout <= 1)
    out = G.biconncomp (varargin{:});
  else
    [out, iC] = G.biconncomp (varargin{:});
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## biconncomp on a digraph is an error.
%!error <not defined for a digraph>
%! G = digraph ([1 2], [2 3]);
%! biconncomp (G);

## Alternate digraph error message mention of undirected graph.
%!error <undirected graph>
%! G = digraph ();
%! biconncomp (G);

## biconncomp on a non-graph input is an error.
%!error <must be a graph object>
%! biconncomp (42);

%!error <must be a graph object>
%! biconncomp ("foo");

## biconncomp with no args is an error via print_usage.
%!error biconncomp ()

## -------------------- vector form (default) --------------------

## Empty graph: biconncomp returns a 1x0 row vector of class double.
%!test
%! G = graph ();
%! bins = biconncomp (G);
%! assert (size (bins), [1, 0]);
%! assert (class (bins), "double");

## Single isolated node: no edges, returns a 1x0 row vector.
%!test
%! G = graph (1);
%! bins = biconncomp (G);
%! assert (size (bins), [1, 0]);
%! assert (class (bins), "double");

## Three isolated nodes: no edges, returns a 1x0 row vector.
%!test
%! G = graph (3);
%! bins = biconncomp (G);
%! assert (size (bins), [1, 0]);

## Single undirected edge 1-2: one BCC of size 1 (bridge).
%!test
%! G = graph ([1], [2]);
%! bins = biconncomp (G);
%! assert (bins, 1);
%! assert (size (bins), [1, 1]);

## Path 1-2-3: two bridges, two BCCs.  Edges in lex order are (1,2) and
## (2,3); the DFS starts at 1, so (1,2) is label 1, (2,3) is label 2.
%!test
%! G = graph ([1 2], [2 3]);
%! bins = biconncomp (G);
%! assert (bins, [1, 2]);
%! assert (isrow (bins));

## Path 1-2-3-4: three bridges, three BCCs.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! bins = biconncomp (G);
%! assert (bins, [1, 2, 3]);

## Triangle 1-2, 2-3, 3-1: single BCC of size 3 (cycle has no
## articulation points).
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1]);

## Star with centre 1 and leaves 2..5: four bridges, four BCCs.  The
## centre is an articulation point.
%!test
%! G = graph ([1 1 1 1], [2 3 4 5]);
%! bins = biconncomp (G);
%! assert (bins, [1, 2, 3, 4]);

## K4 (complete graph on 4 nodes): one BCC containing all 6 edges.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1, 1, 1, 1]);

## Lollipop: triangle 1-2-3-1 plus bridge 3-4.  Two BCCs: the triangle
## (edges in lex order (1,2), (1,3), (2,3)) then the bridge (3,4).
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1, 2]);

## Bowtie: two triangles sharing node 3.  Two BCCs of size 3 each.
## Edges lex: (1,2),(1,3),(2,3),(3,4),(3,5),(4,5).
%!test
%! G = graph ([1 1 2 3 3 4], [2 3 3 4 5 5]);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1, 2, 2, 2]);

## Two disjoint triangles: two BCCs of size 3 each.  Edges lex:
## (1,2),(1,3),(2,3),(4,5),(4,6),(5,6).
%!test
%! G = graph ([1 1 2 4 4 5], [2 3 3 5 6 6]);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1, 2, 2, 2]);

## Two triangles connected by a bridge: three BCCs (triangle, bridge,
## triangle).  Edges lex: (1,2),(1,3),(2,3),(3,4),(4,5),(4,6),(5,6).
%!test
%! G = graph ([1 1 2 3 4 4 5], [2 3 3 4 5 6 6]);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1, 2, 3, 3, 3]);

## Two disconnected edges 1-2 and 3-4: two BCCs (bridges).
%!test
%! G = graph ([1 3], [2 4]);
%! bins = biconncomp (G);
%! assert (bins, [1, 2]);

## Disconnected edge + isolated node: one BCC (just the edge).  The
## isolated node does not appear in bins (no edge to label).
%!test
%! G = graph ([1], [2], [], 3);
%! bins = biconncomp (G);
%! assert (bins, 1);
%! assert (size (bins), [1, 1]);

## Disconnected triangle + isolated node 4: one BCC.
%!test
%! G = graph ([1 2 3], [2 3 1], [], 4);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1]);

## Self-loop alone (graph with only a self-loop on node 1): one BCC
## containing the self-loop edge.
%!test
%! G = graph ([1], [1]);
%! bins = biconncomp (G);
%! assert (bins, 1);
%! assert (size (bins), [1, 1]);

## Self-loop plus a real edge: two BCCs.  The self-loop is its own BCC;
## the bridge 1-2 is another.  Lex order puts (1,1) before (1,2).
%!test
%! G = graph ([1 1], [1 2]);
%! bins = biconncomp (G);
%! assert (bins, [1, 2]);

## Two self-loops: two BCCs.
%!test
%! G = graph ([1 2], [1 2]);
%! bins = biconncomp (G);
%! assert (bins, [1, 2]);

## Long path 1-2-3-4-5 with a triangle at the end (4-5, 5-6, 6-4).
## Edges lex: (1,2),(2,3),(3,4),(4,5),(4,6),(5,6).
## 3 bridges + 1 triangle = 4 BCCs.
%!test
%! G = graph ([1 2 3 4 5 4], [2 3 4 5 6 6]);
%! bins = biconncomp (G);
%! assert (bins, [1, 2, 3, 4, 4, 4]);

## Result is always class double.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! bins = biconncomp (G);
%! assert (class (bins), "double");

## Result is always a row vector.
%!test
%! G = graph ([1 2], [2 3]);
%! bins = biconncomp (G);
%! assert (isrow (bins));

## Bin labels form a contiguous range 1..K.
%!test
%! G = graph ([1 2 3 4 5 4], [2 3 4 5 6 6]);
%! bins = biconncomp (G);
%! K = max (bins);
%! assert (sort (unique (bins)), 1:K);

## Edge weights do not affect BCC labels.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4], [10 20 30 40]);
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1, 2]);

## Named nodes: labels do not depend on names.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4], [], {"a","b","c","d"});
%! bins = biconncomp (G);
%! assert (bins, [1, 1, 1, 2]);

## -------------------- articulation points (iC) --------------------

## Empty graph: no articulation points, empty row vector.
%!test
%! G = graph ();
%! [~, iC] = biconncomp (G);
%! assert (size (iC), [1, 0]);
%! assert (class (iC), "double");

## Single node: no articulation points.
%!test
%! G = graph (1);
%! [~, iC] = biconncomp (G);
%! assert (size (iC), [1, 0]);

## Single edge 1-2: no articulation points (removing either leaves a
## single node, which is trivially connected).
%!test
%! G = graph ([1], [2]);
%! [~, iC] = biconncomp (G);
%! assert (size (iC), [1, 0]);

## Path 1-2-3: node 2 is an articulation point.
%!test
%! G = graph ([1 2], [2 3]);
%! [~, iC] = biconncomp (G);
%! assert (iC, 2);
%! assert (isrow (iC));

## Path 1-2-3-4: nodes 2 and 3 are articulation points.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! [~, iC] = biconncomp (G);
%! assert (iC, [2, 3]);

## Triangle: no articulation points.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [~, iC] = biconncomp (G);
%! assert (size (iC), [1, 0]);

## Star with centre 1, 4 leaves: centre (node 1) is an articulation point.
%!test
%! G = graph ([1 1 1 1], [2 3 4 5]);
%! [~, iC] = biconncomp (G);
%! assert (iC, 1);

## Lollipop: triangle 1-2-3-1 + bridge 3-4.  Node 3 is the articulation.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! [~, iC] = biconncomp (G);
%! assert (iC, 3);

## Bowtie: two triangles sharing node 3.  Node 3 is articulation.
%!test
%! G = graph ([1 1 2 3 3 4], [2 3 3 4 5 5]);
%! [~, iC] = biconncomp (G);
%! assert (iC, 3);

## Two triangles connected by a bridge (nodes 3 and 4 are articulation
## points).
%!test
%! G = graph ([1 1 2 3 4 4 5], [2 3 3 4 5 6 6]);
%! [~, iC] = biconncomp (G);
%! assert (iC, [3, 4]);

## K4: no articulation points.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! [~, iC] = biconncomp (G);
%! assert (size (iC), [1, 0]);

## Self-loop alone: no articulation point.
%!test
%! G = graph ([1], [1]);
%! [~, iC] = biconncomp (G);
%! assert (size (iC), [1, 0]);

## Disjoint components: articulation points from each component are
## merged (no articulations in either 1-2 or 3-4).
%!test
%! G = graph ([1 3], [2 4]);
%! [~, iC] = biconncomp (G);
%! assert (size (iC), [1, 0]);

## iC is sorted ascending.
%!test
%! G = graph ([1 1 2 3 4 4 5], [2 3 3 4 5 6 6]);
%! [~, iC] = biconncomp (G);
%! assert (iC, sort (iC));

## iC is row vector of class double.
%!test
%! G = graph ([1 2], [2 3]);
%! [~, iC] = biconncomp (G);
%! assert (class (iC), "double");
%! assert (isrow (iC));

## -------------------- OutputForm = "cell" --------------------

## Empty graph: cell form returns an empty row cell array.
%!test
%! G = graph ();
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (iscell (C));
%! assert (size (C), [1, 0]);

## Single isolated node: one cell containing [1].
%!test
%! G = graph (1);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (iscell (C));
%! assert (numel (C), 1);
%! assert (C{1}, 1);
%! assert (size (C{1}), [1, 1]);

## Three isolated nodes: three singleton cells.
%!test
%! G = graph (3);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 3);
%! for k = 1:3
%!   assert (C{k}, k);
%! endfor

## Single edge 1-2: one cell with [1;2].
%!test
%! G = graph ([1], [2]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 1);
%! assert (C{1}, [1; 2]);

## Path 1-2-3: two cells {[1;2],[2;3]} -- node 2 in both.
%!test
%! G = graph ([1 2], [2 3]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2]);
%! assert (C{2}, [2; 3]);

## Triangle: one cell with [1;2;3].
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 1);
%! assert (C{1}, [1; 2; 3]);

## Lollipop: triangle + bridge.  Two cells {[1;2;3],[3;4]}.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2; 3]);
%! assert (C{2}, [3; 4]);

## Bowtie: two triangles sharing node 3.  Two cells.
%!test
%! G = graph ([1 1 2 3 3 4], [2 3 3 4 5 5]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2; 3]);
%! assert (C{2}, [3; 4; 5]);

## Disconnected: single edge + isolated node.  Two cells: the edge BCC
## and the isolated node.
%!test
%! G = graph ([1], [2], [], 3);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2]);
%! assert (C{2}, 3);

## Disconnected: triangle + isolated node 4.  Two cells.
%!test
%! G = graph ([1 2 3], [2 3 1], [], 4);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2; 3]);
%! assert (C{2}, 4);

## Two disjoint edges: two cells.
%!test
%! G = graph ([1 3], [2 4]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2]);
%! assert (C{2}, [3; 4]);

## Self-loop alone: one cell with [1].
%!test
%! G = graph ([1], [1]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 1);
%! assert (C{1}, 1);

## Self-loop + bridge 1-2: two cells: [1] and [1;2].
%!test
%! G = graph ([1 1], [1 2]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (C{1}, 1);
%! assert (C{2}, [1; 2]);

## Every cell is a sorted column vector of doubles.
%!test
%! G = graph ([1 1 2 3 3 4 5 5 6], [2 3 3 4 5 5 6 7 7]);
%! C = biconncomp (G, "OutputForm", "cell");
%! for k = 1:numel (C)
%!   assert (size (C{k}, 2), 1);
%!   assert (class (C{k}), "double");
%!   assert (all (diff (C{k}) > 0));    # strictly ascending (no dupes within cell)
%! endfor

## Cell output is a row cell array.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! C = biconncomp (G, "OutputForm", "cell");
%! assert (size (C, 1), 1);

## Cell + two outputs: second output is iC.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! [C, iC] = biconncomp (G, "OutputForm", "cell");
%! assert (numel (C), 2);
%! assert (iC, 3);

## Explicit OutputForm = "vector" matches default.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! b1 = biconncomp (G);
%! b2 = biconncomp (G, "OutputForm", "vector");
%! assert (b1, b2);

## Case-insensitive OutputForm name and value.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! C = biconncomp (G, "outputform", "CELL");
%! assert (numel (C), 2);
%! assert (C{1}, [1; 2; 3]);
%! assert (C{2}, [3; 4]);

## Dot notation dispatch G.biconncomp() works.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! bins = G.biconncomp ();
%! assert (bins, [1, 1, 1, 2]);

## Dot notation dispatch with OutputForm = "cell" works.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! C = G.biconncomp ("OutputForm", "cell");
%! assert (numel (C), 2);

## Dot notation with two outputs works.
%!test
%! G = graph ([1 2 3 3], [2 3 1 4]);
%! [bins, iC] = G.biconncomp ();
%! assert (bins, [1, 1, 1, 2]);
%! assert (iC, 3);

## -------------------- option error cases --------------------

## Unknown option name.
%!error <unknown option|unknown name>
%! G = graph ([1 2], [2 3]);
%! biconncomp (G, "Bogus", "cell");

## Odd number of Name-Value args.
%!error <Name-Value|expected pairs>
%! G = graph ([1 2], [2 3]);
%! biconncomp (G, "OutputForm");

## Unknown OutputForm value.
%!error <OutputForm.*must be.*vector|cell>
%! G = graph ([1 2], [2 3]);
%! biconncomp (G, "OutputForm", "matrix");

## Non-char Name argument.
%!error <must be a string>
%! G = graph ([1 2], [2 3]);
%! biconncomp (G, 42, "cell");
