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
## @deftypefn  {} {@var{cycles} =} allcycles (@var{G})
## @deftypefnx {} {[@var{cycles}, @var{edgecycles}] =} allcycles (@var{G})
## @deftypefnx {} {[@dots{}] =} allcycles (@dots{}, @var{name}, @var{value})
## Return all elementary cycles of the graph or digraph @var{G}.
##
## A @emph{cycle} (also called @emph{elementary circuit} or
## @emph{simple cycle}) is a closed walk in which no node and no edge
## is repeated, except that the walk starts and ends at the same node.
## A self-loop on a node is a 1-cycle.  For a multigraph, two parallel
## edges between the same pair of distinct nodes form a 2-cycle.
##
## @var{G} must be a @code{graph} or @code{digraph} object.
##
## @var{cycles} is an @var{n}-by-1 column cell array, where @var{n} is
## the number of cycles found.  Each entry @code{@var{cycles}@{@var{k}@}}
## is a row vector of node indices listing the nodes traversed by that
## cycle in order.  The closing node (equal to the first node) is not
## repeated in the listing -- a cycle of length @math{L} is represented
## by @math{L} node entries and @math{L} edge entries.
##
## @var{edgecycles} is an @var{n}-by-1 column cell array of edge-index
## row vectors; @code{@var{edgecycles}@{@var{k}@}(i)} is the edge index
## (in @code{@var{G}.Edges}) of the edge connecting
## @code{@var{cycles}@{@var{k}@}(i)} to
## @code{@var{cycles}@{@var{k}@}(i+1)} (with the wraparound that the
## final edge connects the last node back to the first).
##
## For a directed graph the algorithm enumerates all elementary
## directed circuits using a Johnson-style depth-first search:
## for each starting node @math{s} from 1 to @math{N}, the search is
## restricted to candidate cycles whose smallest node index is
## @math{s}.  Each directed cycle is reported exactly once with its
## smallest node first.
##
## For an undirected graph each elementary cycle is reported exactly
## once.  The chosen orientation visits the smaller of the two
## neighbours of the starting node first, except for 2-cycles formed
## by parallel edges, where the smaller-indexed edge is taken first.
##
## Optional Name-Value pairs restrict which cycles are returned:
##
## @table @asis
## @item @qcode{"MaxNumCycles"}
## Stop the search after this many cycles have been recorded.  Defaults
## to @code{Inf}.  Must be a positive integer scalar (or @code{Inf}).
##
## @item @qcode{"MinCycleLength"}
## Lower bound on the number of nodes (equivalently, edges) in the
## returned cycles.  Defaults to @code{1}.
##
## @item @qcode{"MaxCycleLength"}
## Upper bound on the number of nodes in the returned cycles.  Defaults
## to @code{Inf}.  When finite the bound is also used to prune the
## depth-first search.
## @end table
##
## @example
## @group
## ## A directed 3-cycle.
## G = digraph ([1 2 3], [2 3 1]);
## allcycles (G)
##    @result{}  @{ [1 2 3] @}
##
## ## An undirected square plus a diagonal -> three cycles
## ## (two triangles + one 4-cycle).
## G = graph ([1 1 2 2 3], [2 3 3 4 4]);
## C = allcycles (G)
##    @result{}  C = @{ [1 2 3]; [1 2 4 3]; [2 3 4] @}
## @end group
## @end example
##
## @seealso{graph, digraph, allpaths, conncomp, isdag}
## @end deftypefn

function [cycles, edgecycles] = allcycles (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{allcycles} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help allcycles} works outside the context of
  ## an instance) and as a fallback that gives a helpful error for
  ## non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "allcycles: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a graph/digraph first arg, but route through dot
  ## notation just in case.
  if (nargout <= 1)
    cycles = G.allcycles (varargin{:});
  else
    [cycles, edgecycles] = G.allcycles (varargin{:});
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## allcycles on a non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! allcycles (42);

## allcycles on a non-graph string input is an error.
%!error <must be a graph or digraph object>
%! allcycles ("foo");

## allcycles with no args is an error via print_usage.
%!error allcycles ()

## Unknown Name-Value option errors.
%!error <unknown option>
%! G = digraph ();
%! allcycles (G, "NotAnOption", 5);

## Odd trailing arg count (one Name without a Value) errors.
%!error <Name,Value arguments must appear in pairs>
%! G = digraph ();
%! allcycles (G, "MaxNumCycles");

## Non-numeric MaxNumCycles errors.
%!error <MaxNumCycles>
%! G = digraph ();
%! allcycles (G, "MaxNumCycles", "five");

## Non-scalar MaxNumCycles errors.
%!error <MaxNumCycles>
%! G = digraph ();
%! allcycles (G, "MaxNumCycles", [1 2]);

## Non-positive MaxNumCycles errors.
%!error <MaxNumCycles>
%! G = digraph ();
%! allcycles (G, "MaxNumCycles", 0);

## Negative MaxNumCycles errors.
%!error <MaxNumCycles>
%! G = digraph ();
%! allcycles (G, "MaxNumCycles", -1);

## Non-numeric MinCycleLength errors.
%!error <MinCycleLength>
%! G = digraph ();
%! allcycles (G, "MinCycleLength", "zero");

## Non-scalar MinCycleLength errors.
%!error <MinCycleLength>
%! G = digraph ();
%! allcycles (G, "MinCycleLength", [0 1]);

## Negative MinCycleLength errors.
%!error <MinCycleLength>
%! G = digraph ();
%! allcycles (G, "MinCycleLength", -1);

## Non-numeric MaxCycleLength errors.
%!error <MaxCycleLength>
%! G = digraph ();
%! allcycles (G, "MaxCycleLength", "ten");

## Non-scalar MaxCycleLength errors.
%!error <MaxCycleLength>
%! G = digraph ();
%! allcycles (G, "MaxCycleLength", [3 4]);

## Negative MaxCycleLength errors.
%!error <MaxCycleLength>
%! G = digraph ();
%! allcycles (G, "MaxCycleLength", -2);

## -------------------- empty / trivial graphs --------------------

## Empty digraph -> no cycles (cycles = cell(0,1), edgecycles = cell(0,1)).
%!test
%! G = digraph ();
%! [C, EC] = allcycles (G);
%! assert (iscell (C));
%! assert (size (C), [0, 1]);
%! assert (iscell (EC));
%! assert (size (EC), [0, 1]);

## Empty graph -> no cycles.
%!test
%! G = graph ();
%! [C, EC] = allcycles (G);
%! assert (size (C), [0, 1]);
%! assert (size (EC), [0, 1]);

## Single-node digraph (no self-loop) -> no cycles.
%!test
%! G = digraph (1);
%! C = allcycles (G);
%! assert (size (C), [0, 1]);

## Multi-node edgeless digraph -> no cycles.
%!test
%! G = digraph (5);
%! C = allcycles (G);
%! assert (size (C), [0, 1]);

## Multi-node edgeless graph -> no cycles.
%!test
%! G = graph (5);
%! C = allcycles (G);
%! assert (size (C), [0, 1]);

## DAG digraph (line) -> no cycles.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! C = allcycles (G);
%! assert (size (C), [0, 1]);

## Tree graph (no cycles) -> no cycles.
%!test
%! G = graph ([1 1 2 2], [2 3 4 5]);
%! C = allcycles (G);
%! assert (size (C), [0, 1]);

## -------------------- self-loops (1-cycles) --------------------

## Single self-loop on a single node -> one 1-cycle.
%!test
%! G = digraph (1, 1);
%! [C, EC] = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, 1);
%! assert (EC{1}, 1);

## Self-loops on multiple nodes -> one 1-cycle each.
%!test
%! G = digraph ([1 2 3], [1 2 3]);
%! [C, EC] = allcycles (G);
%! assert (size (C), [3, 1]);
%! assert (sort (cellfun (@(c) c(1), C)), [1; 2; 3]);

## Self-loop on undirected graph -> one 1-cycle.
%!test
%! G = graph (1, 1);
%! [C, EC] = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, 1);
%! assert (EC{1}, 1);

## Mixed: self-loop + DAG edge -> 1 cycle (the self-loop only).
%!test
%! G = digraph ([1 1], [1 2]);
%! C = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, 1);

## -------------------- 2-cycles --------------------

## 2-cycle digraph (1->2, 2->1) -> 1 cycle.
%!test
%! G = digraph ([1 2], [2 1]);
%! [C, EC] = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, [1, 2]);
%! assert (numel (EC{1}), 2);

## 2-cycle in undirected requires parallel edges (single edge is not a cycle).
%!test
%! G = graph (1, 2);
%! C = allcycles (G);
%! assert (size (C), [0, 1]);

## Two parallel edges in digraph multigraph (1->2, 1->2, 2->1)
## -> 2 cycles (one per parallel edge from 1->2, paired with the
## 2->1 return).  Undirected multigraph is not yet implemented for
## the @code{graph} class, so the parallel-edge cases here are
## exercised exclusively via @code{digraph}.
%!test
%! G = digraph ([1 1 2], [2 2 1], "multigraph");
%! C = allcycles (G);
%! assert (size (C), [2, 1]);
%! for k = 1:2
%!   assert (C{k}, [1, 2]);
%! endfor

## Three parallel 1->2 edges + one 2->1 edge -> 3 cycles
## (each parallel forward edge paired with the single return).
%!test
%! G = digraph ([1 1 1 2], [2 2 2 1], "multigraph");
%! C = allcycles (G);
%! assert (size (C), [3, 1]);
%! for k = 1:3
%!   assert (C{k}, [1, 2]);
%! endfor

## Two parallel 1->2 edges + two parallel 2->1 edges -> 4 cycles.
%!test
%! G = digraph ([1 1 2 2], [2 2 1 1], "multigraph");
%! C = allcycles (G);
%! assert (size (C), [4, 1]);
%! for k = 1:4
%!   assert (C{k}, [1, 2]);
%! endfor

## -------------------- 3-cycles --------------------

## Directed 3-cycle 1->2->3->1 -> 1 cycle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [C, EC] = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, [1, 2, 3]);
%! assert (numel (EC{1}), 3);

## Directed 3-cycle reversed orientation: 1->3->2->1 -> 1 cycle [1 3 2].
%!test
%! G = digraph ([1 3 2], [3 2 1]);
%! C = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, [1, 3, 2]);

## Undirected triangle -> 1 cycle in canonical form [1 2 3].
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [C, EC] = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, [1, 2, 3]);
%! assert (numel (EC{1}), 3);

## Two interlocked directed triangles sharing an edge: 1->2->3->1 and
## 1->2->4->1 should give 2 cycles.
%!test
%! G = digraph ([1 2 3 2 4], [2 3 1 4 1]);
%! C = allcycles (G);
%! assert (size (C), [2, 1]);
%! ## Both cycles include node 1; both are length 3.
%! lens = cellfun (@numel, C);
%! assert (sort (lens), [3; 3]);

## -------------------- 4-cycles and longer --------------------

## Directed 4-cycle.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! C = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, [1, 2, 3, 4]);

## Undirected 4-cycle (square).
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! C = allcycles (G);
%! assert (size (C), [1, 1]);
%! ## Canonical: [1 2 3 4] (start at 1, go to smaller neighbour 2 first).
%! assert (C{1}, [1, 2, 3, 4]);

## Undirected K4 has 4 triangles + 3 4-cycles = 7 cycles total.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! C = allcycles (G);
%! assert (size (C), [7, 1]);
%! lens = cellfun (@numel, C);
%! assert (sum (lens == 3), 4);
%! assert (sum (lens == 4), 3);

## Two disjoint directed 3-cycles.
%!test
%! G = digraph ([1 2 3 4 5 6], [2 3 1 5 6 4]);
%! C = allcycles (G);
%! assert (size (C), [2, 1]);
%! lens = cellfun (@numel, C);
%! assert (sort (lens), [3; 3]);

## Two disjoint undirected triangles.
%!test
%! G = graph ([1 2 3 4 5 6], [2 3 1 5 6 4]);
%! C = allcycles (G);
%! assert (size (C), [2, 1]);

## -------------------- length filters --------------------

## MinCycleLength filters out short cycles.
%!test
%! ## Mix: a 2-cycle [1 2] + a 3-cycle [3 4 5].
%! G = digraph ([1 2 3 4 5], [2 1 4 5 3]);
%! C = allcycles (G);
%! assert (size (C), [2, 1]);
%! C2 = allcycles (G, "MinCycleLength", 3);
%! assert (size (C2), [1, 1]);
%! assert (numel (C2{1}), 3);

## MaxCycleLength filters out long cycles.
%!test
%! G = digraph ([1 2 3 4 5], [2 1 4 5 3]);
%! C = allcycles (G, "MaxCycleLength", 2);
%! assert (size (C), [1, 1]);
%! assert (numel (C{1}), 2);

## Min == Max picks exact length.
%!test
%! G = digraph ([1 2 3 4 5], [2 1 4 5 3]);
%! C = allcycles (G, "MinCycleLength", 3, "MaxCycleLength", 3);
%! assert (size (C), [1, 1]);
%! assert (numel (C{1}), 3);

## Min > Max -> empty result.
%!test
%! G = digraph ([1 2 3 4 5], [2 1 4 5 3]);
%! C = allcycles (G, "MinCycleLength", 5, "MaxCycleLength", 3);
%! assert (size (C), [0, 1]);

## Length filter on undirected K4: pick only triangles.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! C = allcycles (G, "MaxCycleLength", 3);
%! assert (size (C), [4, 1]);
%! lens = cellfun (@numel, C);
%! assert (all (lens == 3));

## -------------------- MaxNumCycles --------------------

## MaxNumCycles caps the number of returned cycles.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! C = allcycles (G, "MaxNumCycles", 3);
%! assert (size (C), [3, 1]);

## MaxNumCycles = 1 returns at most one cycle.
%!test
%! G = digraph ([1 2 3 4 5 6], [2 3 1 5 6 4]);
%! C = allcycles (G, "MaxNumCycles", 1);
%! assert (size (C), [1, 1]);

## MaxNumCycles bigger than total -> all returned.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! C = allcycles (G, "MaxNumCycles", 100);
%! assert (size (C), [1, 1]);

## MaxNumCycles == Inf -> all (default behaviour).
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! C = allcycles (G, "MaxNumCycles", Inf);
%! assert (size (C), [7, 1]);

## -------------------- edge index validity --------------------

## edgecycles indices match G.Edges.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! [C, EC] = allcycles (G);
%! assert (size (C), [1, 1]);
%! ## Sum of weights along the returned cycle == 60.
%! W = G.Edges.Weight;
%! assert (sum (W(EC{1})), 60);

## edgecycles consistent with EndNodes for digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [C, EC] = allcycles (G);
%! EN = G.Edges.EndNodes;
%! cyc = C{1};
%! eidx = EC{1};
%! L = numel (cyc);
%! for k = 1:L
%!   src = cyc(k);
%!   dst = cyc(mod (k, L) + 1);
%!   assert (EN(eidx(k), 1), src);
%!   assert (EN(eidx(k), 2), dst);
%! endfor

## edgecycles consistent with EndNodes for undirected (unordered).
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [C, EC] = allcycles (G);
%! EN = G.Edges.EndNodes;
%! cyc = C{1};
%! eidx = EC{1};
%! L = numel (cyc);
%! for k = 1:L
%!   src = cyc(k);
%!   dst = cyc(mod (k, L) + 1);
%!   pair = sort ([src, dst]);
%!   eend = sort (EN(eidx(k), :));
%!   assert (eend, pair);
%! endfor

## -------------------- output shape --------------------

## Outputs always column cell, paths always row vectors.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [C, EC] = allcycles (G);
%! assert (iscolumn (C));
%! assert (iscolumn (EC));
%! assert (isrow (C{1}));
%! assert (isrow (EC{1}));

## numel(cyc) == numel(edgecyc) for every entry.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! [C, EC] = allcycles (G);
%! for k = 1:numel (C)
%!   assert (numel (C{k}), numel (EC{k}));
%! endfor

## -------------------- nargout variants --------------------

## nargout = 0 still permitted (no error).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! allcycles (G);

## nargout = 1 returns just cycles.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! C = allcycles (G);
%! assert (size (C), [1, 1]);

## nargout = 2 returns cycles and edgecycles.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [C, EC] = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (size (EC), [1, 1]);

## -------------------- dot-notation dispatch --------------------

%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! C = G.allcycles ();
%! assert (size (C), [1, 1]);

## -------------------- self-loops mixed with cycles --------------------

## Self-loop + 3-cycle in same digraph -> 2 cycles total.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 1]);
%! C = allcycles (G);
%! assert (size (C), [2, 1]);
%! lens = sort (cellfun (@numel, C));
%! assert (lens, [1; 3]);

## -------------------- larger / mixed sanity --------------------

## Two-node 2-cycle + isolated nodes.
%!test
%! G = digraph ([1 2], [2 1], [], 5);
%! C = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (C{1}, [1, 2]);

## Directed graph with 1->2, 2->3, 3->1, 1->4, 4->2 -> 2 cycles
## of length 3 and 4.
%!test
%! G = digraph ([1 2 3 1 4], [2 3 1 4 2]);
%! C = allcycles (G);
%! assert (size (C), [2, 1]);
%! lens = sort (cellfun (@numel, C));
%! assert (lens, [3; 4]);

## Petersen-like sanity: undirected 5-cycle -> 1 cycle.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! C = allcycles (G);
%! assert (size (C), [1, 1]);
%! assert (numel (C{1}), 5);
%! assert (C{1}(1), 1);

## DAG with named nodes -> still no cycles.
%!test
%! G = digraph ([1 2 3], [2 3 4], [], {"a", "b", "c", "d"});
%! C = allcycles (G);
%! assert (size (C), [0, 1]);
