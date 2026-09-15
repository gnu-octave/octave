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
## @deftypefn  {} {@var{P} =} allpaths (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@var{P}, @var{d}] =} allpaths (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@var{P}, @var{d}, @var{edgepaths}] =} allpaths (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@dots{}] =} allpaths (@dots{}, @var{name}, @var{value})
## Return all simple paths from node @var{s} to node @var{t} in the
## graph or digraph @var{G}.
##
## A @emph{simple path} is a path that does not visit any node more
## than once.  Self-loops are ignored -- a self-loop on any node never
## appears in a returned path.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{s}
## and @var{t} are scalar node identifiers: a positive integer node
## index, a character row vector naming a node, or a 1-element cell
## array of strings naming a node.  When @var{G} has node names a mix
## of numeric and name identifiers is allowed.
##
## @var{P} is an @var{n}-by-1 column cell array of node paths, where
## @var{n} is the number of simple paths found.  Each entry
## @code{@var{P}@{@var{k}@}} is a 1-by-@var{k} row vector identifying
## the nodes visited on that path in order.  When @var{s} and @var{t}
## are both numeric the entries are numeric row vectors; when either
## @var{s} or @var{t} is given as a node name the entries are cellstr
## row vectors of the corresponding node names.
##
## @var{d} is an @var{n}-by-1 column vector giving the total weight of
## each returned path.  For an unweighted graph, @var{d}(@var{k}) is
## the number of edges on the path (@code{numel (@var{P}@{@var{k}@})
## - 1}).  For a weighted graph, @var{d}(@var{k}) is the sum of the
## edge weights along the path.
##
## @var{edgepaths} is an @var{n}-by-1 column cell array of edge-index
## row vectors, each identifying the edges of
## @code{@var{G}.Edges} traversed by the corresponding path.  For a
## multigraph, @var{edgepaths} distinguishes parallel edges between
## the same pair of endpoints: each parallel edge contributes a
## separate entry in @var{P} / @var{d} / @var{edgepaths}, even though
## the node sequence is the same.
##
## When @var{t} is not reachable from @var{s}, or when @code{@var{s}
## == @var{t}}, the outputs are empty: @var{P} is @code{cell (0, 1)},
## @var{d} is @code{zeros (0, 1)}, and @var{edgepaths} is @code{cell
## (0, 1)}.
##
## Optional Name-Value pairs restrict which paths are returned:
##
## @table @asis
## @item @qcode{"MaxPathLength"}
## Upper bound on the total weight of returned paths; any path whose
## weight exceeds @var{MaxPathLength} is dropped.  Defaults to
## @code{Inf}.  For unweighted graphs this is an upper bound on the
## number of edges.  The bound is also used to prune the DFS, which
## can reduce run time on dense graphs with a finite cut-off.
##
## @item @qcode{"MinPathLength"}
## Lower bound on the total weight of returned paths; any path whose
## weight is less than @var{MinPathLength} is dropped.  Defaults to
## @code{0}.
## @end table
##
## For an undirected @code{graph}, edges may be traversed in either
## direction.  For a @code{digraph}, paths must follow edge direction.
##
## @example
## @group
## G = digraph ([1 1 2], [2 3 3]);
## P = allpaths (G, 1, 3)
##    @result{}  P = @{ [1 3]; [1 2 3] @}
##
## [P, d] = allpaths (G, 1, 3)
##    @result{}  P = @{ [1 3]; [1 2 3] @}
##    @result{}  d = [1; 2]
## @end group
## @end example
##
## @seealso{graph, digraph, shortestpath, shortestpathtree, allcycles}
## @end deftypefn

function [P, d, edgepaths] = allpaths (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{allpaths} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help allpaths} works outside the context of
  ## an instance) and as a fallback that gives a helpful error for
  ## non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "allpaths: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a graph/digraph first arg, but route through dot
  ## notation just in case.
  if (nargout <= 1)
    P = G.allpaths (varargin{:});
  elseif (nargout == 2)
    [P, d] = G.allpaths (varargin{:});
  else
    [P, d, edgepaths] = G.allpaths (varargin{:});
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## allpaths on a non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! allpaths (42, 1, 2);

## allpaths on a non-graph string input is an error.
%!error <must be a graph or digraph object>
%! allpaths ("foo", 1, 2);

## allpaths with no args is an error via print_usage.
%!error allpaths ()

## allpaths with G alone is an error (missing s and t).
%!error allpaths (digraph ())

## allpaths with G and s only is an error (missing t).
%!error allpaths (digraph (3), 1)

## Unknown Name-Value option errors.
%!error <unknown option>
%! G = digraph (3);
%! allpaths (G, 1, 2, "NotAnOption", 5);

## Odd trailing arg count (one Name without a Value) errors.
%!error <Name,Value arguments must appear in pairs>
%! G = digraph (3);
%! allpaths (G, 1, 2, "MaxPathLength");

## Non-numeric MaxPathLength errors.
%!error <MaxPathLength>
%! G = digraph (3);
%! allpaths (G, 1, 2, "MaxPathLength", "five");

## Non-scalar MaxPathLength errors.
%!error <MaxPathLength>
%! G = digraph (3);
%! allpaths (G, 1, 2, "MaxPathLength", [1 2]);

## Negative MaxPathLength errors.
%!error <MaxPathLength>
%! G = digraph (3);
%! allpaths (G, 1, 2, "MaxPathLength", -1);

## Non-numeric MinPathLength errors.
%!error <MinPathLength>
%! G = digraph (3);
%! allpaths (G, 1, 2, "MinPathLength", "zero");

## Non-scalar MinPathLength errors.
%!error <MinPathLength>
%! G = digraph (3);
%! allpaths (G, 1, 2, "MinPathLength", [0 1]);

## Negative MinPathLength errors.
%!error <MinPathLength>
%! G = digraph (3);
%! allpaths (G, 1, 2, "MinPathLength", -1);

## -------------------- source == target --------------------

## s == t on a single-node digraph: no simple paths returned (empty).
%!test
%! G = digraph (1);
%! [P, d, ep] = allpaths (G, 1, 1);
%! assert (iscell (P));
%! assert (size (P), [0, 1]);
%! assert (size (d), [0, 1]);
%! assert (isa (d, "double"));
%! assert (size (ep), [0, 1]);
%! assert (iscell (ep));

## s == t on a multi-node digraph: same empty outputs.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P, d, ep] = allpaths (G, 2, 2);
%! assert (size (P), [0, 1]);
%! assert (size (d), [0, 1]);
%! assert (size (ep), [0, 1]);

## s == t on an undirected graph: empty too (no simple s->s path).
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! P = allpaths (G, 1, 1);
%! assert (size (P), [0, 1]);

## -------------------- unreachable --------------------

## Edgeless 3-node digraph: no path from 1 to 2.
%!test
%! G = digraph (3);
%! [P, d, ep] = allpaths (G, 1, 2);
%! assert (size (P), [0, 1]);
%! assert (size (d), [0, 1]);
%! assert (size (ep), [0, 1]);

## Edgeless 3-node graph: no path from 1 to 2.
%!test
%! G = graph (3);
%! P = allpaths (G, 1, 2);
%! assert (size (P), [0, 1]);

## Two disjoint components (digraph): no path across components.
%!test
%! G = digraph ([1 3], [2 4]);
%! P = allpaths (G, 1, 3);
%! assert (size (P), [0, 1]);

## Two disjoint components (graph): no path across components.
%!test
%! G = graph ([1 3], [2 4]);
%! P = allpaths (G, 1, 3);
%! assert (size (P), [0, 1]);

## Directed 1->2: reverse 2->1 is unreachable.
%!test
%! G = digraph (1, 2);
%! [P, d, ep] = allpaths (G, 2, 1);
%! assert (size (P), [0, 1]);
%! assert (size (d), [0, 1]);
%! assert (size (ep), [0, 1]);

## -------------------- single-edge cases --------------------

## Single directed edge 1->2: one path [1 2], d=1, ep=[1].
%!test
%! G = digraph (1, 2);
%! [P, d, ep] = allpaths (G, 1, 2);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 2]);
%! assert (d, 1);
%! assert (numel (ep), 1);
%! assert (ep{1}, 1);

## Single undirected edge 1--2: one path forward, one path reverse
## (tested separately).
%!test
%! G = graph (1, 2);
%! [P, d, ep] = allpaths (G, 1, 2);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 2]);
%! assert (d, 1);
%! assert (ep{1}, 1);

## Reverse on undirected edge: [2 1].
%!test
%! G = graph (1, 2);
%! [P, d, ep] = allpaths (G, 2, 1);
%! assert (P{1}, [2, 1]);
%! assert (d, 1);

## -------------------- multiple paths: triangle --------------------

## Directed triangle 1->2, 1->3, 2->3: two paths from 1 to 3.
##   Direct:   [1 3], d=1
##   Indirect: [1 2 3], d=2
## MATLAB parity: paths are returned in DFS order (direct first when
## successors are tried in ascending index order), but the set of
## paths is order-independent, so we sort by length for the assertion.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! [P, d, ep] = allpaths (G, 1, 3);
%! assert (numel (P), 2);
%! ## sort-by-length for order-independent comparison
%! lens = cellfun ("numel", P);
%! [~, order] = sort (lens);
%! Ps = P(order);  ds = d(order);  eps = ep(order);
%! assert (Ps{1}, [1, 3]);
%! assert (ds(1), 1);
%! assert (numel (eps{1}), 1);
%! assert (Ps{2}, [1, 2, 3]);
%! assert (ds(2), 2);
%! assert (numel (eps{2}), 2);

## Undirected triangle 1--2, 2--3, 1--3: two simple paths from 1 to 3.
%!test
%! G = graph ([1 2 1], [2 3 3]);
%! P = allpaths (G, 1, 3);
%! assert (numel (P), 2);
%! lens = cellfun ("numel", P);
%! [~, order] = sort (lens);
%! Ps = P(order);
%! assert (Ps{1}, [1, 3]);
%! assert (Ps{2}, [1, 2, 3]);

## -------------------- weighted path total distance --------------------

## Weighted directed triangle: d(k) = sum of weights along path k.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 1 7]);
%! [P, d] = allpaths (G, 1, 3);
%! assert (numel (P), 2);
%! ## direct [1 3] weight=1; indirect [1 2 3] weight=5+7=12
%! lens = cellfun ("numel", P);
%! [~, order] = sort (lens);
%! ds = d(order);
%! assert (ds(1), 1);     # [1 3]
%! assert (ds(2), 12);    # [1 2 3]

## sum(G.Edges.Weight(ep)) == d for each returned path.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1], [5 1 7 2]);
%! [P, d, ep] = allpaths (G, 1, 3);
%! for k = 1:numel (P)
%!   assert (sum (G.Edges.Weight(ep{k})), d(k));
%! endfor

## -------------------- more simple paths --------------------

## K4 complete directed (i->j for all i!=j): from 1 to 4 has
##   1 + 2 + 2 = 5 simple paths (1->4; 1->j->4 for j in {2,3};
##   1->i->j->4 for ordered i,j in {2,3}).
%!test
%! i = [1 1 1 2 2 2 3 3 3 4 4 4];
%! j = [2 3 4 1 3 4 1 2 4 1 2 3];
%! G = digraph (i, j);
%! P = allpaths (G, 1, 4);
%! assert (numel (P), 5);
%! ## All paths must start at 1 and end at 4 with no repeats
%! for k = 1:numel (P)
%!   assert (P{k}(1), 1);
%!   assert (P{k}(end), 4);
%!   assert (numel (unique (P{k})), numel (P{k}));
%! endfor

## K4 complete undirected (edges 1-2, 1-3, 1-4, 2-3, 2-4, 3-4):
## simple paths from 1 to 4: 1-4; 1-2-4, 1-3-4; 1-2-3-4, 1-3-2-4. = 5
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! P = allpaths (G, 1, 4);
%! assert (numel (P), 5);
%! for k = 1:numel (P)
%!   assert (P{k}(1), 1);
%!   assert (P{k}(end), 4);
%!   assert (numel (unique (P{k})), numel (P{k}));
%! endfor

## -------------------- self-loops ignored --------------------

## A self-loop on the path vertex should not appear in any simple
## path (the node can't be revisited).
%!test
%! G = digraph ([1 2 2], [2 2 3]);   # 1->2, 2->2 (self-loop), 2->3
%! P = allpaths (G, 1, 3);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 2, 3]);
%! ## No path contains the self-loop (node 2 appears once only).
%! assert (numel (unique (P{1})), numel (P{1}));

## Self-loop on source node alone doesn't add a new path.
%!test
%! G = digraph ([1 1], [1 2]);       # 1->1 self-loop, 1->2
%! P = allpaths (G, 1, 2);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 2]);

## Self-loop on target node alone doesn't add a new path either.
%!test
%! G = digraph ([1 2], [2 2]);       # 1->2, 2->2 self-loop
%! P = allpaths (G, 1, 2);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 2]);

## -------------------- MaxPathLength option --------------------

## MaxPathLength=1 on unweighted directed triangle prunes the
## indirect path [1 2 3] (length 2).
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P = allpaths (G, 1, 3, "MaxPathLength", 1);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 3]);

## MaxPathLength=2 keeps both paths in unweighted triangle.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P = allpaths (G, 1, 3, "MaxPathLength", 2);
%! assert (numel (P), 2);

## MaxPathLength=0 on a single edge gives no paths.
%!test
%! G = digraph (1, 2);
%! P = allpaths (G, 1, 2, "MaxPathLength", 0);
%! assert (size (P), [0, 1]);

## MaxPathLength=Inf default keeps all paths.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P_default = allpaths (G, 1, 3);
%! P_inf = allpaths (G, 1, 3, "MaxPathLength", Inf);
%! assert (numel (P_default), numel (P_inf));

## Weighted: MaxPathLength=10 on directed triangle 1->2 (w=5),
## 1->3 (w=1), 2->3 (w=7).  Direct [1 3] has weight 1;
## indirect [1 2 3] has weight 12 > 10 → dropped.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 1 7]);
%! P = allpaths (G, 1, 3, "MaxPathLength", 10);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 3]);

## Weighted: MaxPathLength=12 keeps both.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 1 7]);
%! [P, d] = allpaths (G, 1, 3, "MaxPathLength", 12);
%! assert (numel (P), 2);
%! assert (max (d), 12);

## -------------------- MinPathLength option --------------------

## MinPathLength=2 on unweighted triangle drops [1 3] (length 1),
## keeps [1 2 3] (length 2).
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P = allpaths (G, 1, 3, "MinPathLength", 2);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 2, 3]);

## MinPathLength=0 keeps everything.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P_none = allpaths (G, 1, 3);
%! P_zero = allpaths (G, 1, 3, "MinPathLength", 0);
%! assert (numel (P_none), numel (P_zero));

## Weighted: MinPathLength=5 drops direct [1 3] (w=1), keeps [1 2 3]
## (w=12).
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 1 7]);
%! [P, d] = allpaths (G, 1, 3, "MinPathLength", 5);
%! assert (numel (P), 1);
%! assert (d, 12);

## MinPathLength > MaxPathLength returns empty.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P = allpaths (G, 1, 3, "MinPathLength", 10, "MaxPathLength", 5);
%! assert (size (P), [0, 1]);

## -------------------- combined Min/Max options ---------------

## Equal Min and Max selects only paths of that exact length.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P = allpaths (G, 1, 3, "MinPathLength", 2, "MaxPathLength", 2);
%! assert (numel (P), 1);
%! assert (P{1}, [1, 2, 3]);

## -------------------- named nodes --------------------

## Numeric s and t on a named digraph return numeric paths.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"a", "b", "c"});
%! P = allpaths (G, 1, 3);
%! assert (iscell (P));
%! assert (numel (P), 2);
%! assert (isa (P{1}, "double"));

## Name s returns cellstr paths.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"a", "b", "c"});
%! P = allpaths (G, "a", 3);
%! assert (numel (P), 2);
%! assert (iscellstr (P{1}));
%! assert (iscellstr (P{2}));
%! ## Sort by length
%! lens = cellfun ("numel", P);
%! [~, order] = sort (lens);
%! Ps = P(order);
%! assert (Ps{1}, {"a", "c"});
%! assert (Ps{2}, {"a", "b", "c"});

## Cellstr t returns cellstr paths.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"a", "b", "c"});
%! P = allpaths (G, 1, {"c"});
%! assert (numel (P), 2);
%! assert (iscellstr (P{1}));

## -------------------- named-nodes error cases -------------------

## String src on an unnamed digraph errors.
%!error <no node names>
%! G = digraph (3);
%! allpaths (G, "a", 2);

## Missing node name on src errors.
%!error <not found>
%! G = digraph ([1 1 2], [2 3 3], [], {"a", "b", "c"});
%! allpaths (G, "z", "a");

## Missing node name on tgt errors.
%!error <not found>
%! G = digraph ([1 1 2], [2 3 3], [], {"a", "b", "c"});
%! allpaths (G, "a", "z");

## -------------------- numeric-index validation -----------------

## Out-of-range numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! allpaths (G, 5, 1);

## Zero numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! allpaths (G, 0, 1);

## Non-integer numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! allpaths (G, 1.5, 1);

## Out-of-range numeric tgt errors.
%!error <invalid node index>
%! G = digraph (3);
%! allpaths (G, 1, 5);

## Non-scalar numeric src errors (vector not allowed for s).
%!error <scalar node identifier>
%! G = digraph (3);
%! allpaths (G, [1 2], 3);

## -------------------- dot notation dispatch --------------------

## G.allpaths(s, t) matches allpaths(G, s, t) for digraph.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P1 = allpaths (G, 1, 3);
%! P2 = G.allpaths (1, 3);
%! assert (P1, P2);

## G.allpaths(s, t) matches allpaths(G, s, t) for graph.
%!test
%! G = graph ([1 1 2], [2 3 3]);
%! P1 = allpaths (G, 1, 3);
%! P2 = G.allpaths (1, 3);
%! assert (P1, P2);

## -------------------- multigraph ------------------------------

## Parallel edges: in a multigraph with two parallel 1->2 edges and
## one 2->3 edge, there should be two paths from 1 to 3 (same node
## sequence but different edge indices).
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! [P, d, ep] = allpaths (G, 1, 3);
%! assert (numel (P), 2);
%! ## Both paths have identical nodes [1 2 3]
%! assert (P{1}, [1, 2, 3]);
%! assert (P{2}, [1, 2, 3]);
%! ## But the edge indices differ.
%! assert (! isequal (ep{1}, ep{2}));
%! ## And edges 1 and 2 are both 1->2.
%! E = G.Edges.EndNodes;
%! edges_used = unique ([ep{1}(1), ep{2}(1)]);
%! assert (all (E(edges_used, 1) == 1));
%! assert (all (E(edges_used, 2) == 2));

## Weighted multigraph: each parallel edge contributes a distinct
## path, and d reflects its specific weight.
%!test
%! G = digraph ([1 1 2], [2 2 3], [5, 100, 1], "multigraph");
%! [P, d, ep] = allpaths (G, 1, 3);
%! assert (numel (P), 2);
%! ## Both paths visit [1 2 3], one uses the weight-5 edge (d=6)
%! ## and one uses the weight-100 edge (d=101).
%! ds = sort (d);
%! assert (ds(1), 6);
%! assert (ds(2), 101);
%! for k = 1:numel (P)
%!   assert (sum (G.Edges.Weight(ep{k})), d(k));
%! endfor

## -------------------- output orientation ----------------------

## P, d, ep are column shaped.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! [P, d, ep] = allpaths (G, 1, 3);
%! assert (size (P, 2), 1);
%! assert (size (d, 2), 1);
%! assert (size (ep, 2), 1);
%! ## Each path entry is a row vector.
%! for k = 1:numel (P)
%!   assert (size (P{k}, 1), 1);
%!   assert (size (ep{k}, 1), 1);
%! endfor

## -------------------- zero / one output ------------------------

## P = allpaths (...) returns only P (zero-arg d and ep skipped).
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! P = allpaths (G, 1, 3);
%! assert (numel (P), 2);

## [P, d] = allpaths (...) returns P and d.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! [P, d] = allpaths (G, 1, 3);
%! assert (numel (P), 2);
%! assert (numel (d), 2);

## -------------------- siever-style larger graph ----------------

## Siever-style 9-node digraph from earlier stories: paths from 1 to 9.
##   1 -> 2 -> 3 -> 4 -> 5 -> 9         (length 5)
##   1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 7 ...  [not simple]
##   The "simple" paths from 1 to 9 under edges
##     s = [1 2 3 3 4 5 5 6 7 7 8 9]
##     t = [2 3 2 4 5 6 9 7 8 9 7 4]
##   include [1 2 3 4 5 9] and [1 2 3 4 5 6 7 9] and [1 2 3 4 5 6 7 8 _]
##   (node 8 -> 7 cycle, but simple excludes repeats).
##   Without exhaustively hand-enumerating, we assert that (a) every
##   returned path starts at 1 and ends at 9 and (b) every path is
##   simple (no repeated nodes), and (c) at least one path has
##   length 6 (the known-short [1 2 3 4 5 9]).
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! P = allpaths (G, 1, 9);
%! assert (numel (P) >= 1);
%! for k = 1:numel (P)
%!   assert (P{k}(1), 1);
%!   assert (P{k}(end), 9);
%!   assert (numel (unique (P{k})), numel (P{k}));
%! endfor
%! ## The known short path [1 2 3 4 5 9] must be in the result.
%! known = [1, 2, 3, 4, 5, 9];
%! found = false;
%! for k = 1:numel (P)
%!   if (isequal (P{k}, known))
%!     found = true;
%!     break;
%!   endif
%! endfor
%! assert (found, true);

## -------------------- negative weights (deferred) ---------------

## Negative edge weights on a weighted digraph: MATLAB allpaths
## doesn't otherwise forbid negative weights (the paths are still
## simple), but MinPathLength and MaxPathLength comparisons may
## become non-monotonic.  Since allpaths enumerates all simple paths
## and returns the total weight unchanged, we don't error here;
## instead we just verify that summing the weights matches d.
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 1 -3]);
%! [P, d, ep] = allpaths (G, 1, 3);
%! assert (numel (P), 2);
%! for k = 1:numel (P)
%!   assert (sum (G.Edges.Weight(ep{k})), d(k));
%! endfor
