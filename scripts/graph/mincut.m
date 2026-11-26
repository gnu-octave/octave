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
## @deftypefn  {} {@var{mf} =} mincut (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@var{mf}, @var{GF}] =} mincut (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@var{mf}, @var{GF}, @var{cs}, @var{ct}] =} mincut (@var{G}, @var{s}, @var{t})
## Return the minimum @math{s}-@math{t} cut in the graph or digraph
## @var{G}.
##
## By the max-flow min-cut theorem the scalar first output @var{mf}
## equals the maximum flow that can be sent from node @var{s} to node
## @var{t}; @code{mincut} and @code{maxflow} therefore agree on their
## first output for the same inputs.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{s}
## and @var{t} are scalar node identifiers: a positive integer node
## index, a character row vector naming a node, or a 1-element cell
## array of strings naming a node.  When @var{G} has node names, a
## mix of numeric and name identifiers is allowed.
##
## Edge weights are interpreted as capacities and must be
## non-negative; @code{NaN} or negative weights raise an error.  When
## @var{G} is unweighted every edge has capacity @code{1}.  For a
## @code{digraph} that is a multigraph, the capacities of parallel
## edges are summed.  For a @code{graph} each undirected edge acts as
## a pair of antiparallel arcs with the stored capacity available in
## either direction.  Self-loops do not contribute to any @math{s-t}
## flow and never appear in @var{GF}.
##
## Additional outputs:
## @table @asis
## @item @var{GF}
## A @code{digraph} on the same node set as @var{G} containing only
## the flow-carrying arcs (flow @math{>} 0) with the per-arc flow
## value stored as the edge weight.
## @item @var{cs}
## A column vector of node identifiers on the source side of a
## minimum cut: the nodes reachable from @var{s} in the residual
## graph after the maximum flow is established.  @var{cs} is numeric
## when @var{G} has no node names and a cellstr when it does.
## @item @var{ct}
## A column vector of node identifiers on the sink side of the same
## minimum cut: the complement of @var{cs}.  Return type matches
## @var{cs}.
## @end table
##
## When @code{@var{s} == @var{t}} or when @var{t} is unreachable from
## @var{s}, @var{mf} is @code{0}, @var{GF} has no edges,
## @var{cs} contains the nodes reachable from @var{s} and @var{ct}
## contains the rest.
##
## @example
## @group
## ## CLRS Fig 26.1 classical example: mf = 23.
## s = [1 1 2 2 3 3 4 4 5 5];
## t = [2 3 3 4 2 5 3 6 4 6];
## w = [16 13 10 12 4 14 9 20 7 4];
## G = digraph (s, t, w);
## [mf, GF, cs, ct] = mincut (G, 1, 6);
## mf                 @result{} 23
## numedges (GF) > 0  @result{} true
## @end group
## @end example
##
## @seealso{maxflow, graph, digraph, shortestpath, distances}
## @end deftypefn

function [mf, GF, cs, ct] = mincut (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal @code{mincut}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so @code{help
  ## mincut} works outside the context of an instance) and as a
  ## fallback that gives a helpful error for non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "mincut: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a graph/digraph first arg, but route through dot
  ## notation just in case.
  switch (nargout)
    case {0, 1}
      mf = G.mincut (varargin{:});
    case 2
      [mf, GF] = G.mincut (varargin{:});
    case 3
      [mf, GF, cs] = G.mincut (varargin{:});
    otherwise
      [mf, GF, cs, ct] = G.mincut (varargin{:});
  endswitch

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## mincut on a non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! mincut (42, 1, 2);

## mincut on a non-graph string input is an error.
%!error <must be a graph or digraph object>
%! mincut ("foo", 1, 2);

## mincut with no args is an error via print_usage.
%!error mincut ()

## mincut with G alone is an error (missing s and t).
%!error mincut (digraph ())

## mincut with G and s only is an error (missing t).
%!error mincut (digraph (3), 1)

## -------------------- single-output parity with maxflow -----

## mf-only call on a diamond digraph matches maxflow.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (mincut (G, 1, 4), maxflow (G, 1, 4));

## mf-only call on CLRS Fig 26.1 matches maxflow (value = 23).
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! G = digraph (s, t, w);
%! assert (mincut (G, 1, 6), 23);

## mf-only on undirected diamond matches maxflow.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (mincut (G, 1, 4), maxflow (G, 1, 4));

## -------------------- trivial s == t --------------------

## Single-node digraph s==t: mf=0, GF empty, cs=[1], ct=zeros(0,1).
%!test
%! G = digraph (1);
%! [mf, GF, cs, ct] = mincut (G, 1, 1);
%! assert (mf, 0);
%! assert (numedges (GF), 0);
%! assert (numnodes (GF), 1);
%! assert (cs, 1);
%! assert (isempty (ct));

## Multi-node digraph s==t: mf=0, cs=[s], ct=other nodes.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [mf, GF, cs, ct] = mincut (G, 2, 2);
%! assert (mf, 0);
%! assert (numedges (GF), 0);
%! assert (cs, 2);
%! assert (sort (ct), [1; 3]);

## Multi-node graph s==t: mf=0.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! [mf, GF, cs, ct] = mincut (G, 2, 2);
%! assert (mf, 0);
%! assert (numedges (GF), 0);
%! assert (cs, 2);
%! assert (sort (ct), [1; 3]);

## -------------------- single edge -----------------------

## Single-arc digraph 1->2 cap 5: mf=5, GF=1->2(5), cs=[1], ct=[2].
%!test
%! G = digraph (1, 2, 5);
%! [mf, GF, cs, ct] = mincut (G, 1, 2);
%! assert (mf, 5);
%! assert (numedges (GF), 1);
%! assert (numnodes (GF), 2);
%! e = GF.Edges;
%! assert (e.EndNodes, [1 2]);
%! assert (e.Weight, 5);
%! assert (cs, 1);
%! assert (ct, 2);

## Single-arc unweighted digraph 1->2: mf=1, GF edge weight 1.
%!test
%! G = digraph (1, 2);
%! [mf, GF, cs, ct] = mincut (G, 1, 2);
%! assert (mf, 1);
%! e = GF.Edges;
%! assert (e.EndNodes, [1 2]);
%! assert (e.Weight, 1);
%! assert (cs, 1);
%! assert (ct, 2);

## Single undirected edge cap 5: mf=5, GF has arc in flow direction.
%!test
%! G = graph (1, 2, 5);
%! [mf, GF, cs, ct] = mincut (G, 1, 2);
%! assert (mf, 5);
%! assert (numedges (GF), 1);
%! e = GF.Edges;
%! assert (e.EndNodes, [1 2]);
%! assert (e.Weight, 5);
%! assert (cs, 1);
%! assert (ct, 2);

## Reverse direction on directed edge: mf=0, GF has no edges.
%!test
%! G = digraph (1, 2, 5);
%! [mf, GF, cs, ct] = mincut (G, 2, 1);
%! assert (mf, 0);
%! assert (numedges (GF), 0);
%! assert (cs, 2);
%! assert (ct, 1);

## -------------------- unreachable / disconnected --------

## Edgeless digraph: mf=0, GF no edges, cs={s}, ct rest.
%!test
%! G = digraph (3);
%! [mf, GF, cs, ct] = mincut (G, 1, 2);
%! assert (mf, 0);
%! assert (numedges (GF), 0);
%! assert (cs, 1);
%! assert (sort (ct), [2; 3]);

## Two disjoint components: mf=0, cs=reachable-from-s component.
%!test
%! G = digraph ([1 3], [2 4]);
%! [mf, GF, cs, ct] = mincut (G, 1, 3);
%! assert (mf, 0);
%! assert (sort (cs), [1; 2]);
%! assert (sort (ct), [3; 4]);

## -------------------- diamond digraph -------------------

## Diamond 5/8/7/3: mf=8 and the partition chosen by Edmonds-Karp
## must be a valid min-cut (cut capacity = mf).
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf, GF, cs, ct] = mincut (G, 1, 4);
%! assert (mf, 8);
%! ## Source side must contain s, sink side must contain t; the
%! ## partition must cover every node exactly once.
%! assert (any (cs == 1));
%! assert (any (ct == 4));
%! assert (sort ([cs; ct]), (1:4).');
%! ## GF conserves flow out of s and into t.
%! e = GF.Edges;
%! out_s = sum (e.Weight(e.EndNodes(:, 1) == 1));
%! in_t  = sum (e.Weight(e.EndNodes(:, 2) == 4));
%! assert (out_s, 8);
%! assert (in_t, 8);
%! ## Cut capacity in the original graph equals mf.
%! cs_mask = false (4, 1);  cs_mask(cs) = true;
%! ct_mask = ~cs_mask;
%! orig = G.Edges;
%! cut_cap = sum (orig.Weight(cs_mask(orig.EndNodes(:, 1)) ...
%!                            & ct_mask(orig.EndNodes(:, 2))));
%! assert (cut_cap, mf);

## Diamond with a bottleneck cut far from s: 1->2(100), 2->3(1), 3->4(100).
## mf=1, the min-cut is {1,2} | {3,4}.
%!test
%! G = digraph ([1 2 3], [2 3 4], [100 1 100]);
%! [mf, GF, cs, ct] = mincut (G, 1, 4);
%! assert (mf, 1);
%! assert (sort (cs), [1; 2]);
%! assert (sort (ct), [3; 4]);

## -------------------- undirected --------------------

## Undirected triangle uniform caps: mf=10 (two paths, each cap 5).
%!test
%! G = graph ([1 2 3], [2 3 1], [5 5 5]);
%! [mf, GF, cs, ct] = mincut (G, 1, 2);
%! assert (mf, 10);
%! ## GF should have edges summing to 10 out of s and 10 into t.
%! e = GF.Edges;
%! out_s = sum (e.Weight(e.EndNodes(:, 1) == 1));
%! in_t  = sum (e.Weight(e.EndNodes(:, 2) == 2));
%! assert (out_s, 10);
%! assert (in_t, 10);
%! ## cs/ct must cover every node exactly once.
%! assert (sort ([cs; ct]), [1; 2; 3]);

## Undirected chain with bottleneck in middle.
%!test
%! G = graph ([1 2 3 4], [2 3 4 5], [10 3 10 10]);
%! [mf, GF, cs, ct] = mincut (G, 1, 5);
%! assert (mf, 3);
%! assert (sort ([cs; ct]), (1:5).');

## Undirected diamond: mf=8 (same as directed analogue).
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf, GF, cs, ct] = mincut (G, 1, 4);
%! assert (mf, 8);
%! e = GF.Edges;
%! out_s = sum (e.Weight(e.EndNodes(:, 1) == 1));
%! in_t  = sum (e.Weight(e.EndNodes(:, 2) == 4));
%! assert (out_s, 8);
%! assert (in_t, 8);

## -------------------- CLRS Figure 26.1 ------------------

## CLRS Fig 26.1: mf=23 and flow conservation at every internal node.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! G = digraph (s, t, w);
%! [mf, GF, cs, ct] = mincut (G, 1, 6);
%! assert (mf, 23);
%! ## Flow conservation: for every internal node v (not 1 and not 6),
%! ## sum of incoming flow equals sum of outgoing flow.
%! e = GF.Edges;
%! for v = [2 3 4 5]
%!   inflow  = sum (e.Weight(e.EndNodes(:, 2) == v));
%!   outflow = sum (e.Weight(e.EndNodes(:, 1) == v));
%!   assert (inflow, outflow);
%! endfor
%! ## Every node belongs to cs or ct (complete partition).
%! assert (sort ([cs; ct]), (1:6).');
%! ## The min-cut capacity equals mf: sum of caps on arcs from cs to ct.
%! cs_mask = false (6, 1);  cs_mask(cs) = true;
%! ct_mask = ~cs_mask;
%! orig = G.Edges;
%! cut_cap = sum (orig.Weight(cs_mask(orig.EndNodes(:, 1)) ...
%!                            & ct_mask(orig.EndNodes(:, 2))));
%! assert (cut_cap, mf);

## -------------------- multigraph ------------------------

## Parallel edges sum capacities: two 1->2 arcs caps 3 and 7, mf=10.
%!test
%! G = digraph ([1 1], [2 2], [3 7], "multigraph");
%! [mf, GF, cs, ct] = mincut (G, 1, 2);
%! assert (mf, 10);
%! ## GF flow totals across all arcs leaving 1 equal mf.
%! e = GF.Edges;
%! out_s = sum (e.Weight(e.EndNodes(:, 1) == 1));
%! assert (out_s, 10);
%! assert (cs, 1);
%! assert (ct, 2);

## -------------------- self-loops ignored ----------------

## Self-loop on source does not contribute and does not appear in GF.
%!test
%! G = digraph ([1 1], [1 2], [100, 5]);
%! [mf, GF, cs, ct] = mincut (G, 1, 2);
%! assert (mf, 5);
%! e = GF.Edges;
%! ## No self-loop (1->1) in GF.
%! assert (all (e.EndNodes(:, 1) != e.EndNodes(:, 2)));
%! assert (cs, 1);
%! assert (ct, 2);

## -------------------- named nodes -----------------------

## Named digraph -> cs/ct are cellstr columns of names.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3], {"a","b","c","d"});
%! [mf, GF, cs, ct] = mincut (G, "a", "d");
%! assert (mf, 8);
%! assert (iscellstr (cs));
%! assert (iscellstr (ct));
%! assert (any (strcmp (cs, "a")));
%! assert (any (strcmp (ct, "d")));
%! assert (sort ([cs; ct]), {"a"; "b"; "c"; "d"});

## Named graph -> cs/ct cellstr.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 5 5], {"a", "b", "c"});
%! [mf, GF, cs, ct] = mincut (G, "a", "b");
%! assert (mf, 10);
%! assert (iscellstr (cs));
%! assert (iscellstr (ct));
%! ## cs contains "a", ct contains "b".  "c" is on one side or the other.
%! assert (any (strcmp (cs, "a")));
%! assert (any (strcmp (ct, "b")));
%! assert (sort ([cs; ct]), {"a"; "b"; "c"});

## Numeric source, string target on named digraph.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3], {"a","b","c","d"});
%! [mf, GF, cs, ct] = mincut (G, 1, "d");
%! assert (mf, 8);
%! ## Mixed numeric/name input still returns cellstr outputs because
%! ## at least one argument was a name.
%! assert (iscellstr (cs));
%! assert (iscellstr (ct));

## Unnamed graph -> cs/ct numeric columns.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf, GF, cs, ct] = mincut (G, 1, 4);
%! assert (isnumeric (cs));
%! assert (isnumeric (ct));
%! assert (iscolumn (cs));
%! assert (iscolumn (ct));

## -------------------- named-nodes error cases ----------

## String src on a digraph without node names errors.
%!error <no node names>
%! G = digraph (3);
%! mincut (G, "a", 2);

## Missing node name on src errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! mincut (G, "z", "a");

## Missing node name on tgt errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! mincut (G, "a", "z");

## -------------------- numeric-index validation ---------

## Out-of-range numeric src errors.
%!error <invalid node index>
%! mincut (digraph (3), 5, 1);

## Zero numeric src errors.
%!error <invalid node index>
%! mincut (digraph (3), 0, 1);

## Non-integer numeric src errors.
%!error <invalid node index>
%! mincut (digraph (3), 1.5, 1);

## Non-scalar numeric src errors.
%!error <scalar node identifier>
%! mincut (digraph (3), [1 2], 3);

## -------------------- capacity validation -------------

## Negative weight errors.
%!error <negative|non-negative>
%! G = digraph ([1 2], [2 3], [5, -1]);
%! mincut (G, 1, 3);

## Negative weight on undirected graph errors.
%!error <negative|non-negative>
%! G = graph ([1 2], [2 3], [5, -1]);
%! mincut (G, 1, 3);

## NaN weight errors.
%!error <NaN|finite>
%! G = digraph ([1 2], [2 3], [5, NaN]);
%! mincut (G, 1, 3);

## -------------------- dot notation dispatch ------------

## G.mincut dot-notation matches mincut(G, ...) on digraph.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf1, GF1, cs1, ct1] = mincut (G, 1, 4);
%! [mf2, GF2, cs2, ct2] = G.mincut (1, 4);
%! assert (mf1, mf2);
%! assert (numedges (GF1), numedges (GF2));
%! assert (sort (cs1), sort (cs2));
%! assert (sort (ct1), sort (ct2));

## G.mincut dot-notation matches mincut(G, ...) on graph.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf1, GF1, cs1, ct1] = mincut (G, 1, 4);
%! [mf2, GF2, cs2, ct2] = G.mincut (1, 4);
%! assert (mf1, mf2);
%! assert (numedges (GF1), numedges (GF2));
%! assert (sort (cs1), sort (cs2));
%! assert (sort (ct1), sort (ct2));

## -------------------- output types -----------------------

## mf is a scalar double.
%!test
%! G = digraph (1, 2, 5);
%! mf = mincut (G, 1, 2);
%! assert (isscalar (mf));
%! assert (isa (mf, "double"));
%! assert (mf, 5);

## GF is a digraph for digraph input.
%!test
%! G = digraph (1, 2, 5);
%! [~, GF] = mincut (G, 1, 2);
%! assert (isa (GF, "digraph"));

## GF is a digraph for graph input (flow has direction).
%!test
%! G = graph (1, 2, 5);
%! [~, GF] = mincut (G, 1, 2);
%! assert (isa (GF, "digraph"));

## -------------------- nargout forms --------------------

## Two-output form: mf and GF.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf, GF] = mincut (G, 1, 4);
%! assert (mf, 8);
%! assert (numedges (GF) >= 2);

## Three-output form: mf, GF, cs.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf, GF, cs] = mincut (G, 1, 4);
%! assert (mf, 8);
%! ## Source must be on the source side.
%! assert (any (cs == 1));

## -------------------- max-flow min-cut duality ----------

## Min-cut capacity equals max-flow (duality) on several graphs.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf, ~, cs, ~] = mincut (G, 1, 4);
%! N = numnodes (G);
%! cs_mask = false (N, 1);  cs_mask(cs) = true;
%! ct_mask = ~cs_mask;
%! e = G.Edges;
%! if (! isfield (e, "Weight"))
%!   e.Weight = ones (size (e.EndNodes, 1), 1);
%! endif
%! cut_cap = sum (e.Weight(cs_mask(e.EndNodes(:, 1)) ...
%!                         & ct_mask(e.EndNodes(:, 2))));
%! assert (cut_cap, mf);

## Same duality on undirected graph: mincut capacity = maxflow.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! [mf, ~, cs, ~] = mincut (G, 1, 4);
%! N = numnodes (G);
%! cs_mask = false (N, 1);  cs_mask(cs) = true;
%! ct_mask = ~cs_mask;
%! e = G.Edges;
%! if (! isfield (e, "Weight"))
%!   e.Weight = ones (size (e.EndNodes, 1), 1);
%! endif
%! ## Undirected edges cross the cut if exactly one endpoint is in cs.
%! crosses = xor (cs_mask(e.EndNodes(:, 1)), cs_mask(e.EndNodes(:, 2)));
%! cut_cap = sum (e.Weight(crosses));
%! assert (cut_cap, mf);
