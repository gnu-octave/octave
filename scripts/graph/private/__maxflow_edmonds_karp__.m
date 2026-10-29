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
## @deftypefn  {} {@var{mf} =} __maxflow_edmonds_karp__ (@var{uu}, @var{vv}, @var{caps}, @var{N}, @var{s}, @var{t})
## @deftypefnx {} {[@var{mf}, @var{flow}, @var{reach_s}] =} __maxflow_edmonds_karp__ (@var{uu}, @var{vv}, @var{caps}, @var{N}, @var{s}, @var{t})
## Private helper computing the maximum flow from node @var{s} to node
## @var{t} using the Edmonds-Karp (BFS-augmenting-path) implementation
## of the Ford-Fulkerson method.
##
## The input graph is given in edge-list form: @var{uu}(i) -> @var{vv}(i)
## is the @math{i}-th directed arc with non-negative capacity
## @var{caps}(i).  @var{N} is the number of nodes.  For an undirected
## graph the caller should expand each undirected edge into two
## antiparallel directed arcs, each carrying the original capacity.
##
## @var{mf} is a scalar non-negative double giving the value of the
## maximum @math{s}-@math{t} flow.  When @code{@var{s} == @var{t}} or
## when @var{t} is not reachable from @var{s}, @var{mf} is @code{0}.
##
## When called with two or three output arguments the helper
## additionally returns:
## @table @asis
## @item @var{flow}
## A column vector of length @code{numel (@var{uu})} giving the flow
## on each input arc.  Self-loops and arcs that were dropped because
## their capacity was zero report @code{0}.
## @item @var{reach_s}
## A logical column vector of length @var{N} marking the nodes
## reachable from @var{s} in the residual graph once the maximum
## flow is established.  The true entries form one side of a
## minimum @math{s}-@math{t} cut.
## @end table
##
## The algorithm runs in @math{O (V E^2)} worst-case time.  Each
## forward arc is augmented with an in-memory reverse arc (initial
## capacity @code{0}) so that flow can be cancelled; pair indices link
## the two arcs of each forward/reverse pair.
## @seealso{maxflow, mincut}
## @end deftypefn

function [mf, flow, reach_s] = __maxflow_edmonds_karp__ (uu, vv, caps, N, s, t)

  if (nargin != 6)
    print_usage ();
  endif

  mf = 0;

  ## Default empty-flow / reach_s outputs -- overwritten below once the
  ## algorithm has run.  reach_s is at least {s} in the trivial case.
  M0 = numel (uu);
  flow = zeros (M0, 1);
  reach_s = false (max (N, 0), 1);
  if (N >= 1 && s >= 1 && s <= N)
    reach_s(s) = true;
  endif

  if (s == t)
    return;
  endif
  if (N <= 1)
    return;
  endif

  ## Validate capacities.  The public caller is expected to do this
  ## too, but defend here so the helper is safe to call directly.
  if (! isempty (caps))
    if (! isreal (caps) || any (isnan (caps(:))))
      error ("Octave:invalid-input-arg", ...
             "__maxflow_edmonds_karp__: capacities must be finite real numbers (NaN not allowed)");
    endif
    if (any (caps(:) < 0))
      error ("Octave:invalid-input-arg", ...
             "__maxflow_edmonds_karp__: capacities must be non-negative");
    endif
  endif

  ## Force column-vector, double shape.
  uu = double (uu(:));
  vv = double (vv(:));
  caps = double (caps(:));
  M = numel (uu);

  ## Drop edges that can never carry flow (self-loops do not
  ## contribute to s-t flow; zero-capacity edges are redundant).
  ## Remember the original index of every surviving edge so we can
  ## scatter the final flows back into the caller's shape.
  keep = (uu != vv) & (caps > 0);
  orig_idx = find (keep);
  uu = uu(keep);
  vv = vv(keep);
  caps = caps(keep);
  M = numel (uu);

  if (M == 0)
    ## No arcs can carry flow; reach_s is just {s} (already set).
    return;
  endif

  ## Remember the initial forward capacities so we can recover flow
  ## values after the algorithm terminates.
  caps0 = caps;

  ## Build the residual graph.  For each forward arc i, edge index i
  ## is the forward arc U -> V with capacity caps(i); edge index M+i
  ## is the reverse arc V -> U with initial capacity 0.  Pair(i) = M+i
  ## and Pair(M+i) = i link the two so augmenting arc e transfers
  ## residual from Edge(e) to Edge(Pair(e)).
  U   = [uu; vv];
  V   = [vv; uu];
  CAP = [caps; zeros(M, 1)];
  PAIR = [(M+1:2*M)'; (1:M)'];
  E = 2 * M;

  ## Build per-node adjacency lists of outgoing edge indices.  Use
  ## accumarray with cell output to aggregate indices by source node.
  adj = accumarray (U, (1:E)', [N, 1], @(x) {x}, {zeros(0, 1)});

  while (true)
    ## BFS in the residual graph from s, stopping at t.  parent_edge
    ## stores the edge used to reach each node; 0 means unvisited.
    ## visited marks nodes to avoid the two cheap checks every step.
    parent_edge = zeros (N, 1);
    visited = false (N, 1);
    visited(s) = true;
    queue = zeros (N, 1);
    queue(1) = s;
    qhead = 1;
    qtail = 1;
    found = false;
    while (qhead <= qtail)
      u = queue(qhead);
      qhead += 1;
      if (u == t)
        found = true;
        break;
      endif
      es = adj{u};
      for k = 1:numel (es)
        e = es(k);
        if (CAP(e) > 0)
          vnext = V(e);
          if (! visited(vnext))
            visited(vnext) = true;
            parent_edge(vnext) = e;
            qtail += 1;
            queue(qtail) = vnext;
            if (vnext == t)
              found = true;
              break;
            endif
          endif
        endif
      endfor
      if (found)
        break;
      endif
    endwhile

    if (! found)
      ## The final BFS could not reach t.  The set of nodes it
      ## visited is the source side of a minimum s-t cut.
      reach_s = visited;
      break;
    endif

    ## Walk the parent-edge chain back from t to s to find the
    ## bottleneck residual capacity along the path.
    bottleneck = Inf;
    v = t;
    while (v != s)
      e = parent_edge(v);
      if (CAP(e) < bottleneck)
        bottleneck = CAP(e);
      endif
      v = U(e);
    endwhile

    ## Augment: decrease residual along forward arcs and increase
    ## residual on their reverse arcs.  Each step walks one edge
    ## back toward s.
    v = t;
    while (v != s)
      e = parent_edge(v);
      CAP(e) -= bottleneck;
      CAP(PAIR(e)) += bottleneck;
      v = U(e);
    endwhile

    mf += bottleneck;
  endwhile

  ## Recover the flow on every kept forward arc: flow = initial
  ## capacity - residual forward capacity.  Scatter back into the
  ## caller's edge-list shape using orig_idx.
  kept_flow = caps0 - CAP(1:M);
  flow = zeros (M0, 1);
  flow(orig_idx) = kept_flow;

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty graph: mf = 0.
%!test
%! mf = __maxflow_edmonds_karp__ ([], [], [], 0, 1, 1);
%! assert (mf, 0);

## Single-node graph, s == t: mf = 0.
%!test
%! mf = __maxflow_edmonds_karp__ ([], [], [], 1, 1, 1);
%! assert (mf, 0);

## Two-node, single edge 1->2 cap 5: mf = 5.
%!test
%! mf = __maxflow_edmonds_karp__ (1, 2, 5, 2, 1, 2);
%! assert (mf, 5);

## Two-node, single edge 1->2 cap 5: reverse direction mf = 0.
%!test
%! mf = __maxflow_edmonds_karp__ (1, 2, 5, 2, 2, 1);
%! assert (mf, 0);

## Chain 1->2(5), 2->3(10): mf(1,3) = 5.
%!test
%! mf = __maxflow_edmonds_karp__ ([1;2], [2;3], [5;10], 3, 1, 3);
%! assert (mf, 5);

## Diamond: two parallel paths give summed mf.
%!test
%! uu = [1;1;2;3];  vv = [2;3;4;4];  w = [5;8;7;3];
%! mf = __maxflow_edmonds_karp__ (uu, vv, w, 4, 1, 4);
%! assert (mf, 8);

## Parallel edges (multi-arc) on a 2-node graph sum capacities.
%!test
%! uu = [1;1];  vv = [2;2];  w = [3;7];
%! mf = __maxflow_edmonds_karp__ (uu, vv, w, 2, 1, 2);
%! assert (mf, 10);

## CLRS Figure 26.1 reference: mf(1,6) = 23.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! mf = __maxflow_edmonds_karp__ (s, t, w, 6, 1, 6);
%! assert (mf, 23);

## Self-loops are ignored.
%!test
%! uu = [1;2;2;2];  vv = [2;2;3;2];  w = [5;100;3;50];
%! mf = __maxflow_edmonds_karp__ (uu, vv, w, 3, 1, 3);
%! assert (mf, 3);

## Zero-capacity edges are ignored.
%!test
%! uu = [1;1;2];  vv = [2;3;3];  w = [5;0;4];
%! mf = __maxflow_edmonds_karp__ (uu, vv, w, 3, 1, 3);
%! assert (mf, 4);

## Negative capacity errors.
%!error <non-negative|negative>
%! __maxflow_edmonds_karp__ ([1;2], [2;3], [5;-1], 3, 1, 3);

## NaN capacity errors.
%!error <NaN|finite>
%! __maxflow_edmonds_karp__ ([1;2], [2;3], [5;NaN], 3, 1, 3);

## Unreachable target returns 0.
%!test
%! mf = __maxflow_edmonds_karp__ (1, 2, 5, 3, 1, 3);
%! assert (mf, 0);

## Undirected-emulated square: each undirected edge expanded into two
## antiparallel arcs.  {1,2}(5), {1,3}(8), {2,4}(7), {3,4}(3) -> mf = 8.
%!test
%! uu = [1;1;2;3; 2;3;4;4];
%! vv = [2;3;4;4; 1;1;2;3];
%! w  = [5;8;7;3; 5;8;7;3];
%! mf = __maxflow_edmonds_karp__ (uu, vv, w, 4, 1, 4);
%! assert (mf, 8);

## No arguments: print_usage.
%!error __maxflow_edmonds_karp__ ()

## Multi-output: flow vector and reach_s for single edge.
%!test
%! [mf, flow, reach_s] = __maxflow_edmonds_karp__ (1, 2, 5, 2, 1, 2);
%! assert (mf, 5);
%! assert (flow, 5);
%! assert (reach_s, [true; false]);

## Multi-output: flow vector matches input edge order on chain.
## After max flow, arc 1->2 is saturated (residual 0), so BFS in the
## residual graph from 1 cannot reach 2.  The source side of the
## min-cut is {1}.
%!test
%! [mf, flow, reach_s] = __maxflow_edmonds_karp__ ([1;2], [2;3], ...
%!                                                  [5;10], 3, 1, 3);
%! assert (mf, 5);
%! assert (flow, [5; 5]);
%! assert (reach_s, [true; false; false]);

## Multi-output: diamond, flow conservation across arcs.
%!test
%! uu = [1;1;2;3];  vv = [2;3;4;4];  w = [5;8;7;3];
%! [mf, flow, reach_s] = __maxflow_edmonds_karp__ (uu, vv, w, 4, 1, 4);
%! assert (mf, 8);
%! ## Out of source = in to sink = mf.
%! assert (sum (flow(uu == 1)), 8);
%! assert (sum (flow(vv == 4)), 8);
%! ## Min-cut capacity equals mf: arcs from cs to ct sum to mf.
%! cs = reach_s;
%! ct = ! cs;
%! cut_cap = sum (w(cs(uu) & ct(vv)));
%! assert (cut_cap, mf);

## Multi-output: unreachable target.
%!test
%! [mf, flow, reach_s] = __maxflow_edmonds_karp__ (1, 2, 5, 3, 1, 3);
%! assert (mf, 0);
%! assert (flow, 0);
%! ## Residual graph lets us reach node 2 from 1 along the 1->2 arc
%! ## (it still has capacity 5 because no flow was sent); 3 stays
%! ## unreachable.
%! assert (reach_s, [true; true; false]);

## Multi-output: s == t single-node graph, reach_s = [true].
%!test
%! [mf, flow, reach_s] = __maxflow_edmonds_karp__ ([], [], [], 1, 1, 1);
%! assert (mf, 0);
%! assert (isempty (flow));
%! assert (reach_s, true);

## Multi-output: self-loops report zero flow in the output vector.
%!test
%! uu = [1;2;2];  vv = [2;2;3];  w = [5;100;3];
%! [mf, flow, reach_s] = __maxflow_edmonds_karp__ (uu, vv, w, 3, 1, 3);
%! assert (mf, 3);
%! ## Self-loop arc 2->2 has zero flow.
%! assert (flow(2), 0);
%! ## Forward arcs carry flow = mf bottleneck = 3.
%! assert (flow(1), 3);
%! assert (flow(3), 3);
%! assert (reach_s, [true; true; false]);

## Multi-output: zero-capacity arcs have zero flow.
%!test
%! uu = [1;1;2];  vv = [2;3;3];  w = [5;0;4];
%! [mf, flow, reach_s] = __maxflow_edmonds_karp__ (uu, vv, w, 3, 1, 3);
%! assert (mf, 4);
%! assert (flow, [4; 0; 4]);
