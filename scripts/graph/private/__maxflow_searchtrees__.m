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
## @deftypefn {} {@var{mf} =} __maxflow_searchtrees__ (@var{uu}, @var{vv}, @var{caps}, @var{N}, @var{s}, @var{t})
## Private helper computing the maximum flow from node @var{s} to node
## @var{t} using a dual-search-tree augmenting-path method inspired by
## the Boykov-Kolmogorov algorithm.
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
## Each iteration grows one BFS tree forward from @var{s} in the
## residual graph and a second BFS tree backward from @var{t} (walking
## edges in reverse).  A @emph{cross-tree} edge is a residual arc
## @math{u -> v} with @var{u} reached from @var{s} and @var{v}
## reached from @var{t}; the algorithm picks the cross-tree edge that
## yields the shortest combined path @math{s \to u \to v \to t} and
## augments along it.  The outer loop terminates when no cross-tree
## edge with positive residual exists.  The forward arcs are paired
## with in-memory reverse arcs (initial capacity @code{0}) so that
## flow can be cancelled; pair indices link each forward/reverse pair.
## The resulting flow value agrees with any other correct augmenting-
## path algorithm (e.g., Edmonds-Karp).
## @seealso{maxflow}
## @end deftypefn

function mf = __maxflow_searchtrees__ (uu, vv, caps, N, s, t)

  if (nargin != 6)
    print_usage ();
  endif

  mf = 0;

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
             "__maxflow_searchtrees__: capacities must be finite real numbers (NaN not allowed)");
    endif
    if (any (caps(:) < 0))
      error ("Octave:invalid-input-arg", ...
             "__maxflow_searchtrees__: capacities must be non-negative");
    endif
  endif

  ## Force column-vector, double shape.
  uu = double (uu(:));
  vv = double (vv(:));
  caps = double (caps(:));

  ## Drop edges that can never carry flow (self-loops do not
  ## contribute to s-t flow; zero-capacity edges are redundant).
  keep = (uu != vv) & (caps > 0);
  uu = uu(keep);
  vv = vv(keep);
  caps = caps(keep);
  M = numel (uu);

  if (M == 0)
    return;
  endif

  ## Build the residual graph.  Forward arc i has index i with
  ## capacity caps(i); its reverse arc has index M+i (initial cap 0).
  ## PAIR(:) links each arc to its partner so augmentation transfers
  ## residual from one to the other in O(1).
  U   = [uu; vv];
  V   = [vv; uu];
  CAP = [caps; zeros(M, 1)];
  PAIR = [(M+1:2*M)'; (1:M)'];
  E = 2 * M;

  ## Per-node adjacency by source (U) and by target (V).  The S-tree
  ## BFS walks forward along U; the T-tree BFS walks backward from t
  ## by following edges whose V endpoint matches the current node
  ## (i.e., edges "pointing into" it) and stepping to U.
  adj_out = accumarray (U, (1:E)', [N, 1], @(x) {x}, {zeros(0, 1)});
  adj_in  = accumarray (V, (1:E)', [N, 1], @(x) {x}, {zeros(0, 1)});

  queue_S = zeros (N, 1);
  queue_T = zeros (N, 1);

  while (true)

    ## ----- S-tree: BFS from s forward along residual arcs -----
    parent_S = zeros (N, 1);     # edge used to reach each S-tree node
    dist_S = -ones (N, 1);       # -1 == unvisited
    dist_S(s) = 0;
    queue_S(1) = s;
    qh = 1;
    qt = 1;
    while (qh <= qt)
      u = queue_S(qh);
      qh += 1;
      es = adj_out{u};
      for k = 1:numel (es)
        e = es(k);
        if (CAP(e) > 0)
          vn = V(e);
          if (dist_S(vn) < 0)
            dist_S(vn) = dist_S(u) + 1;
            parent_S(vn) = e;
            qt += 1;
            queue_S(qt) = vn;
          endif
        endif
      endfor
    endwhile

    ## ----- T-tree: BFS from t backward along residual arcs -----
    ## Step from node u to U(e) whenever V(e) == u and CAP(e) > 0;
    ## the edge e then "points toward t" for U(e).
    parent_T = zeros (N, 1);
    dist_T = -ones (N, 1);
    dist_T(t) = 0;
    queue_T(1) = t;
    qh = 1;
    qt = 1;
    while (qh <= qt)
      u = queue_T(qh);
      qh += 1;
      es = adj_in{u};
      for k = 1:numel (es)
        e = es(k);
        if (CAP(e) > 0)
          vn = U(e);
          if (dist_T(vn) < 0)
            dist_T(vn) = dist_T(u) + 1;
            parent_T(vn) = e;
            qt += 1;
            queue_T(qt) = vn;
          endif
        endif
      endfor
    endwhile

    ## If s cannot reach t in either direction, no augmenting path
    ## exists and we have reached the max flow.
    if (dist_S(t) < 0 || dist_T(s) < 0)
      break;
    endif

    ## ----- Find the shortest cross-tree augmenting edge -----
    ## A cross-tree edge is an arc e with CAP(e) > 0 where U(e) is in
    ## the S-tree and V(e) is in the T-tree.  The resulting
    ## augmenting path length is dist_S(U(e)) + 1 + dist_T(V(e)).
    best_e = 0;
    best_len = Inf;
    pos_cap = (CAP > 0);
    ds_u = dist_S(U);
    dt_v = dist_T(V);
    eligible = pos_cap & (ds_u >= 0) & (dt_v >= 0);
    if (any (eligible))
      lens = ds_u + 1 + dt_v;
      lens(! eligible) = Inf;
      [best_len, best_e] = min (lens);
      if (best_len == Inf)
        best_e = 0;
      endif
    endif

    if (best_e == 0)
      break;
    endif

    ## ----- Assemble the augmenting path -----
    ## S-tree segment: edges from s up to U(best_e), in order.
    s_edges = zeros (0, 1);
    v = U(best_e);
    while (v != s)
      e = parent_S(v);
      s_edges = [e; s_edges];
      v = U(e);
    endwhile

    ## T-tree segment: edges from V(best_e) down to t, in order.
    t_edges = zeros (0, 1);
    v = V(best_e);
    while (v != t)
      e = parent_T(v);
      t_edges(end + 1, 1) = e;
      v = V(e);
    endwhile

    path_edges = [s_edges; best_e; t_edges];

    ## Bottleneck residual capacity along the augmenting path.
    bottleneck = Inf;
    for k = 1:numel (path_edges)
      e = path_edges(k);
      if (CAP(e) < bottleneck)
        bottleneck = CAP(e);
      endif
    endfor

    ## Augment: subtract bottleneck from forward residuals, add to
    ## reverse residuals.
    for k = 1:numel (path_edges)
      e = path_edges(k);
      CAP(e) -= bottleneck;
      CAP(PAIR(e)) += bottleneck;
    endfor

    mf += bottleneck;
  endwhile

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty graph: mf = 0.
%!test
%! mf = __maxflow_searchtrees__ ([], [], [], 0, 1, 1);
%! assert (mf, 0);

## Single-node graph, s == t: mf = 0.
%!test
%! mf = __maxflow_searchtrees__ ([], [], [], 1, 1, 1);
%! assert (mf, 0);

## Two-node, single edge 1->2 cap 5: mf = 5.
%!test
%! mf = __maxflow_searchtrees__ (1, 2, 5, 2, 1, 2);
%! assert (mf, 5);

## Two-node, single edge 1->2 cap 5: reverse direction mf = 0.
%!test
%! mf = __maxflow_searchtrees__ (1, 2, 5, 2, 2, 1);
%! assert (mf, 0);

## Chain 1->2(5), 2->3(10): mf(1,3) = 5 (bottleneck).
%!test
%! mf = __maxflow_searchtrees__ ([1;2], [2;3], [5;10], 3, 1, 3);
%! assert (mf, 5);

## Chain 1->2(10), 2->3(5): mf(1,3) = 5.
%!test
%! mf = __maxflow_searchtrees__ ([1;2], [2;3], [10;5], 3, 1, 3);
%! assert (mf, 5);

## Diamond: two parallel paths give summed mf.
%!test
%! uu = [1;1;2;3];  vv = [2;3;4;4];  w = [5;8;7;3];
%! mf = __maxflow_searchtrees__ (uu, vv, w, 4, 1, 4);
%! assert (mf, 8);

## Uniform diamond: mf = 10.
%!test
%! uu = [1;1;2;3];  vv = [2;3;4;4];  w = [5;5;5;5];
%! mf = __maxflow_searchtrees__ (uu, vv, w, 4, 1, 4);
%! assert (mf, 10);

## Parallel edges (multi-arc) on a 2-node graph sum capacities.
%!test
%! uu = [1;1];  vv = [2;2];  w = [3;7];
%! mf = __maxflow_searchtrees__ (uu, vv, w, 2, 1, 2);
%! assert (mf, 10);

## Triple parallel edges on a 2-node graph sum capacities.
%!test
%! uu = [1;1;1];  vv = [2;2;2];  w = [3;5;7];
%! mf = __maxflow_searchtrees__ (uu, vv, w, 2, 1, 2);
%! assert (mf, 15);

## CLRS Figure 26.1 reference: mf(1,6) = 23.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! mf = __maxflow_searchtrees__ (s, t, w, 6, 1, 6);
%! assert (mf, 23);

## Self-loops are ignored.
%!test
%! uu = [1;2;2;2];  vv = [2;2;3;2];  w = [5;100;3;50];
%! mf = __maxflow_searchtrees__ (uu, vv, w, 3, 1, 3);
%! assert (mf, 3);

## Zero-capacity edges are ignored.
%!test
%! uu = [1;1;2];  vv = [2;3;3];  w = [5;0;4];
%! mf = __maxflow_searchtrees__ (uu, vv, w, 3, 1, 3);
%! assert (mf, 4);

## Negative capacity errors.
%!error <non-negative|negative>
%! __maxflow_searchtrees__ ([1;2], [2;3], [5;-1], 3, 1, 3);

## NaN capacity errors.
%!error <NaN|finite>
%! __maxflow_searchtrees__ ([1;2], [2;3], [5;NaN], 3, 1, 3);

## Unreachable target returns 0.
%!test
%! mf = __maxflow_searchtrees__ (1, 2, 5, 3, 1, 3);
%! assert (mf, 0);

## Undirected-emulated square: each undirected edge expanded into two
## antiparallel arcs.  {1,2}(5), {1,3}(8), {2,4}(7), {3,4}(3) -> mf = 8.
%!test
%! uu = [1;1;2;3; 2;3;4;4];
%! vv = [2;3;4;4; 1;1;2;3];
%! w  = [5;8;7;3; 5;8;7;3];
%! mf = __maxflow_searchtrees__ (uu, vv, w, 4, 1, 4);
%! assert (mf, 8);

## Agreement with Edmonds-Karp on CLRS reference.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! mf_st = __maxflow_searchtrees__ (s, t, w, 6, 1, 6);
%! mf_ek = __maxflow_edmonds_karp__ (s, t, w, 6, 1, 6);
%! assert (mf_st, mf_ek);

## Agreement with Edmonds-Karp on a random-ish 8-node digraph.
%!test
%! s = [1 1 2 2 3 3 4 4 5 5 6 6 7];
%! t = [2 3 4 5 5 6 6 7 6 8 7 8 8];
%! w = [10 5 8 4 3 6 2 9 1 7 5 3 6];
%! mf_st = __maxflow_searchtrees__ (s, t, w, 8, 1, 8);
%! mf_ek = __maxflow_edmonds_karp__ (s, t, w, 8, 1, 8);
%! assert (mf_st, mf_ek);

## Agreement with Edmonds-Karp on a larger unweighted graph.
%!test
%! uu = [1 1 1 2 2 3 3 4 4 5 5 6 7 7 8 9];
%! vv = [2 3 4 5 6 5 7 6 8 8 9 9 8 9 10 10];
%! w  = ones (size (uu));
%! mf_st = __maxflow_searchtrees__ (uu, vv, w, 10, 1, 10);
%! mf_ek = __maxflow_edmonds_karp__ (uu, vv, w, 10, 1, 10);
%! assert (mf_st, mf_ek);

## No arguments: print_usage.
%!error __maxflow_searchtrees__ ()
