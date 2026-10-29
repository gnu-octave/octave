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
## @deftypefn  {} {[@var{perm}, @var{found}] =} __isomorphism_vf2__ (@var{A1}, @var{A2}, @var{directed})
## @deftypefnx {} {[@var{perm}, @var{found}] =} __isomorphism_vf2__ (@var{A1}, @var{A2}, @var{directed}, @var{nc1}, @var{nc2})
## @deftypefnx {} {[@var{perm}, @var{found}] =} __isomorphism_vf2__ (@var{A1}, @var{A2}, @var{directed}, @var{nc1}, @var{nc2}, @var{ec1}, @var{ec2})
## Private helper implementing the VF2 (Cordella, Foggia, Sansone,
## Vento, 2004) graph isomorphism search.
##
## @var{A1} and @var{A2} are N-by-N adjacency matrices (sparse or
## full, numeric).  For an undirected graph, both matrices are
## expected to be symmetric with off-diagonal edges stored at both
## @code{(i, j)} and @code{(j, i)} positions and self-loops stored
## once at @code{(i, i)}.  For a directed graph, @code{A(i, j)}
## counts the number of edges from node @math{i} to node @math{j}.
##
## @var{directed} is a logical flag selecting the directed or
## undirected VF2 variant.
##
## @var{nc1} and @var{nc2} (optional, pass @code{[]} to disable) are
## N-by-1 integer vectors of per-node colors.  When supplied, the
## search prunes any candidate pair @math{(n_1, n_2)} with
## @code{@var{nc1}(n_1) != @var{nc2}(n_2)} and rejects early if
## @code{sort (@var{nc1}) != sort (@var{nc2})}.
##
## @var{ec1} and @var{ec2} (optional, pass @code{[]} to disable) are
## N-by-N numeric matrices of per-edge colors (0 where no edge, a
## positive color elsewhere).  When supplied, the feasibility test
## additionally requires that, for every already-mapped pair
## @math{(x_1 \to x_2)}, @code{@var{ec1}(n_1, x_1) ==
## @var{ec2}(n_2, x_2)} (and, for directed graphs, the transpose
## entry too), plus matching self-loop colors at the candidate pair.
##
## The output @var{perm} is a column vector of length @math{N} such
## that @code{A2 == A1(perm, perm)}.  That is, @code{perm(i)} is the
## node in @var{A1} that matches node @math{i} in @var{A2}.
## (Equivalently, reordering @var{A2} by @var{perm} yields the same
## matrix as @var{A1} when the inverse permutation is applied, and
## vice versa; the scalar logical @var{found} is the final success
## flag.)  When no isomorphism exists, @var{perm} is @code{[]} and
## @var{found} is @code{false}.
##
## The algorithm prunes partial mappings with:
## @itemize @bullet
## @item Degree equality between the candidate pair, computed from
## the adjacency-matrix row and column sums so multigraph edge
## multiplicities are honoured.
## @item Consistency with the already-mapped neighbourhood: the new
## candidate's edges to already-mapped nodes in @var{A1} must mirror
## its partner's edges to the matched nodes in @var{A2}.
## @item Self-loop parity: diagonal entries at the candidate pair
## must be equal.
## @item Quick rejects before the search: node count, total edge
## count, sorted row and column sums, and the multiset of diagonal
## entries must all match.
## @item (Optional) Node-color equality and sorted-color-multiset
## quick reject when @var{nc1}/@var{nc2} are supplied.
## @item (Optional) Edge-color consistency with the mapped
## neighbourhood when @var{ec1}/@var{ec2} are supplied.
## @end itemize
##
## This helper is used internally by @code{isisomorphic} and
## @code{isomorphism}; the two-output form lets the caller
## distinguish an empty-graph match (@code{perm} is a 0-by-1
## column, @code{found} is @code{true}) from a no-isomorphism
## result (@code{perm} is @code{[]}, @code{found} is @code{false}).
## @seealso{isisomorphic, isomorphism}
## @end deftypefn

function [perm, found] = __isomorphism_vf2__ (A1, A2, directed, ...
                                              nc1 = [], nc2 = [], ...
                                              ec1 = [], ec2 = [])

  if (nargin != 3 && nargin != 5 && nargin != 7)
    error ("Octave:invalid-fun-call", ...
           ["__isomorphism_vf2__: expected 3, 5, or 7 arguments ", ...
            "(A1, A2, directed [, nc1, nc2 [, ec1, ec2]])"]);
  endif

  perm = [];
  found = false;

  N = size (A1, 1);
  if (N != size (A1, 2))
    error ("Octave:invalid-input-arg", ...
           "__isomorphism_vf2__: A1 must be square");
  endif
  if (size (A2, 1) != size (A2, 2))
    error ("Octave:invalid-input-arg", ...
           "__isomorphism_vf2__: A2 must be square");
  endif

  ## Node-count mismatch: not isomorphic.
  if (size (A2, 1) != N)
    return;
  endif

  ## Empty graphs: trivially isomorphic, empty permutation.
  if (N == 0)
    perm = zeros (0, 1);
    found = true;
    return;
  endif

  ## Total edge count (sum of entries) must match.
  s1 = full (sum (A1(:)));
  s2 = full (sum (A2(:)));
  if (s1 != s2)
    return;
  endif

  ## Diagonal multiset (self-loops / self-loop multiplicities) must match.
  d1 = sort (full (diag (A1)));
  d2 = sort (full (diag (A2)));
  if (! isequal (d1, d2))
    return;
  endif

  ## Row-sum and column-sum multisets (degrees) must match.  For
  ## undirected graphs the two are equal and we only need one; for
  ## directed graphs both are required.
  r1 = full (sum (A1, 2));
  r2 = full (sum (A2, 2));
  if (! isequal (sort (r1), sort (r2)))
    return;
  endif
  if (directed)
    c1 = full (sum (A1, 1))(:);
    c2 = full (sum (A2, 1))(:);
    if (! isequal (sort (c1), sort (c2)))
      return;
    endif
  endif

  ## Optional node-color quick reject.
  use_nc = ! isempty (nc1) && ! isempty (nc2);
  if (use_nc)
    nc1 = nc1(:);
    nc2 = nc2(:);
    if (numel (nc1) != N || numel (nc2) != N)
      error ("Octave:invalid-input-arg", ...
             "__isomorphism_vf2__: nc1, nc2 must be N-by-1 or empty");
    endif
    if (! isequal (sort (nc1), sort (nc2)))
      return;
    endif
  endif

  ## Optional edge-color quick reject.
  use_ec = ! isempty (ec1) && ! isempty (ec2);
  if (use_ec)
    if (size (ec1, 1) != N || size (ec1, 2) != N ...
        || size (ec2, 1) != N || size (ec2, 2) != N)
      error ("Octave:invalid-input-arg", ...
             "__isomorphism_vf2__: ec1, ec2 must be N-by-N or empty");
    endif
    ## Multiset of non-zero edge colors must match across the two
    ## graphs, otherwise no edge-color-preserving mapping can exist.
    c1_all = full (ec1(:));
    c2_all = full (ec2(:));
    c1_nz = sort (c1_all(c1_all != 0));
    c2_nz = sort (c2_all(c2_all != 0));
    if (! isequal (c1_nz, c2_nz))
      return;
    endif
  endif

  ## Convert to dense arrays for fast elementwise work during the
  ## recursive search.  VF2 is memory-light (O(N^2) bytes for each
  ## matrix) so this is safe for the graph sizes where VF2 is the
  ## right algorithm in the first place.
  B1 = full (A1);
  B2 = full (A2);
  if (use_ec)
    EC1 = full (ec1);
    EC2 = full (ec2);
  else
    EC1 = [];
    EC2 = [];
  endif

  ## Precompute row/column sums used as per-node degree fingerprints
  ## during candidate pruning.  For undirected graphs, column sum ==
  ## row sum, so reuse r.
  r1v = full (sum (B1, 2));
  r2v = full (sum (B2, 2));
  if (directed)
    c1v = full (sum (B1, 1))(:);
    c2v = full (sum (B2, 1))(:);
  else
    c1v = r1v;
    c2v = r2v;
  endif

  ## f12(i) = node in G2 matched to G1 node i (0 = unmapped).
  ## f21(j) = node in G1 matched to G2 node j (0 = unmapped).
  f12 = zeros (N, 1);
  f21 = zeros (N, 1);

  if (! use_nc)
    nc1 = [];
    nc2 = [];
  endif

  [perm, found] = ...
    vf2_search (B1, B2, directed, r1v, r2v, c1v, c2v, ...
                nc1, nc2, EC1, EC2, f12, f21, N, 0);

endfunction


## Recursive VF2 core.  Returns (perm, true) on success, ([], false)
## otherwise.  All state is passed explicitly so that recursion does
## not require closures.
function [perm, found] = vf2_search (B1, B2, directed, r1v, r2v, ...
                                     c1v, c2v, nc1, nc2, EC1, EC2, ...
                                     f12, f21, N, depth)

  if (depth == N)
    ## Full mapping: return the permutation that reorders nodes of
    ## G2 into G1 order.  perm(i) = G1-node matched to G2-node i,
    ## which is exactly f21.
    perm = f21;
    found = true;
    return;
  endif

  ## Build VF2 terminal sets: unmapped nodes that are adjacent (in or
  ## out for directed; either for undirected) to some already-mapped
  ## node.  When the mapping is empty or no terminal nodes are
  ## adjacent, fall back to all unmapped nodes.
  mapped1 = find (f12 != 0);
  if (isempty (mapped1))
    T1 = find (f12 == 0);
    T2 = find (f21 == 0);
  else
    mapped2 = f12(mapped1);
    if (directed)
      ## Outgoing and incoming neighbourhoods of the mapped set.
      out1 = any (B1(mapped1, :) != 0, 1);
      in1  = any (B1(:, mapped1) != 0, 2)';
      cand1 = (out1 | in1) & (f12 == 0)';
      T1 = find (cand1);
      out2 = any (B2(mapped2, :) != 0, 1);
      in2  = any (B2(:, mapped2) != 0, 2)';
      cand2 = (out2 | in2) & (f21 == 0)';
      T2 = find (cand2);
    else
      n1 = any (B1(mapped1, :) != 0, 1);
      cand1 = n1 & (f12 == 0)';
      T1 = find (cand1);
      n2 = any (B2(mapped2, :) != 0, 1);
      cand2 = n2 & (f21 == 0)';
      T2 = find (cand2);
    endif
    if (isempty (T1) && isempty (T2))
      T1 = find (f12 == 0);
      T2 = find (f21 == 0);
    elseif (isempty (T1) || isempty (T2))
      ## Terminal-set size mismatch -> prune this branch.
      perm = [];
      found = false;
      return;
    endif
  endif

  ## VF2 picks one fixed G2 node (smallest index in T2 for
  ## determinism) and tries every candidate in T1 against it.  This
  ## prunes away symmetric branches immediately.
  n2 = T2(1);
  for k = 1 : numel (T1)
    n1 = T1(k);
    if (feasible (B1, B2, directed, r1v, r2v, c1v, c2v, ...
                  nc1, nc2, EC1, EC2, f12, f21, n1, n2))
      f12(n1) = n2;
      f21(n2) = n1;
      [perm, found] = ...
        vf2_search (B1, B2, directed, r1v, r2v, c1v, c2v, ...
                    nc1, nc2, EC1, EC2, f12, f21, N, depth + 1);
      if (found)
        return;
      endif
      f12(n1) = 0;
      f21(n2) = 0;
    endif
  endfor

  perm = [];
  found = false;

endfunction


## Feasibility test for candidate pair (n1, n2) given the current
## partial mapping.  Returns true iff adding (n1 -> n2) keeps the
## mapping a valid partial isomorphism.
function tf = feasible (B1, B2, directed, r1v, r2v, c1v, c2v, ...
                        nc1, nc2, EC1, EC2, f12, f21, n1, n2)

  ## Node-color pruning (optional).
  if (! isempty (nc1))
    if (nc1(n1) != nc2(n2))
      tf = false;
      return;
    endif
  endif

  ## Degree fingerprint must match.  For directed: both in- and
  ## out-degree.  For undirected: single degree (r1v == c1v).
  if (r1v(n1) != r2v(n2))
    tf = false;
    return;
  endif
  if (directed && c1v(n1) != c2v(n2))
    tf = false;
    return;
  endif

  ## Self-loop parity (diagonal entry at the candidate pair).
  if (B1(n1, n1) != B2(n2, n2))
    tf = false;
    return;
  endif

  ## Self-loop color parity (optional).
  if (! isempty (EC1))
    if (EC1(n1, n1) != EC2(n2, n2))
      tf = false;
      return;
    endif
  endif

  ## Consistency with the already-mapped neighbourhood.  For each
  ## mapped G1 node x1 -> x2, the edges between n1 and x1 in B1 must
  ## equal the edges between n2 and x2 in B2 (in both directions for
  ## a digraph; only one direction for a graph since both matrices
  ## are symmetric).
  mapped1 = find (f12 != 0);
  if (isempty (mapped1))
    tf = true;
    return;
  endif
  mapped2 = f12(mapped1);

  if (directed)
    if (any (B1(n1, mapped1)(:) != B2(n2, mapped2)(:)))
      tf = false;
      return;
    endif
    if (any (B1(mapped1, n1)(:) != B2(mapped2, n2)(:)))
      tf = false;
      return;
    endif
    if (! isempty (EC1))
      if (any (EC1(n1, mapped1)(:) != EC2(n2, mapped2)(:)))
        tf = false;
        return;
      endif
      if (any (EC1(mapped1, n1)(:) != EC2(mapped2, n2)(:)))
        tf = false;
        return;
      endif
    endif
  else
    if (any (B1(n1, mapped1)(:) != B2(n2, mapped2)(:)))
      tf = false;
      return;
    endif
    if (! isempty (EC1))
      if (any (EC1(n1, mapped1)(:) != EC2(n2, mapped2)(:)))
        tf = false;
        return;
      endif
    endif
  endif

  tf = true;

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Identity permutation on a 3-cycle (directed).
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [p, f] = __isomorphism_vf2__ (A, A, true);
%! assert (f, true);
%! assert (numel (p), 3);
%! assert (isequal (A, A(p, p)));

## Directed 3-cycle with nodes relabelled.
%!test
%! A1 = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! A2 = sparse ([3 1 2], [1 2 3], 1, 3, 3);    # relabelled 3-cycle
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, true);
%! assert (isequal (A2, A1(p, p)));

## Undirected triangle: two different labellings are isomorphic.
%!test
%! s1 = [1 2 3]; t1 = [2 3 1];
%! A1 = sparse ([s1 t1], [t1 s1], 1, 3, 3);
%! s2 = [2 3 1]; t2 = [3 1 2];
%! A2 = sparse ([s2 t2], [t2 s2], 1, 3, 3);
%! [p, f] = __isomorphism_vf2__ (A1, A2, false);
%! assert (f, true);
%! assert (isequal (A2, A1(p, p)));

## Empty matrices: isomorphic with empty permutation.
%!test
%! [p, f] = __isomorphism_vf2__ (sparse (0, 0), sparse (0, 0), true);
%! assert (f, true);
%! assert (size (p), [0, 1]);

## Different sizes: not isomorphic.
%!test
%! A1 = sparse ([1 2], [2 1], 1, 2, 2);
%! A2 = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, false);
%! assert (isempty (p));

## Different edge counts: not isomorphic.
%!test
%! A1 = sparse ([1 2], [2 3], 1, 3, 3);
%! A2 = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, false);

## Different degree sequences (directed): not isomorphic.
%!test
%! A1 = sparse ([1 2 3], [2 3 1], 1, 3, 3);   # 3-cycle
%! A2 = sparse ([1 2 3], [3 3 1], 1, 3, 3);   # not a cycle: two into 3
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, false);

## Self-loop mismatch.
%!test
%! A1 = sparse ([1 2 3], [1 3 1], 1, 3, 3);
%! A2 = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, false);

## Undirected K_{3,3} vs triangular prism (both 3-regular 6-node, but
## not isomorphic since the prism has triangles and K_{3,3} doesn't).
%!test
%! ## K_{3,3}.
%! e1s = [1 1 1 2 2 2 3 3 3];
%! e1t = [4 5 6 4 5 6 4 5 6];
%! A1 = sparse ([e1s e1t], [e1t e1s], 1, 6, 6);
%! ## Triangular prism: triangles {1,2,3} and {4,5,6} plus matching
%! ## 1-4, 2-5, 3-6.
%! e2s = [1 2 3 4 5 6 1 2 3];
%! e2t = [2 3 1 5 6 4 4 5 6];
%! A2 = sparse ([e2s e2t], [e2t e2s], 1, 6, 6);
%! [p, f] = __isomorphism_vf2__ (A1, A2, false);
%! assert (f, false);

## Directed path P_4 permuted -> isomorphic.
%!test
%! A1 = sparse ([1 2 3], [2 3 4], 1, 4, 4);
%! perm = [3 1 4 2];
%! A2 = A1(perm, perm);
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, true);
%! assert (isequal (A2, A1(p, p)));

## Multigraph (directed): edge multiplicities respected.
%!test
%! ## A1: edges 1->2 twice, 2->3 once.
%! A1 = sparse ([1 1 2], [2 2 3], 1, 3, 3);
%! A2 = A1;
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, true);

## Directed multigraphs with the same sorted in- and out-degrees but
## different multi-edge adjacency.  A1 puts the doubled edge at the
## out-degree-2 -> out-degree-1 hop; A2 puts it at the
## out-degree-2 -> out-degree-0 hop.  Not isomorphic.
%!test
%! A1 = sparse ([1 2], [2 3], [2 1], 3, 3);   # counts: 1->2=2, 2->3=1
%! A2 = sparse ([1 2], [2 3], [1 2], 3, 3);   # counts: 1->2=1, 2->3=2
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, false);

## Non-isomorphic multigraphs with matching degree sequences: one is
## a directed 2-cycle with multi-edge 1->2 weight 2, other has 1->2
## weight 1 plus a self-loop.
%!test
%! A1 = sparse ([1 2 1], [2 1 1], [2 1 1], 2, 2);  # (1,2)=2, (2,1)=1, (1,1)=1
%! A2 = sparse ([1 2], [2 1], [2 1], 2, 2);        # no self-loop
%! [p, f] = __isomorphism_vf2__ (A1, A2, true);
%! assert (f, false);

## Wrong argument count.
%!error __isomorphism_vf2__ ()
%!error __isomorphism_vf2__ (sparse (0, 0), sparse (0, 0))

## Node colors: matching colors on a directed 3-cycle -> isomorphic.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! nc = [1; 2; 3];
%! [p, f] = __isomorphism_vf2__ (A, A, true, nc, nc);
%! assert (f, true);
%! assert (p, (1:3).');

## Node colors: different color multisets -> not isomorphic.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! nc1 = [1; 1; 2];
%! nc2 = [1; 2; 2];
%! [p, f] = __isomorphism_vf2__ (A, A, true, nc1, nc2);
%! assert (f, false);

## Node colors: permuted colors on a 3-cycle force a specific map.
%!test
%! A1 = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! A2 = A1;
%! nc1 = [1; 2; 3];
%! nc2 = [3; 1; 2];   # cyclic shift of nc1
%! [p, f] = __isomorphism_vf2__ (A1, A2, true, nc1, nc2);
%! assert (f, true);
%! assert (nc1(p), nc2);
%! assert (isequal (A2, A1(p, p)));

## Edge colors: matching colors on a directed 3-cycle -> isomorphic.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! EC = sparse ([1 2 3], [2 3 1], [10 20 30], 3, 3);
%! [p, f] = __isomorphism_vf2__ (A, A, true, [], [], EC, EC);
%! assert (f, true);

## Edge colors: different color multisets -> not isomorphic.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! EC1 = sparse ([1 2 3], [2 3 1], [10 20 30], 3, 3);
%! EC2 = sparse ([1 2 3], [2 3 1], [10 20 40], 3, 3);
%! [p, f] = __isomorphism_vf2__ (A, A, true, [], [], EC1, EC2);
%! assert (f, false);

## Combined node and edge colors (undirected triangle).
%!test
%! s = [1 2 3]; t = [2 3 1];
%! A = sparse ([s t], [t s], 1, 3, 3);
%! EC = sparse ([s t], [t s], [10 20 30 10 20 30], 3, 3);
%! nc = [1; 2; 3];
%! [p, f] = __isomorphism_vf2__ (A, A, false, nc, nc, EC, EC);
%! assert (f, true);
%! assert (p, (1:3).');

## nc size mismatch -> error.
%!error <N-by-1> ...
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! __isomorphism_vf2__ (A, A, true, [1; 2], [1; 2; 3]);

## ec size mismatch -> error.
%!error <N-by-N> ...
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! __isomorphism_vf2__ (A, A, true, [], [], sparse (2, 2), sparse (3, 3));

## 6-argument call rejected (must be 3, 5, or 7).
%!error <arguments> ...
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! __isomorphism_vf2__ (A, A, true, [], [], sparse (3, 3));

## Non-square A1.
%!error <square> ...
%! __isomorphism_vf2__ (sparse (2, 3), sparse (2, 2), true)

## Non-square A2.
%!error <square> ...
%! __isomorphism_vf2__ (sparse (2, 2), sparse (2, 3), true)
