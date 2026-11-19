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
## @deftypefn {} {[@var{bins}, @var{is_art}] =} __biconncomp__ (@var{E}, @var{N})
## Private helper: compute the biconnected components of an undirected
## simple graph represented by its edge list @var{E} (an
## @math{m}-by-@math{2} lexicographically sorted matrix of endpoint
## pairs) and node count @var{N}.
##
## The implementation is the classical Hopcroft-Tarjan DFS with an edge
## stack, written iteratively to avoid Octave's recursion limits.  Each
## self-loop is assigned to its own biconnected component up front, so
## the edge-stack logic only sees simple edges.
##
## Returns:
## @itemize
## @item
## @var{bins}: a @code{1}-by-@math{m} row vector of @code{double}
## labels in @code{1:K} giving the BCC index of each edge, in the
## order edges appear in @var{E}.  Labels are reindexed so that the
## BCC containing the smallest-index unlabelled edge always gets the
## next unused label; this yields @code{bins(1) == 1} for @math{m > 0}
## and MATLAB-parity first-edge ordering.
## @item
## @var{is_art}: a @code{1}-by-@math{N} logical row vector where
## @code{is_art(v)} is true iff @math{v} is an articulation point
## (cut vertex).
## @end itemize
##
## @seealso{biconncomp}
## @end deftypefn

function [bins, is_art] = __biconncomp__ (E, N)

  if (nargin != 2)
    print_usage ();
  endif

  M = size (E, 1);
  bins = zeros (1, M);
  is_art = false (1, N);

  if (N == 0)
    return;
  endif

  ## Self-loops: each gets its own BCC up front.  This keeps the edge-
  ## stack logic in the DFS loop free of the awkward "back-edge to
  ## self" special case (disc(v) == disc(u)) and makes the labelling
  ## deterministic.
  bcc_count = 0;
  is_self = (E(:, 1) == E(:, 2));
  self_idx = find (is_self);
  for k = 1:numel (self_idx)
    bcc_count = bcc_count + 1;
    bins(self_idx(k)) = bcc_count;
  endfor

  ## Adjacency list over non-self-loop edges.  Preallocate each row so
  ## filling is O(deg).  Store the edge id alongside the neighbour so
  ## we can filter the "parent edge" cleanly and push edges onto the
  ## BCC stack by id.
  deg = zeros (N, 1);
  for i = 1:M
    if (! is_self(i))
      deg(E(i, 1)) += 1;
      deg(E(i, 2)) += 1;
    endif
  endfor

  adj_nbr = cell (N, 1);
  adj_eid = cell (N, 1);
  for u = 1:N
    adj_nbr{u} = zeros (deg(u), 1);
    adj_eid{u} = zeros (deg(u), 1);
  endfor

  pos = ones (N, 1);
  for i = 1:M
    if (! is_self(i))
      u = E(i, 1);
      v = E(i, 2);
      adj_nbr{u}(pos(u)) = v;
      adj_eid{u}(pos(u)) = i;
      pos(u) += 1;
      adj_nbr{v}(pos(v)) = u;
      adj_eid{v}(pos(v)) = i;
      pos(v) += 1;
    endif
  endfor

  ## DFS state
  disc = zeros (1, N);    # discovery time; 0 means unvisited
  low_v = zeros (1, N);   # low-point
  time = 0;

  ## Edge stack (push/pop tree and back edges during DFS; pop when a
  ## BCC is completed)
  edge_stk = zeros (M, 1);
  top_e = 0;

  ## Work stack: three parallel arrays hold (u, cursor into adj, p_edge)
  stk_u = zeros (N + 1, 1);
  stk_cursor = zeros (N + 1, 1);
  stk_pedge = zeros (N + 1, 1);

  for root = 1:N
    if (disc(root) != 0)
      continue;
    endif

    time = time + 1;
    disc(root) = time;
    low_v(root) = time;

    top_s = 1;
    stk_u(1) = root;
    stk_cursor(1) = 1;
    stk_pedge(1) = 0;

    ## Count tree children of the root.  The root is an articulation
    ## point iff it has two or more tree children in its DFS spanning
    ## tree of this connected component.
    root_children = 0;

    while (top_s > 0)
      u = stk_u(top_s);
      cursor = stk_cursor(top_s);
      p_edge = stk_pedge(top_s);

      nbrs = adj_nbr{u};
      eids = adj_eid{u};

      if (cursor > numel (nbrs))
        ## Finished exploring u; pop this frame and propagate to parent.
        top_s = top_s - 1;

        if (top_s > 0)
          p = stk_u(top_s);

          ## Propagate low(u) up to the parent.
          if (low_v(u) < low_v(p))
            low_v(p) = low_v(u);
          endif

          ## Close a BCC whenever the child's reach does not exceed the
          ## parent's discovery time.
          if (low_v(u) >= disc(p))
            bcc_count = bcc_count + 1;
            ## Pop edges off the stack until (and including) the tree
            ## edge that descended into this subtree.
            while (true)
              e = edge_stk(top_e);
              top_e = top_e - 1;
              bins(e) = bcc_count;
              if (e == p_edge)
                break;
              endif
            endwhile
            ## Parent p is an articulation point unless it is the DFS
            ## root (the root case is handled via the child count).
            if (p != root)
              is_art(p) = true;
            endif
          endif
        else
          ## We just popped the root frame.  The root is an articulation
          ## point iff it has two or more tree children.
          if (root_children >= 2)
            is_art(root) = true;
          endif
        endif

      else
        ## Try the next neighbour.
        v = nbrs(cursor);
        e_id = eids(cursor);
        stk_cursor(top_s) = cursor + 1;

        ## Skip the edge we came in on (graph is simple, so we can
        ## filter by edge id unambiguously).
        if (e_id == p_edge)
          continue;
        endif

        if (disc(v) == 0)
          ## Tree edge: push onto edge stack, descend into v.
          top_e = top_e + 1;
          edge_stk(top_e) = e_id;

          if (u == root)
            root_children = root_children + 1;
          endif

          time = time + 1;
          disc(v) = time;
          low_v(v) = time;

          top_s = top_s + 1;
          stk_u(top_s) = v;
          stk_cursor(top_s) = 1;
          stk_pedge(top_s) = e_id;

        elseif (disc(v) < disc(u))
          ## Back edge to a proper ancestor.  Push onto the edge stack
          ## and update low(u).
          top_e = top_e + 1;
          edge_stk(top_e) = e_id;
          if (disc(v) < low_v(u))
            low_v(u) = disc(v);
          endif
        endif
        ## Remaining case disc(v) > disc(u): the edge was already
        ## pushed when we were at v (as either a tree or back edge), so
        ## do nothing.
      endif
    endwhile
  endfor

  ## Relabel BCCs so that the component containing the smallest
  ## unlabelled edge always gets the next available label.  This gives
  ## MATLAB-parity first-edge-index ordering.
  if (M > 0 && bcc_count > 0)
    remap = zeros (1, bcc_count);
    next_label = 0;
    for i = 1:M
      old = bins(i);
      if (remap(old) == 0)
        next_label = next_label + 1;
        remap(old) = next_label;
      endif
      bins(i) = remap(old);
    endfor
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty (0 nodes, 0 edges): empty results.
%!test
%! [bins, is_art] = __biconncomp__ (zeros (0, 2), 0);
%! assert (size (bins), [1, 0]);
%! assert (size (is_art), [1, 0]);

## Single isolated node: no edges.
%!test
%! [bins, is_art] = __biconncomp__ (zeros (0, 2), 1);
%! assert (size (bins), [1, 0]);
%! assert (is_art, false);

## N isolated nodes.
%!test
%! [bins, is_art] = __biconncomp__ (zeros (0, 2), 4);
%! assert (size (bins), [1, 0]);
%! assert (is_art, [false false false false]);

## Single edge (1-2): one BCC, no articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 2], 2);
%! assert (bins, 1);
%! assert (is_art, [false false]);

## Path 1-2-3: two BCCs, node 2 articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 2; 2 3], 3);
%! assert (bins, [1, 2]);
%! assert (is_art, [false true false]);

## Triangle: one BCC, no articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 2; 1 3; 2 3], 3);
%! assert (bins, [1, 1, 1]);
%! assert (is_art, [false false false]);

## Self-loop: one BCC, no articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 1], 1);
%! assert (bins, 1);
%! assert (is_art, false);

## Self-loop plus bridge: two BCCs, no articulation (a single real
## edge incident to the self-loop's node does not disconnect anything).
%!test
%! [bins, is_art] = __biconncomp__ ([1 1; 1 2], 2);
%! assert (bins, [1, 2]);
%! assert (is_art, [false false]);

## Lollipop: triangle 1-2-3 + bridge 3-4.  Node 3 is articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 2; 1 3; 2 3; 3 4], 4);
%! assert (bins, [1, 1, 1, 2]);
%! assert (is_art, [false false true false]);

## Bowtie: two triangles sharing node 3.  Node 3 is articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 2; 1 3; 2 3; 3 4; 3 5; 4 5], 5);
%! assert (bins, [1, 1, 1, 2, 2, 2]);
%! assert (is_art, [false false true false false]);

## Two disconnected edges: two BCCs, no articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 2; 3 4], 4);
%! assert (bins, [1, 2]);
%! assert (is_art, [false false false false]);

## Disconnected edge + isolated node: one BCC, no articulation.
%!test
%! [bins, is_art] = __biconncomp__ ([1 2], 3);
%! assert (bins, 1);
%! assert (is_art, [false false false]);

## Long path with triangle end (as in public test).
%!test
%! E = [1 2; 2 3; 3 4; 4 5; 4 6; 5 6];
%! [bins, is_art] = __biconncomp__ (E, 6);
%! assert (bins, [1, 2, 3, 4, 4, 4]);
%! assert (is_art, [false true true true false false]);

## K4: one BCC, no articulation.
%!test
%! E = [1 2; 1 3; 1 4; 2 3; 2 4; 3 4];
%! [bins, is_art] = __biconncomp__ (E, 4);
%! assert (bins, [1, 1, 1, 1, 1, 1]);
%! assert (is_art, [false false false false]);
