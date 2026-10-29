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
## @deftypefn {} {@var{bins} =} __conncomp_strong__ (@var{A})
## Private helper: compute strongly connected component (SCC) labels of
## the @math{N}-by-@math{N} (sparse) adjacency matrix @var{A} using an
## iterative variant of Tarjan's algorithm.
##
## Returns a @code{1}-by-@math{N} row vector of double labels in
## @code{1:K}, where @math{K} is the number of SCCs.  Two nodes share a
## label iff there is a directed path from each to the other.  Labels
## are assigned so that when we scan nodes from 1 upward, the smallest
## unlabelled index always receives the smallest unused label -- i.e.
## the SCC containing node 1 is labelled 1, the SCC of the next
## unlabelled node is labelled 2, and so on.
##
## Self-loops and parallel edges do not merge SCCs that would not
## otherwise be merged: any nonzero entry is treated as an edge, but
## a self-loop alone does not join a node to any other node.
##
## @seealso{conncomp, __conncomp_weak__}
## @end deftypefn

function bins = __conncomp_strong__ (A)

  if (nargin != 1)
    print_usage ();
  endif

  N = size (A, 1);
  if (N == 0)
    bins = zeros (1, 0);
    return;
  endif

  ## Iterative Tarjan's SCC.  Work stack entries are columns of a
  ## preallocated matrix: [u; cursor; children_ptr].  We keep the
  ## children lists in a parallel cell array to avoid repeatedly
  ## computing find() on the adjacency.
  idx = zeros (N, 1);       # 0 = unvisited; otherwise DFS discovery index
  lowlink = zeros (N, 1);
  on_stack = false (N, 1);
  raw = zeros (1, N);       # raw SCC id per node

  index_ctr = 0;
  scc_ctr = 0;

  ## SCC stack
  S = zeros (N, 1);
  Stop = 0;

  ## Work stack: parallel arrays.
  work_u = zeros (N + 1, 1);
  work_cursor = zeros (N + 1, 1);
  work_children = cell (N + 1, 1);
  Wtop = 0;

  for start = 1:N
    if (idx(start) != 0)
      continue;
    endif

    ## Push start onto work stack.
    index_ctr = index_ctr + 1;
    idx(start) = index_ctr;
    lowlink(start) = index_ctr;
    Stop = Stop + 1;
    S(Stop) = start;
    on_stack(start) = true;

    Wtop = Wtop + 1;
    work_u(Wtop) = start;
    work_cursor(Wtop) = 1;
    work_children{Wtop} = find (A(start, :));

    while (Wtop > 0)
      u = work_u(Wtop);
      cur = work_cursor(Wtop);
      children = work_children{Wtop};

      if (cur > numel (children))
        ## All children processed.  If u is a root (lowlink==idx), pop
        ## the SCC.
        if (lowlink(u) == idx(u))
          scc_ctr = scc_ctr + 1;
          do
            w = S(Stop);
            Stop = Stop - 1;
            on_stack(w) = false;
            raw(w) = scc_ctr;
          until (w == u)
        endif
        ## Pop this frame.
        Wtop = Wtop - 1;
        ## Propagate lowlink to parent (if any).
        if (Wtop > 0)
          p = work_u(Wtop);
          if (lowlink(u) < lowlink(p))
            lowlink(p) = lowlink(u);
          endif
        endif
        continue;
      endif

      v = children(cur);
      work_cursor(Wtop) = cur + 1;

      if (idx(v) == 0)
        ## Tree edge: recurse.
        index_ctr = index_ctr + 1;
        idx(v) = index_ctr;
        lowlink(v) = index_ctr;
        Stop = Stop + 1;
        S(Stop) = v;
        on_stack(v) = true;

        Wtop = Wtop + 1;
        work_u(Wtop) = v;
        work_cursor(Wtop) = 1;
        work_children{Wtop} = find (A(v, :));
      elseif (on_stack(v))
        ## Back edge.
        if (idx(v) < lowlink(u))
          lowlink(u) = idx(v);
        endif
      endif
      ## else: cross/forward edge, ignore.
    endwhile
  endfor

  ## Relabel so SCC-of-min-index gets label 1, etc.
  bins = zeros (1, N);
  next_label = 0;
  seen = zeros (scc_ctr, 1);    # 0 = unassigned
  for i = 1:N
    r = raw(i);
    if (seen(r) == 0)
      next_label = next_label + 1;
      seen(r) = next_label;
    endif
    bins(i) = seen(r);
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty adjacency -> empty row vector.
%!test
%! bins = __conncomp_strong__ (sparse (0, 0));
%! assert (size (bins), [1, 0]);

## Single node -> one SCC.
%!test
%! bins = __conncomp_strong__ (sparse (1, 1));
%! assert (bins, 1);

## Three isolated nodes -> three SCCs.
%!test
%! bins = __conncomp_strong__ (sparse (3, 3));
%! assert (bins, [1, 2, 3]);

## 3-cycle -> one SCC.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! bins = __conncomp_strong__ (A);
%! assert (bins, [1, 1, 1]);

## Line 1->2->3 -> three SCCs, labels in scan order.
%!test
%! A = sparse ([1 2], [2 3], 1, 3, 3);
%! bins = __conncomp_strong__ (A);
%! assert (bins, [1, 2, 3]);

## Two back-to-back 2-cycles: {1,2} and {3,4}.
%!test
%! A = sparse ([1 2 3 4], [2 1 4 3], 1, 4, 4);
%! bins = __conncomp_strong__ (A);
%! assert (bins, [1, 1, 2, 2]);

## MATLAB doc example: 1->2->3->1 and 4->5.
%!test
%! A = sparse ([1 2 3 4], [2 3 1 5], 1, 5, 5);
%! bins = __conncomp_strong__ (A);
%! assert (bins, [1, 1, 1, 2, 3]);

## Self-loop: one SCC (node alone).
%!test
%! A = sparse ([1], [1], 1, 1, 1);
%! bins = __conncomp_strong__ (A);
%! assert (bins, 1);

## Line with self-loop at 2: still three SCCs.
%!test
%! A = sparse ([1 2 2], [2 2 3], 1, 3, 3);
%! bins = __conncomp_strong__ (A);
%! assert (bins, [1, 2, 3]);

## Two disjoint 3-cycles.
%!test
%! A = sparse ([1 2 3 4 5 6], [2 3 1 5 6 4], 1, 6, 6);
%! bins = __conncomp_strong__ (A);
%! assert (bins, [1, 1, 1, 2, 2, 2]);
