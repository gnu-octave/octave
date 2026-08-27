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
## @deftypefn {} {@var{bins} =} __conncomp_weak__ (@var{A})
## Private helper: compute weak connected-component labels on the
## @math{N}-by-@math{N} sparse (or dense) adjacency matrix @var{A}.
##
## Treats the graph as undirected by working with the symmetrized
## adjacency @code{A | A.'}.  Returns a @code{1}-by-@math{N} row vector
## of double labels in @code{1:K}, where @math{K} is the number of
## components.  Components are labelled in the order they are first
## discovered when scanning nodes from 1 upward: the component
## containing the smallest unlabelled index gets the next available
## label.
##
## Self-loops and parallel edges are inconsequential for component
## discovery and are handled transparently.
##
## @seealso{conncomp, __conncomp_strong__}
## @end deftypefn

function bins = __conncomp_weak__ (A)

  if (nargin != 1)
    print_usage ();
  endif

  N = size (A, 1);
  if (N == 0)
    bins = zeros (1, 0);
    return;
  endif

  ## Symmetrize: treat every edge as undirected for component discovery.
  ## Use logical OR to ignore weights and duplicates.
  S = spones (A) | spones (A.');

  bins = zeros (1, N);
  label = 0;
  ## Simple BFS from each unlabelled node.  Use a manually-managed queue
  ## on a preallocated double vector to avoid O(N^2) shifting.
  queue = zeros (N, 1);
  for start = 1:N
    if (bins(start) != 0)
      continue;
    endif
    label = label + 1;
    bins(start) = label;
    qhead = 1;
    qtail = 1;
    queue(1) = start;
    while (qhead <= qtail)
      u = queue(qhead);
      qhead = qhead + 1;
      nbrs = find (S(u, :));
      for v = nbrs
        if (bins(v) == 0)
          bins(v) = label;
          qtail = qtail + 1;
          queue(qtail) = v;
        endif
      endfor
    endwhile
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty adjacency -> empty row vector.
%!test
%! bins = __conncomp_weak__ (sparse (0, 0));
%! assert (size (bins), [1, 0]);
%! assert (class (bins), "double");

## Single isolated node.
%!test
%! bins = __conncomp_weak__ (sparse (1, 1));
%! assert (bins, 1);

## Three isolated nodes.
%!test
%! bins = __conncomp_weak__ (sparse (3, 3));
%! assert (bins, [1, 2, 3]);

## Directed edge 1->2: symmetrizes so 1 and 2 share a label.
%!test
%! A = sparse ([1], [2], 1, 2, 2);
%! bins = __conncomp_weak__ (A);
%! assert (bins, [1, 1]);

## Line 1->2->3->4: one weak component.
%!test
%! A = sparse ([1 2 3], [2 3 4], 1, 4, 4);
%! bins = __conncomp_weak__ (A);
%! assert (bins, [1, 1, 1, 1]);

## Two disjoint edges: two components.
%!test
%! A = sparse ([1 3], [2 4], 1, 4, 4);
%! bins = __conncomp_weak__ (A);
%! assert (bins, [1, 1, 2, 2]);

## Directed 3-cycle: one weak component.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! bins = __conncomp_weak__ (A);
%! assert (bins, [1, 1, 1]);

## Self-loop alone: one component.
%!test
%! A = sparse ([1], [1], 1, 1, 1);
%! bins = __conncomp_weak__ (A);
%! assert (bins, 1);

## Mixed: 1->2 and isolated 3 -> [1 1 2].
%!test
%! A = sparse ([1], [2], 1, 3, 3);
%! bins = __conncomp_weak__ (A);
%! assert (bins, [1, 1, 2]);
