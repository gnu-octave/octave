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
## @deftypefn  {} {@var{D} =} __distances_dag__ (@var{W}, @var{topo})
## @deftypefnx {} {@var{D} =} __distances_dag__ (@var{W}, @var{topo}, @var{sources})
## Private helper: single-source shortest-path on a directed acyclic
## graph (DAG) using a topological-order relaxation.  Runs in
## @math{O (N + E)} per source, and supports negative edge weights
## without risk of negative cycles (the DAG property excludes them).
##
## @var{W} is a square (sparse or dense) weight matrix for an
## @code{N}-by-@code{N} DAG.  @var{topo} is a vector of length
## @code{N} giving a valid topological ordering of the nodes (every
## edge @math{u -> v} has @code{find (topo == u) < find (topo == v)});
## the caller is responsible for providing a valid topological
## ordering (typically via @code{toposort}).
##
## @var{sources} is an optional column vector of positive integer
## node indices.  When omitted or empty, the helper runs from every
## node and returns the all-pairs @code{N}-by-@code{N} distance
## matrix.  Otherwise @var{D} is @code{numel (@var{sources})}-by-
## @code{N}.
##
## This helper is used when @code{distances} is called with
## @code{Method = 'acyclic'}.  Passing a non-DAG yields undefined
## behaviour (the caller validates DAG-ness before invoking this
## helper).
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{distances, graph, digraph, toposort, isdag}
## @end deftypefn

function D = __distances_dag__ (W, topo, sources)

  if (nargin < 2)
    print_usage ();
  endif

  N = size (W, 1);

  if (nargin < 3 || isempty (sources))
    src_list = (1:N).';
  else
    src_list = double (sources(:));
  endif

  K = numel (src_list);

  if (N == 0)
    D = zeros (K, 0);
    return;
  endif

  D = inf (K, N);

  if (K == 0)
    return;
  endif

  ## Precompute each node's out-edges once.
  [ii, jj, ww] = find (W);
  ii = double (ii(:));
  jj = double (jj(:));
  ww = double (ww(:));

  out_tgt = cell (N, 1);
  out_wgt = cell (N, 1);
  for u = 1:N
    mask = (ii == u);
    out_tgt{u} = jj(mask);
    out_wgt{u} = ww(mask);
  endfor

  topo = double (topo(:)).';

  for k = 1:K
    src = src_list(k);
    dist = inf (1, N);
    dist(src) = 0;

    ## Find src's position in the topological order; only nodes at
    ## or after this position can be reached from src.
    start_pos = find (topo == src, 1);
    if (isempty (start_pos))
      ## Shouldn't happen if topo is a valid permutation of 1:N.
      D(k, :) = dist;
      continue;
    endif

    for pos = start_pos:N
      u = topo(pos);
      du = dist(u);
      if (! isfinite (du))
        continue;
      endif
      tgt = out_tgt{u};
      w = out_wgt{u};
      for e = 1:numel (tgt)
        v = tgt(e);
        alt = du + w(e);
        if (alt < dist(v))
          dist(v) = alt;
        endif
      endfor
    endfor

    D(k, :) = dist;
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty DAG yields 0x0.
%!test
%! D = __distances_dag__ (sparse (0, 0), zeros (1, 0));
%! assert (size (D), [0, 0]);

## Single node DAG.
%!test
%! D = __distances_dag__ (sparse (1, 1), 1);
%! assert (D, 0);

## Linear DAG 1->2->3 positive weights.
%!test
%! W = sparse ([1, 2], [2, 3], [5, 10], 3, 3);
%! D = __distances_dag__ (W, [1, 2, 3]);
%! expected = [0 5 15; Inf 0 10; Inf Inf 0];
%! assert (D, expected);

## Linear DAG with negative weights (Dijkstra would fail).
%!test
%! W = sparse ([1, 2], [2, 3], [-2, -3], 3, 3);
%! D = __distances_dag__ (W, [1, 2, 3]);
%! assert (D(1, 3), -5);
%! assert (D(1, 2), -2);
%! assert (D(2, 3), -3);

## Disconnected DAG: unreachable pairs are Inf.
%!test
%! W = sparse ([1, 3], [2, 4], [1, 1], 4, 4);
%! D = __distances_dag__ (W, [1, 3, 2, 4]);
%! assert (D(1, 2), 1);
%! assert (D(3, 4), 1);
%! assert (D(1, 3), Inf);
%! assert (D(1, 4), Inf);

## Diamond DAG: 1->2, 1->3, 2->4, 3->4.
%!test
%! W = sparse ([1 1 2 3], [2 3 4 4], [1 5 3 1], 4, 4);
%! D = __distances_dag__ (W, [1 2 3 4]);
%! ## 1->4 via 1->2->4=1+3=4 vs 1->3->4=5+1=6; min=4.
%! assert (D(1, 4), 4);

## Negative edge in a diamond DAG.
%!test
%! W = sparse ([1 1 2 3], [2 3 4 4], [1 2 -10 1], 4, 4);
%! D = __distances_dag__ (W, [1 2 3 4]);
%! ## 1->4 via 1->2->4=1+(-10)=-9 vs 1->3->4=2+1=3; min=-9.
%! assert (D(1, 4), -9);

## sources arg: single source yields 1xN.
%!test
%! W = sparse ([1, 2], [2, 3], [5, 10], 3, 3);
%! D = __distances_dag__ (W, [1, 2, 3], 1);
%! assert (size (D), [1, 3]);
%! assert (D, [0, 5, 15]);

## sources arg: multiple sources preserve order.
%!test
%! W = sparse ([1, 2], [2, 3], [5, 10], 3, 3);
%! D = __distances_dag__ (W, [1, 2, 3], [2; 1]);
%! assert (D(1, :), [Inf, 0, 10]);
%! assert (D(2, :), [0, 5, 15]);

## Empty sources falls through to all-pairs.
%!test
%! W = sparse ([1, 2], [2, 3], [5, 10], 3, 3);
%! D1 = __distances_dag__ (W, [1, 2, 3], []);
%! D2 = __distances_dag__ (W, [1, 2, 3]);
%! assert (D1, D2);

## Edgeless DAG: identity distances.
%!test
%! D = __distances_dag__ (sparse (3, 3), [1 2 3]);
%! expected = [0 Inf Inf; Inf 0 Inf; Inf Inf 0];
%! assert (D, expected);

## Missing topo arg errors.
%!error __distances_dag__ (sparse (1, 1))

## No-arg call errors via print_usage.
%!error __distances_dag__ ()
