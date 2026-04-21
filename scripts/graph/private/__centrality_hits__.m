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
## @deftypefn {} {@var{c} =} __centrality_hits__ (@var{G}, @var{mode})
## Private helper: compute the MATLAB-style HITS (Kleinberg) @var{mode}
## centrality of the @code{digraph} @var{G}.
##
## @var{mode} is a character row vector, one of @code{"hubs"} or
## @code{"authorities"}, selecting which of the two HITS score vectors
## is returned.
##
## HITS assigns to every node two scores, a @dfn{hub} score @math{h} and
## an @dfn{authority} score @math{a}, defined as the leading singular
## vectors of the weighted adjacency matrix
## @code{A = adjacency (@var{G}, "weighted")}.  The power-iteration
## update is
##
## @example
## @group
## a_@{k+1@} = A' * h_k,     a_@{k+1@} /= sum (a_@{k+1@})
## h_@{k+1@} = A * a_k,      h_@{k+1@} /= sum (h_@{k+1@})
## @end group
## @end example
##
## starting from @code{h_0 = a_0 = ones (N, 1) / N} and stopping when
## @code{max (abs (h_new - h), abs (a_new - a))} falls to or below an
## internal tolerance @code{1e-6} or after an internal cap of
## @code{1000} iterations, whichever comes first.  The result is
## L1-normalised so that @code{sum (@var{c}) = 1}, following the
## stochastic-output convention shared with @code{"pagerank"} and
## @code{"eigenvector"}.
##
## For @math{N = 0} the result is @code{zeros (0, 1)}; for @math{N = 1}
## it is @code{[1]}.  When the weighted adjacency matrix has no
## nonzero entries (an edgeless digraph with @math{N @ge 2}) the helper
## returns @code{zeros (N, 1)}.  Nodes that neither send nor receive
## any edge after iteration receive a zero score; the remaining mass
## is renormalised to sum to @code{1}.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.  HITS is only defined
## for a directed graph; the caller (the digraph class
## @code{centrality} method) dispatches here and the @code{graph}
## class method rejects the call with a clear error.
##
## @seealso{centrality, adjacency}
## @end deftypefn

function c = __centrality_hits__ (G, mode)

  if (nargin < 2)
    print_usage ();
  endif

  if (! ischar (mode) || ! isrow (mode))
    error ("Octave:invalid-input-arg", ...
           "__centrality_hits__: MODE must be a character row vector");
  endif

  mode = lower (mode);
  if (! (strcmp (mode, "hubs") || strcmp (mode, "authorities")))
    error ("Octave:invalid-input-arg", ...
           "__centrality_hits__: MODE must be 'hubs' or 'authorities'");
  endif

  N = numnodes (G);

  ## Edge cases.
  if (N == 0)
    c = zeros (0, 1);
    return;
  endif
  if (N == 1)
    c = 1;
    return;
  endif

  ## Weighted adjacency matrix (directed).  Parallel edges on a
  ## multigraph are summed into the same entry; an unweighted edge
  ## contributes 1.
  A = adjacency (G, "weighted");

  ## An edgeless digraph has no meaningful hub/authority structure.
  if (nnz (A) == 0)
    c = zeros (N, 1);
    return;
  endif

  At = A.';                               # precompute transpose once

  ## Power iteration on the coupled HITS recursion
  ##   a_new = A' * h,    h_new = A * a,
  ## with L1 normalisation at every step so both vectors stay on the
  ## probability simplex (matching MATLAB's stochastic-output
  ## convention for 'pagerank' and 'eigenvector').  Using the old
  ## ``a`` (not ``a_new``) to update ``h`` keeps the two updates
  ## symmetric; convergence is unchanged.
  max_iter = 1000;
  tol      = 1e-6;
  h = ones (N, 1) / N;
  a = ones (N, 1) / N;

  for iter = 1:max_iter
    a_new = At * h;
    h_new = A  * a;

    ## L1 normalise each.  If the sum is zero (no hub or authority
    ## mass survived into the subspace, e.g. when an all-source or
    ## all-sink component is isolated from every source of incoming
    ## weight), fall back to a zero vector -- no mass to redistribute.
    s_a = sum (a_new);
    if (s_a > 0)
      a_new = a_new / s_a;
    else
      a_new = zeros (N, 1);
    endif

    s_h = sum (h_new);
    if (s_h > 0)
      h_new = h_new / s_h;
    else
      h_new = zeros (N, 1);
    endif

    err = max (max (abs (a_new - a)), max (abs (h_new - h)));
    a = a_new;
    h = h_new;
    if (err <= tol)
      break;
    endif
  endfor

  if (strcmp (mode, "hubs"))
    x = h;
  else
    x = a;
  endif

  ## Clip tiny negative numerical noise (SVD-positive by Perron-style
  ## argument, but synchronous iteration can leave O(eps) negatives).
  x = max (full (x), 0);

  ## Renormalise so the result is exactly a probability vector (sum
  ## = 1), matching MATLAB's convention.  If the iteration collapsed
  ## to all-zero (e.g. edgeless case caught above, or every edge is
  ## a dangling-into-dangling self-loop), return zeros.
  s = sum (x);
  if (s > 0)
    c = x / s;
  else
    c = zeros (N, 1);
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Directed 3-cycle 1->2->3->1: rotation-symmetric, both scores
## uniform [1/3; 1/3; 1/3].
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! h = __centrality_hits__ (G, "hubs");
%! a = __centrality_hits__ (G, "authorities");
%! assert (h, [1/3; 1/3; 1/3], 1e-6);
%! assert (a, [1/3; 1/3; 1/3], 1e-6);

## Fork 1->{2,3,4}: node 1 is the only hub, nodes 2..4 are equal
## authorities.
%!test
%! G = digraph (ones (1, 3), 2:4);
%! h = __centrality_hits__ (G, "hubs");
%! a = __centrality_hits__ (G, "authorities");
%! assert (h, [1; 0; 0; 0], 1e-6);
%! assert (a, [0; 1/3; 1/3; 1/3], 1e-6);

## Reverse-fork {2,3,4}->1: node 1 is the only authority, nodes
## 2..4 are equal hubs.
%!test
%! G = digraph ([2 3 4], [1 1 1]);
%! h = __centrality_hits__ (G, "hubs");
%! a = __centrality_hits__ (G, "authorities");
%! assert (h, [0; 1/3; 1/3; 1/3], 1e-6);
%! assert (a, [1; 0; 0; 0], 1e-6);

## Chain 1->2->3: hubs [1/2; 1/2; 0], authorities [0; 1/2; 1/2].
%!test
%! G = digraph ([1 2], [2 3]);
%! h = __centrality_hits__ (G, "hubs");
%! a = __centrality_hits__ (G, "authorities");
%! assert (h, [1/2; 1/2; 0], 1e-6);
%! assert (a, [0; 1/2; 1/2], 1e-6);

## Empty digraph -> zeros(0, 1) for both.
%!test
%! G = digraph ();
%! assert (__centrality_hits__ (G, "hubs"), zeros (0, 1));
%! assert (__centrality_hits__ (G, "authorities"), zeros (0, 1));

## Single-node digraph -> [1] for both.
%!test
%! G = digraph (1);
%! assert (__centrality_hits__ (G, "hubs"), 1);
%! assert (__centrality_hits__ (G, "authorities"), 1);

## Edgeless N=4 digraph -> zeros(4, 1) for both.
%!test
%! G = digraph (4);
%! assert (__centrality_hits__ (G, "hubs"), zeros (4, 1));
%! assert (__centrality_hits__ (G, "authorities"), zeros (4, 1));

## Isolated node + 2-cycle: the 2-cycle distributes mass uniformly,
## the isolated node gets zero.
%!test
%! G = digraph ([1 2], [2 1], [], 3);
%! h = __centrality_hits__ (G, "hubs");
%! a = __centrality_hits__ (G, "authorities");
%! assert (h, [1/2; 1/2; 0], 1e-6);
%! assert (a, [1/2; 1/2; 0], 1e-6);

## Result is column double for both modes.
%!test
%! G = digraph ([1 2], [2 3]);
%! h = __centrality_hits__ (G, "hubs");
%! a = __centrality_hits__ (G, "authorities");
%! assert (size (h), [3, 1]);
%! assert (class (h), "double");
%! assert (size (a), [3, 1]);
%! assert (class (a), "double");

## Sum-to-one invariant on a generic digraph.
%!test
%! G = digraph ([1 1 2 3 4 4], [2 3 3 4 2 5]);
%! h = __centrality_hits__ (G, "hubs");
%! a = __centrality_hits__ (G, "authorities");
%! assert (sum (h), 1, 1e-6);
%! assert (sum (a), 1, 1e-6);
%! assert (all (h >= 0 - 1e-9));
%! assert (all (a >= 0 - 1e-9));

## Self-loop on a single-node digraph -> [1] for both.
%!test
%! G = digraph (1, 1);
%! assert (__centrality_hits__ (G, "hubs"), 1, 1e-6);
%! assert (__centrality_hits__ (G, "authorities"), 1, 1e-6);

## MODE validation: non-string -> error.
%!error <MODE must be a character row vector>
%! __centrality_hits__ (digraph ([1 2], [2 3]), 42);

## MODE validation: non-row string -> error.
%!error <MODE must be a character row vector>
%! __centrality_hits__ (digraph ([1 2], [2 3]), ['h'; 'u'; 'b']);

## MODE validation: unknown mode -> error.
%!error <MODE must be 'hubs' or 'authorities'>
%! __centrality_hits__ (digraph ([1 2], [2 3]), "pagerank");

## MODE validation: zero-arg call -> print_usage error.
%!error <Invalid call>
%! __centrality_hits__ ();
