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
## @deftypefn  {} {@var{c} =} __centrality_eigenvector__ (@var{G})
## @deftypefnx {} {@var{c} =} __centrality_eigenvector__ (@var{G}, @qcode{"Importance"}, @var{W})
## Private helper: compute the MATLAB-style eigenvector centrality of
## the undirected @code{graph} @var{G}.
##
## Eigenvector centrality is the principal (Perron) eigenvector of the
## weighted adjacency matrix of @var{G}, scaled so the entries sum to
## @code{1}.  We compute it by power iteration on
## @code{A = adjacency (@var{G}, "weighted")}:
##
## @example
## @group
## x_@{k+1@} = A * x_k / norm (A * x_k)
## @end group
## @end example
##
## starting from @code{x_0 = ones (N, 1) / N} and stopping when
## @code{max (abs (x_@{k+1@} - x_k))} falls to or below an internal
## tolerance @code{1e-6} or after an internal cap of @code{1000}
## iterations, whichever comes first.  The resulting @var{x} is then
## L1-normalised so that @code{sum (@var{c}) = 1}, following the same
## stochastic-output convention as @code{"pagerank"}.
##
## For @math{N = 0} the result is @code{zeros (0, 1)}; for @math{N = 1}
## it is @code{[1]}.  When the weighted adjacency matrix has no
## nonzero entries (an edgeless graph with @math{N @ge 2}) the helper
## returns @code{zeros (N, 1)} because no meaningful principal
## eigenvector exists.
##
## The optional @qcode{"Importance"} Name-Value pair supplies a
## vector of non-negative per-edge weights of length
## @code{numedges (@var{G})} that overrides any stored edge weights in
## the iteration.  The weights must be finite and non-negative.
##
## This helper only supports an undirected @code{graph}.  MATLAB's
## @code{centrality} function does not define eigenvector centrality
## for a directed graph; the caller (the class @code{centrality}
## method) is responsible for rejecting digraph inputs with a clear
## error before dispatching here.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
##
## @seealso{centrality, adjacency}
## @end deftypefn

function c = __centrality_eigenvector__ (G, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  [have_imp, importance] = ...
      __parse_importance_option__ (G, "eigenvector", varargin);

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

  ## Weighted adjacency.  For a graph object this is symmetric, and
  ## parallel edges on a multigraph are summed into the same cell.  An
  ## unweighted edge contributes 1.  With the "Importance" option the
  ## user-supplied per-edge vector replaces any stored edge weights.
  if (have_imp)
    A = adjacency (G, importance);
  else
    A = adjacency (G, "weighted");
  endif

  ## An all-zero adjacency has no meaningful principal eigenvector.
  if (nnz (A) == 0)
    c = zeros (N, 1);
    return;
  endif

  ## Power iteration on M = A + I (the identity shift).  Shifting all
  ## eigenvalues of A upward by 1 guarantees that the largest-in-
  ## magnitude eigenvalue of M belongs to the same eigenvector as the
  ## principal eigenvalue of A; without the shift, a bipartite graph
  ## (which has +lambda and -lambda eigenvalue pairs for every non-
  ## zero lambda) would cause the iteration to oscillate rather than
  ## converge.  M*x = A*x + x is computed implicitly to avoid the
  ## sparse-plus-dense construction of speye(N, N).
  max_iter = 1000;
  tol      = 1e-6;
  x = ones (N, 1) / N;

  for iter = 1:max_iter
    y = A * x + x;                      # M * x  where M = A + I
    nm = norm (y);
    if (nm == 0)
      ## Defensive: should not happen once the nnz(A)==0 short-circuit
      ## is hit above, but bail out gracefully if it ever does.
      c = zeros (N, 1);
      return;
    endif
    x_new = y / nm;
    if (max (abs (x_new - x)) <= tol)
      x = x_new;
      break;
    endif
    x = x_new;
  endfor

  ## Perron-Frobenius guarantees a non-negative principal eigenvector
  ## when starting from a positive initial vector, but numerical noise
  ## can produce tiny negatives -- clip them.
  x = max (full (x), 0);

  ## Zero out any completely isolated nodes (no incident edges).  The
  ## identity shift leaves a residual probability mass on their entries
  ## that decays only geometrically per iteration; truncating them
  ## exactly matches MATLAB's convention of assigning zero centrality
  ## to nodes that make no structural contribution to the principal
  ## eigenvector.
  isolated = (full (sum (A, 2)) == 0);
  x(isolated) = 0;

  ## L1-normalise so the result is a probability vector (sum = 1),
  ## matching MATLAB's convention for stochastic centralities.  If the
  ## iteration collapsed to all-zero (e.g. because the only edges are
  ## self-loops summing to zero on an isolated vertex), just return
  ## zeros.
  s = sum (x);
  if (s > 0)
    c = x / s;
  else
    c = zeros (N, 1);
  endif

endfunction

## Parse the "Importance" Name-Value option.  Returns whether the
## option was supplied and the validated column double vector.
## Duplicated across centrality helpers so each file stays
## self-contained (private helpers are not on each other's search
## path).
function [have_imp, importance] = __parse_importance_option__ (G, name, args)
  have_imp = false;
  importance = [];
  if (isempty (args))
    return;
  endif
  if (mod (numel (args), 2) != 0)
    error ("Octave:invalid-input-arg", ...
           ["centrality: %s Name-Value arguments must come in pairs ", ...
            "(missing value for option '%s')"], name, args{end});
  endif
  for k = 1:2:numel (args)
    opt = args{k};
    val = args{k+1};
    if (! ischar (opt) || ! isrow (opt))
      error ("Octave:invalid-input-arg", ...
             ["centrality: %s option name must be a character ", ...
              "row vector (string)"], name);
    endif
    switch (lower (opt))
      case "importance"
        M = numedges (G);
        if (! isnumeric (val) || ! isreal (val))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Importance' must be a numeric real vector");
        endif
        if (! isempty (val) && ! isvector (val))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Importance' must be a vector");
        endif
        if (numel (val) != M)
          error ("Octave:invalid-input-arg", ...
                 ["centrality: 'Importance' must have length %d ", ...
                  "(numedges (G))"], M);
        endif
        if (any (! isfinite (val)))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Importance' entries must be finite");
        endif
        if (any (val < 0))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Importance' entries must be non-negative");
        endif
        importance = double (val(:));
        have_imp = true;
      otherwise
        error ("Octave:invalid-input-arg", ...
               "centrality: unknown %s option '%s'", name, opt);
    endswitch
  endfor
endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Triangle graph (K3): symmetric, eigenvector centrality uniform.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c = __centrality_eigenvector__ (G);
%! assert (c, [1/3; 1/3; 1/3], 1e-6);

## Complete graph K4: every node has identical centrality 1/4.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! c = __centrality_eigenvector__ (G);
%! assert (c, [1/4; 1/4; 1/4; 1/4], 1e-6);

## Path graph P3 (1-2-3): closed-form eigenvector.
## A = [0 1 0; 1 0 1; 0 1 0]; principal eigenvalue = sqrt(2),
## eigenvector proportional to [1, sqrt(2), 1].  Normalised to sum=1:
## [1/(2+sqrt(2)); sqrt(2)/(2+sqrt(2)); 1/(2+sqrt(2))].
%!test
%! G = graph ([1 2], [2 3]);
%! c = __centrality_eigenvector__ (G);
%! expected = [1; sqrt(2); 1];
%! expected = expected / sum (expected);
%! assert (c, expected, 1e-5);

## Star graph K_{1,5}: centre has higher centrality than leaves,
## leaves have equal centrality.
%!test
%! G = graph (ones (1, 5), 2:6);
%! c = __centrality_eigenvector__ (G);
%! ## Centre: sqrt(5)/(sqrt(5)+5);  leaves: 1/(sqrt(5)+5).
%! assert (c(1), sqrt (5) / (sqrt (5) + 5), 1e-5);
%! for k = 2:6
%!   assert (c(k), 1 / (sqrt (5) + 5), 1e-5);
%! endfor

## Empty graph -> zeros(0, 1).
%!test
%! assert (__centrality_eigenvector__ (graph ()), zeros (0, 1));

## Single-node graph -> [1].
%!test
%! assert (__centrality_eigenvector__ (graph (1)), 1);

## Edgeless graph N=4: no meaningful eigenvector -> zeros(4, 1).
%!test
%! assert (__centrality_eigenvector__ (graph (4)), zeros (4, 1));

## Result is column double.
%!test
%! G = graph ([1 2], [2 3]);
%! c = __centrality_eigenvector__ (G);
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Sum-to-one invariant on a generic graph.
%!test
%! G = graph ([1 1 2 3 4], [2 3 3 4 5]);
%! c = __centrality_eigenvector__ (G);
%! assert (sum (c), 1, 1e-6);
%! assert (all (c >= 0 - 1e-9));

## Bipartite K_{2,3}: the two sides each have equal values internally.
%!test
%! G = graph ([1 1 1 2 2 2], [3 4 5 3 4 5]);
%! c = __centrality_eigenvector__ (G);
%! assert (sum (c), 1, 1e-6);
%! assert (c(1), c(2), 1e-5);           # the "2" side is equal
%! assert (c(3), c(4), 1e-5);           # the "3" side is equal
%! assert (c(4), c(5), 1e-5);
%! assert (c(1) > c(3));                # smaller side has larger c

## -------------------- Importance Name-Value option --------------------

## Importance = ones reproduces unweighted eigenvector centrality.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c0 = __centrality_eigenvector__ (G);
%! c1 = __centrality_eigenvector__ (G, "Importance", ones (3, 1));
%! assert (c1, c0, 1e-6);

## Importance overrides stored edge weights.
## Weighted triangle: stored weights [100 100 100] but Importance
## all ones -> same as unweighted uniform 1/3.
%!test
%! G = graph ([1 2 3], [2 3 1], [100 100 100]);
%! c = __centrality_eigenvector__ (G, "Importance", ones (3, 1));
%! assert (c, [1/3; 1/3; 1/3], 1e-6);

## Importance = 0 on every edge -> all-zero adjacency behaves like
## edgeless graph -> zeros(N, 1).
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c = __centrality_eigenvector__ (G, "Importance", zeros (3, 1));
%! assert (c, zeros (3, 1), 1e-12);

## Importance can zero specific edges, shifting mass.
## K4 - remove edge 3-4 by setting Importance[6]=0 (that's the
## 3-4 edge in lex order).  Edges of K4 in lex order:
##   1-2, 1-3, 1-4, 2-3, 2-4, 3-4.
## With [1; 1; 1; 1; 1; 0] we remove edge 3-4 -> diamond 1-3-2-4
## with chord 1-2: eigenvector centrality not symmetric anymore;
## only check sum-to-one and class.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! c = __centrality_eigenvector__ (G, "Importance", [1; 1; 1; 1; 1; 0]);
%! assert (sum (c), 1, 1e-6);
%! assert (all (c >= 0 - 1e-9));

## Row vector Importance works.
%!test
%! G = graph ([1 2], [2 3]);
%! c = __centrality_eigenvector__ (G, "Importance", [1 1]);
%! c0 = __centrality_eigenvector__ (G);
%! assert (c, c0, 1e-6);

## Importance option name is case-insensitive.
%!test
%! G = graph ([1 2], [2 3]);
%! c1 = __centrality_eigenvector__ (G, "Importance", [1; 1]);
%! c2 = __centrality_eigenvector__ (G, "importance", [1; 1]);
%! c3 = __centrality_eigenvector__ (G, "IMPORTANCE", [1; 1]);
%! c4 = __centrality_eigenvector__ (G, "ImPoRtAnCe", [1; 1]);
%! assert (c2, c1, 1e-6);
%! assert (c3, c1, 1e-6);
%! assert (c4, c1, 1e-6);

## Importance wrong length errors.
%!error <Importance.*length>
%! __centrality_eigenvector__ (graph ([1 2], [2 3]), "Importance", [1; 1; 1]);

## Importance with negative entry errors.
%!error <Importance.*non-negative>
%! __centrality_eigenvector__ (graph ([1 2], [2 3]), "Importance", [1; -1]);

## Importance with NaN errors.
%!error <Importance.*finite>
%! __centrality_eigenvector__ (graph ([1 2], [2 3]), "Importance", [1; NaN]);

## Importance non-numeric errors.
%!error <Importance.*numeric>
%! __centrality_eigenvector__ (graph ([1 2], [2 3]), "Importance", "hi");

## Unknown option errors.
%!error <unknown eigenvector option>
%! __centrality_eigenvector__ (graph ([1 2], [2 3]), "Cost", [1; 1]);

## Odd number of Name-Value args errors.
%!error <pair>
%! __centrality_eigenvector__ (graph ([1 2], [2 3]), "Importance");
