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
## @deftypefn {} {@var{c} =} __centrality_pagerank__ (@var{G})
## @deftypefnx {} {@var{c} =} __centrality_pagerank__ (@var{G}, @var{name}, @var{value}, @dots{})
## Private helper: compute the MATLAB-style PageRank centrality on the
## @code{graph} or @code{digraph} @var{G}.
##
## The power-iteration update is
## @example
## @group
## x_@{k+1@} = (1 - d) / N
##            + d * (M' * x_k + m_@{dangling@} / N)
## @end group
## @end example
## where @math{d} is the @qcode{"FollowProbability"} damping factor,
## @math{M} is the row-stochastic transition matrix (an edge's share of a
## source's outgoing weight goes to the destination), and
## @math{m_@{dangling@}} is the total probability mass currently sitting
## on dangling nodes (nodes whose outgoing weight is zero), which is
## redistributed uniformly across all nodes at every step to preserve
## the sum-to-one invariant.  Iteration stops when
## @code{max (abs (x_new - x))} falls to or below @qcode{"Tolerance"}, or
## after @qcode{"MaxIterations"} steps, whichever comes first.
##
## Recognised Name-Value options:
## @table @code
## @item "FollowProbability"
## Non-negative real scalar in @code{[0, 1]}, default @code{0.85}.  This
## is the probability at each step that the random walker follows an
## outgoing edge; with probability @code{1 - FollowProbability} the
## walker instead teleports to a uniformly random node.
## @item "MaxIterations"
## Positive integer scalar, default @code{100}.
## @item "Tolerance"
## Non-negative finite real scalar, default @code{1e-4}.
## @item "Importance"
## Non-negative real vector of length @code{numedges (@var{G})} that
## overrides any stored edge weights when building the transition
## matrix.  Every entry must be finite and non-negative.
## @end table
##
## Edge weights stored on @var{G} are used (via
## @code{adjacency (@var{G}, "weighted")}); for a multigraph the weights
## of parallel edges are summed.  On an undirected @code{graph} every
## edge contributes to both endpoints' outgoing sums, so the transition
## matrix is row-normalised symmetric (i.e.\ PageRank on an undirected
## graph is proportional to weighted degree in the limit of @math{d} =
## 1).  Self-loops are honoured (they add to the source's outgoing mass
## and feed back a portion of it to itself).
##
## For @math{N = 0} the result is @code{zeros (0, 1)}; for @math{N = 1}
## it is @code{[1]} (a single node gets the whole probability mass).
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
##
## @seealso{centrality, adjacency}
## @end deftypefn

function c = __centrality_pagerank__ (G, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  ## Parse Name-Value options.
  follow_prob = 0.85;
  max_iter    = 100;
  tol         = 1e-4;
  have_imp    = false;
  importance  = [];

  if (mod (numel (varargin), 2) != 0)
    error ("Octave:invalid-input-arg", ...
           ["centrality: pagerank Name-Value arguments must come in ", ...
            "pairs (missing value for option '%s')"], ...
           varargin{end});
  endif

  for k = 1:2:numel (varargin)
    name = varargin{k};
    val  = varargin{k+1};

    if (! ischar (name) || ! isrow (name))
      error ("Octave:invalid-input-arg", ...
             ["centrality: pagerank option name must be a character ", ...
              "row vector (string)"]);
    endif

    switch (lower (name))
      case "followprobability"
        if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
               && isfinite (val) && val >= 0 && val <= 1))
          error ("Octave:invalid-input-arg", ...
                 ["centrality: 'FollowProbability' must be a finite ", ...
                  "real scalar in the interval [0, 1]"]);
        endif
        follow_prob = double (val);

      case "maxiterations"
        if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
               && isfinite (val) && val >= 1 && val == fix (val)))
          error ("Octave:invalid-input-arg", ...
                 ["centrality: 'MaxIterations' must be a positive ", ...
                  "integer scalar"]);
        endif
        max_iter = double (val);

      case "tolerance"
        if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
               && isfinite (val) && val >= 0))
          error ("Octave:invalid-input-arg", ...
                 ["centrality: 'Tolerance' must be a non-negative ", ...
                  "finite real scalar"]);
        endif
        tol = double (val);

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
               "centrality: unknown pagerank option '%s'", name);
    endswitch
  endfor

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

  ## Weighted adjacency: for a graph this is symmetric, for a digraph
  ## directed.  Parallel edges on a multigraph are summed into the same
  ## cell.  An unweighted edge contributes 1.  When the "Importance"
  ## option is supplied the user-supplied per-edge vector replaces any
  ## stored edge weights.
  if (have_imp)
    A = adjacency (G, importance);
  else
    A = adjacency (G, "weighted");
  endif

  ## Row sums are each node's total outgoing weight (out-degree for
  ## unweighted graphs, weighted out-strength otherwise).  Dangling
  ## nodes are those whose row sums to zero.
  out_sum = full (sum (A, 2));
  dangling = (out_sum == 0);

  ## Build the row-stochastic transition matrix.  Dangling nodes keep a
  ## row of zeros; their probability mass is redistributed uniformly in
  ## the iteration below.
  nonzero = ! dangling;
  inv_out = zeros (N, 1);
  inv_out(nonzero) = 1 ./ out_sum(nonzero);
  M = spdiags (inv_out, 0, N, N) * A;    # row-normalised

  ## Precompute the transpose so the inner loop is Mt*x (a single
  ## left-multiplication).
  Mt = M.';
  base = (1 - follow_prob) / N;

  x = ones (N, 1) / N;
  for iter = 1:max_iter
    if (any (dangling))
      m_dangling = sum (x(dangling));
    else
      m_dangling = 0;
    endif
    x_new = base + follow_prob * (Mt * x + m_dangling / N);

    if (max (abs (x_new - x)) <= tol)
      x = x_new;
      break;
    endif
    x = x_new;
  endfor

  c = full (x);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Triangle graph: symmetric, uniform pagerank.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c = __centrality_pagerank__ (G);
%! assert (c, [1/3; 1/3; 1/3], 1e-6);

## Directed 3-cycle: symmetric, uniform.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c = __centrality_pagerank__ (G);
%! assert (c, [1/3; 1/3; 1/3], 1e-6);

## Empty -> zeros(0, 1).
%!test
%! assert (__centrality_pagerank__ (graph ()),   zeros (0, 1));
%! assert (__centrality_pagerank__ (digraph ()), zeros (0, 1));

## Single-node -> [1].
%!test
%! assert (__centrality_pagerank__ (graph (1)),   1);
%! assert (__centrality_pagerank__ (digraph (1)), 1);

## Edgeless N-node: uniform [1/N].
%!test
%! assert (__centrality_pagerank__ (graph (4)),   0.25 * ones (4, 1), 1e-12);
%! assert (__centrality_pagerank__ (digraph (3)), (1/3) * ones (3, 1), 1e-12);

## Result is a column double vector.
%!test
%! c = __centrality_pagerank__ (graph ([1 2], [2 3]));
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Sum-to-one invariant on generic digraph.
%!test
%! G = digraph ([1 2 3 1 4], [2 3 1 3 2]);
%! c = __centrality_pagerank__ (G);
%! assert (sum (c), 1, 1e-6);
%! assert (all (c >= 0 - 1e-9));

## FollowProbability = 0 -> uniform regardless of graph shape.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 1 2 1]);
%! c = __centrality_pagerank__ (G, "FollowProbability", 0);
%! assert (c, ones (5, 1) / 5, 1e-12);

## Directed chain: dangling-aware.
%!test
%! G = digraph ([1 2], [2 3]);
%! c = __centrality_pagerank__ (G);
%! assert (sum (c), 1, 1e-6);
%! assert (c(3) > c(2));
%! assert (c(2) > c(1));

## Options are case-insensitive.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! c1 = __centrality_pagerank__ (G, "FollowProbability", 0.5);
%! c2 = __centrality_pagerank__ (G, "followprobability", 0.5);
%! c3 = __centrality_pagerank__ (G, "FOLLOWPROBABILITY", 0.5);
%! assert (c1, c2, 1e-12);
%! assert (c2, c3, 1e-12);

## Tight tolerance + more iterations matches loose tolerance closely.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! c1 = __centrality_pagerank__ (G, "Tolerance", 1e-4);
%! c2 = __centrality_pagerank__ (G, "Tolerance", 1e-12, ...
%!                                 "MaxIterations", 1000);
%! assert (c1, c2, 1e-3);

## Error cases: invalid FollowProbability.
%!error <FollowProbability>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "FollowProbability", -0.1);

%!error <FollowProbability>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "FollowProbability", 1.1);

%!error <FollowProbability>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "FollowProbability", [0.5 0.7]);

%!error <FollowProbability>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "FollowProbability", 0.5 + 1i);

## MaxIterations errors.
%!error <MaxIterations>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "MaxIterations", 0);

%!error <MaxIterations>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "MaxIterations", -3);

%!error <MaxIterations>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "MaxIterations", 3.5);

## Tolerance errors.
%!error <Tolerance>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Tolerance", -1e-4);

%!error <Tolerance>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Tolerance", Inf);

## Unknown option.
%!error <unknown pagerank option>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Nonsense", 1);

## Odd number of name-value args.
%!error <pair>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "FollowProbability");

## Non-string option name.
%!error <option name>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), 42, 0.5);

## -------------------- Importance Name-Value option --------------------

## Importance = ones reproduces unweighted pagerank on a digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c0 = __centrality_pagerank__ (G);
%! c1 = __centrality_pagerank__ (G, "Importance", ones (3, 1));
%! assert (c1, c0, 1e-6);

## Importance overrides stored edge weights.
%!test
%! G = digraph ([1 2 3], [2 3 1], [100 100 100]);
%! c = __centrality_pagerank__ (G, "Importance", ones (3, 1));
%! Gu = digraph ([1 2 3], [2 3 1]);
%! c0 = __centrality_pagerank__ (Gu);
%! assert (c, c0, 1e-6);

## Importance of all zeros on every edge -> every node dangling ->
## uniform distribution (teleportation only).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c = __centrality_pagerank__ (G, "Importance", zeros (3, 1));
%! assert (c, ones (3, 1) / 3, 1e-6);

## Importance with skewed values shifts mass.  Directed 2-node cycle
## 1->2->1.  Uniform: c = [0.5; 0.5].  With Importance [10; 0]
## (edge 1->2 carries weight, edge 2->1 has zero importance) the mass
## flows 1 -> 2 but no flow back to 1, so 2 accumulates more mass.
%!test
%! G = digraph ([1 2], [2 1]);
%! c0 = __centrality_pagerank__ (G);
%! c_bias = __centrality_pagerank__ (G, "Importance", [10; 0]);
%! assert (sum (c_bias), 1, 1e-6);
%! assert (c_bias(2) > c_bias(1));

## Importance can combine with other options.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c = __centrality_pagerank__ (G, "Importance", ones (3, 1), ...
%!                              "FollowProbability", 0.5, ...
%!                              "MaxIterations", 500, ...
%!                              "Tolerance", 1e-10);
%! assert (sum (c), 1, 1e-6);
%! assert (c, [1/3; 1/3; 1/3], 1e-5);

## Row vector Importance accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c = __centrality_pagerank__ (G, "Importance", [1 1 1]);
%! c0 = __centrality_pagerank__ (G);
%! assert (c, c0, 1e-6);

## Importance option name is case-insensitive.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c1 = __centrality_pagerank__ (G, "Importance", [1; 1; 1]);
%! c2 = __centrality_pagerank__ (G, "importance", [1; 1; 1]);
%! c3 = __centrality_pagerank__ (G, "IMPORTANCE", [1; 1; 1]);
%! assert (c2, c1, 1e-6);
%! assert (c3, c1, 1e-6);

## Importance wrong length errors.
%!error <Importance.*length>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Importance", [1; 1; 1]);

## Importance with negative entry errors.
%!error <Importance.*non-negative>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Importance", [1; -1]);

## Importance with NaN errors.
%!error <Importance.*finite>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Importance", [1; NaN]);

## Importance with Inf errors.
%!error <Importance.*finite>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Importance", [1; Inf]);

## Importance non-numeric errors.
%!error <Importance.*numeric>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Importance", "hi");

## Importance complex errors.
%!error <Importance.*numeric>
%! __centrality_pagerank__ (graph ([1 2], [2 3]), "Importance", [1+1i; 2]);
