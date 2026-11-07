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
## @deftypefn  {} {@var{c} =} __centrality_closeness__ (@var{G}, @var{direction})
## @deftypefnx {} {@var{c} =} __centrality_closeness__ (@var{G}, @var{direction}, @qcode{"Cost"}, @var{W})
## Private helper: compute MATLAB-style closeness centrality on the
## @code{graph} or @code{digraph} @var{G}.
##
## @var{direction} must be the character row vector @qcode{"out"} or
## @qcode{"in"} and selects whether the sum of distances should be
## taken along rows of the all-pairs distance matrix (outgoing, used
## by @code{"closeness"} and @code{"outcloseness"}) or columns
## (incoming, used by @code{"incloseness"}).  On an undirected
## @code{graph} the distance matrix is symmetric so both directions
## yield the same result.
##
## The returned @var{c} is a column vector of length
## @code{numnodes (@var{G})} with values
##
## @example
## @group
## c(i) = (N - 1) / sum_{j != i} d(i, j)       [outgoing]
## c(i) = (N - 1) / sum_{j != i} d(j, i)       [incoming]
## @end group
## @end example
##
## Unreachable nodes contribute @code{Inf} to the sum, which drives
## @code{c(i)} to zero -- matching MATLAB's handling of disconnected
## components.  For an empty graph (@math{N = 0}) the result is
## @code{zeros (0, 1)}; for a single-node graph (@math{N = 1}) it is
## @code{zeros (1, 1)} (no other nodes are available to be central
## relative to).
##
## The optional @qcode{"Cost"} Name-Value pair supplies a vector of
## positive per-edge costs of length @code{numedges (@var{G})} that
## overrides any stored edge weights when computing shortest paths.
## The costs must be finite and strictly positive.  Without
## @qcode{"Cost"} the helper delegates to @code{distances (@var{G})} so
## it honours whatever default method that function selects (BFS on
## unweighted graphs, Dijkstra on non-negatively weighted graphs,
## Bellman-Ford on graphs with negative weights, etc.); with
## @qcode{"Cost"} it runs Dijkstra on the supplied cost-weighted
## adjacency directly.
##
## @seealso{centrality, distances}
## @end deftypefn

function c = __centrality_closeness__ (G, direction, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! ischar (direction) || ! any (strcmp (direction, {"out", "in"})))
    error ("Octave:invalid-input-arg", ...
           "__centrality_closeness__: DIRECTION must be \"out\" or \"in\"");
  endif

  [have_cost, cost] = __parse_cost_option__ (G, "closeness", varargin);

  N = numnodes (G);
  if (N <= 1)
    c = zeros (N, 1);
    return;
  endif

  if (have_cost)
    W = adjacency (G, cost);
    D = __distances_dijkstra__ (W);
  else
    D = distances (G);
  endif

  if (strcmp (direction, "in"))
    ## Swap rows and columns so that row i of D becomes the set of
    ## distances *into* node i.
    D = D.';
  endif

  s = sum (D, 2);
  c = (N - 1) ./ s;

endfunction

## Parse the "Cost" Name-Value option.  Returns whether a Cost vector
## was supplied and the validated column double vector.  NAME is the
## centrality type name, used in error messages.  ARGS is the varargin
## cell array.  Unknown options and malformed pairs are reported with
## the @code{centrality:} prefix (the user-visible caller).
function [have_cost, cost] = __parse_cost_option__ (G, name, args)
  have_cost = false;
  cost = [];
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
      case "cost"
        M = numedges (G);
        if (! isnumeric (val) || ! isreal (val))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' must be a numeric real vector");
        endif
        if (! isempty (val) && ! isvector (val))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' must be a vector");
        endif
        if (numel (val) != M)
          error ("Octave:invalid-input-arg", ...
                 ["centrality: 'Cost' must have length %d ", ...
                  "(numedges (G))"], M);
        endif
        if (any (! isfinite (val)))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' entries must be finite");
        endif
        if (any (val <= 0))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' entries must be positive");
        endif
        cost = double (val(:));
        have_cost = true;
      otherwise
        error ("Octave:invalid-input-arg", ...
               "centrality: unknown %s option '%s'", name, opt);
    endswitch
  endfor
endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Triangle graph: symmetric, c = (3-1)/(1+1) = 1.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (__centrality_closeness__ (G, "out"), [1; 1; 1], 1e-12);
%! assert (__centrality_closeness__ (G, "in"),  [1; 1; 1], 1e-12);

## Directed 3-cycle: out and in are both symmetric over the cycle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (__centrality_closeness__ (G, "out"), [2/3; 2/3; 2/3], 1e-12);
%! assert (__centrality_closeness__ (G, "in"),  [2/3; 2/3; 2/3], 1e-12);

## Directed fork 1->{2,3}: node 1 reaches both others, nobody reaches 1.
%!test
%! G = digraph ([1 1], [2 3]);
%! assert (__centrality_closeness__ (G, "out"), [1; 0; 0], 1e-12);
%! assert (__centrality_closeness__ (G, "in"),  [0; 0; 0], 1e-12);

## Empty graph: zeros(0, 1).
%!test
%! assert (__centrality_closeness__ (graph (),   "out"), zeros (0, 1));
%! assert (__centrality_closeness__ (digraph (), "in"),  zeros (0, 1));

## Single-node graph: zeros(1, 1) (no averaging target).
%!test
%! assert (__centrality_closeness__ (graph (1),   "out"), 0);
%! assert (__centrality_closeness__ (digraph (1), "in"),  0);

## Edgeless multi-node graph: every node has Inf sum so c=0.
%!test
%! assert (__centrality_closeness__ (graph (4), "out"), zeros (4, 1));
%! assert (__centrality_closeness__ (digraph (4), "in"), zeros (4, 1));

## Weighted digraph: uses stored edge weights.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 2 3]);
%! ## Out sums (rows of distances, skipping diagonal 0):
%! ##   node 1: d(1,2)=1, d(1,3)=3 -> 4.  c = 2/4.
%! ##   node 2: d(2,3)=2, d(2,1)=5 -> 7.  c = 2/7.
%! ##   node 3: d(3,1)=3, d(3,2)=4 -> 7.  c = 2/7.
%! assert (__centrality_closeness__ (G, "out"), [2/4; 2/7; 2/7], 1e-12);
%! ## In sums (columns):
%! ##   into 1: d(2,1)=5, d(3,1)=3 -> 8.  c = 2/8.
%! ##   into 2: d(1,2)=1, d(3,2)=4 -> 5.  c = 2/5.
%! ##   into 3: d(1,3)=3, d(2,3)=2 -> 5.  c = 2/5.
%! assert (__centrality_closeness__ (G, "in"), [2/8; 2/5; 2/5], 1e-12);

## Self-loops are ignored (diagonal in distances is always 0).
%!test
%! G1 = digraph ([1 2 3], [1 3 1]);
%! G2 = digraph ([2 3], [3 1]);
%! assert (__centrality_closeness__ (G1, "out"), ...
%!         __centrality_closeness__ (G2, "out"), 1e-12);

## Result is a column vector of class double.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c = __centrality_closeness__ (G, "out");
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Invalid direction is an error.
%!error <DIRECTION must be>
%! __centrality_closeness__ (graph (), "sideways");

%!error <DIRECTION must be>
%! __centrality_closeness__ (graph (), 42);

## Missing direction is an error.
%!error __centrality_closeness__ (graph ())

## -------------------- Cost Name-Value option --------------------

## Cost = vector of ones reproduces the unweighted closeness on an
## unweighted graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c0 = __centrality_closeness__ (G, "out");
%! c1 = __centrality_closeness__ (G, "out", "Cost", ones (3, 1));
%! assert (c1, c0, 1e-12);

## Cost scales uniformly so closeness scales inversely.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c0 = __centrality_closeness__ (G, "out");
%! c2 = __centrality_closeness__ (G, "out", "Cost", 2 * ones (3, 1));
%! assert (c2, c0 / 2, 1e-12);

## Cost overrides stored edge weights on a weighted digraph.
## Stored weights [1 2 3], but Cost = [1 1 1] (unweighted).
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 2 3]);
%! Gu = digraph ([1 2 3], [2 3 1]);
%! c_cost = __centrality_closeness__ (G, "out", "Cost", ones (3, 1));
%! c_unw  = __centrality_closeness__ (Gu, "out");
%! assert (c_cost, c_unw, 1e-12);

## Cost with distinct weights gives weighted closeness on a digraph.
## Digraph 1->2, 2->3, 3->1 with cost [1 2 3].
## Out sums: node 1: d(1,2)=1, d(1,3)=3 -> 4 -> c=2/4.
##           node 2: d(2,3)=2, d(2,1)=5 -> 7 -> c=2/7.
##           node 3: d(3,1)=3, d(3,2)=4 -> 7 -> c=2/7.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c = __centrality_closeness__ (G, "out", "Cost", [1; 2; 3]);
%! assert (c, [2/4; 2/7; 2/7], 1e-12);

## Cost with distinct weights for incloseness on a digraph.
## In sums (cols): into 1: d(2,1)=5, d(3,1)=3 -> 8 -> c=2/8.
##                 into 2: d(1,2)=1, d(3,2)=4 -> 5 -> c=2/5.
##                 into 3: d(1,3)=3, d(2,3)=2 -> 5 -> c=2/5.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c = __centrality_closeness__ (G, "in", "Cost", [1; 2; 3]);
%! assert (c, [2/8; 2/5; 2/5], 1e-12);

## Cost as row vector works.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c = __centrality_closeness__ (G, "out", "Cost", [2 2 2]);
%! c0 = __centrality_closeness__ (G, "out");
%! assert (c, c0 / 2, 1e-12);

## Cost option name is case-insensitive.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c1 = __centrality_closeness__ (G, "out", "Cost", ones (3, 1));
%! c2 = __centrality_closeness__ (G, "out", "COST", ones (3, 1));
%! c3 = __centrality_closeness__ (G, "out", "cost", ones (3, 1));
%! c4 = __centrality_closeness__ (G, "out", "CoSt", ones (3, 1));
%! assert (c2, c1, 1e-12);
%! assert (c3, c1, 1e-12);
%! assert (c4, c1, 1e-12);

## Cost wrong length errors.
%!error <Cost.*length>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost", [1; 1; 1]);

## Cost with negative entry errors.
%!error <Cost.*positive>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost", [1; -1]);

## Cost with zero entry errors (must be positive).
%!error <Cost.*positive>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost", [1; 0]);

## Cost with NaN errors.
%!error <Cost.*finite>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost", [1; NaN]);

## Cost with Inf errors.
%!error <Cost.*finite>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost", [1; Inf]);

## Cost non-numeric errors.
%!error <Cost.*numeric>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost", {1, 2});

## Cost complex errors.
%!error <Cost.*numeric>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost", [1+1i; 2]);

## Unknown option errors.
%!error <unknown closeness option>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Importance", [1; 1]);

## Odd number of Name-Value args errors.
%!error <pair>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", "Cost");

## Non-string option name errors.
%!error <option name>
%! __centrality_closeness__ (graph ([1 2], [2 3]), "out", 42, [1; 1]);
