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
## @deftypefn {} {@var{n} =} __edgecount_impl__ (@var{G}, @var{s}, @var{t})
## Private helper shared by the @code{edgecount} methods on both the
## @code{graph} and @code{digraph} classes.
##
## Resolves @var{s} and @var{t} to node indices (numeric, char, or
## cellstr) and returns a column vector of edge counts per query pair.
## For a simple graph/digraph the result is @code{0} or @code{1}; for a
## multigraph digraph the result sums parallel edges.
##
## Works through the public @code{G.Edges.EndNodes},
## @code{numnodes (G)} and (for digraph) @code{ismultigraph (G)}
## accessors so it is usable from both classdef method contexts without
## piercing encapsulation.
##
## @seealso{edgecount, findedge, graph, digraph}
## @end deftypefn

function n = __edgecount_impl__ (G, s, t)

  if (nargin != 3)
    print_usage ();
  endif

  ## Normalise endpoints to column index vectors.  Missing names yield
  ## 0 (findnode-style); numeric entries are validated against
  ## numnodes(G).
  [s_idx, s_n] = resolve_endpoint_vector (G, s, "s");
  [t_idx, t_n] = resolve_endpoint_vector (G, t, "t");

  if (s_n != t_n)
    error ("Octave:invalid-input-arg", ...
           "edgecount: S and T must have the same length");
  endif

  ## Fast exit for empty query.
  if (s_n == 0)
    n = zeros (0, 1);
    return;
  endif

  miss_mask = (s_idx == 0) | (t_idx == 0);
  n = zeros (s_n, 1);

  N = numnodes (G);
  E = G.Edges.EndNodes;
  m = size (E, 1);

  if (m == 0 || all (miss_mask))
    return;
  endif

  ## For an undirected graph, G.Edges.EndNodes stores each edge in
  ## canonical (min, max) form.  Normalise the query to the same form
  ## so we look up the right cells.
  if (isa (G, "graph"))
    q_s = min (s_idx, t_idx);
    q_t = max (s_idx, t_idx);
  else
    q_s = s_idx;
    q_t = t_idx;
  endif

  ## Missing-name entries must remain 0 regardless of what normalisation
  ## produced.  Mask them out before the accumulator lookup.
  keep_idx = find (! miss_mask);
  if (isempty (keep_idx))
    return;
  endif

  ## Build a sparse accumulator over G.Edges.EndNodes.  Duplicate
  ## endpoint pairs (multigraph) accumulate naturally into the same
  ## cell, giving the multiplicity.
  if (isa (G, "graph"))
    ## Already canonical (min, max) in EndNodes.
    C = sparse (E(:, 1), E(:, 2), 1, N, N);
  else
    C = sparse (E(:, 1), E(:, 2), 1, N, N);
  endif

  lin = sub2ind ([N, N], q_s(keep_idx), q_t(keep_idx));
  n(keep_idx) = full (C(lin));

endfunction


function [idx, n_elems] = resolve_endpoint_vector (G, v, name)

  ## Resolve an endpoint argument (numeric, char row, or cellstr) to a
  ## column vector of 1-based node indices.  Missing names yield 0
  ## (match MATLAB findedge/findnode semantics: unknown names are not
  ## an error, they propagate to a 0 count).  Numeric inputs are
  ## validated against numnodes(G) and raise on out-of-range /
  ## non-integer / non-finite.

  nn = G.Nodes.Name;
  has_names = ! isempty (nn);
  N = numnodes (G);

  if (ischar (v))
    if (! isempty (v) && ! isrow (v))
      error ("Octave:invalid-input-arg", ...
             ["edgecount: ", name, " must be a numeric array, a char ", ...
              "row vector, or a cell array of strings"]);
    endif
    if (isempty (v))
      idx = zeros (0, 1);
      n_elems = 0;
      return;
    endif
    if (has_names)
      match = find (strcmp (nn, v), 1);
    else
      match = [];
    endif
    if (isempty (match))
      idx = 0;          # 0 denotes "not found"
    else
      idx = double (match);
    endif
    n_elems = 1;

  elseif (iscell (v))
    if (! iscellstr (v))
      error ("Octave:invalid-input-arg", ...
             ["edgecount: ", name, " must be a numeric array, a char ", ...
              "row vector, or a cell array of strings"]);
    endif
    names = v(:);
    n_elems = numel (names);
    idx = zeros (n_elems, 1);
    if (has_names && n_elems > 0)
      for ii = 1:n_elems
        match = find (strcmp (nn, names{ii}), 1);
        if (! isempty (match))
          idx(ii) = double (match);
        endif
      endfor
    endif

  elseif (isnumeric (v))
    if (! isreal (v))
      error ("Octave:invalid-input-arg", ...
             ["edgecount: ", name, " must be a numeric array, a char ", ...
              "row vector, or a cell array of strings"]);
    endif
    vals = double (v)(:);
    n_elems = numel (vals);
    if (n_elems > 0)
      if (any (! isfinite (vals)) || any (vals < 1) || any (vals > N) ...
          || any (vals != fix (vals)))
        error ("Octave:invalid-input-arg", ...
               ["edgecount: invalid node index in ", name, ...
                " (must be a positive integer <= numnodes (G))"]);
      endif
    endif
    idx = vals;

  else
    error ("Octave:invalid-input-arg", ...
           ["edgecount: ", name, " must be a numeric array, a char ", ...
            "row vector, or a cell array of strings"]);
  endif

endfunction


## ------------------------------------------------------------------
## Private-helper smoke tests.
## ------------------------------------------------------------------

## Scalar existing edge in simple digraph -> 1.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (__edgecount_impl__ (G, 1, 2), 1);

## Scalar missing edge -> 0.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (__edgecount_impl__ (G, 1, 3), 0);

## Simple graph: reverse pair matches same edge.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (__edgecount_impl__ (G, 2, 1), 1);

## Multigraph: parallel edges counted.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (__edgecount_impl__ (G, 1, 2), 2);

## Multigraph: non-duplicate still returns 1.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (__edgecount_impl__ (G, 2, 3), 1);

## Vector (s, t) returns column.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! n = __edgecount_impl__ (G, [1 2 3], [2 3 1]);
%! assert (size (n), [3, 1]);
%! assert (n, [1; 1; 1]);

## Missing names yield 0 per pair (without raising).
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (__edgecount_impl__ (G, "a", "z"), 0);
%! assert (__edgecount_impl__ (G, "z", "a"), 0);

## Error: length mismatch.
%!error <length> __edgecount_impl__ (digraph ([1 2], [2 3]), [1 2], [2])

## Error: out-of-range numeric endpoint.
%!error <invalid node> ...
%! __edgecount_impl__ (digraph ([1 2], [2 3]), 1, 99)

## Error: non-integer numeric endpoint.
%!error <invalid node> ...
%! __edgecount_impl__ (digraph ([1 2], [2 3]), 1, 1.5)

## Empty (s, t) returns zeros(0, 1).
%!test
%! G = digraph ([1 2], [2 3]);
%! n = __edgecount_impl__ (G, [], []);
%! assert (size (n), [0, 1]);

## Edgeless graph: count is 0.
%!test
%! G = digraph (5);
%! assert (__edgecount_impl__ (G, 1, 2), 0);
