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
## @deftypefn  {} {[@var{out1}, @var{out2}] =} __findedge_impl__ (@var{G}, @var{nout})
## @deftypefnx {} {[@var{out1}, @var{out2}] =} __findedge_impl__ (@var{G}, @var{nout}, @var{edgeIdx})
## @deftypefnx {} {[@var{out1}, @var{out2}] =} __findedge_impl__ (@var{G}, @var{nout}, @var{s}, @var{t})
## Private helper shared by the @code{findedge} methods on both the
## @code{graph} and @code{digraph} classes.
##
## @var{nout} is the requested number of output arguments from the
## caller (pass @code{1} or @code{2}).  The remaining arguments match
## the three @code{findedge} call forms:
##
## @itemize
## @item
## No trailing args: return all edges.  With @code{nout==1}, @var{out1}
## is the @code{m}-by-2 endpoints matrix.  With @code{nout==2},
## @var{out1} and @var{out2} are the @code{m}-by-1 source and
## destination column vectors.
## @item
## One trailing arg @var{edgeIdx}: return endpoints at those edge
## indices.  Same output dispatch as above.
## @item
## Two trailing args @var{s}, @var{t}: return edge indices (0 if
## absent).  @var{out1} is always a column vector (or scalar for
## scalar inputs); @var{out2} is unused.
## @end itemize
##
## @seealso{findedge, graph, digraph}
## @end deftypefn

function [out1, out2] = __findedge_impl__ (G, nout, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  narg_extra = numel (varargin);
  out2 = [];

  if (narg_extra == 0)
    ## Form 1: all edges.
    E = G.Edges.EndNodes;
    if (nout <= 1)
      out1 = E;
    else
      out1 = E(:, 1);
      out2 = E(:, 2);
    endif

  elseif (narg_extra == 1)
    ## Form 3: edgeIdx lookup.
    edgeIdx = varargin{1};
    if (! isnumeric (edgeIdx) || ! isreal (edgeIdx))
      error ("Octave:invalid-input-arg", ...
             "findedge: edgeIdx must be a real numeric array");
    endif
    M = numedges (G);
    v = double (edgeIdx)(:);
    if (! isempty (v))
      if (any (! isfinite (v)) || any (v < 1) || any (v > M) ...
          || any (v != fix (v)))
        error ("Octave:invalid-input-arg", ...
               ["findedge: invalid edge index (must be a positive ", ...
                "integer <= numedges (G))"]);
      endif
    endif
    ## Index into EndNodes; empty v yields a 0-by-2 result naturally.
    picked = G.Edges.EndNodes(v, :);
    if (nout <= 1)
      out1 = picked;
    else
      out1 = picked(:, 1);
      out2 = picked(:, 2);
    endif

  elseif (narg_extra == 2)
    ## Form 2: (s, t) -> edge index lookup.
    s = varargin{1};
    t = varargin{2};

    ## Normalise char row vectors to 1-element cellstrs so the
    ## node-resolution path below treats them uniformly.
    [s_idx, s_n] = resolve_endpoint_vector (G, s, "s");
    [t_idx, t_n] = resolve_endpoint_vector (G, t, "t");

    if (s_n != t_n)
      error ("Octave:invalid-input-arg", ...
             "findedge: S and T must have the same length");
    endif

    ## Detect missing-name propagation: resolve_endpoint_vector fills 0
    ## for any name not found (to match findnode semantics), provided
    ## the call came in via a string/cellstr path.  A zero at this
    ## point means "name not found" and the result for that entry must
    ## be 0.  Numeric inputs never produce zeros (they error on out of
    ## range), so any zero here is safe to interpret as a miss.
    miss_mask = (s_idx == 0) | (t_idx == 0);

    idx = zeros (s_n, 1);

    if (s_n > 0)
      N = numnodes (G);
      E = G.Edges.EndNodes;
      m = size (E, 1);

      ## A digraph built with the @qcode{'multigraph'} flag may carry
      ## actual parallel edges; when it does, @code{ismultigraph}
      ## returns true and a sparse-accumulator lookup would add up the
      ## duplicate indices (wrong).  Fall back to a linear row-by-row
      ## scan in that case.  For a graph (no multigraph support today)
      ## or a non-multigraph digraph the sparse lookup is safe.
      is_mg = (isa (G, "digraph") && ismultigraph (G));

      if (is_mg)
        for ii = 1:s_n
          if (miss_mask(ii))
            continue;
          endif
          row = find (E(:, 1) == s_idx(ii) & E(:, 2) == t_idx(ii), 1);
          if (! isempty (row))
            idx(ii) = row;
          endif
        endfor

      elseif (m > 0)
        P = sparse (E(:, 1), E(:, 2), 1:m, N, N);
        keep_idx = find (! miss_mask);
        if (! isempty (keep_idx))
          s_hit = s_idx(keep_idx);
          t_hit = t_idx(keep_idx);
          if (isa (G, "digraph"))
            lin = sub2ind ([N, N], s_hit, t_hit);
          else
            ## Undirected graph: normalise to (min, max) so the lookup
            ## matches the canonical lex-order storage in G.Edges.
            lin = sub2ind ([N, N], ...
                           min (s_hit, t_hit), max (s_hit, t_hit));
          endif
          idx(keep_idx) = full (P(lin));
        endif
      endif
    endif

    out1 = idx;   # column vector (scalar is a 1-by-1 column)

  else
    error ("Octave:invalid-fun-call", ...
           "Invalid call to findedge: expected 1, 2, or 3 arguments");
  endif

endfunction


function [idx, n_elems] = resolve_endpoint_vector (G, v, name)

  ## Resolve an endpoint argument (numeric, char row, or cellstr) to a
  ## column vector of 1-based node indices.  Missing names yield 0
  ## (match MATLAB findedge semantics where unknown names give 0 edge
  ## index without raising).  Numeric inputs are validated against
  ## numnodes(G) and raise on out-of-range / non-integer / non-finite.

  nn = G.Nodes.Name;
  has_names = ! isempty (nn);
  N = numnodes (G);

  if (ischar (v))
    if (! isempty (v) && ! isrow (v))
      error ("Octave:invalid-input-arg", ...
             ["findedge: ", name, " must be a numeric array, a char ", ...
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
             ["findedge: ", name, " must be a numeric array, a char ", ...
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
             ["findedge: ", name, " must be a numeric array, a char ", ...
              "row vector, or a cell array of strings"]);
    endif
    vals = double (v)(:);
    n_elems = numel (vals);
    if (n_elems > 0)
      if (any (! isfinite (vals)) || any (vals < 1) || any (vals > N) ...
          || any (vals != fix (vals)))
        error ("Octave:invalid-input-arg", ...
               ["findedge: invalid node index in ", name, ...
                " (must be a positive integer <= numnodes (G))"]);
      endif
    endif
    idx = vals;

  else
    error ("Octave:invalid-input-arg", ...
           ["findedge: ", name, " must be a numeric array, a char ", ...
            "row vector, or a cell array of strings"]);
  endif

endfunction


## Private-helper smoke tests.

## All edges: 1-output form returns m-by-2 matrix.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = __findedge_impl__ (G, 1);
%! assert (E, [1 2; 2 3; 3 1]);

## All edges: 2-output form returns separate columns.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [s, t] = __findedge_impl__ (G, 2);
%! assert (s, [1; 2; 3]);
%! assert (t, [2; 3; 1]);

## (s, t) lookup: found case.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (__findedge_impl__ (G, 1, 2, 3), 2);

## (s, t) lookup: missing case returns 0.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (__findedge_impl__ (G, 1, 1, 3), 0);

## (s, t) lookup: undirected graph matches either orientation.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (__findedge_impl__ (G, 1, 2, 1), __findedge_impl__ (G, 1, 1, 2));

## (s, t) lookup: vector input.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! idx = __findedge_impl__ (G, 1, [1 2 3], [2 3 1]);
%! assert (idx, [1; 2; 3]);

## edgeIdx lookup: scalar returns 1-by-2 row.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = __findedge_impl__ (G, 1, 1);
%! assert (E, [1 2]);

## edgeIdx lookup: vector returns m-by-2 matrix.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = __findedge_impl__ (G, 1, [1 2]);
%! assert (E, [1 2; 2 3]);

## edgeIdx lookup: 2-output form returns columns.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [s, t] = __findedge_impl__ (G, 2, [1 2]);
%! assert (s, [1; 2]);
%! assert (t, [2; 3]);

## String endpoint lookup: found.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (__findedge_impl__ (G, 1, "a", "b"), 1);

## String endpoint lookup: missing returns 0.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (__findedge_impl__ (G, 1, "a", "z"), 0);

## Multigraph: returns first matching edge.
%!test
%! G = digraph ([1 1 2], [2 2 3], 'multigraph');
%! assert (__findedge_impl__ (G, 1, 1, 2), 1);

## Errors: length mismatch.
%!error <length> __findedge_impl__ (digraph ([1 2], [2 3]), 1, [1 2], [2])

## Errors: out-of-range numeric endpoint.
%!error <invalid node> ...
%! __findedge_impl__ (digraph ([1 2], [2 3]), 1, 1, 99)

## Errors: edgeIdx out-of-range.
%!error <invalid edge> ...
%! __findedge_impl__ (digraph ([1 2], [2 3]), 1, 99)
