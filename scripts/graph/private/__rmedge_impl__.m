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
## @deftypefn  {} {@var{edge_survive} =} __rmedge_impl__ (@var{G}, @var{edgeIdx})
## @deftypefnx {} {@var{edge_survive} =} __rmedge_impl__ (@var{G}, @var{s}, @var{t})
## Private helper shared by the @code{rmedge} methods on both the
## @code{graph} and @code{digraph} classes.
##
## Returns a column boolean vector @var{edge_survive} of length
## @code{numedges (@var{G})} where @code{true} denotes an edge that
## should be retained and @code{false} an edge that should be removed.
## The indexing matches the canonical edge order of
## @code{G.Edges.EndNodes}.
##
## Two call forms are supported:
##
## @itemize
## @item
## One trailing arg @var{edgeIdx}: a numeric array of positive integer
## edge indices in @code{1:numedges (@var{G})}.  Duplicate indices are
## silently deduplicated (an edge is removed only once).
## @item
## Two trailing args @var{s}, @var{t}: same-length endpoint arrays
## (numeric, char row, or cellstr).  For a @code{digraph} the ordered
## pair @code{(@var{s}(i), @var{t}(i))} is matched; for an undirected
## @code{graph} the pair is matched in either orientation.  For a
## multigraph @code{digraph}, @emph{all} parallel edges matching a
## given pair are removed.  It is an error if any requested pair
## corresponds to no edge in @var{G}.
## @end itemize
##
## @seealso{rmedge, graph, digraph}
## @end deftypefn

function edge_survive = __rmedge_impl__ (G, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  n_extra = numel (varargin);
  M = numedges (G);

  if (n_extra == 1)
    ## Form 2: edgeIdx.
    edgeIdx = varargin{1};
    if (! isnumeric (edgeIdx) || ! isreal (edgeIdx))
      error ("Octave:invalid-input-arg", ...
             "rmedge: edgeIdx must be a real numeric array");
    endif
    v = double (edgeIdx)(:);
    if (! isempty (v))
      if (any (! isfinite (v)) || any (v < 1) || any (v > M) ...
          || any (v != fix (v)))
        error ("Octave:invalid-input-arg", ...
               ["rmedge: invalid edge index (must be a positive ", ...
                "integer in the range 1:numedges (G))"]);
      endif
    endif
    edge_survive = true (M, 1);
    if (! isempty (v))
      edge_survive(v) = false;
    endif

  elseif (n_extra == 2)
    ## Form 1: (s, t).
    s = varargin{1};
    t = varargin{2};

    [s_idx, s_n] = resolve_endpoint_vec (G, s, "S");
    [t_idx, t_n] = resolve_endpoint_vec (G, t, "T");

    if (s_n != t_n)
      error ("Octave:invalid-input-arg", ...
             "rmedge: S and T must have the same length");
    endif

    edge_survive = true (M, 1);
    if (s_n == 0)
      return;   # no edges requested for removal
    endif

    E = G.Edges.EndNodes;
    is_digraph = isa (G, "digraph");

    for ii = 1:s_n
      if (is_digraph)
        mask_i = (E(:, 1) == s_idx(ii)) & (E(:, 2) == t_idx(ii));
      else
        a = min (s_idx(ii), t_idx(ii));
        b = max (s_idx(ii), t_idx(ii));
        mask_i = (E(:, 1) == a) & (E(:, 2) == b);
      endif
      if (! any (mask_i))
        error ("Octave:invalid-input-arg", ...
               "rmedge: no such edge in G");
      endif
      edge_survive = edge_survive & ~mask_i;
    endfor

  else
    error ("Octave:invalid-fun-call", ...
           "Invalid call to rmedge: expected 2 or 3 arguments");
  endif

endfunction


function [idx, n_elems] = resolve_endpoint_vec (G, v, name)

  ## Resolve an endpoint argument (numeric, char row, or cellstr) to a
  ## column vector of 1-based node indices.  Unlike findedge, missing
  ## names and out-of-range numeric indices raise an error -- rmedge
  ## requires every endpoint to exist in the graph.

  nn = G.Nodes.Name;
  has_names = ! isempty (nn);
  N = numnodes (G);

  if (ischar (v))
    if (! isempty (v) && ! isrow (v))
      error ("Octave:invalid-input-arg", ...
             ["rmedge: ", name, " must be a numeric array, a char row ", ...
              "vector, or a cell array of strings"]);
    endif
    if (isempty (v))
      idx = zeros (0, 1);
      n_elems = 0;
      return;
    endif
    if (! has_names)
      error ("Octave:invalid-input-arg", ...
             ["rmedge: ", name, " is a node name but the graph has ", ...
              "no node names"]);
    endif
    match = find (strcmp (nn, v), 1);
    if (isempty (match))
      error ("Octave:invalid-input-arg", ...
             "rmedge: node name '%s' not found in G", v);
    endif
    idx = double (match);
    n_elems = 1;

  elseif (iscell (v))
    if (isempty (v))
      idx = zeros (0, 1);
      n_elems = 0;
      return;
    endif
    if (! iscellstr (v))
      error ("Octave:invalid-input-arg", ...
             ["rmedge: ", name, " cell input must contain only ", ...
              "character vectors"]);
    endif
    if (! has_names)
      error ("Octave:invalid-input-arg", ...
             ["rmedge: ", name, " contains node names but the graph ", ...
              "has no node names"]);
    endif
    names = v(:);
    n_elems = numel (names);
    idx = zeros (n_elems, 1);
    for ii = 1:n_elems
      match = find (strcmp (nn, names{ii}), 1);
      if (isempty (match))
        error ("Octave:invalid-input-arg", ...
               "rmedge: node name '%s' not found in G", names{ii});
      endif
      idx(ii) = double (match);
    endfor

  elseif (isnumeric (v) && isreal (v))
    if (isempty (v))
      idx = zeros (0, 1);
      n_elems = 0;
      return;
    endif
    vals = double (v)(:);
    n_elems = numel (vals);
    if (any (! isfinite (vals)) || any (vals < 1) || any (vals > N) ...
        || any (vals != fix (vals)))
      error ("Octave:invalid-input-arg", ...
             ["rmedge: invalid node index in ", name, " (must be a ", ...
              "positive integer in 1:numnodes (G))"]);
    endif
    idx = vals;

  else
    error ("Octave:invalid-input-arg", ...
           ["rmedge: ", name, " must be a numeric array, a char row ", ...
            "vector, or a cell array of strings"]);
  endif

endfunction


## ------------------------------------------------------------------
## Private-helper smoke tests.
## ------------------------------------------------------------------

## Form 1: simple digraph single edge.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! keep = __rmedge_impl__ (G, 1, 2);
%! assert (keep, [false; true; true]);

## Form 1: undirected graph matches either orientation.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! keep_a = __rmedge_impl__ (G, 1, 2);
%! keep_b = __rmedge_impl__ (G, 2, 1);
%! assert (keep_a, keep_b);

## Form 1: multigraph removes all parallel edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! keep = __rmedge_impl__ (G, 1, 2);
%! assert (keep, [false; false; true]);

## Form 1: error when the edge is missing.
%!error <no such edge> ...
%! G = digraph ([1 2], [2 3]);
%! __rmedge_impl__ (G, 1, 3);

## Form 1: length mismatch.
%!error <same length> ...
%! G = digraph ([1 2 3], [2 3 1]);
%! __rmedge_impl__ (G, [1 2], 3);

## Form 2: single index.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! keep = __rmedge_impl__ (G, 2);
%! assert (keep, [true; false; true]);

## Form 2: vector of indices (duplicates allowed).
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! keep = __rmedge_impl__ (G, [1 3 3]);
%! assert (keep, [false; true; false; true]);

## Form 2: empty index returns all-true mask.
%!test
%! G = digraph ([1 2], [2 3]);
%! keep = __rmedge_impl__ (G, []);
%! assert (keep, true (2, 1));

## Form 2: error on out-of-range.
%!error <invalid edge> ...
%! G = digraph ([1 2], [2 3]);
%! __rmedge_impl__ (G, 99);

## Form 2: error on non-integer.
%!error <invalid edge> ...
%! G = digraph ([1 2], [2 3]);
%! __rmedge_impl__ (G, 1.5);

## Form 1: unknown name errors.
%!error <not found> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! __rmedge_impl__ (G, "z", "a");

## Form 1: char name on an unnamed graph errors.
%!error <no node names> ...
%! G = digraph (3);
%! __rmedge_impl__ (G, "x", "y");
