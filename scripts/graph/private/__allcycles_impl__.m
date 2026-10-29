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
## @deftypefn {} {[@var{cycles}, @var{edgecycles}] =} __allcycles_impl__ (@var{G}, @dots{})
## Private helper: enumerate all elementary cycles of the @code{graph}
## or @code{digraph} @var{G}.
##
## Optional trailing arguments are Name-Value pairs:
##
## @table @asis
## @item @qcode{"MaxNumCycles"}
## Cap on the number of cycles returned (default @code{Inf}).
##
## @item @qcode{"MinCycleLength"}
## Lower bound on cycle length in nodes (default @code{1}).
##
## @item @qcode{"MaxCycleLength"}
## Upper bound on cycle length in nodes (default @code{Inf}).  When
## finite the bound is also used to prune the depth-first search.
## @end table
##
## Returns @var{cycles} (column cell of node-index row vectors) and
## @var{edgecycles} (column cell of edge-index row vectors of the
## same length as the corresponding cycle).
##
## A 1-cycle is a self-loop (one node, one edge).  A 2-cycle is either
## a pair of opposite directed edges (digraph) or a pair of parallel
## undirected edges (multigraph).  The algorithm is a Johnson-style
## DFS: for each starting node @math{s} the search is restricted to
## paths whose nodes are all @math{> s} (other than the closing return
## to @math{s}), so each directed cycle is reported exactly once with
## its smallest node first.  For an undirected graph an additional
## canonical-form rule (smaller second node, or smaller first edge
## index for 2-cycles) breaks the orientation symmetry.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{allcycles, graph, digraph}
## @end deftypefn

function [cycles, edgecycles] = __allcycles_impl__ (G, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  ## Parse Name-Value pairs.
  max_num = Inf;
  min_len = 1;
  max_len = Inf;
  nargs = numel (varargin);
  if (mod (nargs, 2) != 0)
    error ("Octave:invalid-input-arg", ...
           "allcycles: Name,Value arguments must appear in pairs");
  endif
  for k = 1:2:nargs
    name = varargin{k};
    if (! (ischar (name) && isrow (name)))
      error ("Octave:invalid-input-arg", ...
             "allcycles: option names must be strings");
    endif
    val = varargin{k+1};
    if (strcmpi (name, "MaxNumCycles"))
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && ! isnan (val) && val >= 1 ...
             && (isinf (val) || val == fix (val))))
        error ("Octave:invalid-input-arg", ...
               ["allcycles: MaxNumCycles must be a positive ", ...
                "integer scalar or Inf"]);
      endif
      max_num = double (val);
    elseif (strcmpi (name, "MinCycleLength"))
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && ! isnan (val) && val >= 0))
        error ("Octave:invalid-input-arg", ...
               ["allcycles: MinCycleLength must be a non-negative ", ...
                "real scalar"]);
      endif
      min_len = double (val);
    elseif (strcmpi (name, "MaxCycleLength"))
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && ! isnan (val) && val >= 0))
        error ("Octave:invalid-input-arg", ...
               ["allcycles: MaxCycleLength must be a non-negative ", ...
                "real scalar"]);
      endif
      max_len = double (val);
    else
      error ("Octave:invalid-input-arg", ...
             "allcycles: unknown option '%s'", name);
    endif
  endfor

  N = numnodes (G);
  is_directed = isa (G, "digraph");
  E = G.Edges;
  EN = E.EndNodes;
  m = size (EN, 1);

  ## Output accumulators.  Grow by doubling.
  cap = 8;
  cycles = cell (cap, 1);
  edgecycles = cell (cap, 1);
  n_cycles = 0;

  ## Quick-exit cases.
  if (N == 0 || m == 0 || max_num <= 0 || min_len > max_len)
    cycles = cell (0, 1);
    edgecycles = cell (0, 1);
    return;
  endif

  ## ----- 1-cycles: self-loops -----
  if (min_len <= 1 && max_len >= 1)
    self_idx = find (EN(:, 1) == EN(:, 2));
    for ii = 1:numel (self_idx)
      e = self_idx(ii);
      n_cycles = n_cycles + 1;
      if (n_cycles > cap)
        cap = cap * 2;
        cycles{cap}     = [];
        edgecycles{cap} = [];
      endif
      cycles{n_cycles}     = double (EN(e, 1));
      edgecycles{n_cycles} = double (e);
      if (n_cycles >= max_num)
        cycles     = cycles(1:n_cycles);
        edgecycles = edgecycles(1:n_cycles);
        return;
      endif
    endfor
  endif

  ## ----- DFS-based cycle enumeration for length >= 2 -----
  if (max_len < 2)
    if (n_cycles == 0)
      cycles     = cell (0, 1);
      edgecycles = cell (0, 1);
    else
      cycles     = cycles(1:n_cycles);
      edgecycles = edgecycles(1:n_cycles);
    endif
    return;
  endif

  ## Build the out-adjacency list.  Each entry is m_u-by-2 with columns
  ## [dst, edge_index].  For an undirected graph each non-self-loop
  ## edge appears in BOTH endpoints' lists with the same edge_index.
  ## Self-loops are stripped (they are 1-cycles, already handled).
  out_adj = __build_out_adj_for_cycles__ (G, N, EN, m, is_directed);

  ## DFS state buffers (size N is enough since cycles use distinct
  ## nodes from {s} U {s+1, ..., N}).
  node_buf  = zeros (1, N);
  edge_buf  = zeros (1, N);
  child_idx = ones  (1, N + 1);
  on_path   = false (N, 1);

  ## Effective max cycle length for the DFS push prune.  Cycles must
  ## have length >= 2 here (1-cycles handled above).
  eff_max = min (max_len, N);

  for s = 1:N
    if (n_cycles >= max_num)
      break;
    endif

    ## Initialise DFS rooted at s.
    on_path(:) = false;
    on_path(s) = true;
    node_buf(1) = s;
    child_idx(1) = 1;
    depth = 1;

    while (depth >= 1)
      u = node_buf(depth);
      A = out_adj{u};
      ci = child_idx(depth);

      if (ci > size (A, 1))
        ## All children examined; backtrack.
        on_path(u) = false;
        depth = depth - 1;
        continue;
      endif

      v    = A(ci, 1);
      eidx = A(ci, 2);
      child_idx(depth) = ci + 1;

      ## Block any node smaller than s -- cycles through such nodes
      ## have already been (or will be) reported when starting at the
      ## smaller s.
      if (v < s)
        continue;
      endif

      ## For an undirected graph, do not use the same edge twice in
      ## succession (otherwise we would just bounce off and "close"
      ## back to s using the edge we arrived on, which is not a
      ## cycle).
      if (! is_directed && depth >= 2 && eidx == edge_buf(depth - 1))
        continue;
      endif

      if (v == s)
        ## Closing back to s.  cycle_len = depth (number of nodes in
        ## the cycle, which equals number of edges).
        cycle_len = depth;
        if (cycle_len < min_len || cycle_len > max_len)
          continue;
        endif

        ## For an undirected graph apply the canonical-form filter:
        ## - 2-cycles: edge_buf(1) < eidx (smaller first edge index)
        ## - >=3-cycle: node_buf(2) < node_buf(depth)  (smaller second
        ##   node than last node)
        if (! is_directed)
          if (cycle_len == 2)
            if (edge_buf(1) >= eidx)
              continue;
            endif
          else
            if (node_buf(2) >= node_buf(depth))
              continue;
            endif
          endif
        endif

        ## Record the cycle.
        n_cycles = n_cycles + 1;
        if (n_cycles > cap)
          cap = cap * 2;
          cycles{cap}     = [];
          edgecycles{cap} = [];
        endif
        cycles{n_cycles}     = double (node_buf(1:depth));
        edgecycles{n_cycles} = double ([edge_buf(1:depth-1), eidx]);

        if (n_cycles >= max_num)
          break;
        endif

        ## Don't descend through s; just continue with next child.
        continue;
      endif

      ## v != s; descend if v is not already on the path and we can
      ## still grow the cycle.
      if (on_path(v))
        continue;
      endif
      if (depth + 1 > eff_max)
        continue;
      endif

      ## Push v.
      depth = depth + 1;
      node_buf(depth) = v;
      edge_buf(depth - 1) = eidx;
      on_path(v) = true;
      child_idx(depth) = 1;
    endwhile
  endfor

  ## Trim accumulators to actual size and ensure row-shaped entries.
  if (n_cycles == 0)
    cycles     = cell (0, 1);
    edgecycles = cell (0, 1);
  else
    cycles     = cycles(1:n_cycles);
    edgecycles = edgecycles(1:n_cycles);
    for k = 1:n_cycles
      cycles{k}     = cycles{k}(:).';
      edgecycles{k} = edgecycles{k}(:).';
    endfor
  endif

endfunction


## ----- subroutine: build out-edge adjacency list with edge indices,
## stripping self-loops -----
##
## Returns out_adj, an N-by-1 cell array where out_adj{u} is an
## m_u-by-2 matrix with columns [dst, edge_index].  For a digraph,
## each directed edge contributes one row to out_adj{src}.  For an
## undirected graph, each non-self-loop edge contributes one row to
## EACH endpoint's list with the same edge_index (so either traversal
## direction is available to the DFS).  Self-loops are excluded -- the
## caller handles them as 1-cycles separately.

function out_adj = __build_out_adj_for_cycles__ (G, N, EN, m, is_directed)

  out_adj = cell (N, 1);
  for u = 1:N
    out_adj{u} = zeros (0, 2);
  endfor

  if (m == 0)
    return;
  endif

  src_list = EN(:, 1);
  dst_list = EN(:, 2);

  ## Count entries per source so we can preallocate.
  cnt = zeros (N, 1);
  for k = 1:m
    s = src_list(k);
    t = dst_list(k);
    if (s == t)
      continue;       # skip self-loops
    endif
    cnt(s) = cnt(s) + 1;
    if (! is_directed)
      cnt(t) = cnt(t) + 1;
    endif
  endfor

  for u = 1:N
    if (cnt(u) > 0)
      out_adj{u} = zeros (cnt(u), 2);
    endif
  endfor

  pos = zeros (N, 1);
  for k = 1:m
    s = src_list(k);
    t = dst_list(k);
    if (s == t)
      continue;
    endif
    pos(s) = pos(s) + 1;
    out_adj{s}(pos(s), :) = [t, k];
    if (! is_directed)
      pos(t) = pos(t) + 1;
      out_adj{t}(pos(t), :) = [s, k];
    endif
  endfor

endfunction
