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
## @deftypefn {} {[@var{P}, @var{d}, @var{ep}] =} __allpaths_impl__ (@var{G}, @var{s_idx}, @var{t_idx}, @var{return_names}, @dots{})
## Private helper: common DFS-based enumeration of all simple paths
## from @var{s_idx} to @var{t_idx} in the @code{graph} or
## @code{digraph} @var{G}.
##
## @var{return_names} is @code{true} when either @var{s} or @var{t}
## was given as a node name, so the returned paths should be cellstr.
##
## Optional trailing arguments are Name-Value pairs:
##
## @table @asis
## @item @qcode{"MaxPathLength"}
## Upper bound on the total weight of returned paths (default
## @code{Inf}).  Used both as a post-filter and as a DFS cut-off.
##
## @item @qcode{"MinPathLength"}
## Lower bound on the total weight of returned paths (default
## @code{0}).
## @end table
##
## Returns @var{P}, @var{d}, @var{ep}: column cell of node paths,
## column vector of total distances, column cell of edge-index paths.
##
## When @code{@var{s_idx} == @var{t_idx}} or @var{t_idx} is
## unreachable from @var{s_idx}, the outputs are empty (@code{cell
## (0, 1)} / @code{zeros (0, 1)} / @code{cell (0, 1)}).
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{allpaths, graph, digraph}
## @end deftypefn

function [P, d, ep] = __allpaths_impl__ (G, s_idx, t_idx, return_names, ...
                                         varargin)

  if (nargin < 4)
    print_usage ();
  endif

  ## Parse Name-Value pairs.
  max_len = Inf;
  min_len = 0;
  nargs = numel (varargin);
  if (mod (nargs, 2) != 0)
    error ("Octave:invalid-input-arg", ...
           "allpaths: Name,Value arguments must appear in pairs");
  endif
  for k = 1:2:nargs
    name = varargin{k};
    if (! (ischar (name) && isrow (name)))
      error ("Octave:invalid-input-arg", ...
             "allpaths: option names must be strings");
    endif
    val = varargin{k+1};
    if (strcmpi (name, "MaxPathLength"))
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && ! isnan (val) && val >= 0))
        error ("Octave:invalid-input-arg", ...
               "allpaths: MaxPathLength must be a non-negative real scalar");
      endif
      max_len = double (val);
    elseif (strcmpi (name, "MinPathLength"))
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && ! isnan (val) && val >= 0))
        error ("Octave:invalid-input-arg", ...
               "allpaths: MinPathLength must be a non-negative real scalar");
      endif
      min_len = double (val);
    else
      error ("Octave:invalid-input-arg", ...
             "allpaths: unknown option '%s'", name);
    endif
  endfor

  N = numnodes (G);
  nn = G.Nodes.Name;

  ## Source == target: MATLAB parity is to return empty (a "simple
  ## path from s to s" is trivial and not considered a path by
  ## allpaths).
  if (s_idx == t_idx)
    P = cell (0, 1);
    d = zeros (0, 1);
    ep = cell (0, 1);
    return;
  endif

  ## Build an adjacency list of out-edges, one list per node.  Each
  ## entry is an m-by-3 matrix [dst, weight, edge_index] listing every
  ## directed edge.  For an undirected graph, each edge appears in
  ## both endpoints' lists with the same edge_index so either
  ## direction is traversable.
  out_adj = __build_out_adj__ (G, N);

  ## Self-loops are irrelevant to simple paths -- strip them from each
  ## node's out-list so the DFS has fewer branches to examine.
  for u = 1:N
    A = out_adj{u};
    if (! isempty (A))
      keep = A(:, 1) != u;
      out_adj{u} = A(keep, :);
    endif
  endfor

  ## DFS state.  We maintain the current path (node_buf / edge_buf /
  ## weight_buf) and an iterator per depth (child_idx) into the
  ## out_adj of the node at that depth.  cur_len tracks the running
  ## total weight.  on_stack is a boolean membership mask for simple-
  ## path pruning.
  ##
  ## Path layout at depth D:
  ##   node_buf(1..D)     nodes visited in order (node_buf(1)==s_idx)
  ##   edge_buf(1..D-1)   edge index used to step from node_buf(k) to
  ##                      node_buf(k+1)
  ##   weight_buf(1..D-1) weight of that edge
  ##   cur_len            == sum(weight_buf(1..D-1))

  node_buf   = zeros (1, N);
  edge_buf   = zeros (1, N);
  weight_buf = zeros (1, N);
  child_idx  = ones (1, N + 1);   # guard at depth=N+1
  on_stack   = false (N, 1);

  depth = 1;
  node_buf(1) = s_idx;
  on_stack(s_idx) = true;
  child_idx(1) = 1;
  cur_len = 0;

  ## Output accumulators.  Grow by doubling to avoid O(n^2) appends on
  ## dense enumerations.
  cap = 8;
  P  = cell (cap, 1);
  d  = zeros (cap, 1);
  ep = cell (cap, 1);
  n_paths = 0;

  while (depth >= 1)
    u = node_buf(depth);

    ## Reaching the target at this depth records the current path,
    ## subject to length bounds.  We do NOT descend through the
    ## target (a simple path stops once t is reached), so after
    ## recording we backtrack.
    if (u == t_idx)
      if (cur_len >= min_len && cur_len <= max_len)
        n_paths = n_paths + 1;
        if (n_paths > cap)
          cap = cap * 2;
          P{cap}  = [];
          d(cap)  = 0;
          ep{cap} = [];
        endif
        P{n_paths}  = node_buf(1:depth);
        d(n_paths)  = cur_len;
        if (depth == 1)
          ep{n_paths} = zeros (1, 0);
        else
          ep{n_paths} = edge_buf(1:depth-1);
        endif
      endif
      ## Backtrack: pop u from the path.
      on_stack(u) = false;
      if (depth > 1)
        cur_len = cur_len - weight_buf(depth - 1);
      endif
      depth = depth - 1;
      continue;
    endif

    ## Fetch the next child to try at this depth.
    A = out_adj{u};
    ci = child_idx(depth);
    if (ci > size (A, 1))
      ## No more children; backtrack.
      on_stack(u) = false;
      if (depth > 1)
        cur_len = cur_len - weight_buf(depth - 1);
      endif
      depth = depth - 1;
      continue;
    endif

    v    = A(ci, 1);
    w    = A(ci, 2);
    eidx = A(ci, 3);
    child_idx(depth) = ci + 1;

    if (on_stack(v))
      continue;                     # would repeat a node
    endif

    new_len = cur_len + w;
    if (new_len > max_len)
      continue;                     # DFS prune (upper bound)
    endif

    ## Push v.
    depth = depth + 1;
    node_buf(depth) = v;
    edge_buf(depth - 1)   = eidx;
    weight_buf(depth - 1) = w;
    on_stack(v) = true;
    child_idx(depth) = 1;
    cur_len = new_len;
  endwhile

  ## Trim accumulators to actual size.
  if (n_paths == 0)
    P  = cell (0, 1);
    d  = zeros (0, 1);
    ep = cell (0, 1);
  else
    P  = P(1:n_paths);
    d  = d(1:n_paths);
    ep = ep(1:n_paths);
  endif

  ## Format each node path as numeric or cellstr row, and each edge
  ## path as a numeric row.
  for k = 1:n_paths
    p_idx = P{k};
    if (return_names)
      row = nn(p_idx);
      P{k} = row(:).';
    else
      P{k} = double (p_idx(:).');
    endif
    ep{k} = double (ep{k}(:).');
  endfor

endfunction


## ----- subroutine: build out-edge adjacency list with weights and
## edge indices -----
##
## Returns out_adj, an N-by-1 cell array where out_adj{u} is an
## m_u-by-3 matrix with columns [dst, weight, edge_index].  For
## unweighted graphs, weight == 1.  For a digraph multigraph, every
## parallel edge appears as a separate row.  For an undirected graph,
## each edge appears in both endpoints' lists (so either direction is
## traversable) with the same edge_index.

function out_adj = __build_out_adj__ (G, N)

  out_adj = cell (N, 1);
  for u = 1:N
    out_adj{u} = zeros (0, 3);
  endfor

  E = G.Edges;
  EN = E.EndNodes;
  m = size (EN, 1);
  if (m == 0)
    return;
  endif
  if (isfield (E, "Weight"))
    W = double (E.Weight(:));
  else
    W = ones (m, 1);
  endif

  is_directed = isa (G, "digraph");
  src_list = EN(:, 1);
  dst_list = EN(:, 2);

  ## Count so we can preallocate each bucket.
  cnt = zeros (N, 1);
  for k = 1:m
    cnt(src_list(k)) = cnt(src_list(k)) + 1;
    if (! is_directed && src_list(k) != dst_list(k))
      cnt(dst_list(k)) = cnt(dst_list(k)) + 1;
    endif
  endfor

  for u = 1:N
    if (cnt(u) > 0)
      out_adj{u} = zeros (cnt(u), 3);
    endif
  endfor

  pos = zeros (N, 1);
  for k = 1:m
    s = src_list(k);
    t = dst_list(k);
    w = W(k);
    pos(s) = pos(s) + 1;
    out_adj{s}(pos(s), :) = [t, w, k];
    if (! is_directed && s != t)
      pos(t) = pos(t) + 1;
      out_adj{t}(pos(t), :) = [s, w, k];
    endif
  endfor

endfunction
