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
## @deftypefn {} {@var{TR} =} __shortestpathtree_impl__ (@var{G}, @var{W}, @var{s_idx}, @var{s_by_name}, @dots{})
## Private helper: common body of the @code{shortestpathtree} methods
## of the @code{graph} and @code{digraph} classes.
##
## The caller is responsible for resolving @var{s} to an integer index
## @var{s_idx} and for preparing the weight matrix @var{W} used by
## Dijkstra (collapsing parallel edges, applying the directed /
## undirected convention, etc.).  This helper parses the remaining
## arguments -- an optional positional target list @var{t} followed by
## Name-Value option pairs -- runs Dijkstra, and constructs the output
## @var{TR} according to the @qcode{"OutputForm"} option.
##
## @var{s_by_name} is a boolean flag provided by the caller's
## @code{__resolve_single_node__} call: @code{true} when @var{s} was
## given as a node name (so the @qcode{"cell"} output should contain
## cellstr paths), @code{false} when @var{s} was a numeric index.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{shortestpathtree, graph, digraph, __shortestpathtree_dijkstra__}
## @end deftypefn

function TR = __shortestpathtree_impl__ (G, W, s_idx, s_by_name, varargin)

  if (nargin < 4)
    print_usage ();
  endif

  N = size (W, 1);
  args = varargin;
  nargs = numel (args);

  ## Parse an optional positional target list.  Use a parity rule on
  ## the trailing arg count: odd => first trailing arg is the target T
  ## and the rest are Name-Value pairs; even => all trailing args are
  ## Name-Value pairs.  This resolves the ambiguity between
  ##   shortestpathtree (G, s, "OutputForm", val)      # NV pair form
  ## and
  ##   shortestpathtree (G, s, t, "OutputForm", val)   # target + NV
  ## without having to enumerate all valid option names.
  have_t = false;
  t_idx = zeros (0, 1);
  t_by_name = false;

  if (mod (nargs, 2) == 1)
    have_t = true;
    [t_idx, t_by_name] = __resolve_spt_targets__ (G, args{1});
    args(1) = [];
    nargs = numel (args);
  endif

  ## Name-Value pairs: currently only OutputForm.
  output_form = "tree";
  if (mod (nargs, 2) != 0)
    error ("Octave:invalid-input-arg", ...
           "shortestpathtree: Name,Value arguments must appear in pairs");
  endif
  for k = 1:2:nargs
    name = args{k};
    if (! (ischar (name) && isrow (name)))
      error ("Octave:invalid-input-arg", ...
             "shortestpathtree: option names must be strings");
    endif
    if (strcmpi (name, "OutputForm"))
      val = args{k+1};
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "shortestpathtree: OutputForm value must be a string");
      endif
      if (strcmpi (val, "tree"))
        output_form = "tree";
      elseif (strcmpi (val, "vector"))
        output_form = "vector";
      elseif (strcmpi (val, "cell"))
        output_form = "cell";
      else
        error ("Octave:invalid-input-arg", ...
               "shortestpathtree: OutputForm must be 'tree', 'vector', or 'cell'");
      endif
    else
      error ("Octave:invalid-input-arg", ...
             "shortestpathtree: unknown option '%s'", name);
    endif
  endfor

  [pred, dist] = __shortestpathtree_dijkstra__ (W, s_idx);

  ## When targets are given, prune the predecessor tree to nodes on a
  ## shortest path from s to some reachable target.  keep_mask(i) is
  ## true when node i is on such a path.
  if (have_t)
    keep_mask = false (N, 1);
    keep_mask(s_idx) = true;
    for ii = 1:numel (t_idx)
      ti = t_idx(ii);
      if (! isfinite (dist(ti)))
        continue;
      endif
      u = ti;
      while (u != 0 && ! keep_mask(u))
        keep_mask(u) = true;
        if (u == s_idx)
          break;
        endif
        u = pred(u);
      endwhile
    endfor
  else
    keep_mask = true (N, 1);
  endif

  ## Whether to return names-paths: true if the source or any target
  ## was given by name (MATLAB parity for shortestpath).
  return_names = s_by_name || t_by_name;

  switch (output_form)
    case "tree"
      TR = __spt_build_tree__ (G, pred, dist, keep_mask, s_idx, N);
    case "vector"
      TR = __spt_build_vector__ (pred, keep_mask, s_idx, N);
    case "cell"
      if (have_t)
        TR = __spt_build_cell_targets__ (G, pred, dist, t_idx, ...
                                         s_idx, return_names);
      else
        TR = __spt_build_cell_all__ (G, pred, dist, s_idx, N, ...
                                     return_names);
      endif
  endswitch

endfunction


## ----- subroutine: resolve the target argument -----
##
## t may be a numeric vector of indices, a character row name, or a
## cellstr of names.  Returns t_idx as a column vector of positive
## integer indices and t_by_name as a scalar logical.

function [t_idx, t_by_name] = __resolve_spt_targets__ (G, t)

  if (ischar (t) || iscell (t))
    [t_idx, ~] = __resolve_node_list__ (G, t, "shortestpathtree");
    t_by_name = true;
  elseif (isnumeric (t))
    [t_idx, ~] = __resolve_node_list__ (G, t, "shortestpathtree");
    t_by_name = false;
  else
    error ("Octave:invalid-input-arg", ...
           ["shortestpathtree: target T must be a numeric index ", ...
            "vector, a character row name, or a cellstr of names"]);
  endif

endfunction


## ----- subroutine: build the 'tree' output digraph -----
##
## Collect the predecessor edges (p -> i) for every i with pred(i) > 0
## and keep_mask(i), then build a digraph with N nodes that preserves
## G's node names and carries the weight of each kept edge in W
## (where W is recomputed here from G to match G's own storage of
## parallel edges -- we want TR.Edges to carry the original weights of
## the picked parallel edges).
##
## MATLAB parity: for a multigraph digraph, the tree carries the
## minimum weight among parallel edges (matching what shortestpath
## stores in its edgepath indexing).

function TR = __spt_build_tree__ (G, pred, dist, keep_mask, ~, N)

  ## Gather kept tree edges.
  kept = find (pred > 0 & keep_mask);
  if (isempty (kept))
    tree_s = zeros (0, 1);
    tree_t = zeros (0, 1);
    tree_w = [];
  else
    tree_t = kept(:);
    tree_s = pred(tree_t);
    ## Weight of edge p -> i in the tree is dist(i) - dist(p).
    tree_w = dist(tree_t) - dist(tree_s);
  endif

  ## Decide whether the tree should carry a Weight column:
  ## - has_weights_ true on source G -> carry weights in tree.
  ## - otherwise build an unweighted tree.
  source_weighted = __graph_has_weights__ (G);

  nn = G.Nodes.Name;
  has_names = ! isempty (nn);

  if (has_names)
    if (source_weighted)
      TR = digraph (tree_s, tree_t, tree_w, nn);
    else
      TR = digraph (tree_s, tree_t, [], nn);
    endif
  else
    if (isempty (tree_s))
      TR = digraph (N);
    else
      if (source_weighted)
        TR = digraph (tree_s, tree_t, tree_w);
      else
        TR = digraph (tree_s, tree_t);
      endif
      ## Pad to N nodes if the tree's max endpoint was below N.
      if (numnodes (TR) < N)
        TR = addnode (TR, N - numnodes (TR));
      endif
    endif
  endif

endfunction


## ----- subroutine: build the 'vector' output predecessor array -----

function v = __spt_build_vector__ (pred, keep_mask, s_idx, N)

  v = zeros (1, N);
  for i = 1:N
    if (keep_mask(i) && (pred(i) != 0 || i == s_idx))
      if (i == s_idx)
        v(i) = s_idx;
      else
        v(i) = pred(i);
      endif
    endif
  endfor

endfunction


## ----- subroutine: build the 'cell' output (two-arg form) -----

function C = __spt_build_cell_all__ (G, pred, dist, s_idx, N, ...
                                     return_names)

  C = cell (N, 1);
  nn = G.Nodes.Name;
  for i = 1:N
    if (i == s_idx)
      p_idx = s_idx;
    elseif (isfinite (dist(i)) && pred(i) != 0)
      p_idx = __spt_reconstruct__ (pred, s_idx, i);
    else
      p_idx = zeros (1, 0);
    endif
    C{i} = __spt_format_path__ (p_idx, nn, return_names);
  endfor

endfunction


## ----- subroutine: build the 'cell' output (three-arg form) -----

function C = __spt_build_cell_targets__ (G, pred, dist, t_idx, ...
                                         s_idx, return_names)

  nt = numel (t_idx);
  C = cell (nt, 1);
  nn = G.Nodes.Name;
  for k = 1:nt
    i = t_idx(k);
    if (i == s_idx)
      p_idx = s_idx;
    elseif (isfinite (dist(i)) && pred(i) != 0)
      p_idx = __spt_reconstruct__ (pred, s_idx, i);
    else
      p_idx = zeros (1, 0);
    endif
    C{k} = __spt_format_path__ (p_idx, nn, return_names);
  endfor

endfunction


## ----- subroutine: reconstruct a single path by walking predecessors -----

function p_idx = __spt_reconstruct__ (pred, s_idx, t_idx)

  ## Walk from t_idx back to s_idx via pred, then flip to forward order.
  N = numel (pred);
  buf = zeros (1, N);
  k = 1;
  buf(k) = t_idx;
  while (buf(k) != s_idx)
    u = pred(buf(k));
    if (u == 0)
      p_idx = zeros (1, 0);
      return;
    endif
    k = k + 1;
    buf(k) = u;
  endwhile
  p_idx = fliplr (buf(1:k));

endfunction


## ----- subroutine: format a reconstructed path for cell output -----

function out = __spt_format_path__ (p_idx, nn, return_names)

  if (return_names)
    if (isempty (p_idx))
      out = cell (1, 0);
    else
      out = nn(p_idx);
      out = out(:).';  # row shape
    endif
  else
    if (isempty (p_idx))
      out = zeros (1, 0);
    else
      out = double (p_idx);
      out = out(:).';
    endif
  endif

endfunction


## ----- subroutine: ask the source G whether it carries weights -----

function tf = __graph_has_weights__ (G)

  ## G is a graph or digraph.  We can't access has_weights_ directly
  ## from this private helper (different package scope), so use the
  ## public Edges property.
  e = G.Edges;
  tf = isfield (e, "Weight");

endfunction
