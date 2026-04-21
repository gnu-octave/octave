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
## @deftypefn  {} {[@var{X}, @var{Y}] =} __graph_plot_layered__ (@var{G})
## @deftypefnx {} {[@var{X}, @var{Y}] =} __graph_plot_layered__ (@var{G}, @var{direction})
## @deftypefnx {} {[@var{X}, @var{Y}] =} __graph_plot_layered__ (@var{G}, @var{direction}, @var{sources})
## @deftypefnx {} {[@var{X}, @var{Y}] =} __graph_plot_layered__ (@var{G}, @var{direction}, @var{sources}, @var{sinks})
## @deftypefnx {} {[@var{X}, @var{Y}] =} __graph_plot_layered__ (@var{G}, @var{direction}, @var{sources}, @var{sinks}, @var{assign_layers})
## Compute a 2-D Sugiyama-style layered (hierarchical) layout for a
## @code{graph} or @code{digraph}.
##
## Each node is assigned to an integer-indexed horizontal layer by a
## longest-path computation on a DAG projection of @var{G}.  Within a
## layer, node order is reduced by an iterated barycenter heuristic
## that lowers the number of edge crossings between consecutive layers.
## The final @var{X} and @var{Y} coordinates are returned as column
## vectors of length @code{numnodes (@var{G})}.
##
## @var{direction} selects how layers are unrolled in the plane and
## must be one of (case-insensitive):
##
## @table @code
## @item @qcode{"down"}
## Default.  Layer 1 (sources) at the top, later layers below.
## @item @qcode{"up"}
## Layer 1 at the bottom, later layers above.
## @item @qcode{"right"}
## Layer 1 on the left, later layers to the right.
## @item @qcode{"left"}
## Layer 1 on the right, later layers to the left.
## @end table
##
## @var{sources} is a (possibly empty) numeric vector of node indices
## that must be placed in the first layer.  @var{sinks} is a
## (possibly empty) numeric vector of node indices that must be placed
## in the last layer.  Out-of-range or non-integer indices raise an
## error.
##
## @var{assign_layers} is @qcode{"auto"} (default, equivalent to
## @qcode{"asap"}), @qcode{"asap"} (as-soon-as-possible, longest path
## from any node with no incoming edge), or @qcode{"alap"}
## (as-late-as-possible, each node placed as close to the sinks layer
## as its outgoing edges allow).
##
## For a non-DAG @code{digraph}, cycles are broken internally by
## reversing back-edges discovered during a depth-first traversal
## starting at node 1.  For an undirected @code{graph}, edges are
## oriented by a BFS from the chosen source (@code{sources(1)} if
## given, else node 1).  Layout is otherwise identical.
##
## The layout is fully deterministic: no random initialisation is
## used, so repeated calls on the same graph produce byte-identical
## coordinates.  @code{N == 0} returns @code{0-by-1} empty columns;
## @code{N == 1} returns the origin.
## @seealso{__graph_plot_auto_layout__, plot, GraphPlot}
## @end deftypefn

function [X, Y] = __graph_plot_layered__ (G, direction, sources, sinks, ...
                                          assign_layers)

  if (nargin < 1 || nargin > 5)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_layered__: G must be a graph or digraph");
  endif

  if (nargin < 2 || isempty (direction))
    direction = "down";
  endif
  if (! (ischar (direction) && isrow (direction)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_layered__: DIRECTION must be a character vector");
  endif
  direction = lower (direction);
  if (! any (strcmp (direction, {"down", "up", "left", "right"})))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_layered__: unknown DIRECTION '%s'", direction);
  endif

  if (nargin < 3)
    sources = [];
  endif
  if (nargin < 4)
    sinks = [];
  endif

  if (nargin < 5 || isempty (assign_layers))
    assign_layers = "auto";
  endif
  if (! (ischar (assign_layers) && isrow (assign_layers)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_layered__: ASSIGNLAYERS must be a character vector");
  endif
  assign_layers = lower (assign_layers);
  if (! any (strcmp (assign_layers, {"auto", "asap", "alap"})))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_layered__: unknown ASSIGNLAYERS '%s'", ...
           assign_layers);
  endif

  N = numnodes (G);

  sources = __gpl_resolve_node_list__ (sources, N, "SOURCES");
  sinks   = __gpl_resolve_node_list__ (sinks,   N, "SINKS");

  if (N == 0)
    X = zeros (0, 1);
    Y = zeros (0, 1);
    return;
  elseif (N == 1)
    X = 0;
    Y = 0;
    return;
  endif

  is_digraph = isa (G, "digraph");

  ## Build a 0/1 adjacency matrix, drop self-loops, break cycles.
  A = full (double (logical (adjacency (G))));
  if (N >= 1)
    A(1:(N+1):end) = 0;
  endif

  if (is_digraph)
    A = __gpl_break_cycles__ (A);
  else
    start_node = 1;
    if (! isempty (sources))
      start_node = sources(1);
    endif
    A = __gpl_undirected_to_dag__ (A, start_node);
  endif

  ## Detach user-declared sources from their predecessors (force them
  ## to rank 1 by making them roots in the working graph) and sinks
  ## from their successors (so they become leaves).
  A_work = A;
  if (! isempty (sources))
    A_work(:, sources) = 0;
  endif
  if (! isempty (sinks))
    A_work(sinks, :) = 0;
  endif

  ## Compute ranks.
  ranks = __gpl_asap_ranks__ (A_work);
  max_asap = max (ranks);

  ## ALAP override: re-rank each node to be as late as its outgoing
  ## edges allow.
  if (strcmp (assign_layers, "alap"))
    rev_ranks = __gpl_asap_ranks__ (A_work.');
    ranks = max_asap - rev_ranks + 1;
  endif

  ## Re-anchor sources (roots) to rank 1 and sinks (leaves) to max
  ## rank.  After A_work adjustment, sources already have no
  ## predecessors, so asap puts them at rank 1 naturally, but
  ## belt-and-braces:
  if (! isempty (sources))
    ranks(sources) = 1;
  endif
  if (! isempty (sinks))
    max_rank = max (ranks);
    ranks(sinks) = max_rank;
    ## Propagate in case a sink predecessor now requires a bigger
    ## value.  Use A_work so we do not violate the source roots.
    ranks = __gpl_propagate_down__ (A_work, ranks);
    max_rank = max (ranks);
    ranks(sinks) = max_rank;
  endif

  ## Normalise ranks to 1..L contiguously.
  [~, ~, ranks] = unique (ranks);
  num_layers = max (ranks);

  ## Build layer lists.
  layers = cell (num_layers, 1);
  for L = 1:num_layers
    layers{L} = find (ranks == L)(:);
  endfor

  ## Barycenter crossing reduction.  Use the symmetric adjacency
  ## (connectivity only, direction irrelevant here) so that nodes in a
  ## layer are repositioned by the mean rank-indexed coordinate of
  ## their neighbours in the adjacent layer.
  pos = zeros (N, 1);
  for L = 1:num_layers
    nodes = layers{L};
    pos(nodes) = 1:length (nodes);
  endfor

  A_sym = (A | A.');
  num_sweeps = 8;
  for sweep = 1:num_sweeps
    for L = 2:num_layers
      [layers{L}, pos] = __gpl_reorder_layer__ (layers{L}, A_sym, ...
                                                layers{L-1}, pos);
    endfor
    for L = (num_layers - 1):-1:1
      [layers{L}, pos] = __gpl_reorder_layer__ (layers{L}, A_sym, ...
                                                layers{L+1}, pos);
    endfor
  endfor

  ## Assign base coordinates.  "Down" convention: layer 1 at Y = 0,
  ## later layers at progressively more-negative Y; X centered within
  ## the layer.
  X = zeros (N, 1);
  Y = zeros (N, 1);
  for L = 1:num_layers
    nodes = layers{L};
    nL = length (nodes);
    if (nL == 1)
      X(nodes(1)) = 0;
    else
      offsets = (0:(nL-1)).' - (nL - 1) / 2;
      X(nodes) = offsets;
    endif
    Y(nodes) = -(L - 1);
  endfor

  ## Apply direction.
  switch (direction)
    case "down"
      ## No change.
    case "up"
      Y = -Y;
    case "right"
      ## Layer axis becomes X (increasing rightward), position becomes Y.
      tmp = X;
      X = -Y;
      Y = tmp;
    case "left"
      ## Layer axis becomes X (increasing leftward == decreasing X).
      tmp = X;
      X = Y;
      Y = tmp;
  endswitch

endfunction


## ------------- Local helpers -------------

function idx = __gpl_resolve_node_list__ (v, N, label)

  if (isempty (v))
    idx = [];
    return;
  endif
  if (! (isnumeric (v) && isreal (v) && all (isfinite (v(:)))))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_layered__: %s must be a numeric vector", label);
  endif
  v = double (v(:));
  if (any (v != round (v)) || any (v < 1) || any (v > N))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_layered__: %s contains an invalid node index", ...
           label);
  endif
  idx = v;

endfunction


function A_dag = __gpl_break_cycles__ (A)

  ## Iterative DFS that reverses edges encountered into "gray"
  ## (currently on the DFS stack) ancestors.  The resulting graph is
  ## acyclic.
  N = size (A, 1);
  A_dag = A;
  state = zeros (N, 1);  # 0 white, 1 gray, 2 black
  for start = 1:N
    if (state(start) != 0)
      continue;
    endif
    state(start) = 1;
    stack = start;
    iter = 0;
    iters = {find(A_dag(start, :))(:).'};
    while (! isempty (stack))
      node = stack(end);
      kids = iters{end};
      if (isempty (kids))
        state(node) = 2;
        stack(end) = [];
        iters(end) = [];
        continue;
      endif
      child = kids(1);
      iters{end} = kids(2:end);
      if (state(child) == 0)
        state(child) = 1;
        stack(end+1) = child;
        iters{end+1} = find (A_dag(child, :))(:).';
      elseif (state(child) == 1)
        ## Back edge -> reverse it.
        A_dag(node, child) = 0;
        A_dag(child, node) = 1;
      endif
      ## state == 2 (cross/forward edge): leave untouched.
    endwhile
  endfor

endfunction


function A_dag = __gpl_undirected_to_dag__ (A_sym, start)

  N = size (A_sym, 1);
  depth = Inf (N, 1);

  todo = start;
  while (! isempty (todo))
    depth(todo) = 0;
    queue = todo;
    while (! isempty (queue))
      node = queue(1);
      queue(1) = [];
      nb = find (A_sym(node, :));
      for v = nb
        if (isinf (depth(v)))
          depth(v) = depth(node) + 1;
          queue(end+1) = v;
        endif
      endfor
    endwhile
    max_d = max (depth(! isinf (depth)));
    unreached = find (isinf (depth));
    if (isempty (unreached))
      break;
    endif
    ## Start a new BFS from the first unreached node, offsetting
    ## its depth so its component appears below the previous one.
    todo = unreached(1);
    depth(todo) = max_d + 2;
  endwhile

  A_dag = zeros (N, N);
  [rs, cs] = find (A_sym);
  for k = 1:length (rs)
    u = rs(k);
    v = cs(k);
    if (u == v)
      continue;
    endif
    if (u >= v)
      ## Use each unordered pair once (u < v).
      continue;
    endif
    ## Direct the edge toward the higher-depth endpoint.  If equal,
    ## direct lower-index -> higher-index for determinism.
    if (depth(u) < depth(v))
      A_dag(u, v) = 1;
    elseif (depth(u) > depth(v))
      A_dag(v, u) = 1;
    else
      A_dag(u, v) = 1;   # u < v already
    endif
  endfor

endfunction


function ranks = __gpl_asap_ranks__ (A)

  ## Longest path from any node with no incoming edge.
  N = size (A, 1);
  ranks = ones (N, 1);
  ## Kahn's algorithm on A (in-degrees).
  indeg = sum (A, 1).';   # column vector of in-degrees
  queue = find (indeg == 0)(:);
  processed = false (N, 1);
  while (! isempty (queue))
    u = queue(1);
    queue(1) = [];
    processed(u) = true;
    succs = find (A(u, :));
    for v = succs
      if (ranks(v) < ranks(u) + 1)
        ranks(v) = ranks(u) + 1;
      endif
      indeg(v) = indeg(v) - 1;
      if (indeg(v) == 0)
        queue(end+1) = v;
      endif
    endfor
  endwhile

endfunction


function ranks = __gpl_propagate_down__ (A, ranks)

  ## Ensure that for every edge u -> v, ranks(v) >= ranks(u) + 1.
  ## This is done by iterative relaxation until no more updates.
  N = size (A, 1);
  changed = true;
  max_iter = N + 2;
  iter = 0;
  while (changed && iter < max_iter)
    changed = false;
    iter = iter + 1;
    [us, vs] = find (A);
    for k = 1:length (us)
      u = us(k);
      v = vs(k);
      if (ranks(v) < ranks(u) + 1)
        ranks(v) = ranks(u) + 1;
        changed = true;
      endif
    endfor
  endwhile

endfunction


function [layer_new, pos] = __gpl_reorder_layer__ (layer, A_sym, ...
                                                   neighbor_layer, pos)

  nL = length (layer);
  if (nL <= 1)
    layer_new = layer;
    if (nL == 1)
      pos(layer_new(1)) = 1;
    endif
    return;
  endif

  if (isempty (neighbor_layer))
    ## No neighbour info: preserve current order.
    layer_new = layer;
    for k = 1:nL
      pos(layer_new(k)) = k;
    endfor
    return;
  endif

  bary = zeros (nL, 1);
  for k = 1:nL
    node = layer(k);
    nb_mask = logical (A_sym(node, neighbor_layer));
    if (any (nb_mask))
      nb_nodes = neighbor_layer(nb_mask);
      bary(k) = mean (pos(nb_nodes));
    else
      ## No neighbour in that layer: keep current position to stay
      ## stable.
      bary(k) = pos(node);
    endif
  endfor

  ## Stable sort by (barycenter, current_pos, node_index) for full
  ## determinism.
  cur_pos = pos(layer);
  keys = [bary, cur_pos, layer];
  [~, idx] = sortrows (keys);
  layer_new = layer(idx);
  for k = 1:nL
    pos(layer_new(k)) = k;
  endfor

endfunction


## ---------------- BIST ----------------

## N == 0: empty 0-by-1 columns.
%!test
%! G = digraph ();
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);

## N == 1: origin.
%!test
%! G = digraph (1);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (X, 0);
%! assert (Y, 0);

## N == 1 undirected: origin.
%!test
%! G = graph (1);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (X, 0);
%! assert (Y, 0);

## 2-node chain: node 1 at Y = 0 (top), node 2 at Y = -1 (below).
%!test
%! G = digraph ([1], [2]);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (size (X), [2, 1]);
%! assert (size (Y), [2, 1]);
%! assert (Y(1), 0);
%! assert (Y(2), -1);

## 3-node chain 1->2->3: ranks 1, 2, 3.
%!test
%! G = digraph ([1 2], [2 3]);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);
%! assert (Y(2), -1);
%! assert (Y(3), -2);

## 3-node chain: X coordinates equal (single-node layers centered).
%!test
%! G = digraph ([1 2], [2 3]);
%! [X, ~] = __graph_plot_layered__ (G);
%! assert (X, zeros (3, 1));

## Diamond DAG: 1->2, 1->3, 2->4, 3->4. Node 1 layer 1, 2 & 3 layer 2,
## node 4 layer 3.  Y distinct across layers, equal within layer.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);
%! assert (Y(2), -1);
%! assert (Y(3), -1);
%! assert (Y(4), -2);
%! assert (X(1), 0);
%! assert (X(4), 0);
%! assert (X(2) != X(3));

## Deterministic: repeat calls produce identical coordinates.
%!test
%! G = digraph ([1 1 2 3 4], [2 3 4 4 5]);
%! [X1, Y1] = __graph_plot_layered__ (G);
%! [X2, Y2] = __graph_plot_layered__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);

## Column-vector outputs on a non-trivial DAG.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));

## All coordinates finite.
%!test
%! G = digraph ([1 1 2 3 4], [2 3 4 4 5]);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Direction 'down' (default): Y non-positive, layer 1 at Y = 0.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X, Y] = __graph_plot_layered__ (G, "down");
%! assert (max (Y), 0);
%! assert (min (Y), -3);
%! assert (Y(1), 0);
%! assert (Y(4), -3);

## Direction 'up': Y flipped relative to 'down'.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X_d, Y_d] = __graph_plot_layered__ (G, "down");
%! [X_u, Y_u] = __graph_plot_layered__ (G, "up");
%! assert (Y_u, -Y_d);
%! assert (X_u, X_d);
%! assert (Y_u(1), 0);
%! assert (Y_u(4), 3);

## Direction 'right': rank axis becomes X, position becomes Y.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X, Y] = __graph_plot_layered__ (G, "right");
%! assert (X(1), 0);
%! assert (X(4), 3);
%! assert (Y, zeros (4, 1));

## Direction 'left': rank axis is X, direction reversed.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X, Y] = __graph_plot_layered__ (G, "left");
%! assert (X(1), 0);
%! assert (X(4), -3);
%! assert (Y, zeros (4, 1));

## Direction is case-insensitive.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X1, Y1] = __graph_plot_layered__ (G, "down");
%! [X2, Y2] = __graph_plot_layered__ (G, "DOWN");
%! [X3, Y3] = __graph_plot_layered__ (G, "Down");
%! assert (X1, X2);
%! assert (X1, X3);
%! assert (Y1, Y2);
%! assert (Y1, Y3);

## Sources override: force node 3 to layer 1.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [~, Y] = __graph_plot_layered__ (G, "down", 3);
%! assert (Y(3), 0);

## Sinks override: force node 1 to final layer.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [~, Y] = __graph_plot_layered__ (G, "down", [], 1);
%! assert (Y(1), min (Y));

## Sources respected alongside original: auto node 1 at layer 1.
%!test
%! G = digraph ([1 2], [2 3]);
%! [~, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);

## AssignLayers 'alap': chain 1->2->3->4 is unchanged (already tight).
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X_as, Y_as] = __graph_plot_layered__ (G, "down", [], [], "asap");
%! [X_al, Y_al] = __graph_plot_layered__ (G, "down", [], [], "alap");
%! assert (Y_as, Y_al);

## AssignLayers 'alap': diamond 1->2, 1->3, 2->4, 3->4 has short path
## 1->3->4; asap places node 3 at layer 2, alap keeps it at layer 2 as
## well (constrained by the edges).  So the resulting ranks are
## equivalent here; check that both produce valid 3-layer layouts.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! [~, Y_as] = __graph_plot_layered__ (G, "down", [], [], "asap");
%! [~, Y_al] = __graph_plot_layered__ (G, "down", [], [], "alap");
%! assert (unique (Y_as), [-2; -1; 0]);
%! assert (unique (Y_al), [-2; -1; 0]);

## AssignLayers 'alap' pulls a dangling branch down to the final
## layer:  1->2, 1->3, 3->4, 2->4.  Node 3 can be on layer 2 (asap) or
## layer 2 (alap); same result.  Change to: 1->2, 2->4, 1->3 (node 3
## isolated from sink).  Under asap: node 3 at layer 2.  Under alap:
## node 3 at layer 3 (= max).
%!test
%! G = digraph ([1 2 1], [2 4, 3]);
%! [~, Y_as] = __graph_plot_layered__ (G, "down", [], [], "asap");
%! [~, Y_al] = __graph_plot_layered__ (G, "down", [], [], "alap");
%! assert (Y_as(3), -1);
%! assert (Y_al(3), -2);

## 'auto' matches 'asap'.
%!test
%! G = digraph ([1 2 1], [2 4, 3]);
%! [~, Y_au] = __graph_plot_layered__ (G, "down", [], [], "auto");
%! [~, Y_as] = __graph_plot_layered__ (G, "down", [], [], "asap");
%! assert (Y_au, Y_as);

## Cyclic digraph is handled (back-edges reversed internally).
%!test
%! G = digraph ([1 2 3], [2 3 1]);   # 3-cycle
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (size (X), [3, 1]);
%! assert (size (Y), [3, 1]);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Self-loops ignored (do not break layering).
%!test
%! G = digraph ([1 1 2], [1 2 3]);   # includes self-loop on 1
%! [~, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);
%! assert (Y(3), -2);

## Undirected graph: laid out via BFS from sources(1) or node 1.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! [~, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);
%! assert (Y(4), -3);

## Undirected graph with explicit source changes the BFS root.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! [~, Yd] = __graph_plot_layered__ (G, "down", 4);
%! assert (Yd(4), 0);
%! assert (Yd(1), min (Yd));

## Isolated nodes get their own layer in an edgeless graph.
%!test
%! G = digraph (3);   # 3 isolated nodes
%! [~, Y] = __graph_plot_layered__ (G);
%! assert (all (Y == 0));

## Disconnected digraph: two chains; both start at rank 1.
%!test
%! G = digraph ([1 3], [2 4]);
%! [~, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);
%! assert (Y(3), 0);
%! assert (Y(2), -1);
%! assert (Y(4), -1);

## Barycenter reduction: parallel 4-node crossing example.  Edges
## 1->3, 2->4, 1->4, 2->3 cross; after barycenter the layout is still
## 2-layer with finite coords.
%!test
%! G = digraph ([1 2 1 2], [3 4 4 3]);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);
%! assert (Y(2), 0);
%! assert (Y(3), -1);
%! assert (Y(4), -1);
%! assert (all (isfinite (X)));

## Node 1 at layer 1, multi-successor layer 2 X coordinates centered.
%!test
%! G = digraph ([1 1 1], [2 3 4]);
%! [X, Y] = __graph_plot_layered__ (G);
%! assert (Y(1), 0);
%! assert (Y(2), -1);
%! assert (Y(3), -1);
%! assert (Y(4), -1);
%! assert (X(1), 0);
%! assert (abs (mean (X(2:4))), 0, 1e-12);

## Sources of wrong type / range raise a clear error.
%!error <SOURCES> __graph_plot_layered__ (digraph (3), "down", 5)
%!error <SOURCES> __graph_plot_layered__ (digraph (3), "down", 0)
%!error <SOURCES> __graph_plot_layered__ (digraph (3), "down", 1.5)
%!error <SOURCES> __graph_plot_layered__ (digraph (3), "down", "bogus")

%!error <SINKS> __graph_plot_layered__ (digraph (3), "down", [], 5)
%!error <SINKS> __graph_plot_layered__ (digraph (3), "down", [], -1)

## Direction validation.
%!error <DIRECTION> __graph_plot_layered__ (digraph (3), 1)
%!error <DIRECTION> __graph_plot_layered__ (digraph (3), "nowhere")

## AssignLayers validation.
%!error <ASSIGNLAYERS> __graph_plot_layered__ (digraph (3), "down", [], [], 1)
%!error <ASSIGNLAYERS> __graph_plot_layered__ (digraph (3), "down", ...
%!                                            [], [], "bogus")

## Non-graph first arg rejected.
%!error <graph or digraph> __graph_plot_layered__ (1)
%!error <graph or digraph> __graph_plot_layered__ ("hello")

## Invalid call (no args).
%!error <Invalid call> __graph_plot_layered__ ()

## Direction 'down' layer 1 always at Y = 0.
%!test
%! G1 = digraph ([1 2], [2 3]);
%! G2 = digraph ([1 1 2], [2 3 3]);
%! G3 = graph ([1 2], [2 3]);
%! [~, Y1] = __graph_plot_layered__ (G1, "down");
%! [~, Y2] = __graph_plot_layered__ (G2, "down");
%! [~, Y3] = __graph_plot_layered__ (G3, "down");
%! assert (max (Y1), 0);
%! assert (max (Y2), 0);
%! assert (max (Y3), 0);
