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
## @deftypefn {} {@var{out} =} __dfsearch_events_impl__ (@var{A}, @var{s}, @var{events})
## Private helper implementing the @code{dfsearch (G, s, events)} event
## machinery on a binary or count-valued sparse adjacency @var{A}.
##
## @var{A} is an @math{N}-by-@math{N} sparse matrix whose nonzero entries
## mark edges.  For an undirected graph pass the symmetric adjacency; for
## a directed graph pass the directed adjacency (row = source).  @var{s}
## is a validated 1-based source node index.  @var{events} is one of:
##
## @itemize
## @item
## a single char row naming one of the six event types -- one of
## @qcode{"discovernode"}, @qcode{"edgetonew"},
## @qcode{"edgetodiscovered"}, @qcode{"edgetofinished"},
## @qcode{"finishnode"}, or @qcode{"startnode"};
## @item
## the char row @qcode{"allevents"};
## @item
## a cell array of strings, each naming one of the six event types.
## @end itemize
##
## Return value:
##
## @itemize
## @item
## For a single node event (@qcode{"discovernode"}, @qcode{"finishnode"},
## @qcode{"startnode"}): an @math{m}-by-1 numeric column vector of node
## indices in the order the event fired.
## @item
## For a single edge event (@qcode{"edgetonew"},
## @qcode{"edgetodiscovered"}, @qcode{"edgetofinished"}): an
## @math{m}-by-2 numeric matrix of @code{[src, dst]} pairs.
## @item
## For @qcode{"allevents"} or a cellstr: a scalar struct with fields
## @code{Event} (@math{m}-by-1 cellstr), @code{Node} (@math{m}-by-1
## double column, 0 for edge-only events), and @code{Edge} (@math{m}-by-2
## double matrix, @code{[0 0]} for node-only events).
## @end itemize
##
## Events are emitted in DFS traversal order.  For the classical DFS
## interpretation:
##
## @itemize
## @item
## @qcode{"edgetonew"} is a @emph{tree edge} -- the target was
## undiscovered and becomes part of the DFS tree.
## @item
## @qcode{"edgetodiscovered"} is a @emph{back edge} -- the target is
## currently on the DFS stack (discovered but not yet finished).  Back
## edges witness cycles.
## @item
## @qcode{"edgetofinished"} is a @emph{cross} or @emph{forward edge} --
## the target has already finished.
## @end itemize
##
## Parallel edges in a multigraph collapse to a single event per distinct
## @math{(u, v)} pair, matching the behaviour of @code{dfsearch (G, s)}.
## Self-loops at a currently-processing node @math{u} generate an
## @qcode{"edgetodiscovered"} event because @math{u} is discovered but
## not yet finished at the moment the self-loop is examined.
##
## @seealso{dfsearch, graph, digraph, __dfsearch_impl__}
## @end deftypefn

function out = __dfsearch_events_impl__ (A, s, events)

  if (nargin != 3)
    print_usage ();
  endif

  ## ---- Validate events argument -------------------------------------

  valid_events = {"discovernode", "edgetonew", "edgetodiscovered", ...
                  "edgetofinished", "finishnode", "startnode"};
  node_events = {"discovernode", "finishnode", "startnode"};

  single_char_event = false;
  return_struct = false;
  events_list = {};

  if (ischar (events))
    if (! isrow (events) && ! isempty (events))
      error ("Octave:invalid-input-arg", ...
             "dfsearch: EVENTS must be a character string or cell array of strings");
    endif
    if (strcmp (events, "allevents"))
      return_struct = true;
      events_list = valid_events;
    elseif (any (strcmp (events, valid_events)))
      single_char_event = true;
      events_list = {events};
    else
      error ("Octave:invalid-input-arg", ...
             "dfsearch: unknown event name '%s'", events);
    endif
  elseif (iscellstr (events))
    return_struct = true;
    events_list = events(:).';
    for k = 1:numel (events_list)
      if (! any (strcmp (events_list{k}, valid_events)))
        error ("Octave:invalid-input-arg", ...
               "dfsearch: unknown event name '%s'", events_list{k});
      endif
    endfor
  else
    error ("Octave:invalid-input-arg", ...
           "dfsearch: EVENTS must be a character string or cell array of strings");
  endif

  ## ---- Run DFS and collect the full event log ----------------------

  N = size (A, 1);

  ## State machine: 0 = undiscovered, 1 = discovered (on stack), 2 =
  ## finished (all out-edges examined, popped from stack).
  state = zeros (N, 1);

  ## Preallocate.  Upper bound on total events:
  ##   1 startnode + N discovernode + N finishnode + nnz(A) edge events.
  max_events = 1 + 2 * N + nnz (A);
  if (max_events == 0)
    max_events = 1;
  endif
  ev_name = cell (max_events, 1);
  ev_node = zeros (max_events, 1);
  ev_edge = zeros (max_events, 2);
  k = 0;

  ## startnode
  k = k + 1;
  ev_name{k} = "startnode";
  ev_node(k) = s;

  ## initial discovernode for source
  state(s) = 1;
  k = k + 1;
  ev_name{k} = "discovernode";
  ev_node(k) = s;

  ## Explicit DFS stack with per-frame children list + cursor (see
  ## __dfsearch_impl__ for the same pattern).
  stack_nodes = zeros (N, 1);
  stack_children = cell (N, 1);
  stack_cursor = zeros (N, 1);
  sp = 1;

  stack_nodes(sp) = s;
  cols = find (A(s, :));
  stack_children{sp} = cols(:).';  # ascending order
  stack_cursor(sp) = 1;

  while (sp >= 1)
    u = stack_nodes(sp);
    children = stack_children{sp};
    idx = stack_cursor(sp);

    advanced = false;
    while (idx <= numel (children))
      v = children(idx);
      idx = idx + 1;
      if (state(v) == 0)
        ## Tree edge.
        k = k + 1;
        ev_name{k} = "edgetonew";
        ev_edge(k, :) = [u, v];

        stack_cursor(sp) = idx;   # save resume position on parent
        state(v) = 1;
        k = k + 1;
        ev_name{k} = "discovernode";
        ev_node(k) = v;

        sp = sp + 1;
        stack_nodes(sp) = v;
        cols = find (A(v, :));
        stack_children{sp} = cols(:).';
        stack_cursor(sp) = 1;
        advanced = true;
        break;
      elseif (state(v) == 1)
        k = k + 1;
        ev_name{k} = "edgetodiscovered";
        ev_edge(k, :) = [u, v];
      else  # state(v) == 2
        k = k + 1;
        ev_name{k} = "edgetofinished";
        ev_edge(k, :) = [u, v];
      endif
    endwhile

    if (! advanced)
      ## All children of u processed; pop.
      state(u) = 2;
      k = k + 1;
      ev_name{k} = "finishnode";
      ev_node(k) = u;
      sp = sp - 1;
    endif
  endwhile

  ## Trim preallocated buffers.
  ev_name = ev_name(1:k);
  ev_node = ev_node(1:k);
  ev_edge = ev_edge(1:k, :);

  ## ---- Filter to the requested event list --------------------------

  keep_mask = false (k, 1);
  for j = 1:numel (events_list)
    keep_mask |= strcmp (ev_name, events_list{j});
  endfor

  filtered_name = ev_name(keep_mask);
  filtered_node = ev_node(keep_mask);
  filtered_edge = ev_edge(keep_mask, :);

  ## ---- Format output ------------------------------------------------

  if (single_char_event)
    if (any (strcmp (events, node_events)))
      out = filtered_node;
    else
      out = filtered_edge;
      if (isempty (out))
        out = zeros (0, 2);
      endif
    endif
  else
    out = struct ();
    if (isempty (filtered_name))
      out.Event = cell (0, 1);
      out.Node = zeros (0, 1);
      out.Edge = zeros (0, 2);
    else
      out.Event = filtered_name(:);
      out.Node = filtered_node(:);
      out.Edge = filtered_edge;
    endif
  endif

endfunction


## Private-helper smoke tests.  (Private helpers are not loaded from a
## plain script context by default, so these tests only run when the
## private directory is on the load path -- i.e. inside the BIST runs
## of scripts/graph/dfsearch.m or scripts/graph/digraph.m.)

## Singleton: allevents returns startnode, discovernode, finishnode.
%!test
%! T = __dfsearch_events_impl__ (sparse (1, 1), 1, "allevents");
%! assert (T.Event, {"startnode"; "discovernode"; "finishnode"});
%! assert (T.Node, [1; 1; 1]);
%! assert (T.Edge, [0 0; 0 0; 0 0]);

## 3-cycle, single char event 'discovernode'.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! v = __dfsearch_events_impl__ (A, 1, "discovernode");
%! assert (v, [1; 2; 3]);

## 3-cycle, single char event 'finishnode' -> [3; 2; 1] (post-order).
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! v = __dfsearch_events_impl__ (A, 1, "finishnode");
%! assert (v, [3; 2; 1]);

## 3-cycle, single char event 'edgetonew'.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! E = __dfsearch_events_impl__ (A, 1, "edgetonew");
%! assert (E, [1 2; 2 3]);

## 3-cycle, single char event 'edgetodiscovered' -> (3,1) back edge.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! E = __dfsearch_events_impl__ (A, 1, "edgetodiscovered");
%! assert (E, [3 1]);

## No edge events fires -> 0x2 empty matrix of class double.
%!test
%! A = sparse ([1 1], [2 3], 1, 3, 3);
%! E = __dfsearch_events_impl__ (A, 1, "edgetofinished");
%! assert (size (E), [0, 2]);
%! assert (class (E), "double");

## Cellstr events returns struct with selected rows only.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! T = __dfsearch_events_impl__ (A, 1, {"discovernode"});
%! assert (isstruct (T));
%! assert (T.Event, {"discovernode"; "discovernode"; "discovernode"});
%! assert (T.Node, [1; 2; 3]);

## Triangle 1->{2,3}, 2->3: forward edge (1,3) becomes edgetofinished.
%!test
%! A = sparse ([1 1 2], [2 3 3], 1, 3, 3);
%! E = __dfsearch_events_impl__ (A, 1, "edgetofinished");
%! assert (E, [1 3]);

## 'allevents' on 3-cycle returns the full DFS event sequence.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! T = __dfsearch_events_impl__ (A, 1, "allevents");
%! assert (T.Event, {"startnode"; "discovernode"; "edgetonew"; ...
%!                   "discovernode"; "edgetonew"; "discovernode"; ...
%!                   "edgetodiscovered"; "finishnode"; "finishnode"; ...
%!                   "finishnode"});

## Error: unknown event name.
%!error <unknown event> ...
%! __dfsearch_events_impl__ (sparse (2, 2), 1, "bogus")

## Error: numeric events argument.
%!error <character string or cell array of strings> ...
%! __dfsearch_events_impl__ (sparse (2, 2), 1, 42)

## Empty cellstr -> empty struct with right field shapes.
%!test
%! T = __dfsearch_events_impl__ (sparse (2, 2), 1, cell (0));
%! assert (isstruct (T));
%! assert (size (T.Event), [0, 1]);
%! assert (size (T.Node), [0, 1]);
%! assert (size (T.Edge), [0, 2]);
