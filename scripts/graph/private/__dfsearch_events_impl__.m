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
## @deftypefn  {} {@var{out} =} __dfsearch_events_impl__ (@var{A}, @var{s}, @var{events})
## @deftypefnx {} {@var{out} =} __dfsearch_events_impl__ (@var{A}, @var{s}, @var{events}, @var{opts})
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
## Optional struct @var{opts} has fields:
##
## @itemize
## @item
## @code{restart} (default @code{false}): when @code{true}, DFS continues
## from the smallest-indexed undiscovered node after the initial source
## component is exhausted, emitting a new @qcode{"startnode"} event at
## each restart.  Repeats until every node has been visited.
## @item
## @code{edgecolors} (default @code{false}): when @code{true} and the
## output is struct-valued, an additional @code{EdgeColor} cellstr column
## is included with tags @qcode{"tree"} (@code{edgetonew}), @qcode{"back"}
## (@code{edgetodiscovered}), @qcode{"forward"} (@code{edgetofinished}
## with target discovered after source), or @qcode{"cross"}
## (@code{edgetofinished} with target discovered before source).
## Node-event rows get @qcode{""}.
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
## double matrix, @code{[0 0]} for node-only events).  With
## @code{opts.edgecolors = true} a fourth field @code{EdgeColor}
## (@math{m}-by-1 cellstr) is added.
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
## the target has already finished.  A finished target discovered after
## the source is a forward edge (descendant); a finished target
## discovered before the source is a cross edge.
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

function out = __dfsearch_events_impl__ (A, s, events, opts)

  if (nargin < 3 || nargin > 4)
    print_usage ();
  endif

  if (nargin < 4)
    opts = struct ();
  endif
  if (! isfield (opts, "restart"))
    opts.restart = false;
  endif
  if (! isfield (opts, "edgecolors"))
    opts.edgecolors = false;
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

  ## EdgeColors only makes sense with a struct-valued result.
  if (opts.edgecolors && ! return_struct)
    error ("Octave:invalid-input-arg", ...
           "dfsearch: EdgeColors requires events to be 'allevents' or a cell array of event names");
  endif

  ## ---- Run DFS and collect the full event log ----------------------

  N = size (A, 1);

  ## State machine: 0 = undiscovered, 1 = discovered (on stack), 2 =
  ## finished (all out-edges examined, popped from stack).
  state = zeros (N, 1);

  ## Discovery time, for classifying edgetofinished as forward or cross.
  disc_time = zeros (N, 1);
  dtime = 0;

  ## Preallocate.  Upper bound on total events:
  ##   R startnode + N discovernode + N finishnode + nnz(A) edge events,
  ## where R = up to N restarts.
  if (opts.restart)
    max_starts = N;
  else
    max_starts = 1;
  endif
  max_events = max_starts + 2 * N + nnz (A);
  if (max_events == 0)
    max_events = 1;
  endif
  ev_name = cell (max_events, 1);
  ev_node = zeros (max_events, 1);
  ev_edge = zeros (max_events, 2);
  if (opts.edgecolors)
    ev_color = cell (max_events, 1);
  endif
  k = 0;

  ## Explicit DFS stack with per-frame children list + cursor.
  stack_nodes = zeros (N, 1);
  stack_children = cell (N, 1);
  stack_cursor = zeros (N, 1);

  seed = s;
  while (true)
    ## startnode
    k = k + 1;
    ev_name{k} = "startnode";
    ev_node(k) = seed;
    if (opts.edgecolors)
      ev_color{k} = "";
    endif

    ## initial discovernode for seed
    state(seed) = 1;
    dtime = dtime + 1;
    disc_time(seed) = dtime;
    k = k + 1;
    ev_name{k} = "discovernode";
    ev_node(k) = seed;
    if (opts.edgecolors)
      ev_color{k} = "";
    endif

    sp = 1;
    stack_nodes(sp) = seed;
    cols = find (A(seed, :));
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
          if (opts.edgecolors)
            ev_color{k} = "tree";
          endif

          stack_cursor(sp) = idx;   # save resume position on parent
          state(v) = 1;
          dtime = dtime + 1;
          disc_time(v) = dtime;
          k = k + 1;
          ev_name{k} = "discovernode";
          ev_node(k) = v;
          if (opts.edgecolors)
            ev_color{k} = "";
          endif

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
          if (opts.edgecolors)
            ev_color{k} = "back";
          endif
        else  # state(v) == 2
          k = k + 1;
          ev_name{k} = "edgetofinished";
          ev_edge(k, :) = [u, v];
          if (opts.edgecolors)
            ## Forward edge: v is a descendant of u in the DFS tree iff
            ## v was discovered after u.  Otherwise it is a cross edge.
            if (disc_time(v) > disc_time(u))
              ev_color{k} = "forward";
            else
              ev_color{k} = "cross";
            endif
          endif
        endif
      endwhile

      if (! advanced)
        ## All children of u processed; pop.
        state(u) = 2;
        k = k + 1;
        ev_name{k} = "finishnode";
        ev_node(k) = u;
        if (opts.edgecolors)
          ev_color{k} = "";
        endif
        sp = sp - 1;
      endif
    endwhile

    if (! opts.restart)
      break;
    endif

    ## Find the next undiscovered node in ascending index order.
    next_seed = 0;
    for ii = 1:N
      if (state(ii) == 0)
        next_seed = ii;
        break;
      endif
    endfor
    if (next_seed == 0)
      break;
    endif
    seed = next_seed;
  endwhile

  ## Trim preallocated buffers.
  ev_name = ev_name(1:k);
  ev_node = ev_node(1:k);
  ev_edge = ev_edge(1:k, :);
  if (opts.edgecolors)
    ev_color = ev_color(1:k);
  endif

  ## ---- Filter to the requested event list --------------------------

  keep_mask = false (k, 1);
  for j = 1:numel (events_list)
    keep_mask |= strcmp (ev_name, events_list{j});
  endfor

  filtered_name = ev_name(keep_mask);
  filtered_node = ev_node(keep_mask);
  filtered_edge = ev_edge(keep_mask, :);
  if (opts.edgecolors)
    filtered_color = ev_color(keep_mask);
  endif

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
      if (opts.edgecolors)
        out.EdgeColor = cell (0, 1);
      endif
    else
      out.Event = filtered_name(:);
      out.Node = filtered_node(:);
      out.Edge = filtered_edge;
      if (opts.edgecolors)
        out.EdgeColor = filtered_color(:);
      endif
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

## US-T04: Restart option on disconnected digraph.
%!test
%! A = sparse ([1 4], [2 5], 1, 5, 5);
%! opts = struct ("restart", true);
%! T = __dfsearch_events_impl__ (A, 1, "allevents", opts);
%! starts = T.Node(strcmp (T.Event, "startnode"));
%! assert (starts, [1; 3; 4]);

## US-T04: EdgeColors adds EdgeColor field.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! opts = struct ("edgecolors", true);
%! T = __dfsearch_events_impl__ (A, 1, "allevents", opts);
%! assert (isfield (T, "EdgeColor"));

## US-T04: DFS back edge tagged 'back'.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! opts = struct ("edgecolors", true);
%! T = __dfsearch_events_impl__ (A, 1, "allevents", opts);
%! back_idx = strcmp (T.Event, "edgetodiscovered");
%! assert (T.EdgeColor(back_idx), {"back"});

## US-T04: DFS forward edge (triangle 1->{2,3}, 2->3) tagged 'forward'.
%!test
%! A = sparse ([1 1 2], [2 3 3], 1, 3, 3);
%! opts = struct ("edgecolors", true);
%! T = __dfsearch_events_impl__ (A, 1, "allevents", opts);
%! fin_idx = strcmp (T.Event, "edgetofinished");
%! assert (T.EdgeColor(fin_idx), {"forward"});

## US-T04: DFS cross edge (1->{2,3}, 3->2) tagged 'cross'.
%!test
%! A = sparse ([1 1 3], [2 3 2], 1, 3, 3);
%! opts = struct ("edgecolors", true);
%! T = __dfsearch_events_impl__ (A, 1, "allevents", opts);
%! fin_idx = strcmp (T.Event, "edgetofinished");
%! assert (T.EdgeColor(fin_idx), {"cross"});

## US-T04: EdgeColors without struct output errors.
%!error <EdgeColors requires> ...
%! __dfsearch_events_impl__ (sparse ([1 2], [2 3], 1, 3, 3), 1, ...
%!                           "discovernode", struct ("edgecolors", true))
