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
## @deftypefn {} {[@var{idx}, @var{by_name}] =} __resolve_single_node__ (@var{G}, @var{nodeID}, @var{method})
## Private helper for @code{digraph} and @code{graph} methods that take
## a single scalar node identifier.
##
## Resolve @var{nodeID} into a positive integer index @var{idx} in the
## range @code{1:numnodes (@var{G})}.  @var{nodeID} may be:
##
## @itemize
## @item
## a real positive-integer scalar, interpreted as a 1-based node index
## into @code{@var{G}.Nodes.Name};
## @item
## a char row vector, interpreted as a node name;
## @item
## a 1-element cell array of strings, interpreted as a node name.
## @end itemize
##
## @var{method} is the display name used in error messages (typically
## the calling method, e.g. @qcode{"successors"} or
## @qcode{"predecessors"}).
##
## The second return value @var{by_name} is @code{true} when
## @var{nodeID} was a name (string or 1-element cellstr) and
## @code{false} when it was a numeric index.  Callers use this flag to
## decide whether to return numeric node indices or a cellstr of node
## names (MATLAB parity: the return type matches the input type).
## @seealso{digraph, graph, successors, predecessors, neighbors}
## @end deftypefn

function [idx, by_name] = __resolve_single_node__ (G, nodeID, method)

  if (nargin != 3)
    print_usage ();
  endif

  nn = G.Nodes.Name;
  has_names = ! isempty (nn);

  ## A bare char row vector counts as a single node name.  Promote to
  ## a 1-element cellstr so the cellstr branch below handles it.
  if (ischar (nodeID))
    if (! isrow (nodeID) && ! isempty (nodeID))
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: NODEID must be a scalar node ", ...
              "identifier (char row vector, 1-element cellstr, or ", ...
              "scalar positive integer)"], method);
    endif
    nodeID = {nodeID};
  endif

  if (iscellstr (nodeID))
    if (! isscalar (nodeID))
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: NODEID must be a scalar node ", ...
              "identifier (char row vector, 1-element cellstr, or ", ...
              "scalar positive integer)"], method);
    endif
    if (! has_names)
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: NODEID is a node name but this ", ...
              "digraph has no node names"], method);
    endif
    match = find (strcmp (nn, nodeID{1}), 1);
    if (isempty (match))
      error ("Octave:invalid-input-arg", ...
             "digraph: %s: node name '%s' not found", method, nodeID{1});
    endif
    idx = match;
    by_name = true;
  elseif (isnumeric (nodeID) && isreal (nodeID))
    if (! isscalar (nodeID))
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: NODEID must be a scalar node ", ...
              "identifier (char row vector, 1-element cellstr, or ", ...
              "scalar positive integer)"], method);
    endif
    N = numnodes (G);
    if (! isfinite (nodeID) || nodeID < 1 || nodeID != fix (nodeID) ...
        || nodeID > N)
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: invalid node index %g (must be a ", ...
              "positive integer <= numnodes (G))"], method, nodeID);
    endif
    idx = double (nodeID);
    by_name = false;
  else
    error ("Octave:invalid-input-arg", ...
           ["digraph: %s: NODEID must be a numeric index or a ", ...
            "string node name"], method);
  endif

endfunction


## Private-helper smoke tests.  (Private helpers are not loaded from a
## plain script context by default, so these tests only run when the
## private directory is on the load path -- i.e. inside the BIST runs
## of scripts/graph/digraph.m or scripts/graph/graph.m.)
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [idx, by_name] = __resolve_single_node__ (G, 2, "successors");
%! assert (idx, 2);
%! assert (by_name, false);

%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, by_name] = __resolve_single_node__ (G, "b", "successors");
%! assert (idx, 2);
%! assert (by_name, true);

%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, by_name] = __resolve_single_node__ (G, {"c"}, "predecessors");
%! assert (idx, 3);
%! assert (by_name, true);

%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, by_name] = __resolve_single_node__ (G, 2, "successors");
%! assert (idx, 2);
%! assert (by_name, false);

%!error <invalid node index> ...
%! __resolve_single_node__ (digraph (3), 5, "successors")

%!error <invalid node index> ...
%! __resolve_single_node__ (digraph (3), 0, "successors")

%!error <invalid node index> ...
%! __resolve_single_node__ (digraph (3), 1.5, "successors")

%!error <scalar node identifier> ...
%! __resolve_single_node__ (digraph (3), [1 2], "successors")

%!error <not found> ...
%! __resolve_single_node__ (digraph ([1 2],[2 3],[],{"a","b","c"}), ...
%!                         "z", "successors")

%!error <no node names> ...
%! __resolve_single_node__ (digraph (3), "x", "successors")

%!error <scalar node identifier> ...
%! __resolve_single_node__ (digraph ([1 2],[2 3],[],{"a","b","c"}), ...
%!                         {"a","b"}, "successors")

%!error <numeric index or a string> ...
%! __resolve_single_node__ (digraph (3), true, "successors")
