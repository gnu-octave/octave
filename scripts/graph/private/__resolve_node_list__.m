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
## @deftypefn {} {[@var{idx}, @var{out_shape}] =} __resolve_node_list__ (@var{G}, @var{nodeIDs}, @var{method})
## Private helper for @code{digraph} and @code{graph} methods that take
## a possibly-vector node identifier argument (such as @code{indegree}
## or @code{outdegree}).
##
## Resolve @var{nodeIDs} into a column vector of positive integer node
## indices @var{idx} in the range @code{1:numnodes (@var{G})}.
## @var{nodeIDs} may be:
##
## @itemize
## @item
## a real numeric array of positive integer indices (any shape);
## @item
## a character row vector, interpreted as a single node name (yielding
## @var{out_shape} @code{[1 1]});
## @item
## a cell array of character vectors (any shape), each interpreted as
## a node name;
## @item
## an empty array (@code{[]} or @code{@{@}}), producing an empty
## @var{idx} and @var{out_shape} equal to @code{size (@var{nodeIDs})}.
## @end itemize
##
## @var{out_shape} is the size of the caller's original @var{nodeIDs}
## argument.  Callers use @code{reshape (result(idx), out_shape)} to
## produce a result of the same shape as the input.
##
## @var{method} is a short string identifying the caller, included in
## error messages for easier diagnosis (typically the Octave method
## name, e.g. @qcode{"indegree"} or @qcode{"outdegree"}).
##
## @seealso{digraph, graph, indegree, outdegree, __resolve_single_node__}
## @end deftypefn

function [idx, out_shape] = __resolve_node_list__ (G, nodeIDs, method)

  if (nargin != 3)
    print_usage ();
  endif

  nn = G.Nodes.Name;
  has_names = ! isempty (nn);
  N = numnodes (G);

  ## A bare char input is a single node name.
  if (ischar (nodeIDs))
    if (! isrow (nodeIDs) && ! isempty (nodeIDs))
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: NODEIDS char argument must be a row ", ...
              "vector (a single node name)"], method);
    endif
    nodeIDs = {nodeIDs};
    out_shape = [1, 1];
  else
    out_shape = size (nodeIDs);
  endif

  if (iscell (nodeIDs))
    if (isempty (nodeIDs))
      idx = zeros (0, 1);
      return;
    endif
    if (! iscellstr (nodeIDs))
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: NODEIDS cell input must contain only ", ...
              "character vectors"], method);
    endif
    if (! has_names)
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: NODEIDS contain node names but this ", ...
              "digraph has no node names"], method);
    endif
    n = numel (nodeIDs);
    idx = zeros (n, 1);
    for k = 1:n
      match = find (strcmp (nn, nodeIDs{k}), 1);
      if (isempty (match))
        error ("Octave:invalid-input-arg", ...
               "digraph: %s: node name '%s' not found", ...
               method, nodeIDs{k});
      endif
      idx(k) = match;
    endfor
  elseif (isnumeric (nodeIDs) && isreal (nodeIDs))
    if (isempty (nodeIDs))
      idx = zeros (0, 1);
      return;
    endif
    v = double (nodeIDs(:));
    if (any (! isfinite (v)) || any (v < 1) ...
        || any (v != fix (v)) || any (v > N))
      error ("Octave:invalid-input-arg", ...
             ["digraph: %s: invalid node index (NODEIDS must be ", ...
              "positive integers in the range 1:numnodes (G))"], method);
    endif
    idx = v;
  else
    error ("Octave:invalid-input-arg", ...
           ["digraph: %s: NODEIDS must be a numeric index array, a ", ...
            "character row vector, or a cell array of node names"], method);
  endif

endfunction


## ------------------------------------------------------------------
## Smoke tests for the private helper.  Private helpers are only loaded
## when scripts/graph/private is on the path, so these run as part of
## scripts/graph/digraph.m's BIST harness (which puts the private dir on
## the path via the module.mk wiring).
## ------------------------------------------------------------------

## Numeric scalar.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [idx, sh] = __resolve_node_list__ (G, 2, "indegree");
%! assert (idx, 2);
%! assert (sh, [1, 1]);

## Numeric row vector preserves row shape.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [idx, sh] = __resolve_node_list__ (G, [1 3], "indegree");
%! assert (idx, [1; 3]);
%! assert (sh, [1, 2]);

## Numeric column vector preserves column shape.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [idx, sh] = __resolve_node_list__ (G, [1; 3], "indegree");
%! assert (idx, [1; 3]);
%! assert (sh, [2, 1]);

## 2-D numeric matrix preserves matrix shape.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [idx, sh] = __resolve_node_list__ (G, [1 2; 3 1], "indegree");
%! assert (idx, [1; 3; 2; 1]);
%! assert (sh, [2, 2]);

## Empty numeric.
%!test
%! G = digraph (5);
%! [idx, sh] = __resolve_node_list__ (G, [], "indegree");
%! assert (size (idx), [0, 1]);
%! assert (sh, [0, 0]);

## Char row vector -> scalar.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, sh] = __resolve_node_list__ (G, "b", "indegree");
%! assert (idx, 2);
%! assert (sh, [1, 1]);

## 1-element cellstr -> scalar shape [1 1].
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, sh] = __resolve_node_list__ (G, {"c"}, "indegree");
%! assert (idx, 3);
%! assert (sh, [1, 1]);

## Cellstr row preserves row shape.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, sh] = __resolve_node_list__ (G, {"a", "c"}, "indegree");
%! assert (idx, [1; 3]);
%! assert (sh, [1, 2]);

## Cellstr column preserves column shape.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, sh] = __resolve_node_list__ (G, {"a"; "c"}, "indegree");
%! assert (idx, [1; 3]);
%! assert (sh, [2, 1]);

## Empty cell preserves shape [0 0].
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [idx, sh] = __resolve_node_list__ (G, {}, "indegree");
%! assert (size (idx), [0, 1]);
%! assert (sh, [0, 0]);

## Error: out-of-range index.
%!error <invalid node index> ...
%! __resolve_node_list__ (digraph (3), 4, "indegree")

## Error: zero index.
%!error <invalid node index> ...
%! __resolve_node_list__ (digraph (3), 0, "indegree")

## Error: non-integer index.
%!error <invalid node index> ...
%! __resolve_node_list__ (digraph (3), 1.5, "indegree")

## Error: one bad entry in a vector.
%!error <invalid node index> ...
%! __resolve_node_list__ (digraph (3), [1 2 4], "indegree")

## Error: Inf index.
%!error <invalid node index> ...
%! __resolve_node_list__ (digraph (3), Inf, "indegree")

## Error: name not found.
%!error <not found> ...
%! __resolve_node_list__ (digraph ([1 2], [2 3], [], {"a","b","c"}), ...
%!                       "z", "indegree")

## Error: name given but no node names on this digraph.
%!error <no node names> ...
%! __resolve_node_list__ (digraph (3), "x", "indegree")

## Error: unsupported type (logical).
%!error <numeric index array> ...
%! __resolve_node_list__ (digraph (3), true, "indegree")

## Error: cell containing non-strings.
%!error <character vectors> ...
%! __resolve_node_list__ (digraph ([1 2], [2 3], [], {"a","b","c"}), ...
%!                       {1, 2}, "indegree")
