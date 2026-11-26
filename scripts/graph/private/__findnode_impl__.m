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
## @deftypefn {} {@var{idx} =} __findnode_impl__ (@var{G}, @var{nodeID})
## Private helper shared by the @code{findnode} methods on both the
## @code{graph} and @code{digraph} classes.  Implements MATLAB's
## @code{findnode} semantics: numeric input is validated and returned
## with shape preserved; a char row vector is looked up as a single
## node name and returns a scalar (0 if not found); a cell array of
## character vectors is looked up element-by-element and returns a
## column vector of indices (0 for any missing name).
##
## Unlike @code{__resolve_single_node__} and @code{__resolve_node_list__},
## this helper does @strong{not} raise an error on an unrecognised name;
## missing names yield @code{0} to match MATLAB's findnode contract.
## Numeric validation still raises on out-of-range, non-integer,
## non-finite, or complex entries.
## @seealso{findnode, graph, digraph, __resolve_single_node__, __resolve_node_list__}
## @end deftypefn

function idx = __findnode_impl__ (G, nodeID)

  if (nargin != 2)
    print_usage ();
  endif

  N = numnodes (G);
  nn = G.Nodes.Name;
  has_names = ! isempty (nn);

  if (ischar (nodeID))
    ## Single node-name lookup.  MATLAB treats any char input as a
    ## single name regardless of row/column shape, but Octave-style
    ## input-validation tends to require a row vector; reject char
    ## matrices explicitly to avoid the "is it a list or a single
    ## name" ambiguity.
    if (! isempty (nodeID) && ! isrow (nodeID))
      error ("Octave:invalid-input-arg", ...
             ["findnode: nodeID must be a numeric array, a char ", ...
              "row vector, or a cell array of strings"]);
    endif
    if (isempty (nodeID) || ! has_names)
      idx = 0;
      return;
    endif
    match = find (strcmp (nn, nodeID), 1);
    if (isempty (match))
      idx = 0;
    else
      idx = double (match);
    endif

  elseif (iscell (nodeID))
    ## Cellstr lookup.  Reject cells containing non-strings up front
    ## (iscellstr returns false for any non-string element).
    if (! iscellstr (nodeID))
      error ("Octave:invalid-input-arg", ...
             ["findnode: nodeID must be a numeric array, a char ", ...
              "row vector, or a cell array of strings"]);
    endif
    ## Output is always a column vector regardless of input shape.
    names = nodeID(:);
    n = numel (names);
    idx = zeros (n, 1);
    if (has_names && n > 0)
      for ii = 1:n
        match = find (strcmp (nn, names{ii}), 1);
        if (! isempty (match))
          idx(ii) = double (match);
        endif
      endfor
    endif

  elseif (isnumeric (nodeID))
    if (! isreal (nodeID))
      error ("Octave:invalid-input-arg", ...
             ["findnode: nodeID must be a numeric array, a char ", ...
              "row vector, or a cell array of strings"]);
    endif
    if (isempty (nodeID))
      ## Preserve shape and return empty double.
      idx = double (nodeID);
      return;
    endif
    vals = double (nodeID);
    v = vals(:);
    if (any (! isfinite (v)) || any (v < 1) || any (v > N) ...
        || any (v != fix (v)))
      error ("Octave:invalid-input-arg", ...
             ["findnode: invalid node index (must be a positive ", ...
              "integer <= numnodes (G))"]);
    endif
    idx = vals;

  else
    error ("Octave:invalid-input-arg", ...
           ["findnode: nodeID must be a numeric array, a char ", ...
            "row vector, or a cell array of strings"]);
  endif

endfunction


## Private-helper smoke tests.  (Private helpers are not loaded from a
## plain script context by default, so these tests only run when the
## private directory is on the load path.)

## Numeric passthrough, scalar.
%!test
%! G = graph ([1 2], [2 3]);
%! assert (__findnode_impl__ (G, 2), 2);

## Numeric passthrough, row preserved.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! idx = __findnode_impl__ (G, [2 3]);
%! assert (idx, [2 3]);
%! assert (size (idx), [1, 2]);

## Char name lookup returns scalar, found case.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (__findnode_impl__ (G, "b"), 2);

## Char name lookup returns 0, missing case.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (__findnode_impl__ (G, "z"), 0);

## Char name lookup on graph with no names returns 0 (not an error).
%!test
%! G = graph (3);
%! assert (__findnode_impl__ (G, "foo"), 0);

## Cellstr lookup returns column vector.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! idx = __findnode_impl__ (G, {"a", "c"});
%! assert (size (idx), [2, 1]);
%! assert (idx, [1; 3]);

## Cellstr lookup with missing names fills zeros.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (__findnode_impl__ (G, {"a", "missing"}), [1; 0]);

## Digraph also supported.
%!test
%! G = digraph ([1 2], [2 3], [], {"x", "y", "z"});
%! assert (__findnode_impl__ (G, "y"), 2);

## Out-of-range numeric index errors.
%!error <invalid node index> ...
%! __findnode_impl__ (graph (3), 4)

## Non-integer numeric index errors.
%!error <invalid node index> ...
%! __findnode_impl__ (graph (3), 1.5)

## Logical (non-supported type) errors.
%!error <nodeID must be> ...
%! __findnode_impl__ (graph (3), true)

## Cell with non-string element errors.
%!error <nodeID must be> ...
%! __findnode_impl__ (graph (3), {1, 2})
