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
## @deftypefn  {} {@var{idx} =} findnode (@var{G}, @var{nodeID})
## Return the numeric node indices corresponding to @var{nodeID} in the
## graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.
##
## @var{nodeID} may be:
##
## @itemize
## @item
## A numeric array of positive integer node indices in the range
## @code{1:numnodes (@var{G})}.  In this case @code{findnode} validates
## each index and returns @var{nodeID} coerced to class @code{double},
## preserving its shape.  An out-of-range, non-integer, or non-finite
## entry raises an error.  This form is useful as a means of validating
## input passed to another function.
## @item
## A character row vector, interpreted as a single node name.  In this
## case @var{idx} is a scalar @code{double}: the 1-based node index, or
## @code{0} if no node in @var{G}.Nodes.Name matches the requested name.
## @item
## A cell array of character vectors, interpreted as a list of node
## names.  In this case @var{idx} is a @code{numel (nodeID)}-by-1
## @code{double} column vector.  Each entry is the 1-based node index,
## or @code{0} for any name that is not present in @var{G}.Nodes.Name.
## Note that the output is @emph{always} a column vector for the
## cellstr call form, regardless of the shape of @var{nodeID}.
## @end itemize
##
## When @var{G} has no node names (@code{G.Nodes.Name} is empty), any
## name lookup yields @code{0}; this is not an error.  Numeric lookups
## are validated against @code{numnodes (@var{G})} regardless of whether
## the graph has names.
##
## @example
## @group
## G = graph ([1 2 3], [2 3 1], [], @{"alpha", "beta", "gamma"@});
## findnode (G, "beta")           # @result{} 2
## findnode (G, @{"alpha", "zed"@}) # @result{} [1; 0]
## findnode (G, [3 1])            # @result{} [3 1]
## @end group
## @end example
##
## @seealso{graph, digraph, findedge, numnodes, neighbors}
## @end deftypefn

function idx = findnode (G, nodeID)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{findnode} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help findnode} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "findnode: G must be a graph or digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function route back to the class method via dot notation, which is
  ## always class-dispatched.
  idx = G.findnode (nodeID);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Numeric input: validate-and-return -------------

## Scalar numeric index returns the index unchanged.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (findnode (G, 2), 2);

## Numeric input returns class double.
%!test
%! G = graph ([1 2], [2 3]);
%! assert (class (findnode (G, 1)), "double");

## Integer types are coerced to double.
%!test
%! G = graph ([1 2], [2 3]);
%! idx = findnode (G, int32 (2));
%! assert (idx, 2);
%! assert (class (idx), "double");

## Row-vector numeric input preserves row shape.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! idx = findnode (G, [1 2 3]);
%! assert (size (idx), [1, 3]);
%! assert (idx, [1 2 3]);

## Column-vector numeric input preserves column shape.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! idx = findnode (G, [1; 2; 3]);
%! assert (size (idx), [3, 1]);
%! assert (idx, [1; 2; 3]);

## 2-D numeric nodeIDs matrix preserves shape.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! idx = findnode (G, [1 2; 3 4]);
%! assert (size (idx), [2, 2]);
%! assert (idx, [1 2; 3 4]);

## Empty numeric input returns empty double (shape preserved).
%!test
%! G = graph ([1 2], [2 3]);
%! idx = findnode (G, []);
%! assert (isempty (idx));
%! assert (class (idx), "double");

## Empty numeric row-vector preserves shape.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! idx = findnode (G, zeros (1, 0));
%! assert (size (idx), [1, 0]);

## Numeric validation works on digraph too.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (findnode (G, [3 1 2]), [3 1 2]);

## Numeric validation works without node names.
%!test
%! G = graph (5);
%! assert (findnode (G, [1 3 5]), [1 3 5]);

## Numeric validation on a graph with named nodes still works.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (findnode (G, 2), 2);

## ---------------- Char row vector: single name lookup ------------

## Single-name lookup returns scalar index.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! assert (findnode (G, "beta"), 2);

## Char name lookup returns class double scalar.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! idx = findnode (G, "alpha");
%! assert (class (idx), "double");
%! assert (isscalar (idx));

## Char-name lookup on first node.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! assert (findnode (G, "alpha"), 1);

## Char-name lookup on last node.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! assert (findnode (G, "gamma"), 3);

## Char-name lookup returns 0 when name is not found (MATLAB convention).
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! assert (findnode (G, "zed"), 0);

## Char-name lookup on graph with no names returns 0 (no error).
%!test
%! G = graph (3);
%! assert (findnode (G, "foo"), 0);

## Char-name lookup on empty graph (no nodes) returns 0.
%!test
%! G = graph ();
%! assert (findnode (G, "foo"), 0);

## Empty char vector returns 0.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (findnode (G, ""), 0);

## Char-name lookup works on digraph.
%!test
%! G = digraph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! assert (findnode (G, "gamma"), 3);

## Char-name with duplicate detection unnecessary: unique enforcement at
## construction time, so any name match is unambiguous.  Spot-check:
%!test
%! G = graph ([1 1], [2 3], [], {"x", "y", "z"});
%! assert (findnode (G, "x"), 1);
%! assert (findnode (G, "y"), 2);

## ---------------- Cellstr input: vectorized name lookup ----------

## Cellstr lookup returns column vector.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! idx = findnode (G, {"alpha", "beta", "gamma"});
%! assert (size (idx), [3, 1]);
%! assert (idx, [1; 2; 3]);

## Cellstr lookup always returns column vector regardless of input
## shape (MATLAB convention).
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! idx = findnode (G, {"alpha"; "beta"; "gamma"});
%! assert (size (idx), [3, 1]);
%! assert (idx, [1; 2; 3]);

## 1-element cellstr returns 1-by-1 column.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! idx = findnode (G, {"beta"});
%! assert (size (idx), [1, 1]);
%! assert (idx, 2);

## Cellstr lookup puts 0 for any missing name.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! idx = findnode (G, {"alpha", "missing", "gamma"});
%! assert (idx, [1; 0; 3]);

## Cellstr lookup with all missing names returns zeros column.
%!test
%! G = graph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! idx = findnode (G, {"p", "q", "r"});
%! assert (idx, [0; 0; 0]);

## Cellstr lookup on a graph with no names returns zeros.
%!test
%! G = graph (3);
%! idx = findnode (G, {"a", "b"});
%! assert (idx, [0; 0]);

## Cellstr lookup returns class double.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! idx = findnode (G, {"a", "b"});
%! assert (class (idx), "double");

## Empty cellstr returns zeros(0, 1) (column shape).
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! idx = findnode (G, cell (0, 0));
%! assert (size (idx), [0, 1]);
%! assert (class (idx), "double");

## Cellstr lookup works on digraph.
%!test
%! G = digraph ([1 2], [2 3], [], {"alpha", "beta", "gamma"});
%! idx = findnode (G, {"alpha", "gamma"});
%! assert (idx, [1; 3]);

## 2-D cellstr gets flattened to a column in index order.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! ## {"a","b";"c","a"} column-major order: "a","c","b","a" -> [1 3 2 1]
%! idx = findnode (G, {"a", "b"; "c", "a"});
%! assert (size (idx), [4, 1]);
%! assert (idx, [1; 3; 2; 1]);

## ---------------- Error: invalid numeric index -------------------

## Error: numeric index out of range (too large).
%!error <invalid node index> findnode (graph (3), 4)

## Error: numeric index out of range (zero).
%!error <invalid node index> findnode (graph (3), 0)

## Error: non-integer numeric index.
%!error <invalid node index> findnode (graph (3), 1.5)

## Error: NaN as numeric index.
%!error <invalid node index> findnode (graph (3), NaN)

## Error: Inf as numeric index.
%!error <invalid node index> findnode (graph (3), Inf)

## Error: negative numeric index.
%!error <invalid node index> findnode (graph (3), -1)

## Error: numeric vector with any out-of-range entry.
%!error <invalid node index> findnode (graph (3), [1 2 99])

## Error: numeric vector with any non-integer entry.
%!error <invalid node index> findnode (graph (5), [1 2.5 3])

## Error: complex numeric input.
%!error <numeric.*real|nodeID must be> findnode (graph (3), 1 + 2i)

## ---------------- Error: unsupported input types -----------------

## Error: logical input (not a supported nodeID type).
%!error <nodeID must be> findnode (graph (3), true)

## Error: struct input.
%!error <nodeID must be> findnode (graph (3), struct ())

## Error: cell of numbers (not a cellstr).
%!error <nodeID must be> findnode (graph (3), {1, 2, 3})

## Error: non-row char matrix.
%!error <nodeID must be> findnode (graph (3), ["ab"; "cd"])

## ---------------- Error: guard on first argument -----------------

## Error: non-graph first argument routes through the free-function
## guard.
%!error <G must be a graph or digraph> findnode (3, 1)
%!error <G must be a graph or digraph> findnode ("hello", 1)
%!error <G must be a graph or digraph> findnode (struct (), 1)

## Error: nargin mismatch.
%!error <Invalid call> findnode ()
%!error <Invalid call> findnode (graph (3))
