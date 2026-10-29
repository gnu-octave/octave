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
## @deftypefn  {} {@var{endpoints} =} findedge (@var{G})
## @deftypefnx {} {[@var{sOut}, @var{tOut}] =} findedge (@var{G})
## @deftypefnx {} {@var{idx} =} findedge (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {@var{endpoints} =} findedge (@var{G}, @var{edgeIdx})
## @deftypefnx {} {[@var{sOut}, @var{tOut}] =} findedge (@var{G}, @var{edgeIdx})
## Look up edges of a graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  There are
## three supported call forms:
##
## @itemize
## @item
## @code{findedge (@var{G})}: return an @code{m}-by-2 matrix @var{endpoints}
## whose rows are the @code{[source, destination]} pairs of every edge of
## @var{G} in the edge order used by @code{G.Edges} (lexicographic with
## source-major ordering; for an undirected @code{graph} the smaller
## endpoint is in column 1).  When two output arguments are requested
## (@code{[@var{sOut}, @var{tOut}] = findedge (@var{G})}) the source and
## destination columns are returned separately as @code{m}-by-1 column
## vectors.
## @item
## @code{findedge (@var{G}, @var{s}, @var{t})}: look up edge indices for
## one or more @code{(source, destination)} pairs.  @var{s} and @var{t}
## must have the same number of elements and may be numeric node indices,
## a char row vector (single node name), or a cell array of strings.
## Each entry @code{@var{idx}(i)} is the 1-based index of the edge from
## @code{@var{s}(i)} to @code{@var{t}(i)} in @code{G.Edges}, or
## @code{0} if no such edge exists.  For an undirected @code{graph}, the
## pair is matched in either orientation.  For a multigraph with
## parallel edges, the @emph{first} matching edge index is returned;
## see @code{edgecount} for multiplicity.  The result is a scalar when
## @var{s} and @var{t} are scalars, and a column vector otherwise.  A
## name that is not present in @code{@var{G}.Nodes.Name} is not an
## error; the corresponding @var{idx} entry is simply @code{0} (this
## matches the @code{findnode} convention).
## @item
## @code{findedge (@var{G}, @var{edgeIdx})}: return the endpoints of the
## edges identified by the positive integer indices in @var{edgeIdx}.
## The result is an @code{m}-by-2 numeric matrix; with two output
## arguments the source and destination columns are returned separately.
## @end itemize
##
## @example
## @group
## G = graph ([1 2 3], [2 3 1]);
## findedge (G)                # @result{} [1 2; 1 3; 2 3]
## findedge (G, 2, 3)          # @result{} 3
## findedge (G, [1 2], [3 3])  # @result{} [2; 3]
## findedge (G, [1; 3])        # @result{} [1 2; 2 3]
## @end group
## @end example
##
## @seealso{graph, digraph, findnode, numedges, edgecount}
## @end deftypefn

function varargout = findedge (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{findedge} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help findedge} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "findedge: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but we route through dot
  ## notation to be safe.
  if (nargout <= 1)
    varargout{1} = G.findedge (varargin{:});
  else
    [varargout{1:nargout}] = G.findedge (varargin{:});
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Form 1: findedge (G) ---------------------------

## Empty graph returns 0-by-2 endpoints matrix.
%!test
%! G = graph ();
%! E = findedge (G);
%! assert (size (E), [0, 2]);
%! assert (class (E), "double");

## Empty digraph returns 0-by-2 endpoints matrix.
%!test
%! G = digraph ();
%! E = findedge (G);
%! assert (size (E), [0, 2]);

## Edgeless N-node graph returns 0-by-2.
%!test
%! G = graph (5);
%! E = findedge (G);
%! assert (size (E), [0, 2]);

## Edgeless N-node digraph returns 0-by-2.
%!test
%! G = digraph (4);
%! E = findedge (G);
%! assert (size (E), [0, 2]);

## Simple graph 3-cycle: lex-sorted undirected endpoints.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! E = findedge (G);
%! assert (E, [1 2; 1 3; 2 3]);

## Simple digraph 3-cycle: lex-sorted directed endpoints.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G);
%! assert (E, [1 2; 2 3; 3 1]);

## Result class is double even with logical-adjacency constructor.
%!test
%! G = graph (logical ([0 1; 1 0]));
%! E = findedge (G);
%! assert (class (E), "double");
%! assert (E, [1 2]);

## Two-output form returns column vectors for graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [s, t] = findedge (G);
%! assert (size (s), [3, 1]);
%! assert (size (t), [3, 1]);
%! assert (s, [1; 1; 2]);
%! assert (t, [2; 3; 3]);

## Two-output form returns column vectors for digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [s, t] = findedge (G);
%! assert (s, [1; 2; 3]);
%! assert (t, [2; 3; 1]);

## Two-output empty graph: 0-by-1 column vectors.
%!test
%! G = graph ();
%! [s, t] = findedge (G);
%! assert (size (s), [0, 1]);
%! assert (size (t), [0, 1]);

## Multigraph digraph: parallel edges all listed.
%!test
%! G = digraph ([1 1 2], [2 2 3], 'multigraph');
%! E = findedge (G);
%! assert (size (E), [3, 2]);
%! assert (E, [1 2; 1 2; 2 3]);

## Graph with self-loop: the self-loop is listed as (n, n).
%!test
%! G = graph ([1 2 3], [2 3 3]);
%! E = findedge (G);
%! assert (E, [1 2; 2 3; 3 3]);

## Digraph with self-loop: the self-loop is listed as (n, n).
%!test
%! G = digraph ([1 2 3], [2 3 3]);
%! E = findedge (G);
%! assert (E, [1 2; 2 3; 3 3]);

## Graph edges match G.Edges.EndNodes exactly.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! assert (findedge (G), G.Edges.EndNodes);

## Digraph edges match G.Edges.EndNodes exactly.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! assert (findedge (G), G.Edges.EndNodes);

## ---------------- Form 2: findedge (G, s, t) ---------------------

## Scalar (s, t) lookup returns scalar index.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! idx = findedge (G, 2, 3);
%! assert (idx, 2);
%! assert (isscalar (idx));

## Scalar index is class double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (class (findedge (G, 1, 2)), "double");

## Scalar lookup for missing edge returns 0.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (findedge (G, 1, 3), 0);

## Scalar lookup for reverse pair in digraph returns 0.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (findedge (G, 2, 1), 0);

## Graph (undirected): reverse pair matches the same edge.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (findedge (G, 1, 2), findedge (G, 2, 1));

## Graph matches the (min, max) canonical form.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (findedge (G, 3, 1), findedge (G, 1, 3));

## Vector (s, t) lookup returns column of indices.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! idx = findedge (G, [1 2 3], [2 3 1]);
%! assert (size (idx), [3, 1]);
%! assert (idx, [1; 2; 3]);

## Vector lookup with missing edge returns 0 for that slot.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! idx = findedge (G, [1 1 2], [2 3 3]);
%! assert (idx, [1; 0; 2]);

## Column-vector inputs accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! idx = findedge (G, [1; 2; 3], [2; 3; 1]);
%! assert (size (idx), [3, 1]);
%! assert (idx, [1; 2; 3]);

## Mixed row/column vector inputs work (length only).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! idx = findedge (G, [1 2 3], [2; 3; 1]);
%! assert (idx, [1; 2; 3]);

## Empty (s, t) input returns zeros(0, 1).
%!test
%! G = digraph ([1 2], [2 3]);
%! idx = findedge (G, [], []);
%! assert (size (idx), [0, 1]);
%! assert (class (idx), "double");

## String endpoints on named graph (char row vectors).
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! assert (findedge (G, "alpha", "beta"), 1);

## String endpoint missing-name returns 0.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! assert (findedge (G, "alpha", "zed"), 0);

## Cellstr endpoints on named graph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! idx = findedge (G, {"alpha", "beta", "gamma"}, {"beta", "gamma", "alpha"});
%! assert (idx, [1; 2; 3]);

## Cellstr endpoints with missing names give 0 per pair.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! idx = findedge (G, {"alpha", "alpha"}, {"beta", "zed"});
%! assert (idx, [1; 0]);

## Named graph (undirected): string endpoints match either orientation.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (findedge (G, "b", "a"), findedge (G, "a", "b"));

## Self-loop lookup: graph.
%!test
%! G = graph ([1 2 3], [2 3 3]);
%! assert (findedge (G, 3, 3), 3);

## Self-loop lookup: digraph.
%!test
%! G = digraph ([1 2 3], [2 3 3]);
%! assert (findedge (G, 3, 3), 3);

## Multigraph: findedge returns the first matching edge index.
%!test
%! G = digraph ([1 1 2], [2 2 3], 'multigraph');
%! ## Edges (lex): (1,2) at idx 1, (1,2) at idx 2, (2,3) at idx 3.
%! assert (findedge (G, 1, 2), 1);
%! assert (findedge (G, 2, 3), 3);

## Multigraph vector lookup: parallel pair returns first index.
%!test
%! G = digraph ([1 1 2 2], [2 2 3 3], 'multigraph');
%! idx = findedge (G, [1 2], [2 3]);
%! assert (idx, [1; 3]);

## Edge-list in user-provided order doesn't affect edge indices (lex sorted).
%!test
%! G = digraph ([3 1 2], [1 2 3]);
%! ## G.Edges.EndNodes in lex order: (1,2), (2,3), (3,1).
%! assert (findedge (G, 2, 3), 2);
%! assert (findedge (G, 3, 1), 3);

## Unnamed graph works with numeric-only input.
%!test
%! G = graph (5);
%! idx = findedge (G, 1, 2);
%! assert (idx, 0);

## 2-D matrix inputs for s and t: flattened in column-major order.
## (The result is a column vector of length numel(s).)
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! idx = findedge (G, [1 2; 3 4], [2 3; 4 1]);
%! assert (idx, [1; 3; 2; 4]);

## String and numeric mixing: second arg numeric, first arg string.
## (Allowed in MATLAB if both resolve to valid indices.)
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! idx = findedge (G, "a", 2);
%! assert (idx, 1);

## Integer class inputs coerce to double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! idx = findedge (G, int32 (1), int32 (2));
%! assert (idx, 1);
%! assert (class (idx), "double");

## ---------------- Form 3: findedge (G, edgeIdx) ------------------

## Scalar edge index returns 1-by-2 endpoints row.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G, 1);
%! assert (size (E), [1, 2]);
%! assert (E, [1 2]);

## Vector edge indices: row-vector input preserved as rows in m-by-2.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G, [1 2 3]);
%! assert (size (E), [3, 2]);
%! assert (E, [1 2; 2 3; 3 1]);

## Vector edge indices: column-vector input.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G, [1; 2; 3]);
%! assert (E, [1 2; 2 3; 3 1]);

## Edge-index form on graph: returns canonical (min, max) pair.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! E = findedge (G, [1 2 3]);
%! assert (E, [1 2; 1 3; 2 3]);

## Two-output edgeIdx form returns column vectors.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [s, t] = findedge (G, [1 2]);
%! assert (size (s), [2, 1]);
%! assert (size (t), [2, 1]);
%! assert (s, [1; 2]);
%! assert (t, [2; 3]);

## Permuted edgeIdx gives permuted endpoints.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G, [3 1 2]);
%! assert (E, [3 1; 1 2; 2 3]);

## Empty edgeIdx returns 0-by-2.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G, []);
%! assert (size (E), [0, 2]);

## Integer class index coerced.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G, int32 (2));
%! assert (E, [2 3]);

## Result class is double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = findedge (G, 1);
%! assert (class (E), "double");

## Self-loop shows up correctly by edge index.
%!test
%! G = digraph ([1 2 3], [2 3 3]);
%! E = findedge (G, 3);
%! assert (E, [3 3]);

## Multigraph: edgeIdx form selects the right parallel.
%!test
%! G = digraph ([1 1 2], [2 2 3], 'multigraph');
%! E = findedge (G, [1 2 3]);
%! assert (E, [1 2; 1 2; 2 3]);

## Edge-index form on a 2-D index matrix: flattened column-major.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! E = findedge (G, [1 2; 3 4]);
%! assert (size (E), [4, 2]);
%! assert (E, [1 2; 3 4; 2 3; 4 1]);

## ---------------- Errors: invalid inputs -------------------------

## Error: s and t length mismatch.
%!error <length> findedge (digraph ([1 2], [2 3]), [1 2], [2])

## Non-existent name in (s, t) lookup yields 0 (MATLAB findnode-style
## semantics: missing names are NOT an error, they propagate to a 0
## edge-index result).
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (findedge (G, "zed", "a"), 0);
%! assert (findedge (G, "a", "zed"), 0);

## Error: out-of-range numeric endpoint.
%!error <invalid node|exceed|out of range> ...
%!   findedge (digraph ([1 2], [2 3]), 1, 99)

## Error: zero-valued numeric endpoint.
%!error <invalid node|positive|out of range> ...
%!   findedge (digraph ([1 2], [2 3]), 0, 1)

## Error: non-integer numeric endpoint.
%!error <invalid node|integer> findedge (digraph ([1 2], [2 3]), 1, 1.5)

## Error: NaN as numeric endpoint.
%!error <invalid node|finite|NaN> findedge (digraph ([1 2], [2 3]), 1, NaN)

## Error: edgeIdx out of range (too large).
%!error <invalid edge|exceed|out of range> ...
%!   findedge (digraph ([1 2], [2 3]), 99)

## Error: edgeIdx zero.
%!error <invalid edge|positive|out of range> ...
%!   findedge (digraph ([1 2], [2 3]), 0)

## Error: edgeIdx non-integer.
%!error <invalid edge|integer> findedge (digraph ([1 2], [2 3]), 1.5)

## Error: edgeIdx NaN.
%!error <invalid edge|finite|NaN> findedge (digraph ([1 2], [2 3]), NaN)

## Error: edgeIdx negative.
%!error <invalid edge|positive|out of range> ...
%!   findedge (digraph ([1 2], [2 3]), -1)

## Name lookup on unnamed graph yields 0 (not an error, same
## rationale as findnode's behavior on a nameless graph).
%!test
%! assert (findedge (digraph (3), "a", "b"), 0);

## Error: non-graph first arg routes through the free-function guard.
%!error <G must be a graph or digraph> findedge (3, 1, 2)
%!error <G must be a graph or digraph> findedge ("hello", 1, 2)

## Error: nargin mismatch.
%!error <Invalid call> findedge ()
%!error <Invalid call> findedge (digraph (3), 1, 2, 3)
