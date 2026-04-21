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
## @deftypefn {} {@var{P} =} isomorphism (@var{G1}, @var{G2})
## Return an isomorphism between the graphs @var{G1} and @var{G2}, if
## one exists.
##
## @var{P} is a column permutation vector of length @code{numnodes
## (@var{G2})} such that @code{reordernodes (@var{G2}, @var{P})} has
## the same structure (adjacency, self-loops, and for multigraphs the
## edge multiplicities) as @var{G1}.  Equivalently, @var{P} satisfies
## @code{adjacency (@var{G2})(@var{P}, @var{P}) == adjacency
## (@var{G1})}.  Node names and edge weights are not considered; only
## the underlying graph structure is compared.
##
## When no isomorphism exists, @var{P} is returned as the empty matrix
## @code{[]}.  When both graphs are empty, @var{P} is the empty column
## @code{zeros (0, 1)}.
##
## @var{G1} and @var{G2} must both be @code{graph} objects or both be
## @code{digraph} objects; mixing the two classes is an error.  The
## search uses the VF2 algorithm (Cordella, Foggia, Sansone, and
## Vento, 2004) --- the same backtracking engine as
## @code{isisomorphic}.  Non-isomorphic graphs are rejected by the
## degree-based quick reject when possible and otherwise by the VF2
## feasibility-pruned search.
##
## @example
## @group
## G1 = digraph ([1 2 3], [2 3 1]);            # directed 3-cycle
## perm = [3 1 2];
## G2 = reordernodes (G1, perm);                # same cycle relabelled
## P = isomorphism (G1, G2);
## isequal (full (adjacency (G2)(P, P)), full (adjacency (G1)))
##                                              # @result{} true
##
## H1 = graph ([1 2 3 4], [2 3 4 1]);          # C_4
## H2 = graph ([1 2 3], [2 3 4]);              # P_4
## isomorphism (H1, H2)                         # @result{} [] (not iso)
## @end group
## @end example
##
## @seealso{graph, digraph, isisomorphic, reordernodes}
## @end deftypefn

function P = isomorphism (G1, G2)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{isomorphism} method
  ## and this free-function body is not reached.  This file exists both
  ## as a canonical documentation target (so @code{help isomorphism}
  ## works outside the context of an instance) and as a fallback that
  ## gives a helpful error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G1, "graph") && ! isa (G1, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "isomorphism: G1 must be a graph or digraph object");
  endif

  if (! isa (G2, "graph") && ! isa (G2, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "isomorphism: G2 must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but route through dot notation
  ## in case a future subclassing edge case skips the free function.
  P = G1.isomorphism (G2);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Digraph: isomorphic cases ----------------------

## Two identical 3-cycles -> identity permutation.
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3], [2 3 1]);
%! P = isomorphism (G1, G2);
%! assert (size (P), [3, 1]);
%! assert (class (P), "double");
%! ## Roundtrip check: reordering G2 by P yields G1's adjacency.
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## 3-cycle relabelled via reordernodes -> returned P must invert the
## relabelling so that A2(P, P) == A1.
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! perm = [3 1 2];
%! G2 = reordernodes (G1, perm);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (size (P), [3, 1]);
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Directed path 1->2->3 vs same path with nodes [3 1 2].
%!test
%! G1 = digraph ([1 2], [2 3]);
%! perm = [3 1 2];
%! G2 = reordernodes (G1, perm);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Self-loop at node 1 vs self-loop at node 2 -> must match that one
## specific node so P places the self-loop node at position 1.
%!test
%! G1 = digraph (1, 1);
%! G1 = addnode (G1, 1);
%! G2 = digraph (2, 2);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Empty digraphs -> zeros(0,1) permutation.
%!test
%! G1 = digraph ();
%! G2 = digraph ();
%! P = isomorphism (G1, G2);
%! assert (P, zeros (0, 1));

## Edgeless N=4 digraphs -> some valid permutation of length 4.
%!test
%! G1 = digraph (4);
%! G2 = digraph (4);
%! P = isomorphism (G1, G2);
%! assert (size (P), [4, 1]);
%! assert (sort (P), (1:4).');
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Single-node digraph.
%!test
%! G1 = digraph (1);
%! G2 = digraph (1);
%! P = isomorphism (G1, G2);
%! assert (P, 1);

## Weights differ but structure matches -> isomorphism ignores weights.
%!test
%! G1 = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! G2 = digraph ([1 2 3], [2 3 1], [1 1 1]);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (spones (adjacency (G2)(P, P))), ...
%!         full (spones (adjacency (G1))));

## Node names differ but structure matches -> isomorphism ignores names.
%!test
%! G1 = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! G2 = digraph ([1 2 3], [2 3 1], [], {"x", "y", "z"});
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Siever-like 9-node digraph vs itself with a deep permutation.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G1 = digraph (s, t);
%! perm = [5 2 1 8 9 3 4 7 6];
%! G2 = reordernodes (G1, perm);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (size (P), [9, 1]);
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## ---------------- Digraph: non-isomorphic cases ------------------

## Different node counts -> [].
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3 4], [2 3 4 1]);
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## Same N and M but different structure -> [].
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);      # 3-cycle
%! G2 = digraph ([1 2 3], [3 3 1]);      # not a cycle
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## Different edge counts -> [].
%!test
%! G1 = digraph ([1 2 3 4], [2 3 4 1]);
%! G2 = digraph ([1 2 3], [2 3 4]);
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## Self-loop present on one side only -> [].
%!test
%! G1 = digraph ([1 2 3], [2 3 3]);      # has 3->3 self-loop
%! G2 = digraph ([1 2 3], [2 3 1]);      # no self-loop
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## ---------------- Graph (undirected): isomorphic -----------------

## Triangle with identity mapping.
%!test
%! G1 = graph ([1 2 3], [2 3 1]);
%! G2 = graph ([1 2 3], [2 3 1]);
%! P = isomorphism (G1, G2);
%! assert (size (P), [3, 1]);
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Triangle relabelled.
%!test
%! G1 = graph ([1 2 3], [2 3 1]);
%! G2 = graph ([2 3 1], [3 1 2]);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## K_4 permuted -> must return a valid permutation.
%!test
%! G1 = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! perm = [3 1 4 2];
%! G2 = reordernodes (G1, perm);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (size (P), [4, 1]);
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Path P_4 vs its reversal -> isomorphic, specific P reverses order.
%!test
%! G1 = graph ([1 2 3], [2 3 4]);
%! G2 = graph ([4 3 2], [3 2 1]);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## Empty undirected graphs -> zeros(0,1).
%!test
%! G1 = graph ();
%! G2 = graph ();
%! P = isomorphism (G1, G2);
%! assert (P, zeros (0, 1));

## C_4 permuted.
%!test
%! G1 = graph ([1 2 3 4], [2 3 4 1]);
%! perm = [3 1 4 2];
%! G2 = reordernodes (G1, perm);
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## ---------------- Graph (undirected): non-isomorphic --------------

## K_{3,3} vs triangular prism -> [].
%!test
%! G1 = graph ([1 1 1 2 2 2 3 3 3], [4 5 6 4 5 6 4 5 6]);
%! G2 = graph ([1 2 3 4 5 6 1 2 3], [2 3 1 5 6 4 4 5 6]);
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## 4-cycle vs path P_4 -> [].
%!test
%! G1 = graph ([1 2 3 4], [2 3 4 1]);
%! G2 = graph ([1 2 3], [2 3 4]);
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## Star K_{1,4} vs path P_5 -> different degree sequences -> [].
%!test
%! G1 = graph ([1 1 1 1], [2 3 4 5]);
%! G2 = graph ([1 2 3 4], [2 3 4 5]);
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## Self-loop on one side only -> [].
%!test
%! G1 = graph ([1 1 2], [1 2 3]);
%! G2 = graph ([1 2], [2 3]);
%! P = isomorphism (G1, G2);
%! assert (isequal (P, []));

## ---------------- Dot-notation dispatch ---------------------------

## Call as a method: G1.isomorphism (G2).
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3], [2 3 1]);
%! P = G1.isomorphism (G2);
%! assert (size (P), [3, 1]);
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

%!test
%! G1 = graph ([1 2 3], [2 3 1]);
%! G2 = graph ([1 2 3], [2 3 1]);
%! P = G1.isomorphism (G2);
%! assert (size (P), [3, 1]);
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## ---------------- Named graphs round-trip through P --------------

## Named digraph with nontrivial relabelling: the returned P is a
## permutation of node indices (ignoring names) such that G2's
## adjacency reordered by P matches G1's adjacency.
%!test
%! s1 = {"a", "b", "c"};
%! t1 = {"b", "c", "a"};
%! s2 = {"x", "y", "z"};
%! t2 = {"y", "z", "x"};
%! G1 = digraph (s1, t1, [], {"a", "b", "c"});
%! G2 = digraph (s2, t2, [], {"x", "y", "z"});
%! P = isomorphism (G1, G2);
%! assert (! isempty (P));
%! assert (full (adjacency (G2)(P, P)), full (adjacency (G1)));

## ---------------- Cross-class and error cases --------------------

## graph vs digraph: different classes -> error.
%!error <same class>
%! G1 = graph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3], [2 3 1]);
%! isomorphism (G1, G2);

## Non-graph G1 -> error caught by the free function.
%!error <must be a graph or digraph>
%! isomorphism (5, digraph ());

## Non-graph G2 -> error caught by the classdef method (same-class
## check on the graph/digraph side).
%!error <same class>
%! isomorphism (digraph (), "abc");

## Wrong number of arguments -> print_usage error.
%!error isomorphism (digraph ())
%!error isomorphism ()
