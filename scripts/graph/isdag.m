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
## @deftypefn {} {@var{tf} =} isdag (@var{G})
## Return @code{true} if the digraph @var{G} is a directed acyclic
## graph (DAG), and @code{false} otherwise.
##
## @var{G} must be a @code{digraph} object.  Acyclicity is not defined
## for the undirected @code{graph} class; calling @code{isdag} on a
## @code{graph} raises an error.
##
## The result @var{tf} is a scalar logical.  @code{true} means every
## directed walk in @var{G} eventually terminates; equivalently, the
## edges of @var{G} admit a valid topological ordering (see
## @code{toposort}).  Any cycle makes @var{G} non-acyclic, including a
## self-loop @code{n -> n}.  The empty digraph (@code{numnodes (@var{G})
## == 0}) and any edgeless digraph are DAGs.
##
## @example
## @group
## G1 = digraph ([1 2 3], [2 3 4]);
## isdag (G1)                     # @result{} true
## G2 = digraph ([1 2 3], [2 3 1]);
## isdag (G2)                     # @result{} false (3-cycle)
## G3 = digraph (1, 1);
## isdag (G3)                     # @result{} false (self-loop)
## @end group
## @end example
##
## @seealso{digraph, toposort, conncomp, condensation}
## @end deftypefn

function tf = isdag (G)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal @code{isdag}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so
  ## @code{help isdag} works outside the context of an instance) and
  ## as a fallback that gives a helpful error for non-digraph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "isdag: not defined for an undirected graph; isdag requires a digraph");
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "isdag: G must be a digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a digraph first arg, but route through dot notation
  ## just in case.
  tf = G.isdag ();

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## isdag on an undirected graph is an error.
%!error <not defined for an undirected graph>
%! G = graph ([1 2], [2 3]);
%! isdag (G);

## isdag on an empty undirected graph is still an error.
%!error <requires a digraph>
%! G = graph ();
%! isdag (G);

## isdag on a non-graph numeric input is an error.
%!error <must be a digraph object>
%! isdag (42);

## isdag on a non-graph string input is an error.
%!error <must be a digraph object>
%! isdag ("foo");

## isdag with no args is an error via print_usage.
%!error isdag ()

## -------------------- return type --------------------

## Result is a scalar logical.
%!test
%! G = digraph ([1 2], [2 3]);
%! tf = isdag (G);
%! assert (islogical (tf));
%! assert (isscalar (tf));

## Result on empty digraph is a scalar logical.
%!test
%! G = digraph ();
%! tf = isdag (G);
%! assert (islogical (tf));
%! assert (isscalar (tf));

## Result on cyclic digraph is a scalar logical false.
%!test
%! G = digraph ([1 2], [2 1]);
%! tf = isdag (G);
%! assert (islogical (tf));
%! assert (isscalar (tf));
%! assert (tf, false);

## -------------------- acyclic cases --------------------

## Empty digraph is a DAG.
%!test
%! G = digraph ();
%! assert (isdag (G), true);

## Single-node digraph with no edges is a DAG.
%!test
%! G = digraph (1);
%! assert (isdag (G), true);

## Edgeless multi-node digraph is a DAG.
%!test
%! G = digraph (5);
%! assert (isdag (G), true);

## Single edge 1->2 is a DAG.
%!test
%! G = digraph (1, 2);
%! assert (isdag (G), true);

## Line 1->2->3 is a DAG.
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (isdag (G), true);

## Diamond DAG.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! assert (isdag (G), true);

## Tree 1->{2,3}, 2->{4,5} is a DAG.
%!test
%! G = digraph ([1 1 2 2], [2 3 4 5]);
%! assert (isdag (G), true);

## Larger layered DAG.
%!test
%! s = [1 2 3 4 5  6 7 8 9 10  11 12 13 14 15];
%! t = [6 7 8 9 10 11 12 13 14 15  16 17 18 19 20];
%! G = digraph (s, t);
%! assert (isdag (G), true);

## Disconnected components, each acyclic, overall DAG.
%!test
%! G = digraph ([1 2 10 11], [2 3 11 12], [], 12);
%! assert (isdag (G), true);

## Isolated nodes + a few edges still a DAG.
%!test
%! G = digraph ([1 3], [2 4], [], 6);
%! assert (isdag (G), true);

## Weighted DAG is still a DAG (weights don't matter).
%!test
%! G = digraph ([1 2 3], [2 3 4], [10 20 30]);
%! assert (isdag (G), true);

## Negative-weight DAG is still a DAG (weights don't matter).
%!test
%! G = digraph ([1 2 3], [2 3 4], [-1 -2 -3]);
%! assert (isdag (G), true);

## -------------------- cyclic cases --------------------

## Self-loop makes digraph non-DAG.
%!test
%! G = digraph (1, 1);
%! assert (isdag (G), false);

## Self-loop on a multi-node digraph is still a cycle.
%!test
%! G = digraph ([1 2 2], [2 2 3]);
%! assert (isdag (G), false);

## 2-cycle is not a DAG.
%!test
%! G = digraph ([1 2], [2 1]);
%! assert (isdag (G), false);

## 3-cycle is not a DAG.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (isdag (G), false);

## 4-cycle is not a DAG.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! assert (isdag (G), false);

## Cycle in one component; other component is a DAG.
## Edges: 1->2, 2->1 (cycle); 3->4, 4->5 (DAG).
%!test
%! G = digraph ([1 2 3 4], [2 1 4 5]);
%! assert (isdag (G), false);

## Larger cyclic digraph: 5-cycle with additional back-edge.
%!test
%! G = digraph ([1 2 3 4 5 5], [2 3 4 5 6 1]);
%! assert (isdag (G), false);

## Cycle hidden in a larger mostly-acyclic digraph.
%!test
%! s = [1 1 2 3 3 4 4 5 6 6 7 8 8 9 10];
%! t = [2 3 4 4 5 6 7 7 8 9 9 10 9 10  1];
%! G = digraph (s, t);
%! assert (isdag (G), false);

## -------------------- named nodes --------------------

## Named DAG: isdag returns true.
%!test
%! G = digraph ([1 2 3], [2 3 4], [], {"a","b","c","d"});
%! assert (isdag (G), true);

## Named digraph with a cycle still returns false.
%!test
%! G = digraph ({"a","b"}, {"b","a"}, [], {"a","b"});
%! assert (isdag (G), false);

## -------------------- multigraph / parallel edges --------------------

## Parallel edges in the same direction stay acyclic.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (isdag (G), true);

## Parallel edges in opposing directions form a cycle.
%!test
%! G = digraph ([1 2 1], [2 1 2], "multigraph");
%! assert (isdag (G), false);

## Multi-edge self-loop is still a cycle.
%!test
%! G = digraph ([1 1 1], [1 1 1], "multigraph");
%! assert (isdag (G), false);

## -------------------- dot notation dispatch --------------------

## G.isdag() returns the same as isdag(G).
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! tf1 = isdag (G);
%! tf2 = G.isdag ();
%! assert (tf1, tf2);

## G.isdag() on a cyclic graph matches isdag(G).
%!test
%! G = digraph ([1 2], [2 1]);
%! tf1 = isdag (G);
%! tf2 = G.isdag ();
%! assert (tf1, tf2);
%! assert (tf1, false);

## -------------------- consistency with toposort --------------------

## If isdag is true, toposort succeeds; if false, toposort errors.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! assert (isdag (G), true);
%! n = toposort (G);
%! assert (numel (n), 4);

## toposort raises for non-DAG consistent with isdag being false.
%!test
%! G = digraph ([1 2], [2 1]);
%! assert (isdag (G), false);
%! try
%!   toposort (G);
%!   ok = false;
%! catch
%!   ok = true;
%! end_try_catch
%! assert (ok, true);
