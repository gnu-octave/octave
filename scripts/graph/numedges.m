########################################################################
##
## Copyright (C) 2013-2026 The Octave Project Developers
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
## @deftypefn {} {@var{m} =} numedges (@var{G})
## Return the number of edges in the graph or digraph @var{G}.
##
## @var{G} must be either a @code{graph} or @code{digraph} object.  For
## a simple graph or digraph each edge is counted once: in an undirected
## @code{graph} the pair @code{(i, j)} and @code{(j, i)} count as one
## edge, and a self-loop counts as one edge as well.  For a
## @code{digraph} the ordered pair @code{(i, j)} differs from
## @code{(j, i)} and each is counted separately.
##
## For a multigraph (a graph or digraph constructed with the
## @qcode{'multigraph'} flag) parallel edges between the same endpoint
## pair are counted individually -- so @code{numedges (G)} matches the
## row count of @code{G.Edges.EndNodes}.
##
## @var{G} is not mutated; @code{numedges} is a pure query.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## numedges (G)                # @result{} 3
## H = graph ([1 1 2], [2 3 3]);
## numedges (H)                # @result{} 3 (each undirected edge once)
## @end group
## @end example
##
## @seealso{graph, digraph, numnodes, ismultigraph}
## @end deftypefn

function m = numedges (G)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{numedges} method and
  ## this free-function body is not reached.  This file exists both as
  ## a canonical documentation target (so @code{help numedges} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 1)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg",
           "numedges: G must be a graph or digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  m = G.numedges ();

endfunction


%!test
%! G = digraph ();
%! assert (numedges (G), 0);

%!test
%! G = digraph (5);
%! assert (numedges (G), 0);

%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (numedges (G), 3);

%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (numedges (G), 3);

%!test
%! ## Self-loops count individually in a digraph.
%! G = digraph ([1 2 3], [1 2 3]);
%! assert (numedges (G), 3);

%!test
%! ## Isolated nodes do not contribute edges.
%! G = digraph ([1 2], [2 3], 1, 10);
%! assert (numedges (G), 2);

%!test
%! ## Siever-style 12-edge directed fixture.
%! G = digraph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
%!              [2 3 2 4 5 6 9 7 8 9 7 4]);
%! assert (numedges (G), 12);

%!test
%! ## Multigraph parallel edges count individually.
%! G = digraph ([1 1 1 2], [2 2 2 3], [1 1 1 1], "multigraph");
%! assert (numedges (G), 4);

%!test
%! ## Digraph from adjacency matrix: one edge per nonzero.
%! A = [0 1 0; 0 0 1; 1 0 0];
%! G = digraph (A);
%! assert (numedges (G), 3);

%!test
%! G = graph ();
%! assert (numedges (G), 0);

%!test
%! G = graph (5);
%! assert (numedges (G), 0);

%!test
%! ## Undirected: (1,2) and (2,1) are the same edge.
%! G = graph ([1 2 3], [2 3 1]);
%! assert (numedges (G), 3);

%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (numedges (G), 3);

%!test
%! ## Self-loop in undirected graph counts as one edge.
%! G = graph ([1 2 3], [1 2 3]);
%! assert (numedges (G), 3);

%!test
%! ## Isolated nodes do not contribute edges.
%! G = graph ([1 2], [2 3], 1, 10);
%! assert (numedges (G), 2);

%!test
%! ## Symmetric adjacency; undirected edge counted once.
%! A = [0 1 0; 1 0 1; 0 1 0];
%! G = graph (A);
%! assert (numedges (G), 2);

%!test
%! ## Class return type is double for MATLAB parity.
%! G = digraph ([1 2], [2 3]);
%! assert (class (numedges (G)), "double");
%! G2 = graph ([1 2], [2 3]);
%! assert (class (numedges (G2)), "double");

%!error <G must be a graph or digraph> numedges (3)
%!error <G must be a graph or digraph> numedges ([1 2 3])
%!error <G must be a graph or digraph> numedges ("hello")
%!error <G must be a graph or digraph> numedges ({1, 2})
%!error <G must be a graph or digraph> numedges (struct ("a", 1))
%!error <Invalid call> numedges ()
