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
## @deftypefn {} {@var{n} =} numnodes (@var{G})
## Return the number of nodes in the graph or digraph @var{G}.
##
## @var{G} must be either a @code{graph} or @code{digraph} object.  The
## number of nodes is a non-negative integer that includes isolated
## nodes (nodes with no incident edges) as well as nodes carrying
## self-loops only.  For an empty graph the return value is zero.
##
## @var{G} is not mutated; @code{numnodes} is a pure query.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## numnodes (G)   # @result{} 3
## H = graph (5);
## numnodes (H)   # @result{} 5
## @end group
## @end example
##
## @seealso{graph, digraph, numedges, ismultigraph}
## @end deftypefn

function n = numnodes (G)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{numnodes} method and
  ## this free-function body is not reached.  This file exists both as
  ## a canonical documentation target (so @code{help numnodes} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 1)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg",
           "numnodes: G must be a graph or digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  n = G.numnodes ();

endfunction


%!test
%! G = digraph ();
%! assert (numnodes (G), 0);

%!test
%! G = digraph (5);
%! assert (numnodes (G), 5);

%!test
%! G = digraph (1);
%! assert (numnodes (G), 1);

%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (numnodes (G), 3);

%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (numnodes (G), 3);

%!test
%! G = digraph ([1 2], [2 3], 1, 10);
%! assert (numnodes (G), 10);

%!test
%! G = digraph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
%!              [2 3 2 4 5 6 9 7 8 9 7 4]);
%! assert (numnodes (G), 9);

%!test
%! G = digraph ([1 2], [2 3], [], {"a"; "b"; "c"; "d"});
%! assert (numnodes (G), 4);

%!test
%! G = digraph (sparse (eye (4)));
%! assert (numnodes (G), 4);

%!test
%! G = graph ();
%! assert (numnodes (G), 0);

%!test
%! G = graph (5);
%! assert (numnodes (G), 5);

%!test
%! G = graph (1);
%! assert (numnodes (G), 1);

%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (numnodes (G), 3);

%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (numnodes (G), 3);

%!test
%! G = graph ([1 2], [2 3], 1, 10);
%! assert (numnodes (G), 10);

%!test
%! A = [0 1 0; 1 0 1; 0 1 0];
%! G = graph (A);
%! assert (numnodes (G), 3);

%!test
%! ## Class return type is double for MATLAB parity.
%! G = digraph (3);
%! assert (class (numnodes (G)), "double");
%! G2 = graph (3);
%! assert (class (numnodes (G2)), "double");

%!error <G must be a graph or digraph> numnodes (3)
%!error <G must be a graph or digraph> numnodes ([1 2 3])
%!error <G must be a graph or digraph> numnodes ("hello")
%!error <G must be a graph or digraph> numnodes ({1, 2})
%!error <G must be a graph or digraph> numnodes (struct ("a", 1))
%!error <Invalid call> numnodes ()
