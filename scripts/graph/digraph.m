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

classdef digraph

  ## -*- texinfo -*-
  ## @deftypefn  {} {@var{G} =} digraph ()
  ## @deftypefnx {} {@var{G} =} digraph (@var{N})
  ## Create a directed graph.
  ##
  ## With no arguments, return an empty directed graph with zero nodes
  ## and zero edges.
  ##
  ## With a single non-negative integer scalar @var{N}, return a directed
  ## graph with @var{N} isolated nodes and no edges.
  ##
  ## @code{digraph} is a value class: every mutator returns a new object,
  ## leaving the input unchanged.
  ##
  ## Examples:
  ##
  ## @example
  ## @group
  ## G = digraph ();        # empty digraph
  ## numnodes (G)           # ==> 0
  ## numedges (G)           # ==> 0
  ##
  ## G = digraph (5);       # 5 isolated nodes
  ## numnodes (G)           # ==> 5
  ## numedges (G)           # ==> 0
  ## @end group
  ## @end example
  ##
  ## @seealso{graph, numnodes, numedges}
  ## @end deftypefn

  properties (Access = private)
    ## Sparse N-by-N adjacency matrix.  The value at (i, j) is the weight
    ## of the edge from node i to node j, or zero when no edge exists.
    ## For unweighted graphs the stored value is 1.
    adj_ = sparse (0, 0);

    ## Optional cellstr of node names.  Empty cell means nodes are
    ## referred to by integer index only.
    nodenames_ = {};
  endproperties

  methods

    function G = digraph (varargin)

      if (nargin == 0)
        ## Default constructor: empty graph.  Property defaults apply.
        return;
      elseif (nargin == 1)
        arg1 = varargin{1};
        if (isnumeric (arg1) && isscalar (arg1) && isreal (arg1) ...
            && isfinite (arg1) && arg1 >= 0 && arg1 == fix (arg1))
          N = double (arg1);
          G.adj_ = sparse (N, N);
        else
          error ("Octave:invalid-input-arg", ...
                 "digraph: N must be a non-negative integer scalar");
        endif
      else
        error ("Octave:invalid-input-arg", ...
               "digraph: unsupported number of arguments");
      endif

    endfunction

    function n = numnodes (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{n} =} numnodes (@var{G})
      ## Return the number of nodes in the digraph @var{G}.
      ## @seealso{digraph, numedges}
      ## @end deftypefn

      n = size (G.adj_, 1);

    endfunction

    function m = numedges (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{m} =} numedges (@var{G})
      ## Return the number of edges in the digraph @var{G}.
      ## @seealso{digraph, numnodes}
      ## @end deftypefn

      m = nnz (G.adj_);

    endfunction

  endmethods

endclassdef


## BIST — default constructor.
%!test
%! G = digraph ();
%! assert (class (G), "digraph");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — N-node edgeless digraph.
%!test
%! G = digraph (5);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 0);

## BIST — digraph(0) is equivalent to digraph().
%!test
%! G = digraph (0);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — digraph(1) is a single isolated node.
%!test
%! G = digraph (1);
%! assert (numnodes (G), 1);
%! assert (numedges (G), 0);

## BIST — large N works without densifying.
%!test
%! G = digraph (1000);
%! assert (numnodes (G), 1000);
%! assert (numedges (G), 0);

## BIST — value-class semantics: copy is independent (baseline).
%!test
%! G1 = digraph (3);
%! G2 = G1;
%! assert (numnodes (G1), 3);
%! assert (numnodes (G2), 3);

## BIST — input validation.
%!error <non-negative integer> digraph (-3)
%!error <non-negative integer> digraph (3.5)
%!error <non-negative integer> digraph ([1 2 3])
%!error <non-negative integer> digraph (Inf)
%!error <non-negative integer> digraph (NaN)
%!error <non-negative integer> digraph (-1)
%!error <unsupported number of arguments> digraph (1, 2, 3, 4, 5, 6)
