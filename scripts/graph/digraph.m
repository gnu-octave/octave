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
  ## @deftypefnx {} {@var{G} =} digraph (@var{s}, @var{t})
  ## Create a directed graph.
  ##
  ## With no arguments, return an empty directed graph with zero nodes
  ## and zero edges.
  ##
  ## With a single non-negative integer scalar @var{N}, return a directed
  ## graph with @var{N} isolated nodes and no edges.
  ##
  ## With two numeric vectors @var{s} and @var{t} of equal length, return
  ## a directed graph with one edge from @code{@var{s}(i)} to
  ## @code{@var{t}(i)} for each index @var{i}.  Entries of @var{s} and
  ## @var{t} must be positive integers referring to node indices; the
  ## node count is automatically set to @code{max([@var{s}(:); @var{t}(:)])}.
  ## Passing two empty vectors is equivalent to @code{digraph()}.
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
  ##
  ## s = [1 2 3];
  ## t = [2 3 1];
  ## G = digraph (s, t);    # 3-cycle 1->2->3->1
  ## numedges (G)           # ==> 3
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
      elseif (nargin == 2)
        ## Edge-list constructor: digraph (s, t).
        s = varargin{1};
        t = varargin{2};
        if (! (isnumeric (s) && isreal (s) ...
               && isnumeric (t) && isreal (t)))
          error ("Octave:invalid-input-arg", ...
                 "digraph: S and T must be numeric vectors");
        endif
        if (! (isvector (s) || isempty (s)) ...
            || ! (isvector (t) || isempty (t)))
          error ("Octave:invalid-input-arg", ...
                 "digraph: S and T must be vectors");
        endif
        if (numel (s) != numel (t))
          error ("Octave:invalid-input-arg", ...
                 "digraph: S and T must have the same length");
        endif
        s = double (s(:));
        t = double (t(:));
        if (! isempty (s))
          if (any (! isfinite (s)) || any (! isfinite (t)) ...
              || any (s < 1) || any (t < 1) ...
              || any (s != fix (s)) || any (t != fix (t)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: S and T must be positive integer vectors");
          endif
          N = max (max (s), max (t));
          G.adj_ = sparse (s, t, 1, N, N);
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

## BIST — US-C02: digraph(s, t) edge-list constructor with numeric row vectors.
%!test
%! s = [1 2 3];
%! t = [2 3 1];
%! G = digraph (s, t);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);

## BIST — US-C02: column vectors accepted.
%!test
%! s = [1; 2; 3];
%! t = [2; 3; 1];
%! G = digraph (s, t);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);

## BIST — US-C02: mixed row/column orientation.
%!test
%! G = digraph ([1 2 3], [2; 3; 1]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);

## BIST — US-C02: node count auto-computed from max endpoint.
%!test
%! G = digraph ([1 2], [5 3]);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);

## BIST — US-C02: endpoints above max in t still counted.
%!test
%! G = digraph (1, 10);
%! assert (numnodes (G), 10);
%! assert (numedges (G), 1);

## BIST — US-C02: empty edge list produces empty digraph.
%!test
%! G = digraph ([], []);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C02: self-loop permitted.
%!test
%! G = digraph (3, 3);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 1);

## BIST — US-C02: siever-like 1-based example (12 edges on 9 nodes).
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! assert (numnodes (G), 9);
%! assert (numedges (G), 12);

## BIST — US-C02: length mismatch error.
%!error <same length> digraph ([1 2 3], [1 2])
%!error <same length> digraph ([1 2], [1 2 3])

## BIST — US-C02: non-numeric s/t errors.
%!error <numeric> digraph ({"a"}, {"b"})
%!error <numeric> digraph ("abc", "def")

## BIST — US-C02: indices must be positive integers.
%!error <positive integer> digraph (0, 1)
%!error <positive integer> digraph (1, 0)
%!error <positive integer> digraph (-1, 1)
%!error <positive integer> digraph (1, -1)
%!error <positive integer> digraph (1.5, 2)
%!error <positive integer> digraph (1, 2.5)
%!error <positive integer> digraph (Inf, 1)
%!error <positive integer> digraph (1, Inf)
%!error <positive integer> digraph (NaN, 1)
%!error <positive integer> digraph (1, NaN)

## BIST — US-C02: s and t must be vectors (not matrices).
%!error <vector> digraph ([1 2; 3 4], [1 2; 3 4])
