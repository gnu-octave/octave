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
  ## @deftypefnx {} {@var{G} =} digraph (@var{A})
  ## @deftypefnx {} {@var{G} =} digraph (@var{A}, @var{nodenames})
  ## @deftypefnx {} {@var{G} =} digraph (@var{s}, @var{t})
  ## @deftypefnx {} {@var{G} =} digraph (@var{s}, @var{t}, @var{w})
  ## @deftypefnx {} {@var{G} =} digraph (@var{s}, @var{t}, @var{w}, @var{nodenames})
  ## @deftypefnx {} {@var{G} =} digraph (@var{s}, @var{t}, @var{w}, @var{N})
  ## Create a directed graph.
  ##
  ## With no arguments, return an empty directed graph with zero nodes
  ## and zero edges.
  ##
  ## With a single non-negative integer scalar @var{N}, return a directed
  ## graph with @var{N} isolated nodes and no edges.
  ##
  ## With a single non-scalar numeric or logical square matrix @var{A},
  ## treat @var{A} as an adjacency matrix: one edge from node @var{i} to
  ## node @var{j} is created for every nonzero @code{@var{A}(i,j)}, and
  ## the weight of that edge is the value of @code{@var{A}(i,j)}.  The
  ## node count is @code{size (@var{A}, 1)}.  Sparse @var{A} is used
  ## directly without densifying, so very large sparse adjacency matrices
  ## are supported.  @var{A} must be real; complex or @code{NaN} entries
  ## are rejected.  Self-loops are permitted when
  ## @code{@var{A}(i,i) != 0}.
  ##
  ## With two arguments @var{A} and @var{nodenames} where @var{nodenames}
  ## is a cell array of unique strings, the adjacency-matrix semantics
  ## above apply and the nodes are given the supplied names.
  ## @code{numel (@var{nodenames})} must equal @code{size (@var{A}, 1)}.
  ## @code{@var{G}.Nodes.Name} returns the names as a column cell array.
  ##
  ## With two numeric vectors @var{s} and @var{t} of equal length, return
  ## a directed graph with one edge from @code{@var{s}(i)} to
  ## @code{@var{t}(i)} for each index @var{i}.  Entries of @var{s} and
  ## @var{t} must be positive integers referring to node indices; the
  ## node count is automatically set to @code{max([@var{s}(:); @var{t}(:)])}.
  ## Passing two empty vectors is equivalent to @code{digraph()}.
  ##
  ## With three numeric inputs @var{s}, @var{t}, and @var{w}, each edge
  ## @code{@var{s}(i)->@var{t}(i)} is created with weight @code{@var{w}(i)}.
  ## @var{w} may be a scalar (broadcast to every edge) or a vector of
  ## length @code{numel (@var{s})}.  The weights are returned in
  ## @code{@var{G}.Edges.Weight} in edge-index order (edges are listed in
  ## lexicographic @code{(source, destination)} order).
  ##
  ## With a fourth argument @var{nodenames} (a cell array of unique
  ## strings), nodes are named.  The number of nodes is
  ## @code{numel (@var{nodenames})} regardless of the maximum endpoint
  ## index, so isolated named nodes are preserved.  When @var{s} and
  ## @var{t} are numeric, their entries must be integer indices in the
  ## range @code{1:numel (@var{nodenames})}.  When @var{s} and @var{t}
  ## are strings (char row) or cell arrays of strings, each entry is
  ## looked up in @var{nodenames} to resolve its integer index.  Pass
  ## @code{[]} for @var{w} to create an unweighted named digraph.
  ## @code{@var{G}.Nodes.Name} returns the node names as a column
  ## cell array.
  ##
  ## When the fourth argument is a non-negative integer scalar @var{N},
  ## the resulting digraph has exactly @var{N} nodes.  Any node indices
  ## in @var{s} or @var{t} must lie in the range @code{1:@var{N}}, and
  ## node indices greater than @code{max([@var{s}(:); @var{t}(:)])}
  ## correspond to isolated nodes.  Pass @code{[]} for @var{w} to create
  ## an unweighted digraph with @var{N} nodes.
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
  ##
  ## w = [1.5 2.5 3.5];
  ## G = digraph (s, t, w); # weighted 3-cycle
  ## G.Edges.Weight         # ==> [1.5; 2.5; 3.5]
  ##
  ## names = @{"a", "b", "c"@};
  ## G = digraph (@{"a", "b"@}, @{"b", "c"@}, [10 20], names);
  ## G.Nodes.Name           # ==> @{"a"; "b"; "c"@}
  ##
  ## G = digraph ([1 2], [2 3], [1 1], 5);  # 5 nodes, 2 edges, 2 isolated
  ## numnodes (G)           # ==> 5
  ## numedges (G)           # ==> 2
  ##
  ## A = [0 1 0; 0 0 1; 1 0 0];
  ## G = digraph (A);       # 3-cycle from adjacency matrix
  ## G.Edges.EndNodes       # ==> [1 2; 2 3; 3 1]
  ##
  ## G = digraph (A, @{"alpha", "beta", "gamma"@});
  ## G.Nodes.Name           # ==> @{"alpha"; "beta"; "gamma"@}
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

    ## Whether the user supplied explicit edge weights.  When false, the
    ## Edges struct has no Weight field (MATLAB parity).  When true, the
    ## nonzero values of adj_ are the edge weights.
    has_weights_ = false;
  endproperties

  properties (Dependent, SetAccess = private)
    ## Struct-of-arrays node list.  Fields:
    ##   Name  m-by-1 column cellstr of node names.  When the digraph
    ##         was constructed without names, this is an empty
    ##         @code{cell (0, 1)}.
    ## This stands in for MATLAB's @code{table} until Octave has a
    ## built-in table class.
    Nodes

    ## Struct-of-arrays edge list.  Fields:
    ##   EndNodes  m-by-2 matrix of [source, destination] pairs in
    ##             lexicographic order.
    ##   Weight    m-by-1 column of edge weights (present only when the
    ##             graph was constructed with explicit weights).
    ## This stands in for MATLAB's @code{table} until Octave has a
    ## built-in table class.
    Edges
  endproperties

  methods

    function G = digraph (varargin)

      if (nargin == 0)
        ## Default constructor: empty graph.  Property defaults apply.
        return;
      elseif (nargin == 1)
        arg1 = varargin{1};
        if (isnumeric (arg1) && isscalar (arg1))
          ## Scalar numeric input: node count N.
          if (! (isreal (arg1) && isfinite (arg1) && arg1 >= 0 ...
                 && arg1 == fix (arg1)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: N must be a non-negative integer scalar");
          endif
          N = double (arg1);
          G.adj_ = sparse (N, N);
        elseif ((isnumeric (arg1) || islogical (arg1)) ...
                && ismatrix (arg1) && ndims (arg1) == 2)
          ## Non-scalar 2-D input: adjacency matrix.  Each nonzero A(i,j)
          ## becomes an edge i->j with weight A(i,j).  Sparse input is
          ## preserved without densifying.
          A = arg1;
          if (! isreal (A))
            error ("Octave:invalid-input-arg", ...
                   "digraph: adjacency matrix A must be real");
          endif
          if (size (A, 1) != size (A, 2))
            error ("Octave:invalid-input-arg", ...
                   "digraph: adjacency matrix A must be square");
          endif
          if (any (isnan (A(:))))
            error ("Octave:invalid-input-arg", ...
                   "digraph: adjacency matrix A must not contain NaN");
          endif
          if (issparse (A))
            ## Coerce value type to double without densifying; logical
            ## sparse gets promoted here via the 1.0 * trick.
            if (! isa (A, "double"))
              A = sparse (double (A));
            endif
            G.adj_ = A;
          else
            ## Dense path: sparsify.  double() handles int* / logical.
            G.adj_ = sparse (double (A));
          endif
          ## Non-empty adjacency always carries a Weight column (matrix
          ## form implies weighted, MATLAB parity).  0x0 stays empty and
          ## unweighted.
          if (size (A, 1) > 0)
            G.has_weights_ = true;
          endif
        else
          error ("Octave:invalid-input-arg", ...
                 ["digraph: single-argument input must be a ", ...
                  "non-negative integer scalar or a real square ", ...
                  "adjacency matrix"]);
        endif
      elseif (nargin == 2 && iscellstr (varargin{2}))
        ## Adjacency-matrix + nodenames constructor:
        ## digraph (A, NODENAMES).  A must be a real square numeric or
        ## logical 2-D matrix; NODENAMES must be a cellstr of unique
        ## strings whose length equals size (A, 1).  Semantics otherwise
        ## mirror the US-C06 adjacency path (sparse preserved, weights
        ## drawn from A(i,j)).
        A = varargin{1};
        nn = varargin{2};
        nn = nn(:);  # store as column cellstr
        if (! ((isnumeric (A) || islogical (A)) ...
               && ismatrix (A) && ndims (A) == 2))
          error ("Octave:invalid-input-arg", ...
                 ["digraph: adjacency matrix A must be a real ", ...
                  "square numeric or logical matrix"]);
        endif
        if (! isreal (A))
          error ("Octave:invalid-input-arg", ...
                 "digraph: adjacency matrix A must be real");
        endif
        if (size (A, 1) != size (A, 2))
          error ("Octave:invalid-input-arg", ...
                 "digraph: adjacency matrix A must be square");
        endif
        if (any (isnan (A(:))))
          error ("Octave:invalid-input-arg", ...
                 "digraph: adjacency matrix A must not contain NaN");
        endif
        if (numel (nn) != numel (unique (nn)))
          error ("Octave:invalid-input-arg", ...
                 "digraph: NODENAMES must contain unique strings");
        endif
        if (numel (nn) != size (A, 1))
          error ("Octave:invalid-input-arg", ...
                 ["digraph: NODENAMES numel must equal ", ...
                  "size (A, 1)"]);
        endif

        if (issparse (A))
          if (! isa (A, "double"))
            A = sparse (double (A));
          endif
          G.adj_ = A;
        else
          G.adj_ = sparse (double (A));
        endif
        if (size (A, 1) > 0)
          G.has_weights_ = true;
        endif
        G.nodenames_ = nn;
      elseif (nargin == 2 || nargin == 3)
        ## Edge-list constructor: digraph (s, t) or digraph (s, t, w).
        s = varargin{1};
        t = varargin{2};
        have_weights = (nargin == 3);
        if (have_weights)
          w = varargin{3};
        endif
        if (! (isnumeric (s) && isreal (s) ...
               && isnumeric (t) && isreal (t)))
          error ("Octave:invalid-input-arg", ...
                 "digraph: S and T must be numeric vectors");
        endif
        if (have_weights && ! (isnumeric (w) && isreal (w)))
          error ("Octave:invalid-input-arg", ...
                 "digraph: W must be a numeric real vector or scalar");
        endif
        if (! (isvector (s) || isempty (s)) ...
            || ! (isvector (t) || isempty (t)))
          error ("Octave:invalid-input-arg", ...
                 "digraph: S and T must be vectors");
        endif
        if (have_weights ...
            && ! (isvector (w) || isempty (w) || isscalar (w)))
          error ("Octave:invalid-input-arg", ...
                 "digraph: W must be a vector or scalar");
        endif
        if (numel (s) != numel (t))
          error ("Octave:invalid-input-arg", ...
                 "digraph: S and T must have the same length");
        endif
        if (have_weights && ! isscalar (w) && numel (w) != numel (s))
          error ("Octave:invalid-input-arg", ...
                 ["digraph: weight vector W must have length ", ...
                  "numel (S) or be a scalar"]);
        endif
        s = double (s(:));
        t = double (t(:));
        if (have_weights)
          w = double (w(:));
        endif
        if (! isempty (s))
          if (any (! isfinite (s)) || any (! isfinite (t)) ...
              || any (s < 1) || any (t < 1) ...
              || any (s != fix (s)) || any (t != fix (t)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: S and T must be positive integer vectors");
          endif
          if (have_weights)
            if (any (isnan (w)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: weight vector W must not contain NaN");
            endif
            if (isscalar (w))
              w = repmat (w, numel (s), 1);
            endif
          endif
          N = max (max (s), max (t));
          if (have_weights)
            G.adj_ = sparse (s, t, w, N, N);
            G.has_weights_ = true;
          else
            G.adj_ = sparse (s, t, 1, N, N);
          endif
        endif
      elseif (nargin == 4)
        ## Four-argument constructor.  Dispatch on the type of the
        ## fourth argument:
        ##   cellstr           -> digraph (s, t, w, nodenames)
        ##   numeric scalar    -> digraph (s, t, w, N)
        ## Any other shape is rejected.
        s = varargin{1};
        t = varargin{2};
        w = varargin{3};
        arg4 = varargin{4};

        if (iscellstr (arg4))
          ## Named edge-list constructor: digraph (s, t, w, nodenames).
          ## Endpoints may be numeric indices or strings looked up in
          ## nodenames.  Node count is numel (nodenames) -- isolated
          ## named nodes are preserved.  Pass [] for W to omit weights.
          nn = arg4;
          nn = nn(:);  # store as column cellstr
          if (numel (nn) != numel (unique (nn)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: NODENAMES must contain unique strings");
          endif
          N = numel (nn);

          ## Resolve endpoints to numeric indices.
          s_idx = __resolve_endpoint__ (s, nn, "S");
          t_idx = __resolve_endpoint__ (t, nn, "T");
          if (numel (s_idx) != numel (t_idx))
            error ("Octave:invalid-input-arg", ...
                   "digraph: S and T must have the same length");
          endif

          ## W may be [] (no weights), a scalar (broadcast), or a vector
          ## of length numel(s).  An all-NaN/non-numeric W is rejected.
          have_weights = ! (isnumeric (w) && isempty (w));
          if (have_weights)
            if (! (isnumeric (w) && isreal (w)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: W must be a numeric real vector or scalar");
            endif
            if (! (isvector (w) || isscalar (w)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: W must be a vector or scalar");
            endif
            if (! isscalar (w) && numel (w) != numel (s_idx))
              error ("Octave:invalid-input-arg", ...
                     ["digraph: weight vector W must have length ", ...
                      "numel (S) or be a scalar"]);
            endif
            w = double (w(:));
            if (any (isnan (w)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: weight vector W must not contain NaN");
            endif
            if (isscalar (w))
              w = repmat (w, numel (s_idx), 1);
            endif
          endif

          G.nodenames_ = nn;
          if (isempty (s_idx))
            G.adj_ = sparse (N, N);
          elseif (have_weights)
            G.adj_ = sparse (s_idx, t_idx, w, N, N);
            G.has_weights_ = true;
          else
            G.adj_ = sparse (s_idx, t_idx, 1, N, N);
          endif
        elseif (isnumeric (arg4) && isscalar (arg4))
          ## Integer-node-count constructor: digraph (s, t, w, N).
          ## Creates a digraph with exactly N nodes (isolated trailing
          ## nodes preserved when max endpoint < N).  Endpoints must be
          ## positive-integer indices bounded by N.  W may be [] (no
          ## weights), scalar (broadcast), or a vector of length
          ## numel (S).
          if (! (isreal (arg4) && isfinite (arg4) && arg4 >= 0 ...
                 && arg4 == fix (arg4)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: N must be a non-negative integer scalar");
          endif
          N = double (arg4);

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

          have_weights = ! (isnumeric (w) && isempty (w));
          if (have_weights)
            if (! (isnumeric (w) && isreal (w)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: W must be a numeric real vector or scalar");
            endif
            if (! (isvector (w) || isscalar (w)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: W must be a vector or scalar");
            endif
            if (! isscalar (w) && numel (w) != numel (s))
              error ("Octave:invalid-input-arg", ...
                     ["digraph: weight vector W must have length ", ...
                      "numel (S) or be a scalar"]);
            endif
            w = double (w(:));
            if (any (isnan (w)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: weight vector W must not contain NaN");
            endif
            if (isscalar (w))
              w = repmat (w, numel (s), 1);
            endif
          endif

          if (! isempty (s))
            if (any (! isfinite (s)) || any (! isfinite (t)) ...
                || any (s < 1) || any (t < 1) ...
                || any (s != fix (s)) || any (t != fix (t)))
              error ("Octave:invalid-input-arg", ...
                     "digraph: S and T must be positive integer vectors");
            endif
            if (any (s > N) || any (t > N))
              error ("Octave:invalid-input-arg", ...
                     ["digraph: S and T entries must not exceed ", ...
                      "the node count N"]);
            endif
          endif

          if (isempty (s))
            G.adj_ = sparse (N, N);
          elseif (have_weights)
            G.adj_ = sparse (s, t, w, N, N);
            G.has_weights_ = true;
          else
            G.adj_ = sparse (s, t, 1, N, N);
          endif
        else
          error ("Octave:invalid-input-arg", ...
                 ["digraph: fourth argument must be a cell array ", ...
                  "of strings (node names) or a non-negative ", ...
                  "integer scalar (node count)"]);
        endif
      else
        error ("Octave:invalid-input-arg", ...
               "digraph: unsupported number of arguments");
      endif

    endfunction

    function e = get.Edges (G)

      ## Extract edges in lexicographic (source, destination) order.
      ## find(A.') iterates A column-by-column of the transpose, which
      ## corresponds to iterating rows of A (i.e. sources) in outer
      ## order and within-row columns (destinations) in inner order.
      [dst, src, w] = find (G.adj_.');
      e.EndNodes = [src, dst];
      if (G.has_weights_)
        e.Weight = w;
      endif

    endfunction

    function n = get.Nodes (G)

      ## Return the node table (struct of arrays).  MATLAB parity: the
      ## Name column is a column cellstr, empty when the digraph was
      ## built without names.
      if (isempty (G.nodenames_))
        n.Name = cell (0, 1);
      else
        n.Name = G.nodenames_;
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

## BIST — input validation (scalar-N branch).
%!error <non-negative integer> digraph (-3)
%!error <non-negative integer> digraph (3.5)
%!error <non-negative integer> digraph (Inf)
%!error <non-negative integer> digraph (NaN)
%!error <non-negative integer> digraph (-1)
%!error <unsupported number of arguments> digraph (1, 2, 3, 4, 5, 6)

## BIST — row vector is now interpreted as a non-square adjacency
## matrix (US-C06 change), not as an invalid N.
%!error <square> digraph ([1 2 3])

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

## BIST — US-C03: digraph(s, t, w) stores weights; vector weights round-trip.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1.5 2.5 3.5]);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.Weight, [1.5; 2.5; 3.5]);

## BIST — US-C03: column-vector weights accepted.
%!test
%! G = digraph ([1; 2; 3], [2; 3; 1], [10; 20; 30]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — US-C03: row-vector weights accepted (result is always column).
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — US-C03: scalar weight broadcasts to all edges.
%!test
%! G = digraph ([1 2 3], [2 3 1], 7);
%! assert (numedges (G), 3);
%! assert (G.Edges.Weight, [7; 7; 7]);

## BIST — US-C03: Edges.EndNodes and Weight are in (src, dst) lex order.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! E = G.Edges;
%! assert (size (E.EndNodes), [3, 2]);
%! assert (E.EndNodes, [1 2; 2 3; 3 1]);
%! assert (E.Weight,   [10; 20; 30]);

## BIST — US-C03: edges re-sorted into (src, dst) lex order regardless of
## input order; weights follow their associated edge.
%!test
%! G = digraph ([3 1 2], [1 2 3], [30 10 20]);
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 2 3; 3 1]);
%! assert (E.Weight,   [10; 20; 30]);

## BIST — US-C03: empty endpoints plus empty weight yields empty digraph.
%!test
%! G = digraph ([], [], []);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C03: empty endpoints plus scalar weight still yields empty digraph.
%!test
%! G = digraph ([], [], 7);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C03: unweighted digraph's Edges struct has no Weight field.
%!test
%! G = digraph ([1 2], [2 3]);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (! isfield (E, "Weight"));

## BIST — US-C03: weighted digraph's Edges struct has a Weight field.
%!test
%! G = digraph ([1 2], [2 3], [5 10]);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (isfield (E, "Weight"));
%! assert (E.EndNodes, [1 2; 2 3]);
%! assert (E.Weight,   [5; 10]);

## BIST — US-C03: negative weights permitted (Bellman-Ford is a later story).
%!test
%! G = digraph ([1 2], [2 3], [-1 -2]);
%! assert (G.Edges.Weight, [-1; -2]);

## BIST — US-C03: single-edge scalar weight.
%!test
%! G = digraph (1, 2, 3.14);
%! assert (numedges (G), 1);
%! assert (G.Edges.Weight, 3.14);

## BIST — US-C03: weight vector length mismatch errors.
%!error <length> digraph ([1 2 3], [2 3 1], [1 2])
%!error <length> digraph ([1 2 3], [2 3 1], [1 2 3 4])

## BIST — US-C03: non-numeric weight errors.
%!error <numeric> digraph ([1 2], [2 1], {"a", "b"})
%!error <numeric> digraph ([1 2], [2 1], "ab")

## BIST — US-C03: complex weight errors.
%!error <numeric> digraph ([1 2], [2 1], [1+1i, 2])

## BIST — US-C03: non-vector weight errors.
%!error <vector> digraph ([1 2 3 4], [2 3 4 1], [1 2; 3 4])

## BIST — US-C03: NaN weight errors.
%!error <NaN> digraph ([1 2], [2 1], [NaN 1])

## BIST — US-C03: three-arg form with positive-integer endpoint rule preserved.
%!error <positive integer> digraph (0, 1, 5)
%!error <positive integer> digraph (1, -1, 5)

## BIST — US-C04: digraph(s, t, w, nodenames) with numeric endpoints and
## a cellstr of node names.  Nodes.Name holds the names; numnodes equals
## numel(nodenames).
%!test
%! names = {"alpha", "beta", "gamma"};
%! G = digraph ([1 2 3], [2 3 1], [1 2 3], names);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"alpha"; "beta"; "gamma"});

## BIST — US-C04: column-cellstr nodenames accepted, returned as column.
%!test
%! names = {"a"; "b"; "c"};
%! G = digraph ([1 2], [2 3], [10 20], names);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (numnodes (G), 3);

## BIST — US-C04: node count comes from numel(nodenames), not from
## max endpoint.  Isolated nodes are preserved.
%!test
%! names = {"a", "b", "c", "d", "e"};
%! G = digraph ([1 2], [2 3], [1 1], names);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});

## BIST — US-C04: string endpoints are looked up in nodenames.
%!test
%! names = {"A", "B", "C"};
%! G = digraph ({"A", "B", "C"}, {"B", "C", "A"}, [1 2 3], names);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"A"; "B"; "C"});
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 2 3; 3 1]);
%! assert (E.Weight,   [1; 2; 3]);

## BIST — US-C04: mixed-case string endpoints round-trip weights.
%!test
%! names = {"red", "green", "blue"};
%! G = digraph ({"red", "green"}, {"green", "blue"}, [0.5 1.5], names);
%! assert (G.Edges.Weight, [0.5; 1.5]);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);

## BIST — US-C04: scalar weight broadcast still works with nodenames.
%!test
%! names = {"x", "y", "z"};
%! G = digraph ([1 2], [2 3], 7, names);
%! assert (G.Edges.Weight, [7; 7]);
%! assert (G.Nodes.Name, {"x"; "y"; "z"});

## BIST — US-C04: empty endpoints + nodenames gives an edgeless named graph.
%!test
%! names = {"p", "q", "r"};
%! G = digraph ([], [], [], names);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);
%! assert (G.Nodes.Name, {"p"; "q"; "r"});

## BIST — US-C04: single string endpoint (not cellstr) is accepted as one name.
%!test
%! names = {"A", "B"};
%! G = digraph ("A", "B", 1, names);
%! assert (numedges (G), 1);
%! assert (G.Edges.EndNodes, [1 2]);

## BIST — US-C04: duplicate node names rejected.
%!error <unique> digraph ([1 2], [2 1], [1 1], {"a", "a"})
%!error <unique> digraph ([1 2 3], [2 3 1], [1 1 1], {"a", "b", "a"})

## BIST — US-C04: non-cellstr nodenames rejected.
%!error <cell> digraph ([1 2], [2 1], [1 1], [1 2])
%!error <cell> digraph ([1 2], [2 1], [1 1], "ab")
%!error <cell> digraph ([1 2], [2 1], [1 1], {1, 2})

## BIST — US-C04: numeric endpoint out of range rejected.
%!error <node index> digraph ([1 4], [2 1], [1 1], {"a", "b", "c"})
%!error <node index> digraph ([1 2], [2 4], [1 1], {"a", "b", "c"})

## BIST — US-C04: string endpoint not matching any node name is rejected.
%!error <not found> digraph ({"A", "X"}, {"B", "A"}, [1 1], {"A", "B"})
%!error <not found> digraph ({"A", "B"}, {"B", "Z"}, [1 1], {"A", "B"})

## BIST — US-C04: Nodes property without names still returns a Name column
## (empty default cellstr).
%!test
%! G = digraph (3);
%! assert (isstruct (G.Nodes));
%! assert (isfield (G.Nodes, "Name"));
%! assert (G.Nodes.Name, cell (0, 1));

## BIST — US-C04: Nodes property read-only (SetAccess=private).
%!test
%! names = {"a", "b"};
%! G = digraph ([1], [2], [5], names);
%! fail ("G.Nodes = struct ();", "private access");

## BIST — US-C05: digraph(s, t, w, N) with N > max(s, t) creates the
## extra isolated nodes.
%!test
%! G = digraph ([1 2], [2 3], [1 1], 5);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [1; 1]);

## BIST — US-C05: N equal to max endpoint is valid (no isolated nodes).
%!test
%! G = digraph ([1 2], [2 3], [1 1], 3);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);

## BIST — US-C05: scalar weight broadcast preserved with N.
%!test
%! G = digraph ([1 2 3], [2 3 1], 5, 10);
%! assert (numnodes (G), 10);
%! assert (numedges (G), 3);
%! assert (G.Edges.Weight, [5; 5; 5]);

## BIST — US-C05: W = [] with N yields an unweighted digraph with N nodes.
%!test
%! G = digraph ([1 2], [2 3], [], 5);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (! isfield (E, "Weight"));

## BIST — US-C05: empty endpoints + N produces N isolated nodes.
%!test
%! G = digraph ([], [], [], 5);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 0);

## BIST — US-C05: N = 0 with empty endpoints is equivalent to digraph().
%!test
%! G = digraph ([], [], [], 0);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C05: unnamed N-form still has empty Name cellstr.
%!test
%! G = digraph ([1 2], [2 3], [1 1], 7);
%! assert (isstruct (G.Nodes));
%! assert (G.Nodes.Name, cell (0, 1));

## BIST — US-C05: column-vector endpoints accepted alongside N.
%!test
%! G = digraph ([1; 2], [2; 3], [1; 1], 4);
%! assert (numnodes (G), 4);
%! assert (numedges (G), 2);

## BIST — US-C05: large N preserved without densifying.
%!test
%! G = digraph (1, 2, 1, 1000);
%! assert (numnodes (G), 1000);
%! assert (numedges (G), 1);

## BIST — US-C05: edges still returned in lex (src, dst) order when
## isolated trailing nodes exist.
%!test
%! G = digraph ([3 1 2], [1 2 3], [30 10 20], 5);
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 2 3; 3 1]);
%! assert (E.Weight,   [10; 20; 30]);

## BIST — US-C05: Siever-style fixture padded with isolated nodes.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t, 1, 20);
%! assert (numnodes (G), 20);
%! assert (numedges (G), 12);

## BIST — US-C05: error when N is smaller than the largest endpoint index.
%!error <exceed> digraph ([1 5], [2 3], [1 1], 3)
%!error <exceed> digraph ([1 2], [2 5], [1 1], 3)
%!error <exceed> digraph (1, 10, 1, 5)

## BIST — US-C05: N must be a non-negative integer scalar.
%!error <non-negative integer> digraph ([1 2], [2 3], [1 1], -1)
%!error <non-negative integer> digraph ([1 2], [2 3], [1 1], 3.5)
%!error <non-negative integer> digraph ([1 2], [2 3], [1 1], Inf)
%!error <non-negative integer> digraph ([1 2], [2 3], [1 1], NaN)

## BIST — US-C05: weight-vector length mismatch still errors under N form.
%!error <length> digraph ([1 2 3], [2 3 1], [1 2], 5)
%!error <length> digraph ([1 2 3], [2 3 1], [1 2 3 4], 5)

## BIST — US-C05: non-numeric weights still error under N form.
%!error <numeric> digraph ([1 2], [2 1], {"a", "b"}, 5)

## BIST — US-C05: complex weights still error under N form.
%!error <numeric> digraph ([1 2], [2 1], [1+1i, 2], 5)

## BIST — US-C05: NaN in weight still errors under N form.
%!error <NaN> digraph ([1 2], [2 1], [NaN 1], 5)

## BIST — US-C05: positive-integer endpoint rule preserved under N form.
%!error <positive integer> digraph (0, 1, 1, 5)
%!error <positive integer> digraph (1.5, 2, 1, 5)
%!error <positive integer> digraph (1, -1, 1, 5)

## BIST — US-C05: non-vector s/t still error under N form.
%!error <vector> digraph ([1 2; 3 4], [1 2; 3 4], [1 1 1 1], 5)

## BIST — US-C05: s/t length mismatch still errors under N form.
%!error <same length> digraph ([1 2 3], [1 2], [1 1 1], 5)

## BIST — US-C05: fourth argument of a disallowed type errors.
%!error <fourth argument> digraph ([1 2], [2 1], [1 1], [3 4])
%!error <fourth argument> digraph ([1 2], [2 1], [1 1], true)
%!error <fourth argument> digraph ([1 2], [2 1], [1 1], {1, 2})

## BIST — US-C06: digraph(A) from dense adjacency matrix.  Each nonzero
## A(i,j) becomes an edge i->j; there are 3 nodes and 3 edges.
%!test
%! A = [0 1 0; 0 0 1; 1 0 0];
%! G = digraph (A);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);

## BIST — US-C06: weights default to the nonzero A(i,j) value.  Edges
## listed in lex (src, dst) order; weights follow their edge.
%!test
%! A = [0 1.5 0; 0 0 2.5; 3.5 0 0];
%! G = digraph (A);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight,   [1.5; 2.5; 3.5]);

## BIST — US-C06: all-ones adjacency still records Weight field (matrix
## form implies weighted, MATLAB parity).
%!test
%! A = [0 1; 1 0];
%! G = digraph (A);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (isfield (E, "Weight"));
%! assert (E.Weight, [1; 1]);

## BIST — US-C06: self-loops on the diagonal are preserved.
%!test
%! A = [1 1; 0 1];
%! G = digraph (A);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 1; 1 2; 2 2]);
%! assert (G.Edges.Weight,   [1; 1; 1]);

## BIST — US-C06: 5x5 adjacency with mostly-zero rows still gives 5 nodes
## (isolated rows/columns become isolated nodes).
%!test
%! A = zeros (5);
%! A(1,2) = 1;
%! A(2,3) = 2;
%! A(3,4) = 3;
%! G = digraph (A);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 4]);
%! assert (G.Edges.Weight,   [1; 2; 3]);

## BIST — US-C06: all-zeros adjacency yields an N-node edgeless digraph.
%!test
%! G = digraph (zeros (4));
%! assert (numnodes (G), 4);
%! assert (numedges (G), 0);

## BIST — US-C06: 0-by-0 adjacency yields the empty digraph.
%!test
%! G = digraph (zeros (0, 0));
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C06: sparse adjacency works and is not densified.
%!test
%! A = sparse ([1 2 3], [2 3 1], [10 20 30], 3, 3);
%! G = digraph (A);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight,   [10; 20; 30]);

## BIST — US-C06: sparse adjacency with isolated trailing nodes
## (sparse (s, t, w, N, N) form).
%!test
%! A = sparse ([1 2], [2 3], [5 10], 5, 5);
%! G = digraph (A);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight,   [5; 10]);

## BIST — US-C06: adjacency form Nodes.Name is an empty column cellstr.
%!test
%! G = digraph (eye (3));
%! assert (isstruct (G.Nodes));
%! assert (G.Nodes.Name, cell (0, 1));

## BIST — US-C06: negative weights permitted.
%!test
%! A = [0 -1; -2 0];
%! G = digraph (A);
%! assert (G.Edges.Weight, [-1; -2]);

## BIST — US-C06: Inf weight permitted (large-weight / shortest-path use).
%!test
%! A = [0 Inf; 1 0];
%! G = digraph (A);
%! assert (G.Edges.Weight, [Inf; 1]);

## BIST — US-C06: Siever-style adjacency (9 nodes, 12 edges) via sparse.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! A = sparse (s, t, 1, 9, 9);
%! G = digraph (A);
%! assert (numnodes (G), 9);
%! assert (numedges (G), 12);

## BIST — US-C06: integer-typed adjacency (int8) coerced to double.
%!test
%! A = int8 ([0 1; 1 0]);
%! G = digraph (A);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 2);
%! assert (isa (G.Edges.Weight, "double"));

## BIST — US-C06: logical adjacency accepted.
%!test
%! A = logical ([0 1; 1 0]);
%! G = digraph (A);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 2);

## BIST — US-C06: non-square adjacency rejected.
%!error <square> digraph ([0 1 0; 1 0 0])
%!error <square> digraph (ones (2, 5))
%!error <square> digraph (ones (4, 3))

## BIST — US-C06: 3-D input rejected (must be a 2-D matrix).
%!error <matrix> digraph (ones (2, 2, 2))

## BIST — US-C06: complex adjacency rejected.
%!error <real> digraph ([0 1i; 0 0])
%!error <real> digraph (complex (eye (3), eye (3)))

## BIST — US-C06: NaN in adjacency rejected.
%!error <NaN> digraph ([0 1; NaN 0])
%!error <NaN> digraph (sparse ([1 2], [2 1], [1 NaN], 2, 2))

## BIST — US-C06: sparse zero-valued structural entries are dropped by
## sparse's own compression, so the digraph has only true nonzero edges.
%!test
%! A = sparse ([1 2], [2 3], [5 0], 3, 3);
%! G = digraph (A);
%! assert (numedges (G), 1);
%! assert (G.Edges.EndNodes, [1 2]);
%! assert (G.Edges.Weight,   5);

## BIST — US-C07: digraph(A, nodenames) from dense adjacency plus cellstr.
## Each nonzero A(i,j) becomes an edge i->j; node names take the place of
## integer indices.
%!test
%! A = [0 1 0; 0 0 1; 1 0 0];
%! names = {"a", "b", "c"};
%! G = digraph (A, names);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight, [1; 1; 1]);

## BIST — US-C07: weights taken from A(i,j) values.
%!test
%! A = [0 1.5 0; 0 0 2.5; 3.5 0 0];
%! names = {"x", "y", "z"};
%! G = digraph (A, names);
%! assert (G.Edges.Weight, [1.5; 2.5; 3.5]);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Nodes.Name, {"x"; "y"; "z"});

## BIST — US-C07: column cellstr nodenames accepted, returned as column.
%!test
%! A = eye (3);
%! names = {"a"; "b"; "c"};
%! G = digraph (A, names);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);

## BIST — US-C07: sparse adjacency + nodenames stays sparse, weights
## round-trip.
%!test
%! A = sparse ([1 2 3], [2 3 1], [10 20 30], 3, 3);
%! names = {"A", "B", "C"};
%! G = digraph (A, names);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.Weight, [10; 20; 30]);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Nodes.Name, {"A"; "B"; "C"});

## BIST — US-C07: sparse adjacency with trailing isolated named nodes.
%!test
%! A = sparse ([1 2], [2 3], [5 10], 5, 5);
%! names = {"a", "b", "c", "d", "e"};
%! G = digraph (A, names);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [5; 10]);

## BIST — US-C07: logical adjacency + nodenames.
%!test
%! A = logical ([0 1; 1 0]);
%! names = {"x", "y"};
%! G = digraph (A, names);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"x"; "y"});
%! assert (isa (G.Edges.Weight, "double"));

## BIST — US-C07: int8 adjacency + nodenames coerced to double weights.
%!test
%! A = int8 ([0 1; 1 0]);
%! names = {"a", "b"};
%! G = digraph (A, names);
%! assert (numedges (G), 2);
%! assert (isa (G.Edges.Weight, "double"));
%! assert (G.Nodes.Name, {"a"; "b"});

## BIST — US-C07: isolated nodes (zero rows/cols) keep their names.
%!test
%! A = zeros (5);
%! A(1,2) = 1;
%! A(2,3) = 2;
%! names = {"p", "q", "r", "s", "t"};
%! G = digraph (A, names);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"p"; "q"; "r"; "s"; "t"});
%! assert (G.Edges.EndNodes, [1 2; 2 3]);

## BIST — US-C07: self-loops preserved alongside named nodes.
%!test
%! A = [1 1; 0 1];
%! names = {"loop1", "loop2"};
%! G = digraph (A, names);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"loop1"; "loop2"});
%! assert (G.Edges.EndNodes, [1 1; 1 2; 2 2]);

## BIST — US-C07: 0x0 adjacency plus empty cellstr yields the empty digraph.
%!test
%! G = digraph (zeros (0, 0), cell (0, 1));
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);
%! assert (G.Nodes.Name, cell (0, 1));

## BIST — US-C07: negative weights accepted with nodenames.
%!test
%! A = [0 -1; -2 0];
%! names = {"neg1", "neg2"};
%! G = digraph (A, names);
%! assert (G.Edges.Weight, [-1; -2]);
%! assert (G.Nodes.Name, {"neg1"; "neg2"});

## BIST — US-C07: Siever-style sparse adjacency with 9 named nodes.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! A = sparse (s, t, 1, 9, 9);
%! names = {"n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9"};
%! G = digraph (A, names);
%! assert (numnodes (G), 9);
%! assert (numedges (G), 12);
%! assert (G.Nodes.Name, {"n1"; "n2"; "n3"; "n4"; "n5"; "n6"; "n7"; "n8"; "n9"});

## BIST — US-C07: length mismatch — too few nodenames for A.
%!error <numel> digraph ([0 1; 1 0], {"a"})
%!error <numel> digraph (eye (5), {"a", "b", "c"})

## BIST — US-C07: length mismatch — too many nodenames for A.
%!error <numel> digraph ([0 1; 1 0], {"a", "b", "c"})

## BIST — US-C07: duplicate nodenames rejected.
%!error <unique> digraph ([0 1; 1 0], {"a", "a"})
%!error <unique> digraph (eye (3), {"a", "b", "a"})

## BIST — US-C07: non-square A with nodenames rejected.
%!error <square> digraph (ones (2, 3), {"a", "b"})
%!error <square> digraph (ones (4, 2), {"a", "b"})

## BIST — US-C07: complex A with nodenames rejected.
%!error <real> digraph ([0 1i; 0 0], {"a", "b"})

## BIST — US-C07: NaN in A with nodenames rejected.
%!error <NaN> digraph ([0 NaN; 1 0], {"a", "b"})
%!error <NaN> digraph (sparse ([1 2], [2 1], [1 NaN], 2, 2), {"a", "b"})

## BIST — US-C07: adjacency form of digraph(A, names) with named nodes
## stores names; Name is always a column cellstr.
%!test
%! A = [0 1; 0 0];
%! G = digraph (A, {"row", "col"});
%! assert (iscolumn (G.Nodes.Name));
%! assert (G.Nodes.Name, {"row"; "col"});
