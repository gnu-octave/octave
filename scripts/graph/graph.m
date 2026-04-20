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

classdef graph

  ## -*- texinfo -*-
  ## @deftypefn  {} {@var{G} =} graph ()
  ## @deftypefnx {} {@var{G} =} graph (@var{N})
  ## @deftypefnx {} {@var{G} =} graph (@var{s}, @var{t})
  ## @deftypefnx {} {@var{G} =} graph (@var{s}, @var{t}, @var{w})
  ## @deftypefnx {} {@var{G} =} graph (@var{s}, @var{t}, @var{w}, @var{nodenames})
  ## @deftypefnx {} {@var{G} =} graph (@var{s}, @var{t}, @var{w}, @var{N})
  ## @deftypefnx {} {@var{G} =} graph (@var{A})
  ## @deftypefnx {} {@var{G} =} graph (@var{A}, "upper")
  ## @deftypefnx {} {@var{G} =} graph (@var{A}, "lower")
  ## Create an undirected graph.
  ##
  ## With no arguments, return an empty undirected graph with zero nodes
  ## and zero edges.
  ##
  ## With a single non-negative integer scalar @var{N}, return an
  ## undirected graph with @var{N} isolated nodes and no edges.
  ##
  ## With two numeric vectors @var{s} and @var{t} of equal length, return
  ## an undirected graph with one edge between @code{@var{s}(i)} and
  ## @code{@var{t}(i)} for each index @var{i}.  Entries of @var{s} and
  ## @var{t} must be positive integers referring to node indices; the
  ## node count is automatically set to @code{max([@var{s}(:); @var{t}(:)])}.
  ## Edges are stored as sorted pairs
  ## @code{(min(s(i),t(i)), max(s(i),t(i)))}, so
  ## @code{@var{G}.Edges.EndNodes} always has the smaller endpoint in
  ## column 1.  Passing two empty vectors is equivalent to
  ## @code{graph()}.  Self-loops @code{s(i) == t(i)} are permitted.
  ## Duplicate unordered endpoint pairs are rejected.
  ##
  ## With three numeric inputs @var{s}, @var{t}, and @var{w}, each edge
  ## @code{(@var{s}(i), @var{t}(i))} is created with weight @code{@var{w}(i)}.
  ## @var{w} may be a scalar (broadcast to every edge) or a vector of
  ## length @code{numel (@var{s})}.  The weights are returned in
  ## @code{@var{G}.Edges.Weight} in edge-index order.
  ##
  ## With a fourth argument @var{nodenames} (a cell array of unique
  ## strings), nodes are named.  The number of nodes is
  ## @code{numel (@var{nodenames})} regardless of the maximum endpoint
  ## index, so isolated named nodes are preserved.  When @var{s} and
  ## @var{t} are numeric, their entries must be integer indices in the
  ## range @code{1:numel (@var{nodenames})}.  When @var{s} and @var{t}
  ## are strings (char row) or cell arrays of strings, each entry is
  ## looked up in @var{nodenames} to resolve its integer index.  Pass
  ## @code{[]} for @var{w} to create an unweighted named graph.
  ## @code{@var{G}.Nodes.Name} returns the node names as a column
  ## cell array.
  ##
  ## When the fourth argument is a non-negative integer scalar @var{N},
  ## the resulting graph has exactly @var{N} nodes.  Any node indices
  ## in @var{s} or @var{t} must lie in the range @code{1:@var{N}}, and
  ## node indices greater than @code{max([@var{s}(:); @var{t}(:)])}
  ## correspond to isolated nodes.  Pass @code{[]} for @var{w} to create
  ## an unweighted graph with @var{N} nodes.
  ##
  ## With a single 2-D numeric or logical matrix @var{A},
  ## @code{graph (@var{A})} treats @var{A} as a symmetric adjacency
  ## matrix.  Each nonzero entry @code{A(i, j) = A(j, i)} creates an
  ## undirected edge @code{@{i, j@}} with weight @code{A(i, j)}; a
  ## nonzero diagonal entry @code{A(i, i)} becomes a self-loop on node
  ## @var{i}.  @var{A} must be real and square; a non-symmetric
  ## @var{A} is rejected.  Sparse input is preserved without
  ## densifying, and integer or logical inputs are coerced to
  ## @code{double}.  Node count is @code{size (@var{A}, 1)}.  The
  ## resulting @var{G}@code{.Edges} always carries a @code{Weight}
  ## column (matrix form implies weighted, MATLAB parity).
  ##
  ## With @code{graph (@var{A}, "upper")}, only the upper triangle of
  ## @var{A} (including its diagonal) is used; the lower triangle is
  ## ignored.  Equivalently,
  ## @code{@var{A}_sym = triu (@var{A}) + triu (@var{A}, 1).'} is
  ## taken as the symmetric adjacency.  @code{graph (@var{A}, "lower")}
  ## likewise uses only the lower triangle.  These flags let you
  ## build an undirected graph from an asymmetric matrix while
  ## selecting which half of the matrix defines the edges.  The flag
  ## is matched case-insensitively.
  ##
  ## @code{graph} is a value class: every mutator returns a new object,
  ## leaving the input unchanged.
  ##
  ## Examples:
  ##
  ## @example
  ## @group
  ## G = graph ();          # empty graph
  ## numnodes (G)           # ==> 0
  ## numedges (G)           # ==> 0
  ##
  ## G = graph (5);         # 5 isolated nodes
  ## numnodes (G)           # ==> 5
  ## numedges (G)           # ==> 0
  ##
  ## s = [1 2 3];
  ## t = [2 3 1];
  ## G = graph (s, t);      # triangle 1-2, 2-3, 1-3
  ## numedges (G)           # ==> 3
  ## G.Edges.EndNodes       # ==> [1 2; 1 3; 2 3]  (sorted pairs)
  ##
  ## w = [1.5 2.5 3.5];
  ## G = graph (s, t, w);   # weighted triangle
  ## G.Edges.Weight         # ==> [1.5; 3.5; 2.5]
  ##
  ## names = @{"a", "b", "c"@};
  ## G = graph (@{"a", "b"@}, @{"b", "c"@}, [10 20], names);
  ## G.Nodes.Name           # ==> @{"a"; "b"; "c"@}
  ##
  ## G = graph ([1 2], [2 3], [1 1], 5);  # 5 nodes, 2 edges, 3 isolated
  ## numnodes (G)           # ==> 5
  ## numedges (G)           # ==> 2
  ##
  ## A = [0 1.5 2.5; 1.5 0 3.5; 2.5 3.5 0];
  ## G = graph (A);                      # symmetric adjacency
  ## G.Edges.Weight                      # ==> [1.5; 2.5; 3.5]
  ##
  ## B = [0 1 2; 0 0 3; 0 0 0];          # upper-triangular
  ## G = graph (B, "upper");             # use upper triangle only
  ## G.Edges.Weight                      # ==> [1; 2; 3]
  ## @end group
  ## @end example
  ##
  ## @seealso{digraph, numnodes, numedges}
  ## @end deftypefn

  properties (Access = private)
    ## Symmetric sparse N-by-N adjacency matrix.  For an undirected
    ## edge @{i, j@} with i != j, both @code{adj_(i, j)} and
    ## @code{adj_(j, i)} hold the edge weight (or 1 for unweighted
    ## graphs).  A self-loop @{i, i@} contributes a single entry at
    ## @code{adj_(i, i)}.  The matrix is always symmetric.
    adj_ = sparse (0, 0);

    ## Optional cellstr of node names.  Empty cell means nodes are
    ## referred to by integer index only.
    nodenames_ = {};

    ## Whether the user supplied explicit edge weights.  When false,
    ## the Edges struct has no Weight field (MATLAB parity).
    has_weights_ = false;
  endproperties

  properties (Dependent, SetAccess = private)
    ## Struct-of-arrays node list.  Fields:
    ##   Name  m-by-1 column cellstr of node names.  When the graph
    ##         was constructed without names, this is an empty
    ##         @code{cell (0, 1)}.
    ## Stands in for MATLAB's @code{table} until Octave has a
    ## built-in table class.
    Nodes

    ## Struct-of-arrays edge list.  Fields:
    ##   EndNodes  m-by-2 matrix of sorted endpoint pairs in
    ##             lexicographic order, with the smaller endpoint in
    ##             column 1.
    ##   Weight    m-by-1 column of edge weights (present only when
    ##             the graph was constructed with explicit weights).
    ## Stands in for MATLAB's @code{table} until Octave has a
    ## built-in table class.
    Edges
  endproperties

  methods

    function G = graph (varargin)

      nargs = numel (varargin);

      if (nargs == 0)
        ## Default constructor: empty graph.  Property defaults apply.
      elseif (nargs == 1)
        arg1 = varargin{1};
        if (isnumeric (arg1) && isscalar (arg1))
          ## Scalar numeric input: node count N.
          if (! (isreal (arg1) && isfinite (arg1) && arg1 >= 0 ...
                 && arg1 == fix (arg1)))
            error ("Octave:invalid-input-arg", ...
                   "graph: N must be a non-negative integer scalar");
          endif
          N = double (arg1);
          G.adj_ = sparse (N, N);
        elseif ((isnumeric (arg1) || islogical (arg1)) ...
                && ismatrix (arg1) && ndims (arg1) == 2)
          ## Non-scalar 2-D input: symmetric adjacency matrix.
          [G.adj_, G.has_weights_] = ...
              build_adj_from_matrix (arg1, "full");
        else
          error ("Octave:invalid-input-arg", ...
                 ["graph: single-argument input must be a ", ...
                  "non-negative integer scalar or a real square ", ...
                  "adjacency matrix"]);
        endif
      elseif (nargs == 2 && ischar (varargin{2}) && isrow (varargin{2}) ...
              && (isnumeric (varargin{1}) || islogical (varargin{1})) ...
              && ismatrix (varargin{1}) && ndims (varargin{1}) == 2 ...
              && ! isvector (varargin{1}))
        ## Adjacency matrix with 'upper' or 'lower' triangle flag.
        ## Dispatch requires arg1 to be a non-vector 2-D matrix so that
        ## graph([1 2], "ab") (bad edge-list call) still falls through
        ## to the edge-list branch below and reports the expected
        ## "S and T must be numeric vectors" error.
        arg1 = varargin{1};
        flag = varargin{2};
        if (! (strcmpi (flag, "upper") || strcmpi (flag, "lower")))
          error ("Octave:invalid-input-arg", ...
                 ["graph: second argument must be 'upper' or 'lower' ", ...
                  "when building from an adjacency matrix"]);
        endif
        if (strcmpi (flag, "upper"))
          [G.adj_, G.has_weights_] = build_adj_from_matrix (arg1, "upper");
        else
          [G.adj_, G.has_weights_] = build_adj_from_matrix (arg1, "lower");
        endif
      elseif (nargs == 2 || nargs == 3)
        ## Edge-list: graph (s, t) or graph (s, t, w).
        s = varargin{1};
        t = varargin{2};
        have_weights = (nargs == 3);
        if (have_weights)
          w = varargin{3};
        endif
        if (! (isnumeric (s) && isreal (s) ...
               && isnumeric (t) && isreal (t)))
          error ("Octave:invalid-input-arg", ...
                 "graph: S and T must be numeric vectors");
        endif
        if (have_weights && ! (isnumeric (w) && isreal (w)))
          error ("Octave:invalid-input-arg", ...
                 "graph: W must be a numeric real vector or scalar");
        endif
        if (! (isvector (s) || isempty (s)) ...
            || ! (isvector (t) || isempty (t)))
          error ("Octave:invalid-input-arg", ...
                 "graph: S and T must be vectors");
        endif
        if (have_weights ...
            && ! (isvector (w) || isempty (w) || isscalar (w)))
          error ("Octave:invalid-input-arg", ...
                 "graph: W must be a vector or scalar");
        endif
        if (numel (s) != numel (t))
          error ("Octave:invalid-input-arg", ...
                 "graph: S and T must have the same length");
        endif
        if (have_weights && ! isscalar (w) && numel (w) != numel (s))
          error ("Octave:invalid-input-arg", ...
                 ["graph: weight vector W must have length ", ...
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
                   "graph: S and T must be positive integer vectors");
          endif
          if (have_weights)
            if (any (isnan (w)))
              error ("Octave:invalid-input-arg", ...
                     "graph: weight vector W must not contain NaN");
            endif
            if (isscalar (w))
              w = repmat (w, numel (s), 1);
            endif
          endif
          N = max (max (s), max (t));
          if (have_weights)
            [G.adj_, G.has_weights_] = build_adj (s, t, w, N, true);
          else
            [G.adj_, G.has_weights_] = build_adj (s, t, [], N, false);
          endif
        endif
      elseif (nargs == 4)
        ## Four-argument constructor.  Dispatch on the fourth argument:
        ##   cellstr         -> graph (s, t, w, nodenames)
        ##   numeric scalar  -> graph (s, t, w, N)
        s = varargin{1};
        t = varargin{2};
        w = varargin{3};
        arg4 = varargin{4};

        if (iscellstr (arg4))
          nn = arg4;
          nn = nn(:);
          if (numel (nn) != numel (unique (nn)))
            error ("Octave:invalid-input-arg", ...
                   "graph: NODENAMES must contain unique strings");
          endif
          N = numel (nn);

          s_idx = __resolve_endpoint__ (s, nn, "S");
          t_idx = __resolve_endpoint__ (t, nn, "T");
          if (numel (s_idx) != numel (t_idx))
            error ("Octave:invalid-input-arg", ...
                   "graph: S and T must have the same length");
          endif

          have_weights = ! (isnumeric (w) && isempty (w));
          if (have_weights)
            if (! (isnumeric (w) && isreal (w)))
              error ("Octave:invalid-input-arg", ...
                     "graph: W must be a numeric real vector or scalar");
            endif
            if (! (isvector (w) || isscalar (w)))
              error ("Octave:invalid-input-arg", ...
                     "graph: W must be a vector or scalar");
            endif
            if (! isscalar (w) && numel (w) != numel (s_idx))
              error ("Octave:invalid-input-arg", ...
                     ["graph: weight vector W must have length ", ...
                      "numel (S) or be a scalar"]);
            endif
            w = double (w(:));
            if (any (isnan (w)))
              error ("Octave:invalid-input-arg", ...
                     "graph: weight vector W must not contain NaN");
            endif
            if (isscalar (w))
              w = repmat (w, numel (s_idx), 1);
            endif
          endif

          G.nodenames_ = nn;
          if (isempty (s_idx))
            G.adj_ = sparse (N, N);
          elseif (have_weights)
            [G.adj_, G.has_weights_] = build_adj (s_idx, t_idx, w, N, true);
          else
            [G.adj_, G.has_weights_] = build_adj (s_idx, t_idx, [], N, false);
          endif
        elseif (isnumeric (arg4) && isscalar (arg4))
          if (! (isreal (arg4) && isfinite (arg4) && arg4 >= 0 ...
                 && arg4 == fix (arg4)))
            error ("Octave:invalid-input-arg", ...
                   "graph: N must be a non-negative integer scalar");
          endif
          N = double (arg4);

          if (! (isnumeric (s) && isreal (s) ...
                 && isnumeric (t) && isreal (t)))
            error ("Octave:invalid-input-arg", ...
                   "graph: S and T must be numeric vectors");
          endif
          if (! (isvector (s) || isempty (s)) ...
              || ! (isvector (t) || isempty (t)))
            error ("Octave:invalid-input-arg", ...
                   "graph: S and T must be vectors");
          endif
          if (numel (s) != numel (t))
            error ("Octave:invalid-input-arg", ...
                   "graph: S and T must have the same length");
          endif
          s = double (s(:));
          t = double (t(:));

          have_weights = ! (isnumeric (w) && isempty (w));
          if (have_weights)
            if (! (isnumeric (w) && isreal (w)))
              error ("Octave:invalid-input-arg", ...
                     "graph: W must be a numeric real vector or scalar");
            endif
            if (! (isvector (w) || isscalar (w)))
              error ("Octave:invalid-input-arg", ...
                     "graph: W must be a vector or scalar");
            endif
            if (! isscalar (w) && numel (w) != numel (s))
              error ("Octave:invalid-input-arg", ...
                     ["graph: weight vector W must have length ", ...
                      "numel (S) or be a scalar"]);
            endif
            w = double (w(:));
            if (any (isnan (w)))
              error ("Octave:invalid-input-arg", ...
                     "graph: weight vector W must not contain NaN");
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
                     "graph: S and T must be positive integer vectors");
            endif
            if (any (s > N) || any (t > N))
              error ("Octave:invalid-input-arg", ...
                     ["graph: S and T entries must not exceed ", ...
                      "the node count N"]);
            endif
          endif

          if (isempty (s))
            G.adj_ = sparse (N, N);
          elseif (have_weights)
            [G.adj_, G.has_weights_] = build_adj (s, t, w, N, true);
          else
            [G.adj_, G.has_weights_] = build_adj (s, t, [], N, false);
          endif
        else
          error ("Octave:invalid-input-arg", ...
                 ["graph: fourth argument must be a cell array ", ...
                  "of strings (node names) or a non-negative ", ...
                  "integer scalar (node count)"]);
        endif
      else
        error ("Octave:invalid-input-arg", ...
               "graph: unsupported number of arguments");
      endif

    endfunction

    function e = get.Edges (G)

      ## Extract unique undirected edges in lex (s, t) order, s <= t.
      ## find(tril(adj_)) walks column-major through the lower triangle:
      ## outer loop is column c (= smaller endpoint s), inner loop is
      ## row r (= larger endpoint t) with r >= c.  This yields each
      ## undirected edge exactly once, in sort-by-s-then-t order.
      [t_end, s_end, w] = find (tril (G.adj_));
      e.EndNodes = [s_end, t_end];
      if (G.has_weights_)
        e.Weight = w;
      endif

    endfunction

    function n = get.Nodes (G)

      if (isempty (G.nodenames_))
        n.Name = cell (0, 1);
      else
        n.Name = G.nodenames_;
      endif

    endfunction

    function n = numnodes (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{n} =} numnodes (@var{G})
      ## Return the number of nodes in the graph @var{G}.
      ## @seealso{graph, numedges}
      ## @end deftypefn

      n = size (G.adj_, 1);

    endfunction

    function m = numedges (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{m} =} numedges (@var{G})
      ## Return the number of edges in the graph @var{G}.  Each
      ## undirected edge is counted once; self-loops count as one edge.
      ## @seealso{graph, numnodes}
      ## @end deftypefn

      m = nnz (tril (G.adj_));

    endfunction

  endmethods

endclassdef

## Helper: build a symmetric sparse adjacency from (s, t[, w]).
## For off-diagonal edges, store the weight at both (s, t) and (t, s).
## For self-loops, store the weight once at (i, i).  Rejects duplicate
## unordered endpoint pairs.
function [A, hw] = build_adj (s, t, w, N, have_weights)

  m = numel (s);

  ## Normalize to (min, max) pairs for duplicate detection.  Since
  ## s_n <= t_n, the resulting sparse matrix only uses the upper
  ## triangle (and diagonal), so the nnz check reliably detects
  ## duplicates of either (s, t) or (t, s).
  s_n = min (s, t);
  t_n = max (s, t);
  p = sparse (s_n, t_n, 1:m, N, N);
  if (nnz (p) != m)
    error ("Octave:invalid-input-arg", ...
           ["graph: duplicate edges in S and T; parallel edges ", ...
            "require the 'multigraph' flag"]);
  endif

  ## Build the symmetric adjacency matrix.  Self-loops contribute a
  ## single (i, i) entry; non-self-loops contribute both (s, t) and
  ## (t, s).  If a weight vector is supplied, entries hold the weight;
  ## otherwise they hold 1.
  if (have_weights)
    vals = w;
  else
    vals = ones (m, 1);
  endif
  sl = (s == t);
  nonself = ! sl;
  ss = [s(nonself); t(nonself); s(sl)];
  tt = [t(nonself); s(nonself); s(sl)];
  vv = [vals(nonself); vals(nonself); vals(sl)];
  A = sparse (ss, tt, vv, N, N);
  hw = have_weights;

endfunction


## Helper: turn a 2-D numeric/logical matrix A into a symmetric sparse
## adjacency suitable for the graph class.  MODE selects how the input is
## interpreted:
##   "full"   A must already be symmetric; use it as-is.
##   "upper"  Use only triu (A); mirror it across the diagonal.
##   "lower"  Use only tril (A); mirror it across the diagonal.
## Returns (A_sparse, hw).  hw is true whenever the matrix is non-empty
## (matrix form implies weighted, MATLAB parity); 0x0 stays unweighted.
function [A_sparse, hw] = build_adj_from_matrix (A, mode)

  if (! isreal (A))
    error ("Octave:invalid-input-arg", ...
           "graph: adjacency matrix A must be real");
  endif
  if (size (A, 1) != size (A, 2))
    error ("Octave:invalid-input-arg", ...
           "graph: adjacency matrix A must be square");
  endif
  if (any (isnan (A(:))))
    error ("Octave:invalid-input-arg", ...
           "graph: adjacency matrix A must not contain NaN");
  endif

  ## Coerce value type to double without densifying sparse inputs.
  if (issparse (A))
    if (! isa (A, "double"))
      A = sparse (double (A));
    endif
  else
    A = sparse (double (A));
  endif

  switch (mode)
    case "full"
      ## Plain graph(A) requires symmetry.  Full adjacency matrices
      ## cannot express parallel edges so no extra work is needed.
      if (! isequal (A, A.'))
        error ("Octave:invalid-input-arg", ...
               ["graph: adjacency matrix A must be symmetric; ", ...
                "use 'upper' or 'lower' to build from a triangle"]);
      endif
      A_sparse = A;
    case "upper"
      ## Strict upper + diagonal, mirrored across the diagonal to keep
      ## adj_ symmetric.  triu(A, 1).' puts the strictly-upper part
      ## into the strictly-lower half.
      U  = triu (A);
      U1 = triu (A, 1);
      A_sparse = U + U1.';
    case "lower"
      L  = tril (A);
      L1 = tril (A, -1);
      A_sparse = L + L1.';
    otherwise
      ## Defensive -- caller validates this, but guard just in case.
      error ("Octave:invalid-input-arg", ...
             "graph: internal error -- unknown triangle mode '%s'", mode);
  endswitch

  ## Matrix form implies weighted (MATLAB parity); 0x0 stays unweighted.
  hw = (size (A_sparse, 1) > 0);

endfunction


## BIST — US-C11 default constructor.
%!test
%! G = graph ();
%! assert (class (G), "graph");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C11 N-node edgeless graph.
%!test
%! G = graph (5);
%! assert (class (G), "graph");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 0);

## BIST — graph(0) is equivalent to graph().
%!test
%! G = graph (0);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — graph(1) is a single isolated node.
%!test
%! G = graph (1);
%! assert (numnodes (G), 1);
%! assert (numedges (G), 0);

## BIST — large N works without densifying.
%!test
%! G = graph (1000);
%! assert (numnodes (G), 1000);
%! assert (numedges (G), 0);

## BIST — value-class semantics: copy is independent (baseline).
%!test
%! G1 = graph (3);
%! G2 = G1;
%! assert (numnodes (G1), 3);
%! assert (numnodes (G2), 3);

## BIST — input validation (scalar-N branch).
%!error <non-negative integer> graph (-3)
%!error <non-negative integer> graph (3.5)
%!error <non-negative integer> graph (Inf)
%!error <non-negative integer> graph (NaN)
%!error <non-negative integer> graph (-1)
## Note: graph([1 2 3]) is a 1x3 matrix, so it dispatches to the adjacency
## path (US-C12) rather than the scalar-N path.  It fails the square check.
%!error <square> graph ([1 2 3])
%!error <unsupported number of arguments> graph (1, 2, 3, 4, 5, 6)

## BIST — US-C11 edge-list constructor with numeric row vectors.
## A triangle 1-2-3 with the pairs in different orientations still
## normalises to sorted (s, t) pairs with s <= t.
%!test
%! s = [1 2 3];
%! t = [2 3 1];
%! G = graph (s, t);
%! assert (class (G), "graph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);

## BIST — edge-list with already-sorted pairs.
%!test
%! G = graph ([1 1 2], [2 3 3]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);

## BIST — column vectors accepted.
%!test
%! G = graph ([1; 2; 3], [2; 3; 1]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);

## BIST — edge (2, 1) stores as (1, 2); adjacency matrix is symmetric.
%!test
%! G = graph ([2 3 1], [1 2 3]);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);

## BIST — isolated trailing nodes are NOT inferred by max(s, t) when
## some node has index less than N; here, max(s, t) = 3 -> 3 nodes.
%!test
%! G = graph ([1 2], [2 3]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);

## BIST — self-loop permitted in edge list; counted once.
%!test
%! G = graph ([1 2 3], [1 2 3]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 1; 2 2; 3 3]);

## BIST — empty edge list yields empty graph.
%!test
%! G = graph ([], []);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — mixed orientations accepted.
%!test
%! G = graph ([1 3 2 4], [2 1 3 5]);
%! assert (numedges (G), 4);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3; 4 5]);

## BIST — input validation errors for edge-list form.
%!error <same length> graph ([1 2 3], [1 2])
%!error <same length> graph ([1 2], [1 2 3])
%!error <numeric vectors> graph ("ab", [1 2])
%!error <numeric vectors> graph ([1 2], "ab")
%!error <vectors> graph (ones (2, 2), ones (2, 2))
%!error <positive integer> graph ([0 1], [1 2])
%!error <positive integer> graph ([1 2], [0 1])
%!error <positive integer> graph ([1.5 2], [2 3])
%!error <positive integer> graph ([1 2], [Inf 3])
%!error <positive integer> graph ([1 2], [NaN 3])

## BIST — duplicate edges rejected (undirected: (1,2) and (2,1) are
## the same edge).
%!error <duplicate edges> graph ([1 2], [2 1])
%!error <duplicate edges> graph ([1 1 2], [2 2 3])

## BIST — US-C11 weighted edge-list: row vector weight.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! ## Weight order follows lex (s, t): edge (1,2)=10, (1,3)=30, (2,3)=20.
%! assert (G.Edges.Weight, [10; 30; 20]);

## BIST — column vector weight.
%!test
%! G = graph ([1 2 3]', [2 3 1]', [10 20 30]');
%! assert (numedges (G), 3);
%! assert (G.Edges.Weight, [10; 30; 20]);

## BIST — scalar weight broadcast to every edge.
%!test
%! G = graph ([1 2 3], [2 3 1], 7);
%! assert (numedges (G), 3);
%! assert (G.Edges.Weight, [7; 7; 7]);

## BIST — unweighted graph's Edges has no Weight field.
%!test
%! G = graph ([1 2], [2 3]);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (! isfield (E, "Weight"));

## BIST — weighted graph's Edges has Weight field.
%!test
%! G = graph ([1 2], [2 3], [1 2]);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (isfield (E, "Weight"));

## BIST — negative weights permitted.
%!test
%! G = graph ([1 2], [2 3], [-1.5 -2.5]);
%! assert (G.Edges.Weight, [-1.5; -2.5]);

## BIST — weighted self-loop.
%!test
%! G = graph ([1 2], [1 2], [5 10]);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 1; 2 2]);
%! assert (G.Edges.Weight, [5; 10]);

## BIST — reverse-ordered input re-sorts into lex (s, t).
%!test
%! G = graph ([3 2 1], [1 3 2], [30 20 10]);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 30; 20]);

## BIST — empty endpoints with empty weight.
%!test
%! G = graph ([], [], []);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — empty endpoints with scalar weight.
%!test
%! G = graph ([], [], 5);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — weight validation errors.
%!error <length> graph ([1 2 3], [2 3 1], [10 20])
%!error <numeric real> graph ([1 2], [2 3], "ab")
%!error <numeric real> graph ([1 2], [2 3], [1+1i, 2])
%!error <NaN> graph ([1 2], [2 3], [1 NaN])
%!error <vector> graph ([1 2], [2 3], ones (2, 2))

## BIST — US-C11 named edge-list with numeric endpoints.
%!test
%! names = {"alpha", "beta", "gamma"};
%! G = graph ([1 2 3], [2 3 1], [10 20 30], names);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"alpha"; "beta"; "gamma"});
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 30; 20]);

## BIST — named edge-list with cellstr endpoints.
%!test
%! names = {"alpha", "beta", "gamma"};
%! G = graph ({"alpha", "beta", "gamma"}, {"beta", "gamma", "alpha"}, ...
%!            [10 20 30], names);
%! assert (numnodes (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 30; 20]);

## BIST — isolated named node preserved.
%!test
%! G = graph ([1 2], [2 3], [1 2], {"a", "b", "c", "d"});
%! assert (numnodes (G), 4);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"a"; "b"; "c"; "d"});

## BIST — column cellstr nodenames accepted.
%!test
%! G = graph ([1 2], [2 3], [1 1], {"x"; "y"; "z"});
%! assert (G.Nodes.Name, {"x"; "y"; "z"});

## BIST — row cellstr nodenames stored as column.
%!test
%! G = graph ([1 2], [2 3], [1 1], {"x", "y", "z"});
%! assert (size (G.Nodes.Name), [3 1]);
%! assert (G.Nodes.Name, {"x"; "y"; "z"});

## BIST — duplicate nodenames rejected.
%!error <unique> graph ([1 2], [2 3], [1 1], {"a", "a", "b"})

## BIST — non-cellstr nodenames rejected.
%!error graph ([1 2], [2 3], [1 1], [1 2 3])

## BIST — cellstr endpoint not in nodenames rejected.
%!error graph ({"a", "b"}, {"b", "z"}, [1 1], {"a", "b", "c"})

## BIST — numeric endpoint out of range with named nodes.
%!error graph ([1 4], [2 3], [1 1], {"a", "b", "c"})

## BIST — empty edges with nodenames gives N isolated named nodes.
%!test
%! G = graph ([], [], [], {"a", "b", "c"});
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});

## BIST — [] weight creates unweighted named graph.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! assert (numedges (G), 2);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (! isfield (E, "Weight"));

## BIST — Nodes property exists on unnamed graph and equals cell(0,1).
%!test
%! G = graph ([1 2], [2 3]);
%! assert (G.Nodes.Name, cell (0, 1));

## BIST — US-C11 integer-N form: graph (s, t, w, N).
%!test
%! G = graph ([1 2], [2 3], [1.5 2.5], 10);
%! assert (numnodes (G), 10);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [1.5; 2.5]);
%! assert (G.Nodes.Name, cell (0, 1));

## BIST — N == max(s, t) works without creating isolated trailing nodes.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30], 3);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);

## BIST — N > max(s, t) creates isolated trailing nodes.
%!test
%! G = graph ([1 2], [2 3], [1 1], 5);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);

## BIST — scalar weight broadcast under N form.
%!test
%! G = graph ([1 2 3], [2 3 1], 7, 5);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 3);
%! assert (G.Edges.Weight, [7; 7; 7]);

## BIST — W = [] with N form produces unweighted graph.
%!test
%! G = graph ([1 2], [2 3], [], 5);
%! assert (numedges (G), 2);
%! E = G.Edges;
%! assert (! isfield (E, "Weight"));

## BIST — empty endpoints with N > 0 yields N isolated nodes.
%!test
%! G = graph ([], [], [], 5);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 0);

## BIST — N = 0 with empty edges equals graph().
%!test
%! G = graph ([], [], [], 0);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — large N works without densifying.
%!test
%! G = graph ([1 2], [2 3], [1 1], 1000);
%! assert (numnodes (G), 1000);
%! assert (numedges (G), 2);

## BIST — N form error: endpoint exceeds N.
%!error <exceed> graph ([1 5], [2 3], [1 1], 4)
%!error <exceed> graph ([1 2], [2 6], [1 1], 4)

## BIST — N form error: invalid N.
%!error <non-negative integer> graph ([1 2], [2 3], [1 1], -1)
%!error <non-negative integer> graph ([1 2], [2 3], [1 1], 3.5)
%!error <non-negative integer> graph ([1 2], [2 3], [1 1], Inf)
%!error <non-negative integer> graph ([1 2], [2 3], [1 1], NaN)

## BIST — 4th argument wrong type.
%!error <fourth argument> graph ([1 2], [2 3], [1 1], [3 4])
%!error <fourth argument> graph ([1 2], [2 3], [1 1], true)

## BIST — value-class semantics preserved for weighted named graph.
%!test
%! G1 = graph ([1 2], [2 3], [10 20], {"a", "b", "c"});
%! G2 = G1;
%! assert (G1.Edges.Weight, [10; 20]);
%! assert (G2.Edges.Weight, [10; 20]);

## BIST — mixed (3-cycle) with nodenames and string endpoints.
%!test
%! G = graph ({"a"; "b"; "c"}, {"b"; "c"; "a"}, [1.5; 2.5; 3.5], ...
%!            {"a", "b", "c"});
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1.5; 3.5; 2.5]);

## BIST — US-C12 graph(A): dense symmetric adjacency, triangle.
%!test
%! A = [0 1 1; 1 0 1; 1 1 0];
%! G = graph (A);
%! assert (class (G), "graph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1; 1; 1]);

## BIST — graph(A): symmetric adjacency with real weights, lex order.
%!test
%! A = [0 1.5 2.5; 1.5 0 3.5; 2.5 3.5 0];
%! G = graph (A);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1.5; 2.5; 3.5]);

## BIST — graph(A) rejects a non-symmetric A.
%!error <symmetric> graph ([0 1 0; 0 0 1; 1 0 0])
%!error <symmetric> graph ([0 1; 0 0])
%!error <symmetric> graph ([0 1 2; 1 0 3; 2 4 0])

## BIST — graph(A) rejects non-symmetric sparse A.
%!error <symmetric> graph (sparse ([1 2], [2 3], [1 1], 3, 3))

## BIST — graph(A, 'upper') uses only the upper triangle.
%!test
%! A = [0 1.5 2.5; 0 0 3.5; 0 0 0];  ## upper-triangular
%! G = graph (A, "upper");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1.5; 2.5; 3.5]);

## BIST — graph(A, 'lower') uses only the lower triangle.
%!test
%! A = [0 0 0; 1.5 0 0; 2.5 3.5 0];  ## lower-triangular
%! G = graph (A, "lower");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1.5; 2.5; 3.5]);

## BIST — 'upper' ignores whatever is in the lower triangle.
%!test
%! A = [0 1 2; 99 0 3; 88 77 0];
%! G = graph (A, "upper");
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1; 2; 3]);

## BIST — 'lower' ignores whatever is in the upper triangle.
%!test
%! A = [0 99 88; 1 0 77; 2 3 0];
%! G = graph (A, "lower");
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1; 2; 3]);

## BIST — self-loop via diagonal in symmetric matrix.
%!test
%! A = [2 1 0; 1 0 0; 0 0 0];
%! G = graph (A);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 1; 1 2]);
%! assert (G.Edges.Weight, [2; 1]);

## BIST — 'upper' preserves diagonal self-loop.
%!test
%! A = [5 1 0; 0 0 0; 0 0 0];
%! G = graph (A, "upper");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 1; 1 2]);
%! assert (G.Edges.Weight, [5; 1]);

## BIST — 'lower' preserves diagonal self-loop.
%!test
%! A = [5 0 0; 1 0 0; 0 0 0];
%! G = graph (A, "lower");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 1; 1 2]);
%! assert (G.Edges.Weight, [5; 1]);

## BIST — diagonal-only (all self-loops) symmetric adjacency.
%!test
%! A = [2 0 0; 0 3 0; 0 0 5];
%! G = graph (A);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 1; 2 2; 3 3]);
%! assert (G.Edges.Weight, [2; 3; 5]);

## BIST — sparse symmetric A stays sparse-friendly.
%!test
%! A = sparse (5, 5);
%! A(1, 2) = 10; A(2, 1) = 10;
%! A(2, 3) = 20; A(3, 2) = 20;
%! G = graph (A);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [10; 20]);

## BIST — sparse A + A.' pattern with trailing isolated nodes.
%!test
%! A = sparse ([1 2], [2 3], [10 20], 10, 10);
%! A = A + A.';
%! G = graph (A);
%! assert (numnodes (G), 10);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [10; 20]);

## BIST — all-zeros NxN yields N isolated nodes, zero edges.
%!test
%! G = graph (zeros (4));
%! assert (numnodes (G), 4);
%! assert (numedges (G), 0);

## BIST — 0x0 adjacency yields empty graph, unweighted.
%!test
%! G = graph (zeros (0, 0));
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);
%! E = G.Edges;
%! assert (! isfield (E, "Weight"));

## BIST — logical adjacency coerced to double weights.
%!test
%! A = logical ([0 1 0; 1 0 1; 0 1 0]);
%! G = graph (A);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (class (G.Edges.Weight), "double");

## BIST — int8 adjacency coerced to double weights.
%!test
%! A = int8 ([0 1 0; 1 0 1; 0 1 0]);
%! G = graph (A);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (class (G.Edges.Weight), "double");

## BIST — errors: non-square A rejected.
%!error <square> graph ([1 2 3; 4 5 6])
%!error <square> graph ([1 2 3; 4 5 6], "upper")
%!error <square> graph ([1 2; 3 4; 5 6])
%!error <square> graph ([1 2; 3 4; 5 6], "lower")

## BIST — error: complex adjacency.
%!error <real> graph ([0 1i; -1i 0])
%!error <real> graph ([0 1i; 0 0], "upper")

## BIST — error: NaN in adjacency.
%!error <NaN> graph ([0 NaN; NaN 0])
%!error <NaN> graph ([0 NaN; 0 0], "upper")
%!error <NaN> graph ([0 0; NaN 0], "lower")

## BIST — error: 3-D input rejected.
%!error <square adjacency matrix> graph (ones (2, 2, 2))

## BIST — error: unknown triangle flag.
%!error <'upper' or 'lower'> graph ([0 1; 1 0], "middle")
%!error <'upper' or 'lower'> graph ([0 1; 1 0], "Upp")

## BIST — graph(A, 'Upper') is case-insensitive.
%!test
%! A = [0 1 2; 0 0 3; 0 0 0];
%! G = graph (A, "Upper");
%! assert (numedges (G), 3);
%! G2 = graph (A, "UPPER");
%! assert (numedges (G2), 3);

## BIST — graph(A, 'Lower') is case-insensitive.
%!test
%! A = [0 0 0; 1 0 0; 2 3 0];
%! G = graph (A, "Lower");
%! assert (numedges (G), 3);
%! G2 = graph (A, "LOWER");
%! assert (numedges (G2), 3);

## BIST — 5x5 sparse mostly-zero symmetric matrix.
%!test
%! A = sparse (5, 5);
%! A(1, 2) = 1; A(2, 1) = 1;
%! G = graph (A);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 1);
%! assert (G.Edges.EndNodes, [1 2]);

## BIST — negative weights accepted.
%!test
%! A = [0 -1.5 0; -1.5 0 -2.5; 0 -2.5 0];
%! G = graph (A);
%! assert (G.Edges.Weight, [-1.5; -2.5]);

## BIST — Inf weights accepted (NaN rejected separately).
%!test
%! A = [0 Inf; Inf 0];
%! G = graph (A);
%! assert (numedges (G), 1);
%! assert (G.Edges.Weight, Inf);

## BIST — Nodes.Name is empty cellstr when constructed from adjacency alone.
%!test
%! G = graph ([0 1; 1 0]);
%! assert (G.Nodes.Name, cell (0, 1));

## BIST — matrix form implies weighted: Edges has Weight field.
%!test
%! G = graph ([0 1; 1 0]);
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (isfield (E, "Weight"));

## BIST — 'upper' with an all-zero upper triangle gives an edgeless graph.
%!test
%! A = [0 0 0; 1 0 0; 2 3 0];
%! G = graph (A, "upper");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);

## BIST — 'lower' with an all-zero lower triangle gives an edgeless graph.
%!test
%! A = [0 1 2; 0 0 3; 0 0 0];
%! G = graph (A, "lower");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);

## BIST — 0x0 adjacency with 'upper'/'lower'.
%!test
%! G = graph (zeros (0, 0), "upper");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);
%!test
%! G = graph (zeros (0, 0), "lower");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — sparse adjacency with 'upper' pulls only strict-plus-diag upper.
%!test
%! A = sparse (3, 3);
%! A(1, 2) = 10; A(1, 3) = 20; A(2, 3) = 30;
%! A(3, 1) = 99; A(3, 2) = 99;  ## noise in lower, ignored by 'upper'
%! G = graph (A, "upper");
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — sparse adjacency with 'lower'.
%!test
%! A = sparse (3, 3);
%! A(2, 1) = 10; A(3, 1) = 20; A(3, 2) = 30;
%! A(1, 2) = 99; A(1, 3) = 99;  ## noise in upper, ignored by 'lower'
%! G = graph (A, "lower");
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — 1x1 matrix input: scalar dispatch is N.  [0] -> N=0 path.
%!test
%! G = graph (0);
%! assert (numnodes (G), 0);

## BIST — symmetric 1x1 with zero diag is a single isolated node (N=0 path
## actually — [0] is scalar, so go through N).  Done via explicit 2x2.
%!test
%! G = graph ([0 0; 0 0]);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 0);

## BIST — self-loop-only symmetric adjacency via 'upper'/'lower' agree.
%!test
%! A = diag ([1 2 3]);
%! Gu = graph (A, "upper");
%! Gl = graph (A, "lower");
%! G  = graph (A);
%! assert (numedges (Gu), 3);
%! assert (numedges (Gl), 3);
%! assert (numedges (G),  3);
%! assert (Gu.Edges.EndNodes, [1 1; 2 2; 3 3]);
%! assert (Gl.Edges.EndNodes, [1 1; 2 2; 3 3]);
%! assert (G.Edges.EndNodes,  [1 1; 2 2; 3 3]);
%! assert (Gu.Edges.Weight, [1; 2; 3]);
%! assert (Gl.Edges.Weight, [1; 2; 3]);
%! assert (G.Edges.Weight,  [1; 2; 3]);

## BIST — graph(A) from graph(A, 'upper') of a symmetric A agree.
%!test
%! A = [0 1.5 2.5; 1.5 0 3.5; 2.5 3.5 0];
%! G1 = graph (A);
%! G2 = graph (A, "upper");
%! G3 = graph (A, "lower");
%! assert (G1.Edges.EndNodes, G2.Edges.EndNodes);
%! assert (G1.Edges.Weight,   G2.Edges.Weight);
%! assert (G1.Edges.EndNodes, G3.Edges.EndNodes);
%! assert (G1.Edges.Weight,   G3.Edges.Weight);
