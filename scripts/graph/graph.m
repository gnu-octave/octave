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
          if (! (isreal (arg1) && isfinite (arg1) && arg1 >= 0 ...
                 && arg1 == fix (arg1)))
            error ("Octave:invalid-input-arg", ...
                   "graph: N must be a non-negative integer scalar");
          endif
          N = double (arg1);
          G.adj_ = sparse (N, N);
        else
          error ("Octave:invalid-input-arg", ...
                 "graph: N must be a non-negative integer scalar");
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
%!error <non-negative integer> graph ([1 2 3])
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
