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
  ## @deftypefnx {} {@var{G} =} graph (@var{EdgeTable})
  ## @deftypefnx {} {@var{G} =} graph (@var{EdgeTable}, @var{NodeTable})
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
  ## With a single struct @var{EdgeTable}, build a graph from the
  ## fields of the struct.  @var{EdgeTable} must have an
  ## @code{EndNodes} field holding an m-by-2 matrix of node indices or
  ## a cell array of strings.  An optional @code{Weight} field gives a
  ## per-edge weight (a vector of length @var{m}).  Any other fields
  ## are preserved as extra edge-attribute columns; each must have
  ## @var{m} rows.  Edges are re-sorted into lexicographic
  ## @code{(min, max)} order with the smaller endpoint in column 1,
  ## and every extra edge column is reordered to match.  Duplicate
  ## unordered endpoint pairs are rejected (a future @qcode{'multigraph'}
  ## flag will permit parallel edges).  Self-loops @code{s == t} are
  ## permitted and appear as a single row in @code{Edges.EndNodes}.
  ##
  ## With a second struct @var{NodeTable}, the node set is taken from
  ## @var{NodeTable}.  A @code{Name} field (a cell array of unique
  ## strings) supplies node names and determines the node count.
  ## Any other fields become extra node-attribute columns.  When
  ## @code{EndNodes} is numeric, its entries are 1-based indices into
  ## @var{NodeTable}; when @code{EndNodes} is a cellstr, each entry is
  ## looked up in @code{@var{NodeTable}.Name} (if provided) or in a
  ## name list inferred from the first appearance of each string in
  ## @code{EndNodes}.  If @var{NodeTable} has no @code{Name} field,
  ## the node count is taken from the length of its other columns.
  ##
  ## @code{graph} is a value class: every mutator returns a new object,
  ## leaving the input unchanged.
  ##
  ## @strong{Properties}:
  ##
  ## @code{@var{G}.Nodes} is a struct standing in for MATLAB's @code{table}.
  ## It always has a @code{Name} field, a column cell array of strings
  ## giving each node's name.  For graphs constructed without names the
  ## field is an empty @code{cell (0, 1)}.  Any columns supplied through
  ## the @var{NodeTable} form are preserved as extra fields (in the order
  ## they were declared).
  ##
  ## @code{@var{G}.Edges} is a struct standing in for MATLAB's @code{table}.
  ## It always has an @code{EndNodes} field, an @code{m}-by-2 numeric
  ## matrix whose row @var{i} is the endpoint pair
  ## @code{[min, max]} of edge @var{i} in lexicographic order (the
  ## smaller endpoint is always in column 1).  When the graph was built
  ## with explicit weights (either through @var{w} or through an
  ## adjacency matrix), a @code{Weight} field is also present as an
  ## @code{m}-by-1 column.  Any extra edge-attribute columns supplied
  ## through the @var{EdgeTable} form are preserved (in declaration
  ## order) after @code{Weight}.  Both properties are read-only
  ## (@code{SetAccess = private}); use @code{addedge}, @code{rmedge},
  ## and friends to modify the graph.
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
  ##
  ## ET.EndNodes = [1 2; 2 3; 1 3];
  ## ET.Weight   = [10; 20; 30];
  ## ET.Label    = @{"a"; "b"; "c"@};
  ## NT.Name     = @{"x"; "y"; "z"@};
  ## G = graph (ET, NT);    # EdgeTable + NodeTable form
  ## G.Edges.Weight         # ==> [10; 30; 20]   (lex-ordered)
  ## G.Nodes.Name           # ==> @{"x"; "y"; "z"@}
  ## @end group
  ## @end example
  ##
  ## @seealso{digraph, numnodes, numedges, ismultigraph, addnode, addedge, rmnode, rmedge, reordernodes, subgraph, neighbors, degree, findnode, findedge, edgecount, adjacency, incidence, laplacian, bfsearch, dfsearch, conncomp, biconncomp, condensation, toposort}
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

    ## Extra edge-attribute columns supplied by the user via the
    ## @code{graph(ET)} or @code{graph(ET, NT)} EdgeTable form.
    ## Each field is stored in lexicographic
    ## @code{(min-endpoint, max-endpoint)} edge order so that
    ## @code{get.Edges} can return it directly.  Weight is @emph{not}
    ## stored here (it lives in @code{adj_}).
    edge_attrs_ = struct ();

    ## Extra node-attribute columns supplied by the user via the
    ## NodeTable form.  Each field is stored in node-index order.
    ## Name is @emph{not} stored here (it lives in @code{nodenames_}).
    node_attrs_ = struct ();
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
      elseif ((nargs == 1 && isstruct (varargin{1})) ...
              || (nargs == 2 && isstruct (varargin{1}) ...
                  && isstruct (varargin{2})))
        ## EdgeTable (and optional NodeTable) constructor.
        ## graph (ET) or graph (ET, NT).  ET is a scalar struct
        ## with an EndNodes field (numeric m-by-2 or cellstr m-by-2)
        ## and an optional Weight field; any other fields become extra
        ## edge-attribute columns.  NT is a scalar struct with an
        ## optional Name field; any other fields become extra
        ## node-attribute columns.  Edges are re-sorted into
        ## lexicographic (min-endpoint, max-endpoint) order with the
        ## smaller endpoint in column 1, and every extra edge column is
        ## reordered to match.
        ET = varargin{1};
        have_nt = (nargs == 2);
        if (have_nt)
          NT = varargin{2};
        endif

        if (! isscalar (ET))
          error ("Octave:invalid-input-arg", ...
                 "graph: EdgeTable must be a scalar struct");
        endif
        if (have_nt && ! isscalar (NT))
          error ("Octave:invalid-input-arg", ...
                 "graph: NodeTable must be a scalar struct");
        endif
        if (! isfield (ET, "EndNodes"))
          error ("Octave:invalid-input-arg", ...
                 "graph: EdgeTable must have an EndNodes field");
        endif

        EN = ET.EndNodes;
        if (! (isnumeric (EN) || iscellstr (EN)))
          error ("Octave:invalid-input-arg", ...
                 ["graph: EndNodes must be a numeric matrix or ", ...
                  "a cell array of strings"]);
        endif
        if (ndims (EN) != 2 || size (EN, 2) != 2)
          error ("Octave:invalid-input-arg", ...
                 ["graph: EndNodes must be a 2-D matrix with ", ...
                  "exactly two columns"]);
        endif
        m = size (EN, 1);
        is_cell_end = iscellstr (EN);

        s_idx = zeros (0, 1);
        t_idx = zeros (0, 1);   # resolved after NT ingestion for cellstr
        if (! is_cell_end && m > 0)
          v = EN(:);
          if (! isreal (v) ...
              || any (! isfinite (v) | v < 1 | v != fix (v)))
            error ("Octave:invalid-input-arg", ...
                   ["graph: numeric EndNodes entries must be ", ...
                    "positive integer node indices"]);
          endif
          s_idx = double (EN(:, 1));
          t_idx = double (EN(:, 2));
        endif

        have_weights = isfield (ET, "Weight");
        w_vec = [];
        if (have_weights)
          w_vec = ET.Weight;
          if (! (isnumeric (w_vec) && isreal (w_vec)))
            error ("Octave:invalid-input-arg", ...
                   "graph: Weight must be a numeric real vector");
          endif
          if (! (isvector (w_vec) || isempty (w_vec)))
            error ("Octave:invalid-input-arg", ...
                   "graph: Weight must be a vector");
          endif
          w_vec = double (w_vec(:));
          if (numel (w_vec) != m)
            error ("Octave:invalid-input-arg", ...
                   ["graph: Weight length must match the number ", ...
                    "of rows in EndNodes"]);
          endif
          if (any (isnan (w_vec)))
            error ("Octave:invalid-input-arg", ...
                   "graph: Weight must not contain NaN");
          endif
        endif

        ## Extra edge columns: every ET field except EndNodes and
        ## Weight.  Row count must equal m.
        e_attrs = struct ();
        ef = fieldnames (ET);
        for ii = 1:numel (ef)
          fn_i = ef{ii};
          if (strcmp (fn_i, "EndNodes") || strcmp (fn_i, "Weight"))
            continue;
          endif
          v_i = ET.(fn_i);
          if (size (v_i, 1) != m)
            error ("Octave:invalid-input-arg", ...
                   ["graph: EdgeTable column %s length must ", ...
                    "match EndNodes"], fn_i);
          endif
          e_attrs.(fn_i) = v_i;
        endfor

        ## Ingest NodeTable.
        nodenames_out = {};
        n_attrs = struct ();
        N = 0;
        if (have_nt)
          nf = fieldnames (NT);
          N_from_nt = -1;
          if (numel (nf) > 0)
            N_from_nt = size (NT.(nf{1}), 1);
            for ii = 2:numel (nf)
              if (size (NT.(nf{ii}), 1) != N_from_nt)
                error ("Octave:invalid-input-arg", ...
                       ["graph: NodeTable columns must all ", ...
                        "have the same length"]);
              endif
            endfor
          endif
          if (isfield (NT, "Name"))
            nm = NT.Name;
            if (! iscellstr (nm))
              error ("Octave:invalid-input-arg", ...
                     ["graph: NodeTable Name must be a cell ", ...
                      "array of strings"]);
            endif
            nm = nm(:);
            if (numel (nm) != numel (unique (nm)))
              error ("Octave:invalid-input-arg", ...
                     ["graph: NodeTable Name must contain ", ...
                      "unique strings"]);
            endif
            nodenames_out = nm;
            N = numel (nm);
          elseif (N_from_nt >= 0)
            N = N_from_nt;
          endif
          ## Extra node columns (everything except Name).
          for ii = 1:numel (nf)
            fn_i = nf{ii};
            if (strcmp (fn_i, "Name"))
              continue;
            endif
            v_i = NT.(fn_i);
            if (size (v_i, 1) != N)
              error ("Octave:invalid-input-arg", ...
                     ["graph: NodeTable column %s length must ", ...
                      "match the node count"], fn_i);
            endif
            n_attrs.(fn_i) = v_i;
          endfor
        endif

        ## Resolve cellstr endpoints; set N if not already set.
        if (is_cell_end && m > 0)
          EN_s = EN(:, 1);
          EN_t = EN(:, 2);
          if (have_nt && ! isempty (nodenames_out))
            s_idx = __resolve_endpoint__ (EN_s, nodenames_out, "S");
            t_idx = __resolve_endpoint__ (EN_t, nodenames_out, "T");
          else
            ## Infer names in first-appearance order across
            ## [EN_s; EN_t].
            all_endpoints = [EN_s; EN_t];
            inferred = unique (all_endpoints, "stable");
            inferred = inferred(:);
            nodenames_out = inferred;
            N = numel (nodenames_out);
            s_idx = __resolve_endpoint__ (EN_s, nodenames_out, "S");
            t_idx = __resolve_endpoint__ (EN_t, nodenames_out, "T");
          endif
        elseif (! is_cell_end && m > 0)
          if (have_nt)
            if (any (s_idx > N) || any (t_idx > N))
              error ("Octave:invalid-input-arg", ...
                     ["graph: EndNodes indices must not exceed ", ...
                      "the NodeTable node count"]);
            endif
          else
            N = max (max (s_idx), max (t_idx));
          endif
        endif

        ## Normalize to unordered pair (smaller, bigger) for duplicate
        ## detection and lex-order permutation.  Since graph is
        ## undirected, (s, t) and (t, s) represent the same edge; we
        ## detect and reject parallel edges regardless of input order.
        if (m > 0)
          s_n = min (s_idx, t_idx);
          t_n = max (s_idx, t_idx);

          ## Build lower-triangular sparse index matrix: find() walks
          ## column-major which yields lex (s_n, t_n) order with
          ## s_n <= t_n.  Using the index sequence 1:m (not weights)
          ## avoids a false duplicate report when a user-supplied
          ## weight is zero.
          p = sparse (t_n, s_n, 1:m, N, N);
          if (nnz (p) != m)
            error ("Octave:invalid-input-arg", ...
                   ["graph: EdgeTable contains duplicate edges; ", ...
                    "parallel edges require the 'multigraph' flag"]);
          endif

          ef2 = fieldnames (e_attrs);
          if (! isempty (ef2))
            [~, ~, perm] = find (p);
            for ii = 1:numel (ef2)
              fn_i = ef2{ii};
              e_attrs.(fn_i) = e_attrs.(fn_i)(perm, :);
            endfor
          endif
        endif

        ## Build adj_ and commit state.  Weight is NOT permuted: it
        ## is placed into adj_ at its (s_i, t_i) cell, and get.Edges
        ## retrieves it in lex order via find (tril (adj_)).  The ET
        ## branch already ran its own duplicate-detection sparse
        ## (needed for the edge-attribute permutation), so pass
        ## skip_dup_check=true to avoid a second O(m log m) sparse
        ## build inside build_adj.
        if (m > 0)
          if (have_weights)
            [G.adj_, G.has_weights_] = ...
                build_adj (s_idx, t_idx, w_vec, N, true, true);
          else
            [G.adj_, G.has_weights_] = ...
                build_adj (s_idx, t_idx, [], N, false, true);
          endif
        else
          G.adj_ = sparse (N, N);
        endif
        G.nodenames_ = nodenames_out;
        G.edge_attrs_ = e_attrs;
        G.node_attrs_ = n_attrs;

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
      ## The (:) coercion normalises the shape to m-by-1 even when
      ## @code{adj_} is 0-by-0 (where find returns 0-by-0 arrays),
      ## so @code{EndNodes} is reliably m-by-2 and @code{Weight} is
      ## m-by-1 across every constructor form -- MATLAB parity.
      e.EndNodes = [s_end(:), t_end(:)];
      if (G.has_weights_)
        e.Weight = w(:);
      endif

      ## Merge any extra edge-attribute columns supplied via the
      ## EdgeTable constructor.  Stored in lex-order already.
      efn = fieldnames (G.edge_attrs_);
      for ii = 1:numel (efn)
        e.(efn{ii}) = G.edge_attrs_.(efn{ii});
      endfor

    endfunction

    function n = get.Nodes (G)

      if (isempty (G.nodenames_))
        n.Name = cell (0, 1);
      else
        n.Name = G.nodenames_;
      endif

      ## Merge any extra node-attribute columns supplied via the
      ## NodeTable constructor.
      nfn = fieldnames (G.node_attrs_);
      for ii = 1:numel (nfn)
        n.(nfn{ii}) = G.node_attrs_.(nfn{ii});
      endfor

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

    function tf = ismultigraph (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{tf} =} ismultigraph (@var{G})
      ## Return @code{false} for an undirected @code{graph} object.
      ##
      ## The undirected @code{graph} class in this Octave build does not
      ## accept a @qcode{'multigraph'} constructor flag and therefore
      ## cannot store parallel edges, so @code{ismultigraph} always
      ## returns @code{false}.  The method is provided for parity with
      ## @code{digraph.ismultigraph} so that generic code which works on
      ## either class can call @code{ismultigraph (@var{G})} without a
      ## type check.
      ## @seealso{graph, numedges, numnodes}
      ## @end deftypefn

      tf = false;

    endfunction

    function H = addnode (G, newnodes)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} addnode (@var{G}, @var{N})
      ## @deftypefnx {} {@var{H} =} addnode (@var{G}, @var{NodeNames})
      ## @deftypefnx {} {@var{H} =} addnode (@var{G}, @var{NodeTable})
      ## Append nodes to the undirected graph @var{G} and return the
      ## new graph @var{H}.  See @code{help addnode} for the full
      ## description of the three call forms.  New nodes have no
      ## incident edges, so the adjacency of existing nodes is
      ## preserved as-is.
      ## @seealso{graph, addedge, rmnode, rmedge, numnodes, findnode}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to addnode: expected 2 arguments");
      endif
      H = G;
      [H.adj_, H.nodenames_, H.node_attrs_] = ...
        __addnode_impl__ (G.adj_, G.nodenames_, G.node_attrs_, newnodes);

    endfunction

    function H = addedge (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} addedge (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{H} =} addedge (@var{G}, @var{s}, @var{t}, @var{w})
      ## @deftypefnx {} {@var{H} =} addedge (@var{G}, @var{EdgeTable})
      ## Append edges to the undirected graph @var{G} and return the
      ## new graph @var{H}.  See @code{help addedge} for the full
      ## description of the three call forms.  Endpoints that refer to
      ## node names not already present in @var{G} cause new nodes to
      ## be appended.  The undirected @code{graph} class does not
      ## support parallel edges, so adding an edge already present in
      ## @var{G} is an error.
      ## @seealso{graph, addnode, rmnode, rmedge, numedges, findedge}
      ## @end deftypefn

      if (nargin < 2 || nargin > 4)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to addedge: expected 2, 3, or 4 arguments");
      endif

      Nold = size (G.adj_, 1);
      have_existing_edges = (numedges (G) > 0);

      [s_idx, t_idx, w_vec, N_new, names_out, nattrs_out, hw_out] = ...
        __addedge_impl__ (G.nodenames_, G.node_attrs_, G.has_weights_, ...
                          Nold, have_existing_edges, varargin{:});

      m_new = numel (s_idx);

      ## Extend edge-attribute columns with default rows for the new
      ## edges.
      eattrs_out = G.edge_attrs_;
      efn = fieldnames (eattrs_out);
      for ii = 1:numel (efn)
        fn_i = efn{ii};
        col = eattrs_out.(fn_i);
        eattrs_out.(fn_i) = [col; graph_default_edge_rows(col, m_new)];
      endfor

      H = G;
      H.nodenames_ = names_out;
      H.node_attrs_ = nattrs_out;
      H.has_weights_ = hw_out;
      H.edge_attrs_ = eattrs_out;

      ## Symmetric-adjacency update.  Extend adj_ to new size, then
      ## scatter undirected edges (both (s,t) and (t,s) unless self-loop).
      A = G.adj_;
      if (N_new > Nold)
        A(N_new, N_new) = 0;
      endif

      if (m_new > 0)
        ## Canonicalise each new edge to (min, max) so duplicate
        ## detection works regardless of orientation.
        s_n = min (s_idx, t_idx);
        t_n = max (s_idx, t_idx);

        ## Check duplicates among new edges.
        p_new_tri = sparse (s_n, t_n, 1:m_new, N_new, N_new);
        if (nnz (p_new_tri) != m_new)
          error ("Octave:invalid-input-arg", ...
                 ["addedge: duplicate edges in the input to ", ...
                  "addedge; the graph class does not support ", ...
                  "parallel edges"]);
        endif
        ## Check against existing entries.  adj_ is symmetric, so
        ## checking the upper triangle is sufficient.
        if (Nold > 0)
          Aprev_up = triu (A);
          conflict = p_new_tri & (Aprev_up != 0);
          if (nnz (conflict) > 0)
            error ("Octave:invalid-input-arg", ...
                   ["addedge: edge already exists in G; the graph ", ...
                    "class does not support parallel edges"]);
          endif
        endif

        ## Scatter symmetrically.
        if (hw_out)
          vals = w_vec;
        else
          vals = ones (m_new, 1);
        endif
        sl = (s_idx == t_idx);
        nsl = ! sl;
        ss = [s_idx(nsl); t_idx(nsl); s_idx(sl)];
        tt = [t_idx(nsl); s_idx(nsl); s_idx(sl)];
        vv = [vals(nsl);  vals(nsl);  vals(sl)];
        A = A + sparse (ss, tt, vv, N_new, N_new);
      endif

      H.adj_ = A;

    endfunction

    function H = rmnode (G, nodes)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} rmnode (@var{G}, @var{nodes})
      ## Remove one or more nodes (and their incident edges) from the
      ## undirected graph @var{G} and return the resulting graph
      ## @var{H}.  See @code{help rmnode} for the full description.
      ## Surviving nodes are reindexed compactly into
      ## @code{1:(numnodes (G) - k)}; node names, node-attribute
      ## columns, and edge-attribute columns are filtered to match.
      ## @seealso{graph, addnode, rmedge, addedge, numnodes, findnode}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to rmnode: expected 2 arguments");
      endif

      rm_idx = __resolve_node_list__ (G, nodes, "rmnode");

      Nold = size (G.adj_, 1);

      keep_mask = true (Nold, 1);
      if (! isempty (rm_idx))
        keep_mask(rm_idx) = false;
      endif

      ## Iterate existing edges in lex (min, max) order matching
      ## get.Edges (see graph.get.Edges: find(tril(adj_)) yields
      ## [t_end, s_end] with s_end <= t_end).
      if (nnz (tril (G.adj_)) == 0)
        edge_survive = false (0, 1);
      else
        [t_end, s_end] = find (tril (G.adj_));
        s_end = s_end(:); t_end = t_end(:);
        edge_survive = keep_mask(s_end) & keep_mask(t_end);
      endif

      ## Filter edge-attribute columns by edge_survive.
      eattrs_out = G.edge_attrs_;
      efn = fieldnames (eattrs_out);
      for ii = 1:numel (efn)
        col = eattrs_out.(efn{ii});
        eattrs_out.(efn{ii}) = col(edge_survive, :);
      endfor

      H = G;
      [H.adj_, H.nodenames_, H.node_attrs_] = ...
        __rmnode_impl__ (G.adj_, G.nodenames_, G.node_attrs_, rm_idx);
      H.edge_attrs_ = eattrs_out;

    endfunction

    function H = rmedge (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} rmedge (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{H} =} rmedge (@var{G}, @var{edgeIdx})
      ## Remove edges from the undirected graph @var{G} and return the
      ## resulting graph @var{H}.  See @code{help rmedge} for the full
      ## description of the two call forms.  For an undirected graph,
      ## @code{rmedge (@var{G}, @var{s}, @var{t})} matches the edge in
      ## either orientation.  Node count, node names, and node-attribute
      ## columns are preserved; edge-attribute columns are filtered to
      ## match the surviving edges.
      ## @seealso{graph, addedge, rmnode, addnode, numedges, findedge}
      ## @end deftypefn

      if (nargin < 2 || nargin > 3)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to rmedge: expected 2 or 3 arguments");
      endif

      edge_survive = __rmedge_impl__ (G, varargin{:});
      N = size (G.adj_, 1);

      ## Filter edge-attribute columns by the survive mask.
      eattrs_out = G.edge_attrs_;
      efn = fieldnames (eattrs_out);
      for ii = 1:numel (efn)
        col = eattrs_out.(efn{ii});
        eattrs_out.(efn{ii}) = col(edge_survive, :);
      endfor

      H = G;
      H.edge_attrs_ = eattrs_out;

      ## Rebuild the symmetric adjacency from the surviving edges.  The
      ## iteration order must match get.Edges, which uses
      ## find (tril (adj_)) to produce lex (min, max) storage.  For a
      ## non-self-loop surviving edge we scatter both (s, t) and (t, s);
      ## for a self-loop we scatter a single (s, s) entry.
      if (nnz (tril (G.adj_)) == 0)
        H.adj_ = sparse (N, N);
      else
        [t_end, s_end, w] = find (tril (G.adj_));
        s_end = s_end(:); t_end = t_end(:); w = w(:);
        if (any (edge_survive))
          ss = s_end(edge_survive);
          tt = t_end(edge_survive);
          ww = w(edge_survive);
          sl  = (ss == tt);
          nsl = ! sl;
          ss_all = [ss(nsl); tt(nsl); ss(sl)];
          tt_all = [tt(nsl); ss(nsl); ss(sl)];
          ww_all = [ww(nsl); ww(nsl); ww(sl)];
          H.adj_ = sparse (ss_all, tt_all, ww_all, N, N);
        else
          H.adj_ = sparse (N, N);
        endif
      endif

    endfunction

    function H = reordernodes (G, order)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} reordernodes (@var{G}, @var{order})
      ## Permute the nodes of the undirected graph @var{G} according
      ## to @var{order} and return the reordered graph @var{H}.  See
      ## @code{help reordernodes} for the full description.  Node
      ## @code{i} of @var{H} is node @code{@var{order}(i)} of @var{G};
      ## the adjacency matrix of @var{H} is
      ## @code{adjacency (@var{G})(@var{order}, @var{order})}.  Node
      ## names, node-attribute columns, and edge-attribute columns
      ## are renumbered to match.
      ## @seealso{graph, digraph, subgraph, rmnode, addnode}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to reordernodes: expected 2 arguments");
      endif

      perm = __resolve_node_list__ (G, order, "reordernodes");

      N = size (G.adj_, 1);

      if (numel (perm) != N || numel (unique (perm)) != N)
        error ("Octave:invalid-input-arg", ...
               ["graph: reordernodes: ORDER must be a permutation ", ...
                "of 1:numnodes (G)"]);
      endif

      inv_perm = zeros (N, 1);
      inv_perm(perm) = 1:N;

      H = G;
      [H.adj_, H.nodenames_, H.node_attrs_] = ...
        __reordernodes_impl__ (G.adj_, G.nodenames_, G.node_attrs_, perm);

      ## Reorder edge-attribute rows to match the new adjacency's
      ## iteration order (get.Edges uses find(tril(adj_)) to yield
      ## lex (min, max) storage).  Map each old (s, t) with s <= t
      ## to (inv_perm(s), inv_perm(t)), canonicalize to (min, max),
      ## and stable-sort to obtain the per-edge permutation.
      if (nnz (tril (G.adj_)) == 0)
        ## Nothing to do: edge_attrs_ already has zero rows.
      else
        [t_old, s_old] = find (tril (G.adj_));
        s_old = s_old(:); t_old = t_old(:);
        new_a = inv_perm(s_old);
        new_b = inv_perm(t_old);
        new_min = min (new_a, new_b);
        new_max = max (new_a, new_b);
        [~, p_edge] = sortrows ([new_min, new_max]);
        efn = fieldnames (G.edge_attrs_);
        for ii = 1:numel (efn)
          col = G.edge_attrs_.(efn{ii});
          H.edge_attrs_.(efn{ii}) = col(p_edge, :);
        endfor
      endif

    endfunction

    function H = subgraph (G, nodes)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} subgraph (@var{G}, @var{nodes})
      ## Return the subgraph of the undirected graph @var{G} induced by
      ## the node subset @var{nodes}.  See @code{help subgraph} for the
      ## full description.  Only edges with @emph{both} endpoints in
      ## @var{nodes} are retained.  Nodes appear in @var{H} in the order
      ## given by @var{nodes}; node names, node-attribute columns, and
      ## edge-attribute columns are carried over.
      ## @seealso{graph, rmnode, reordernodes, addnode, numnodes, findnode}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to subgraph: expected 2 arguments");
      endif

      N = size (G.adj_, 1);

      ## Resolve NODES into a column of validated, unique indices.
      ## Logical masks are handled separately; everything else goes
      ## through __resolve_node_list__ (numeric, char row, cellstr,
      ## [] or {}).
      if (islogical (nodes))
        if (numel (nodes) != N)
          error ("Octave:invalid-input-arg", ...
                 ["graph: subgraph: logical mask NODES must have ", ...
                  "length numnodes (G)"]);
        endif
        keep_idx = find (nodes(:));
      else
        keep_idx = __resolve_node_list__ (G, nodes, "subgraph");
      endif

      if (numel (unique (keep_idx)) != numel (keep_idx))
        error ("Octave:invalid-input-arg", ...
               "graph: subgraph: NODES must be unique");
      endif

      Nnew = numel (keep_idx);

      ## Build an N-by-1 map: original index -> new index (or 0 if the
      ## node was dropped).
      idx_map = zeros (N, 1);
      idx_map(keep_idx) = 1:Nnew;

      ## Iterate original edges via find(tril(adj_)) to get (t_end,
      ## s_end) with s_end <= t_end -- matches graph.get.Edges's
      ## lex (min, max) order.
      if (nnz (tril (G.adj_)) == 0)
        edge_survive = false (0, 1);
        p_edge = zeros (0, 1);
      else
        [t_old, s_old] = find (tril (G.adj_));
        s_old = s_old(:); t_old = t_old(:);
        edge_survive = (idx_map(s_old) > 0) & (idx_map(t_old) > 0);
        if (any (edge_survive))
          new_a = idx_map(s_old(edge_survive));
          new_b = idx_map(t_old(edge_survive));
          ## Canonicalise to (min, max) since the reindex may flip
          ## endpoint order.
          new_min = min (new_a, new_b);
          new_max = max (new_a, new_b);
          [~, p_edge] = sortrows ([new_min, new_max]);
        else
          p_edge = zeros (0, 1);
        endif
      endif

      ## Filter edge-attribute columns by survive mask + reorder.
      eattrs_out = struct ();
      efn = fieldnames (G.edge_attrs_);
      for ii = 1:numel (efn)
        col = G.edge_attrs_.(efn{ii});
        survived = col(edge_survive, :);
        eattrs_out.(efn{ii}) = survived(p_edge, :);
      endfor

      H = G;
      [H.adj_, H.nodenames_, H.node_attrs_] = ...
        __subgraph_impl__ (G.adj_, G.nodenames_, G.node_attrs_, keep_idx);
      H.edge_attrs_ = eattrs_out;

    endfunction

    function nb = neighbors (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{n} =} neighbors (@var{G}, @var{nodeID})
      ## Return the nodes adjacent to @var{nodeID} in the undirected
      ## graph @var{G}.  @var{nodeID} is a scalar node identifier --
      ## either a numeric index in @code{1:numnodes (@var{G})} or a node
      ## name (char row vector or 1-element cellstr).  The return type
      ## matches the input type (numeric in / numeric out, string in /
      ## cellstr out).  A self-loop at @var{nodeID} contributes
      ## @var{nodeID} to the result once.
      ## @seealso{graph, degree, successors, predecessors}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to neighbors: expected 2 arguments");
      endif

      [n, return_names] = __resolve_single_node__ (G, nodeID, "neighbors");

      ## The graph adjacency is symmetric; a row/column scan yields the
      ## same set of incident nodes.  @code{find} returns sorted
      ## column-major indices, so the result is already in increasing
      ## node order.  A self-loop at N contributes N once.
      idx = find (G.adj_(n, :));
      idx = idx(:);

      if (return_names)
        nb = G.nodenames_(idx);
        nb = nb(:);
      else
        nb = double (idx);
      endif

    endfunction

    function d = degree (G, nodeIDs)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{d} =} degree (@var{G})
      ## @deftypefnx {} {@var{d} =} degree (@var{G}, @var{nodeIDs})
      ## Return the degrees of nodes in the undirected graph @var{G}.
      ## With one argument, return a @code{numnodes (G)}-by-1 column
      ## vector of edge-end counts.  With two arguments, return the
      ## degrees of the specified nodes, preserving the shape of
      ## @var{nodeIDs}.  A non-self-loop edge contributes 1 to the
      ## degree of each of its endpoints; a self-loop contributes 2 to
      ## the degree of the looped node (MATLAB convention).
      ## @seealso{graph, numnodes, numedges, neighbors}
      ## @end deftypefn

      if (nargin < 1 || nargin > 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to degree: expected 1 or 2 arguments");
      endif

      N = numnodes (G);
      if (N == 0)
        all_d = zeros (0, 1);
      else
        ## spones coerces nonzero entries to 1 so weighted graphs
        ## report edge counts, not weight sums.  The adjacency is
        ## symmetric with non-self-loop edges mirrored and self-loops
        ## stored as a single diagonal entry, so sum (sp, 1) counts
        ## self-loops only once; add diag (sp) a second time to
        ## satisfy MATLAB's "self-loops contribute 2" convention.
        sp = spones (G.adj_);
        all_d = full (sum (sp, 1))(:) + full (diag (sp));
      endif

      if (nargin == 1)
        d = all_d;
        return;
      endif

      [idx, out_shape] = __resolve_node_list__ (G, nodeIDs, "degree");
      d = reshape (all_d(idx), out_shape);

    endfunction

    function idx = findnode (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{idx} =} findnode (@var{G}, @var{nodeID})
      ## Return the numeric node indices corresponding to @var{nodeID}
      ## in the graph @var{G}.  Numeric inputs are validated and
      ## returned with shape preserved.  A char row vector is looked up
      ## as a single node name and returns a scalar (0 if not found).
      ## A cell array of strings is looked up element-wise and returns
      ## a column vector of indices (0 for any missing name).  This
      ## method matches MATLAB's findnode semantics: missing names
      ## yield 0, not an error.
      ## @seealso{graph, findedge, numnodes, neighbors}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to findnode: expected 2 arguments");
      endif

      idx = __findnode_impl__ (G, nodeID);

    endfunction

    function varargout = findedge (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{endpoints} =} findedge (@var{G})
      ## @deftypefnx {} {[@var{sOut}, @var{tOut}] =} findedge (@var{G})
      ## @deftypefnx {} {@var{idx} =} findedge (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{endpoints} =} findedge (@var{G}, @var{edgeIdx})
      ## Look up edges of the graph @var{G}.  For the @code{(s, t)} form
      ## on an undirected @code{graph}, the pair matches in either
      ## orientation.  See @code{help findedge} for the full description
      ## of the three supported call forms.
      ## @seealso{graph, findnode, numedges}
      ## @end deftypefn

      if (nargin < 1 || nargin > 3)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to findedge: expected 1, 2, or 3 arguments");
      endif

      nout = max (nargout, 1);
      if (nout <= 1)
        varargout{1} = __findedge_impl__ (G, 1, varargin{:});
      else
        [out1, out2] = __findedge_impl__ (G, 2, varargin{:});
        varargout{1} = out1;
        varargout{2} = out2;
      endif

    endfunction

    function n = edgecount (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{n} =} edgecount (@var{G}, @var{s}, @var{t})
      ## Count edges between node pairs in the graph @var{G}.  For an
      ## undirected @code{graph} the pair @code{(@var{s}(i), @var{t}(i))}
      ## matches in either orientation.  Returns a scalar for scalar
      ## inputs and a column vector otherwise.  See @code{help edgecount}
      ## for the full description.
      ## @seealso{graph, findedge, numedges}
      ## @end deftypefn

      if (nargin != 3)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to edgecount: expected 3 arguments");
      endif

      n = __edgecount_impl__ (G, varargin{1}, varargin{2});

    endfunction

    function A = adjacency (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{A} =} adjacency (@var{G})
      ## @deftypefnx {} {@var{A} =} adjacency (@var{G}, @qcode{"weighted"})
      ## @deftypefnx {} {@var{A} =} adjacency (@var{G}, @var{W})
      ## Return the sparse symmetric adjacency matrix of the undirected
      ## graph @var{G}.  The one-input form returns a binary (0/1)
      ## matrix; @qcode{"weighted"} uses the stored edge weights; a
      ## numeric vector @var{W} of length @code{numedges (@var{G})}
      ## provides custom per-edge weights.  For a non-self-loop edge,
      ## @var{W}(k) appears at both @var{A}(i, j) and @var{A}(j, i); for
      ## a self-loop it appears once at @var{A}(i, i).  See
      ## @code{help adjacency} for the full description.
      ## @seealso{graph, incidence, laplacian, numedges}
      ## @end deftypefn

      if (nargin < 1 || nargin > 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to adjacency: expected 1 or 2 arguments");
      endif

      N = size (G.adj_, 1);
      M = nnz (tril (G.adj_));

      if (nargin == 1)
        if (N == 0)
          A = sparse (0, 0);
        else
          A = spones (G.adj_);
        endif
        return;
      endif

      arg = varargin{1};

      if (ischar (arg) && isrow (arg)) ...
         || (iscellstr (arg) && isscalar (arg))
        if (iscellstr (arg))
          flag = arg{1};
        else
          flag = arg;
        endif
        if (! strcmpi (flag, "weighted"))
          error ("Octave:invalid-input-arg", ...
                 "adjacency: unknown option '%s'; expected 'weighted' or a numeric weight vector", ...
                 flag);
        endif
        if (N == 0)
          A = sparse (0, 0);
        elseif (G.has_weights_)
          A = G.adj_;
        else
          A = spones (G.adj_);
        endif
        return;
      endif

      ## Custom weight vector W.
      if (iscell (arg))
        error ("Octave:invalid-input-arg", ...
               "adjacency: weight argument must be 'weighted' or a numeric real vector");
      endif
      if (! isnumeric (arg))
        error ("Octave:invalid-input-arg", ...
               "adjacency: weight argument must be 'weighted' or a numeric real vector");
      endif
      if (! isreal (arg))
        error ("Octave:invalid-input-arg", ...
               "adjacency: weight vector must be real (complex values not supported)");
      endif
      if (! isempty (arg) && ! isvector (arg))
        error ("Octave:invalid-input-arg", ...
               "adjacency: weight argument must be a vector");
      endif
      if (numel (arg) != M)
        error ("Octave:invalid-input-arg", ...
               "adjacency: weight vector must have length %d (numedges (G))", M);
      endif

      if (M == 0)
        A = sparse (N, N);
        return;
      endif

      w = double (arg(:));
      ## G.Edges.EndNodes returns edges in lex order with col1 <= col2.
      E = G.Edges.EndNodes;
      s = E(:, 1);
      t = E(:, 2);
      is_self = (s == t);
      s_off = s(! is_self);
      t_off = t(! is_self);
      w_off = w(! is_self);
      s_self = s(is_self);
      w_self = w(is_self);
      A = sparse ([s_off; t_off; s_self], ...
                  [t_off; s_off; s_self], ...
                  [w_off; w_off; w_self], ...
                  N, N);

    endfunction

    function I = incidence (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{I} =} incidence (@var{G})
      ## Return the sparse incidence matrix of the undirected graph
      ## @var{G}.  Column @math{k} of @var{I} has @code{1} at both
      ## endpoint-rows of edge @math{k}.  Self-loop edges produce an
      ## all-zero column.  See @code{help incidence} for the full
      ## description.
      ## @seealso{graph, adjacency, laplacian, numedges, numnodes}
      ## @end deftypefn

      if (nargin != 1)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to incidence: expected 1 argument");
      endif

      N = size (G.adj_, 1);
      M = nnz (tril (G.adj_));
      if (M == 0)
        I = sparse (N, 0);
        return;
      endif

      ## Lex-sorted (col1 <= col2) edge list from G.Edges.EndNodes.
      E = G.Edges.EndNodes;
      s = E(:, 1);
      t = E(:, 2);

      ## Skip self-loop columns (incidence convention: must have
      ## exactly two entries per column, so self-loops contribute no
      ## sparse entries).
      k = (1:M)';
      keep = (s != t);
      s_k = s(keep);
      t_k = t(keep);
      c_k = k(keep);
      rows = [s_k; t_k];
      cols = [c_k; c_k];
      vals = ones (2 * numel (c_k), 1);
      I = sparse (rows, cols, vals, N, M);

    endfunction

    function L = laplacian (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{L} =} laplacian (@var{G})
      ## Return the sparse graph Laplacian @math{L = D - A} of the
      ## undirected graph @var{G}.  @code{L(i, i)} equals
      ## @code{degree (@var{G}, i)} (with self-loops contributing 2);
      ## @code{L(i, j) = -1} when there is an edge between @math{i} and
      ## @math{j} (@math{i} not equal to @math{j}), and @code{0}
      ## otherwise.  Edge weights are ignored (binary Laplacian).  See
      ## @code{help laplacian} for the full description.
      ## @seealso{graph, adjacency, incidence, degree, numnodes}
      ## @end deftypefn

      if (nargin != 1)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to laplacian: expected 1 argument");
      endif

      N = size (G.adj_, 1);
      if (N == 0)
        L = sparse (0, 0);
        return;
      endif

      ## Binary symmetric adjacency: spones collapses any weights to 1
      ## and preserves the symmetric (i, j)/(j, i) plus single (i, i)
      ## storage of self-loops.
      A_bin = spones (G.adj_);

      ## Off-diagonal binary adjacency: zero out the diagonal so that
      ## self-loops contribute nothing to the off-diagonal pattern of L.
      ## Self-loops do still contribute to the diagonal via degree(G).
      A_off = A_bin - diag (sparse (diag (A_bin)));

      ## degree(G) is a column vector of length N.  Place it on the
      ## diagonal as a sparse matrix and subtract A_off.
      d = degree (G);
      L = sparse (1:N, 1:N, d, N, N) - A_off;

    endfunction

    function v = bfsearch (G, s, events, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{v} =} bfsearch (@var{G}, @var{s})
      ## @deftypefnx {} {@var{v} =} bfsearch (@var{G}, @var{s}, @var{event})
      ## @deftypefnx {} {@var{T} =} bfsearch (@var{G}, @var{s}, @var{events})
      ## @deftypefnx {} {@var{T} =} bfsearch (@dots{}, @qcode{"Restart"}, @var{tf})
      ## @deftypefnx {} {@var{T} =} bfsearch (@dots{}, @qcode{"EdgeColors"}, @var{tf})
      ## Perform a breadth-first search of the undirected graph
      ## @var{G} starting at node @var{s} and return nodes (or edges,
      ## or a full event log) in BFS order.  Incident edges are
      ## followed in both directions.  When a node has multiple
      ## unvisited neighbours they are visited in ascending order of
      ## node index (MATLAB parity tie-break).  Nodes in other
      ## connected components are omitted; parallel edges in a
      ## multigraph are collapsed (each neighbour is enqueued at most
      ## once).
      ##
      ## With two arguments, return a numeric column vector @var{v} of
      ## node indices in the order they are discovered.
      ##
      ## With a third argument @var{event} that is a character string
      ## naming a single event type, return the BFS nodes or edges
      ## corresponding to that event.  Valid event names are
      ## @qcode{"discovernode"}, @qcode{"finishnode"}, @qcode{"startnode"}
      ## (return a numeric column vector of node indices),
      ## @qcode{"edgetonew"}, @qcode{"edgetodiscovered"}, and
      ## @qcode{"edgetofinished"} (return an @math{m}-by-2 numeric
      ## matrix of @code{[src, dst]} index pairs).
      ##
      ## With a third argument that is the string @qcode{"allevents"} or
      ## a cell array of event names, return a scalar struct @var{T}
      ## with fields @code{Event} (cellstr column of event names),
      ## @code{Node} (double column of node indices, @code{0} for
      ## edge-only events), and @code{Edge} (@math{m}-by-2 double matrix
      ## of edge endpoints, @code{[0 0]} for node-only events).
      ##
      ## Additional trailing Name-Value options (case-insensitive names):
      ##
      ## @itemize
      ## @item
      ## @qcode{"Restart"} (logical scalar, default @code{false}).  When
      ## @code{true}, BFS continues from the smallest-indexed undiscovered
      ## node after the initial component from @var{s} is exhausted, and
      ## repeats until every node has been visited.  Each restart fires
      ## an additional @qcode{"startnode"} event.
      ## @item
      ## @qcode{"EdgeColors"} (logical scalar, default @code{false}).
      ## When @code{true} and the output is a struct, add a cellstr
      ## column @code{EdgeColor} aligned with @code{Event}.  Edge events
      ## are tagged @qcode{"tree"} (@code{edgetonew}) or @qcode{"cross"}
      ## (@code{edgetodiscovered}, @code{edgetofinished}); node events
      ## get @qcode{""}.  Requires the @var{events} argument to be
      ## @qcode{"allevents"} or a cell array of event names.
      ## @end itemize
      ## @seealso{graph, dfsearch, neighbors, degree}
      ## @end deftypefn

      if (nargin < 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to bfsearch: expected at least 2 arguments");
      endif

      [src, ~] = __resolve_single_node__ (G, s, "bfsearch");

      ## Build a binary / count adjacency.  For an undirected graph
      ## the stored adjacency is symmetric, so adjacency(G) gives a
      ## matrix whose nonzeros mark incident edges in both directions.
      A = adjacency (G);

      if (nargin == 2)
        v = __bfsearch_impl__ (A, src);
      else
        opts = __bfsdfs_parse_opts__ ("bfsearch", varargin);
        v = __bfsearch_events_impl__ (A, src, events, opts);
      endif

    endfunction

    function v = dfsearch (G, s, events, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{v} =} dfsearch (@var{G}, @var{s})
      ## @deftypefnx {} {@var{v} =} dfsearch (@var{G}, @var{s}, @var{event})
      ## @deftypefnx {} {@var{T} =} dfsearch (@var{G}, @var{s}, @var{events})
      ## @deftypefnx {} {@var{T} =} dfsearch (@dots{}, @qcode{"Restart"}, @var{tf})
      ## @deftypefnx {} {@var{T} =} dfsearch (@dots{}, @qcode{"EdgeColors"}, @var{tf})
      ## Perform a depth-first search of the undirected graph
      ## @var{G} starting at node @var{s} and return nodes (or edges,
      ## or a full event log) in DFS order.  Incident edges are
      ## followed in both directions.  When a node has multiple
      ## unvisited neighbours they are visited in ascending order of
      ## node index (MATLAB parity tie-break).  Nodes in other
      ## connected components are omitted; parallel edges in a
      ## multigraph are collapsed (each neighbour is visited at most
      ## once).
      ##
      ## With two arguments, return a numeric column vector @var{v} of
      ## node indices in the order they are discovered.
      ##
      ## With a third argument @var{event} that is a character string
      ## naming a single event type, return the DFS nodes or edges
      ## corresponding to that event.  Valid event names are
      ## @qcode{"discovernode"}, @qcode{"finishnode"}, @qcode{"startnode"}
      ## (return a numeric column vector of node indices),
      ## @qcode{"edgetonew"}, @qcode{"edgetodiscovered"}, and
      ## @qcode{"edgetofinished"} (return an @math{m}-by-2 numeric
      ## matrix of @code{[src, dst]} index pairs).
      ##
      ## With a third argument that is the string @qcode{"allevents"} or
      ## a cell array of event names, return a scalar struct @var{T}
      ## with fields @code{Event} (cellstr column of event names),
      ## @code{Node} (double column of node indices, @code{0} for
      ## edge-only events), and @code{Edge} (@math{m}-by-2 double matrix
      ## of edge endpoints, @code{[0 0]} for node-only events).
      ##
      ## In DFS, @qcode{"edgetodiscovered"} marks a @emph{back edge}
      ## (target on the DFS stack); @qcode{"edgetofinished"} marks an
      ## edge whose target has already finished.  For an undirected
      ## graph every incident edge other than the one used to reach
      ## the current node in the DFS tree is a back edge to an
      ## ancestor, so @qcode{"edgetofinished"} is usually empty.
      ##
      ## Additional trailing Name-Value options (case-insensitive names):
      ##
      ## @itemize
      ## @item
      ## @qcode{"Restart"} (logical scalar, default @code{false}).  When
      ## @code{true}, DFS continues from the smallest-indexed undiscovered
      ## node after the initial component from @var{s} is exhausted, and
      ## repeats until every node has been visited.  Each restart fires
      ## an additional @qcode{"startnode"} event.
      ## @item
      ## @qcode{"EdgeColors"} (logical scalar, default @code{false}).
      ## When @code{true} and the output is a struct, add a cellstr
      ## column @code{EdgeColor} aligned with @code{Event}.  Edge events
      ## are tagged @qcode{"tree"}, @qcode{"back"}, @qcode{"forward"}, or
      ## @qcode{"cross"}; node events get @qcode{""}.  Requires the
      ## @var{events} argument to be @qcode{"allevents"} or a cell array
      ## of event names.
      ## @end itemize
      ## @seealso{graph, bfsearch, neighbors, degree}
      ## @end deftypefn

      if (nargin < 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to dfsearch: expected at least 2 arguments");
      endif

      [src, ~] = __resolve_single_node__ (G, s, "dfsearch");

      ## Build a binary / count adjacency.  For an undirected graph
      ## the stored adjacency is symmetric, so adjacency(G) gives a
      ## matrix whose nonzeros mark incident edges in both directions.
      A = adjacency (G);

      if (nargin == 2)
        v = __dfsearch_impl__ (A, src);
      else
        opts = __bfsdfs_parse_opts__ ("dfsearch", varargin);
        v = __dfsearch_events_impl__ (A, src, events, opts);
      endif

    endfunction

    function out = conncomp (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{bins} =} conncomp (@var{G})
      ## @deftypefnx {} {@var{bins} =} conncomp (@var{G}, @qcode{"Type"}, @qcode{"weak"})
      ## @deftypefnx {} {@var{C} =} conncomp (@dots{}, @qcode{"OutputForm"}, @var{form})
      ## Compute the connected components of the graph @var{G}.
      ##
      ## With no options, return a row vector @var{bins} of length
      ## @code{numnodes (@var{G})} whose @math{i}-th entry is the 1-based
      ## component label of node @math{i}.  Components are labelled in
      ## the order they are first discovered when scanning nodes from 1
      ## upward.
      ##
      ## Recognised Name-Value options (case-insensitive names and values):
      ##
      ## @itemize
      ## @item
      ## @qcode{"Type"} must be @qcode{"weak"} on a graph (the
      ## undirected notion of connectivity); @qcode{"strong"} is a
      ## digraph-only option and is rejected.
      ## @item
      ## @qcode{"OutputForm"} is either @qcode{"vector"} (default) or
      ## @qcode{"cell"}.  @qcode{"vector"} returns the @var{bins} row
      ## vector described above; @qcode{"cell"} returns a cell array
      ## @var{C} of length equal to the number of components, where
      ## @code{@var{C}@{k@}} is a sorted column vector of the node
      ## indices belonging to the @math{k}-th component.
      ## @end itemize
      ## @seealso{graph, bfsearch, dfsearch}
      ## @end deftypefn

      opts = __conncomp_parse_opts__ (false, varargin);

      A = adjacency (G);
      bins = __conncomp_weak__ (A);

      if (strcmp (opts.outputform, "vector"))
        out = bins;
      else
        N = numel (bins);
        if (N == 0)
          out = cell (1, 0);
        else
          K = max (bins);
          out = cell (1, K);
          for k = 1:K
            out{k} = find (bins == k).'(:);
          endfor
        endif
      endif

    endfunction

    function [out, iC] = biconncomp (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{bins} =} biconncomp (@var{G})
      ## @deftypefnx {} {[@var{bins}, @var{iC}] =} biconncomp (@var{G})
      ## @deftypefnx {} {@var{C} =} biconncomp (@var{G}, @qcode{"OutputForm"}, @qcode{"cell"})
      ## Compute the biconnected components of the undirected graph
      ## @var{G}.  With no options, return a row vector of per-edge
      ## labels; with two outputs, also return the articulation points
      ## as a row vector of node indices.  @qcode{"OutputForm"} may be
      ## @qcode{"vector"} (default) or @qcode{"cell"}; the cell form
      ## returns one column vector of node indices per BCC, with
      ## isolated nodes appearing as singleton cells.  See
      ## @code{help biconncomp} for the full description.
      ## @seealso{graph, conncomp, bfsearch, dfsearch}
      ## @end deftypefn

      outputform = "vector";
      nv = numel (varargin);
      if (mod (nv, 2) != 0)
        error ("Octave:invalid-fun-call", ...
               "biconncomp: Name-Value options expected pairs");
      endif
      for k = 1:2:nv
        name = varargin{k};
        val = varargin{k+1};
        if (! (ischar (name) && isrow (name)))
          error ("Octave:invalid-input-arg", ...
                 "biconncomp: option name must be a string");
        endif
        if (strcmpi (name, "OutputForm"))
          if (! (ischar (val) && isrow (val)))
            error ("Octave:invalid-input-arg", ...
                   "biconncomp: OutputForm value must be a string");
          endif
          if (strcmpi (val, "vector"))
            outputform = "vector";
          elseif (strcmpi (val, "cell"))
            outputform = "cell";
          else
            error ("Octave:invalid-input-arg", ...
                   "biconncomp: OutputForm must be \"vector\" or \"cell\"");
          endif
        else
          error ("Octave:invalid-input-arg", ...
                 "biconncomp: unknown option name \"%s\"", name);
        endif
      endfor

      ## Pull the lex-sorted edge list from the public Edges struct so
      ## we honour any constructor path (edge-list, adjacency-matrix,
      ## EdgeTable).  Coerce to double for arithmetic safety.
      edges = G.Edges;
      if (isfield (edges, "EndNodes"))
        E = double (edges.EndNodes);
      else
        E = zeros (0, 2);
      endif
      N = numnodes (G);

      [bins, is_art] = __biconncomp__ (E, N);

      if (strcmp (outputform, "vector"))
        out = bins;
      else
        ## Cell form: one column of node indices per BCC, with isolated
        ## nodes appearing as singleton cells.  Sort entries by
        ## (min_node, first_edge_index) so BCCs sharing a minimum node
        ## (e.g. a self-loop plus an incident simple edge) are ordered
        ## by edge-index, and isolated nodes slot in by their own index.
        M = size (E, 1);
        K = 0;
        if (M > 0)
          K = max (bins);
        endif

        n_entries = K;
        covered = false (1, N);

        if (K > 0)
          cell_nodes = cell (1, K);
          sort_keys = zeros (K, 2);
          for k = 1:K
            eidx = find (bins == k);
            nlist = unique ([E(eidx, 1); E(eidx, 2)]);
            covered(nlist) = true;
            cell_nodes{k} = nlist(:);       # column
            sort_keys(k, :) = [nlist(1), eidx(1)];
          endfor
        else
          cell_nodes = cell (1, 0);
          sort_keys = zeros (0, 2);
        endif

        iso = find (! covered);
        niso = numel (iso);
        if (niso > 0)
          iso_cells = cell (1, niso);
          iso_keys = zeros (niso, 2);
          ## Put isolated entries AFTER any real BCC that shares the
          ## same minimum node (the sort is lex on [min_node,
          ## first_edge] and we give isolated a sentinel that is larger
          ## than any real first_edge, which is at most M).
          sentinel = M + 1;
          for k = 1:niso
            iso_cells{k} = iso(k);
            iso_keys(k, :) = [iso(k), sentinel + k];
          endfor
          all_cells = [cell_nodes, iso_cells];
          all_keys = [sort_keys; iso_keys];
        else
          all_cells = cell_nodes;
          all_keys = sort_keys;
        endif

        if (isempty (all_cells))
          out = cell (1, 0);
        else
          [~, order] = sortrows (all_keys);
          out = all_cells(order);
          out = out(:).';                    # row cell
        endif
      endif

      if (nargout >= 2)
        iC = find (is_art);
        iC = iC(:).';
        if (isempty (iC))
          iC = zeros (1, 0);
        endif
      endif

    endfunction

    function C = condensation (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{C} =} condensation (@var{G})
      ## Condensation is not defined on the undirected @code{graph}
      ## class; this method always raises an error.  Use
      ## @code{condensation} on a @code{digraph} object instead.  See
      ## @code{help condensation} for the full description of the
      ## directed case.
      ## @seealso{graph, digraph, conncomp}
      ## @end deftypefn

      error ("Octave:invalid-input-arg", ...
             "condensation: not defined for an undirected graph; condensation requires a digraph");

    endfunction

    function n = toposort (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{n} =} toposort (@var{G})
      ## Topological sort is not defined on the undirected @code{graph}
      ## class; this method always raises an error.  Use
      ## @code{toposort} on a @code{digraph} object instead.  See
      ## @code{help toposort} for the full description of the directed
      ## case.
      ## @seealso{graph, digraph, condensation}
      ## @end deftypefn

      error ("Octave:invalid-input-arg", ...
             "toposort: not defined for an undirected graph; toposort requires a digraph");

    endfunction

    function tf = isdag (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{tf} =} isdag (@var{G})
      ## The acyclicity predicate @code{isdag} is not defined on the
      ## undirected @code{graph} class; this method always raises an
      ## error.  Use @code{isdag} on a @code{digraph} object instead.
      ## See @code{help isdag} for the full description of the
      ## directed case.
      ## @seealso{graph, digraph, toposort}
      ## @end deftypefn

      error ("Octave:invalid-input-arg", ...
             "isdag: not defined for an undirected graph; isdag requires a digraph");

    endfunction

    function H = transclosure (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} transclosure (@var{G})
      ## Transitive closure is not defined on the undirected
      ## @code{graph} class; this method always raises an error.  Use
      ## @code{transclosure} on a @code{digraph} object instead.  See
      ## @code{help transclosure} for the full description of the
      ## directed case.
      ## @seealso{graph, digraph, condensation}
      ## @end deftypefn

      error ("Octave:invalid-input-arg", ...
             "transclosure: not defined for an undirected graph; transclosure requires a digraph");

    endfunction

    function H = transreduction (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} transreduction (@var{G})
      ## Transitive reduction is not defined on the undirected
      ## @code{graph} class; this method always raises an error.  Use
      ## @code{transreduction} on a @code{digraph} object instead.
      ## See @code{help transreduction} for the full description of
      ## the directed case.
      ## @seealso{graph, digraph, transclosure}
      ## @end deftypefn

      error ("Octave:invalid-input-arg", ...
             "transreduction: not defined for an undirected graph; transreduction requires a digraph");

    endfunction

    function H = simplify (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} simplify (@var{G})
      ## @deftypefnx {} {@var{H} =} simplify (@var{G}, @var{method})
      ## @deftypefnx {} {@var{H} =} simplify (@var{G}, @dots{}, @qcode{"omitselfloops"})
      ## @deftypefnx {} {@var{H} =} simplify (@var{G}, @dots{}, @var{Name}, @var{Value})
      ## Return a simplified copy of the graph @var{G}.  The undirected
      ## @code{graph} class in this Octave build does not accept the
      ## @qcode{'multigraph'} constructor flag and therefore cannot
      ## store parallel edges, so @code{simplify} on a @code{graph} is
      ## a no-op except for optional self-loop removal via the trailing
      ## @qcode{"omitselfloops"} flag or the @qcode{"SelfLoops"}
      ## Name-Value option with value @qcode{"discard"}.  The aggregation
      ## options @var{method} and @qcode{"AggregationVariables"} are
      ## accepted for parity with the @code{digraph} method but have no
      ## effect here.  Node names (when present) are preserved.
      ## @seealso{graph, digraph, numedges}
      ## @end deftypefn

      [~, omit_loops] = __simplify_parse_opts__ (varargin);

      N = numnodes (G);
      has_names = ! isempty (G.nodenames_);

      if (N == 0)
        H = graph ();
        return;
      endif

      E = G.Edges.EndNodes;
      if (isempty (E))
        src = zeros (0, 1);
        dst = zeros (0, 1);
      else
        src = E(:, 1);
        dst = E(:, 2);
      endif

      if (G.has_weights_ && ! isempty (src))
        w = G.Edges.Weight;
      else
        w = ones (numel (src), 1);
      endif

      if (omit_loops && ! isempty (src))
        mask = (src != dst);
        src = src(mask);
        dst = dst(mask);
        w = w(mask);
      endif

      ## graph cannot carry parallel edges, so no aggregation is
      ## required: the remaining edges are already unique.
      if (G.has_weights_)
        if (has_names)
          H = graph (src, dst, w, G.nodenames_);
        else
          H = graph (src, dst, w, N);
        endif
      else
        if (has_names)
          H = graph (src, dst, [], G.nodenames_);
        else
          H = graph (src, dst, [], N);
        endif
      endif

    endfunction

    function D = distances (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{D} =} distances (@var{G})
      ## @deftypefnx {} {@var{d} =} distances (@var{G}, @var{src})
      ## @deftypefnx {} {@var{d} =} distances (@var{G}, @var{src}, @var{tgt})
      ## @deftypefnx {} {@var{D} =} distances (@dots{}, @qcode{"Method"}, @var{method})
      ## Return shortest-path distances on the undirected graph
      ## @var{G}.
      ##
      ## With no extra arguments, return the symmetric all-pairs
      ## @code{numnodes (@var{G})}-by-@code{numnodes (@var{G})}
      ## distance matrix.  With @var{src}, return a
      ## @code{numel (@var{src})}-by-@code{numnodes (@var{G})} matrix
      ## whose @math{k}-th row is the shortest-path distance from
      ## @code{@var{src}(k)} to every node (so a scalar @var{src}
      ## produces a row vector of length @code{numnodes (@var{G})}).
      ## With both @var{src} and @var{tgt}, return a
      ## @code{numel (@var{src})}-by-@code{numel (@var{tgt})} submatrix
      ## (scalar when both arguments are scalar).
      ##
      ## The optional @qcode{"Method"} Name-Value pair chooses the
      ## algorithm: @qcode{"auto"} (default) uses BFS on an unweighted
      ## graph and Dijkstra on a weighted graph (an undirected graph
      ## with a negative edge is a negative cycle and raises an error);
      ## @qcode{"unweighted"} ignores weights and uses BFS;
      ## @qcode{"positive"} uses Dijkstra (error on negative weight);
      ## @qcode{"mixed"} is accepted for parity but errors when any
      ## edge weight is negative (an undirected negative edge forms a
      ## negative cycle), otherwise matches @qcode{"positive"}.
      ## @qcode{"acyclic"} is rejected: undirected graphs with edges
      ## do not satisfy the DAG property expected by this method.
      ##
      ## @var{src} and @var{tgt} may be numeric node indices or node
      ## names (character row vector or cell array of strings) when
      ## @var{G} has node names; see @code{help distances} for details.
      ## @seealso{graph, shortestpath, shortestpathtree, adjacency}
      ## @end deftypefn

      [positional, method] = __distances_parse_opts__ (varargin);

      have_src = (numel (positional) >= 1);
      have_tgt = (numel (positional) >= 2);

      N = numnodes (G);

      ## graph class does not support parallel edges, so the weighted
      ## adjacency matrix is simply adj_ (already symmetric); if G is
      ## unweighted, collapse to 0/1.
      if (G.has_weights_)
        W = G.adj_;
      else
        W = spones (G.adj_);
      endif

      ## Resolve method = "auto" to a concrete choice for undirected
      ## graphs: BFS when unweighted, Dijkstra when weighted (and
      ## nonneg).  A negative edge on an undirected graph is a
      ## negative cycle (u-v-u) and must error; "auto" therefore
      ## promotes to "mixed" so the negative-cycle error fires later.
      if (strcmp (method, "auto"))
        if (! G.has_weights_)
          method = "unweighted";
        elseif (any (nonzeros (W) < 0))
          method = "mixed";
        else
          method = "positive";
        endif
      endif

      ## Resolve positional src / tgt into numeric index vectors.
      if (have_src)
        src_idx = __resolve_node_list__ (G, positional{1}, "distances");
      endif
      if (have_tgt)
        tgt_idx = __resolve_node_list__ (G, positional{2}, "distances");
      endif

      switch (method)
        case "unweighted"
          if (have_src)
            D_src = __distances_unweighted__ (W, src_idx);
          else
            D_src = __distances_unweighted__ (W);
          endif
        case "positive"
          if (have_src)
            D_src = __distances_dijkstra__ (W, src_idx);
          else
            D_src = __distances_dijkstra__ (W);
          endif
        case "mixed"
          ## An undirected negative edge is a negative cycle by
          ## itself (u-v-u has total weight 2 * w < 0); reject
          ## explicitly.
          if (any (nonzeros (W) < 0))
            error ("Octave:invalid-input-arg", ...
                   "distances: graph contains a negative cycle");
          endif
          ## Otherwise Bellman-Ford would give the same result as
          ## Dijkstra; route through Dijkstra for speed.
          if (have_src)
            D_src = __distances_dijkstra__ (W, src_idx);
          else
            D_src = __distances_dijkstra__ (W);
          endif
        case "acyclic"
          error ("Octave:invalid-input-arg", ...
                 "distances: 'acyclic' Method is not supported for undirected graphs");
        otherwise
          error ("Octave:invalid-input-arg", ...
                 "distances: internal error -- unknown method '%s'", method);
      endswitch

      if (! have_src)
        if (N == 0)
          D = zeros (0, 0);
        else
          D = D_src;
        endif
        return;
      endif

      if (have_tgt)
        if (isempty (tgt_idx))
          D = zeros (numel (src_idx), 0);
        else
          D = D_src(:, tgt_idx);
        endif
      else
        D = D_src;
      endif

    endfunction

    function [P, d, edgepath] = shortestpath (G, s, t, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{P} =} shortestpath (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@var{P}, @var{d}] =} shortestpath (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@var{P}, @var{d}, @var{edgepath}] =} shortestpath (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@dots{}] =} shortestpath (@dots{}, @qcode{"Method"}, @var{method})
      ## Return a single shortest path between nodes @var{s} and
      ## @var{t} of the undirected graph @var{G}.
      ##
      ## With one output argument, return only the node path @var{P}.
      ## With two outputs, also return the total distance @var{d}
      ## along @var{P}.  With three outputs, also return
      ## @var{edgepath}, a row vector of indices into
      ## @code{@var{G}.Edges} identifying the traversed edges.
      ##
      ## When @var{s} and @var{t} are both numeric @var{P} is a
      ## numeric row vector; when either is a name @var{P} is a
      ## @code{1}-by-@var{k} cell array of strings.  When
      ## @code{@var{s} == @var{t}}, @var{P} is @code{[@var{s}]},
      ## @var{d} is @code{0} and @var{edgepath} is @code{1}-by-@code{0}.
      ##
      ## When @var{t} is not reachable from @var{s}, @var{P} is a
      ## @code{1}-by-@code{0} empty vector (numeric or cellstr
      ## following the input type), @var{d} is @code{Inf}, and
      ## @var{edgepath} is a @code{1}-by-@code{0} empty vector.
      ##
      ## The optional @qcode{"Method"} Name-Value pair chooses the
      ## algorithm: @qcode{"auto"} (default) picks Dijkstra when all
      ## weights are non-negative, otherwise Bellman-Ford;
      ## @qcode{"positive"} forces Dijkstra (error on negative weight);
      ## @qcode{"mixed"} forces Bellman-Ford.  For an undirected graph
      ## any negative edge is a negative cycle @math{u-v-u}, so
      ## @qcode{"mixed"} (and the auto-promoted default on any negative
      ## weight) errors.
      ##
      ## Edges may be traversed in either direction since @var{G} is
      ## undirected.  Self-loops do not influence the path.
      ## @seealso{graph, distances, shortestpathtree, allpaths}
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif

      method = __shortestpath_parse_method__ ("shortestpath", varargin);

      [s_idx, s_by_name] = __resolve_single_node__ (G, s, "shortestpath");
      [t_idx, t_by_name] = __resolve_single_node__ (G, t, "shortestpath");

      return_names = s_by_name || t_by_name;

      N = numnodes (G);

      ## graph has no multigraph support today.  The weighted
      ## adjacency matrix is simply adj_ (already symmetric); if G is
      ## unweighted, collapse to 0/1.
      if (G.has_weights_)
        W = G.adj_;
      else
        W = spones (G.adj_);
      endif

      ## Resolve 'auto' to a concrete method based on weight signs.
      ## For an undirected graph with a negative weight, 'auto'
      ## promotes to 'mixed' so Bellman-Ford's negative-cycle check
      ## fires (u-v-u round trip has total 2*w < 0).
      if (strcmp (method, "auto"))
        if (any (nonzeros (W) < 0))
          method = "mixed";
        else
          method = "positive";
        endif
      endif

      ## Dispatch to the chosen algorithm.  Both helpers return a
      ## column vector of path indices.
      switch (method)
        case "positive"
          [path_idx, d] = __shortestpath_dijkstra__ (W, s_idx, t_idx);
        case "mixed"
          [path_idx, d] = __shortestpath_bellman_ford__ (W, s_idx, t_idx);
        otherwise
          error ("Octave:invalid-input-arg", ...
                 "shortestpath: internal error -- unknown method '%s'", ...
                 method);
      endswitch
      path_idx = path_idx(:).';

      if (return_names)
        if (isempty (path_idx))
          P = cell (1, 0);
        else
          P = G.nodenames_(path_idx);
          P = P(:).';
        endif
      else
        if (isempty (path_idx))
          P = zeros (1, 0);
        else
          P = double (path_idx);
        endif
      endif

      if (nargout < 3)
        return;
      endif

      k = numel (path_idx);
      if (k <= 1)
        edgepath = zeros (1, 0);
        return;
      endif

      src_pairs = path_idx(1:k-1);
      dst_pairs = path_idx(2:k);
      ep = __findedge_impl__ (G, 1, src_pairs(:), dst_pairs(:));
      edgepath = ep(:).';

    endfunction

    function TR = shortestpathtree (G, s, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{TR} =} shortestpathtree (@var{G}, @var{s})
      ## @deftypefnx {} {@var{TR} =} shortestpathtree (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{TR} =} shortestpathtree (@dots{}, @qcode{"OutputForm"}, @var{form})
      ## @deftypefnx {} {@var{TR} =} shortestpathtree (@dots{}, @qcode{"Method"}, @var{method})
      ## Return a single-source shortest path tree rooted at node
      ## @var{s} of the undirected graph @var{G}.  The returned
      ## @var{TR} is always a @code{digraph} (even though @var{G} is a
      ## @code{graph}), with every edge oriented from parent to child.
      ##
      ## The @qcode{"OutputForm"} option selects the return type:
      ## @qcode{"tree"} (default) returns a @code{digraph} of the
      ## predecessor tree; @qcode{"vector"} returns a row vector of
      ## predecessor indices; @qcode{"cell"} returns a column cell
      ## array of node paths.
      ##
      ## The @qcode{"Method"} option chooses the algorithm:
      ## @qcode{"auto"} (default) picks Dijkstra when all weights are
      ## non-negative and Bellman-Ford when any weight is negative;
      ## @qcode{"positive"} forces Dijkstra; @qcode{"mixed"} forces
      ## Bellman-Ford (which always errors for an undirected graph
      ## with any negative weight, because @math{u-v-u} forms a
      ## negative cycle).
      ## @seealso{graph, shortestpath, distances, allpaths}
      ## @end deftypefn

      if (nargin < 2)
        print_usage ();
      endif

      [s_idx, s_by_name] = __resolve_single_node__ (G, s, ...
                                                   "shortestpathtree");

      ## Undirected graph has no multigraph support today; W is either
      ## the (symmetric) weighted adjacency adj_ or its 0/1 skeleton.
      if (G.has_weights_)
        W = G.adj_;
      else
        W = spones (G.adj_);
      endif

      TR = __shortestpathtree_impl__ (G, W, s_idx, s_by_name, varargin{:});

    endfunction

    function [P, d, edgepaths] = allpaths (G, s, t, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{P} =} allpaths (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@var{P}, @var{d}] =} allpaths (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@var{P}, @var{d}, @var{edgepaths}] =} allpaths (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@dots{}] =} allpaths (@dots{}, @var{name}, @var{value})
      ## Return all simple paths from node @var{s} to node @var{t} in
      ## the undirected graph @var{G}.
      ##
      ## A simple path is a path that does not visit any node more
      ## than once.  Self-loops are ignored.  Edges may be traversed
      ## in either direction since @var{G} is undirected.
      ##
      ## Optional Name-Value pairs @qcode{"MaxPathLength"} and
      ## @qcode{"MinPathLength"} restrict the total weight of
      ## returned paths.  For unweighted graphs each edge weight is
      ## @code{1}, so these bounds act on the number of edges on the
      ## path.
      ## @seealso{graph, shortestpath, shortestpathtree, allcycles}
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif

      [s_idx, s_by_name] = __resolve_single_node__ (G, s, "allpaths");
      [t_idx, t_by_name] = __resolve_single_node__ (G, t, "allpaths");

      return_names = s_by_name || t_by_name;

      [P, d, edgepaths] = __allpaths_impl__ (G, s_idx, t_idx, ...
                                             return_names, varargin{:});

    endfunction

    function [cycles, edgecycles] = allcycles (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{cycles} =} allcycles (@var{G})
      ## @deftypefnx {} {[@var{cycles}, @var{edgecycles}] =} allcycles (@var{G})
      ## @deftypefnx {} {[@dots{}] =} allcycles (@dots{}, @var{name}, @var{value})
      ## Return all elementary cycles of the undirected graph @var{G}.
      ##
      ## A cycle is a closed walk in which no node and no edge is
      ## repeated, except that the walk starts and ends at the same
      ## node.  A self-loop is a 1-cycle.  For a multigraph, two
      ## parallel edges between the same pair of distinct nodes form a
      ## 2-cycle.  Each undirected cycle is reported once in canonical
      ## orientation: the smallest node comes first; for cycles of
      ## length @math{\geq 3} the smaller of the two neighbouring
      ## nodes comes second; for 2-cycles the smaller-indexed parallel
      ## edge is taken first.
      ##
      ## See @code{allcycles} for the full description, the returned
      ## shapes, and the @qcode{"MaxNumCycles"},
      ## @qcode{"MinCycleLength"}, @qcode{"MaxCycleLength"} options.
      ## @seealso{graph, allpaths, conncomp}
      ## @end deftypefn

      [cycles, edgecycles] = __allcycles_impl__ (G, varargin{:});

    endfunction

    function mf = maxflow (G, s, t, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{mf} =} maxflow (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{mf} =} maxflow (@var{G}, @var{s}, @var{t}, @var{algorithm})
      ## Return the maximum flow value @var{mf} from node @var{s} to
      ## node @var{t} in the undirected graph @var{G}.
      ##
      ## @var{s} and @var{t} are scalar node identifiers (a positive
      ## integer index, a char row vector naming a node, or a
      ## 1-element cell array of strings).  Edge weights are
      ## interpreted as capacities and must be non-negative; when
      ## @var{G} is unweighted every edge has capacity @code{1}.
      ## Each undirected edge acts as a pair of antiparallel arcs
      ## with the stored capacity available in either direction.
      ## Self-loops do not contribute to any @math{s-t} flow.  When
      ## @code{@var{s} == @var{t}} or when @var{t} is not reachable
      ## from @var{s} along edges with positive capacity, @var{mf}
      ## is @code{0}.
      ##
      ## The optional @var{algorithm} argument selects the solver
      ## (case-insensitive): @qcode{"augmentpath"} (default) uses the
      ## Edmonds-Karp implementation of Ford-Fulkerson;
      ## @qcode{"searchtrees"} uses a dual-search-tree augmenting-path
      ## method that grows one BFS tree from @var{s} and another
      ## backward from @var{t} and augments along the shortest joining
      ## path.  Both algorithms return the same flow value.
      ## @seealso{graph, mincut, shortestpath, distances}
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif

      algorithm = __maxflow_parse_algorithm__ (varargin);

      [s_idx, ~] = __resolve_single_node__ (G, s, "maxflow");
      [t_idx, ~] = __resolve_single_node__ (G, t, "maxflow");

      N = numnodes (G);

      ## Extract the undirected edge list from the lower triangle of
      ## adj_ (each {u,v} pair with u > v appears exactly once).  For
      ## the residual graph each undirected edge becomes two
      ## antiparallel directed arcs, each carrying the full capacity;
      ## the augmenting-path algorithms then augment correctly even
      ## when flow traverses the edge in the "wrong" orientation.
      [tt_end, ss_end, w_end] = find (tril (G.adj_));
      if (isempty (ss_end))
        uu = zeros (0, 1);
        vv = zeros (0, 1);
        caps = zeros (0, 1);
      else
        if (G.has_weights_)
          caps_one = w_end(:);
        else
          caps_one = ones (numel (ss_end), 1);
        endif
        ## Two antiparallel directed arcs per undirected edge.
        uu = [ss_end(:); tt_end(:)];
        vv = [tt_end(:); ss_end(:)];
        caps = [caps_one; caps_one];
      endif

      ## Validate capacities up front so the error message is
      ## predictable regardless of how the algorithm is dispatched
      ## below.
      if (! isempty (caps))
        if (! isreal (caps) || any (isnan (caps)))
          error ("Octave:invalid-input-arg", ...
                 "maxflow: edge weights must be finite real numbers (NaN not allowed)");
        endif
        if (any (caps < 0))
          error ("Octave:invalid-input-arg", ...
                 "maxflow: edge weights must be non-negative capacities");
        endif
      endif

      switch (algorithm)
        case "augmentpath"
          mf = __maxflow_edmonds_karp__ (uu, vv, caps, N, s_idx, t_idx);
        case "searchtrees"
          mf = __maxflow_searchtrees__ (uu, vv, caps, N, s_idx, t_idx);
        otherwise
          ## Parser guarantees a valid name; safety net.
          error ("Octave:invalid-input-arg", ...
                 "maxflow: internal error -- unknown algorithm '%s'", ...
                 algorithm);
      endswitch

    endfunction

    function [mf, GF, cs, ct] = mincut (G, s, t)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{mf} =} mincut (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@var{mf}, @var{GF}] =} mincut (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {[@var{mf}, @var{GF}, @var{cs}, @var{ct}] =} mincut (@var{G}, @var{s}, @var{t})
      ## Return the minimum @math{s}-@math{t} cut in the undirected
      ## graph @var{G}.  By the max-flow min-cut theorem @var{mf}
      ## equals @code{maxflow (@var{G}, @var{s}, @var{t})}.  When
      ## additional outputs are requested, @var{GF} is a
      ## @code{digraph} of the flow-carrying arcs (each undirected
      ## edge contributes a single directed arc in the direction of
      ## net flow, with the flow value as weight), and @var{cs} /
      ## @var{ct} partition the nodes into the source and sink sides
      ## of the minimum cut.
      ## @seealso{graph, maxflow, shortestpath, distances}
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif

      [s_idx, s_by_name] = __resolve_single_node__ (G, s, "mincut");
      [t_idx, t_by_name] = __resolve_single_node__ (G, t, "mincut");
      return_names = s_by_name || t_by_name;

      N = numnodes (G);

      ## Extract the undirected edge list from the lower triangle of
      ## adj_.  For the residual graph each undirected edge becomes
      ## two antiparallel directed arcs each carrying the full
      ## capacity; we remember the undirected pairing so we can pick
      ## the net flow direction when building GF.
      [tt_end, ss_end, w_end] = find (tril (G.adj_));
      Ku = numel (ss_end);
      if (Ku == 0)
        uu = zeros (0, 1);
        vv = zeros (0, 1);
        caps = zeros (0, 1);
      else
        if (G.has_weights_)
          caps_one = w_end(:);
        else
          caps_one = ones (Ku, 1);
        endif
        ## Two antiparallel arcs per undirected edge.  The first Ku
        ## entries are the "forward" arcs (u = lower index after
        ## tril: stored as ss_end -> tt_end); the next Ku are the
        ## matching reverse arcs.
        uu = [ss_end(:); tt_end(:)];
        vv = [tt_end(:); ss_end(:)];
        caps = [caps_one; caps_one];
      endif

      ## Validate capacities.
      if (! isempty (caps))
        if (! isreal (caps) || any (isnan (caps)))
          error ("Octave:invalid-input-arg", ...
                 "mincut: edge weights must be finite real numbers (NaN not allowed)");
        endif
        if (any (caps < 0))
          error ("Octave:invalid-input-arg", ...
                 "mincut: edge weights must be non-negative capacities");
        endif
      endif

      [mf, flow, reach_s] = ...
          __maxflow_edmonds_karp__ (uu, vv, caps, N, s_idx, t_idx);

      if (nargout <= 1)
        return;
      endif

      ## Reduce the antiparallel arc pair to the net flow direction.
      ## flow(1:Ku) is the forward-direction flow for each undirected
      ## edge; flow(Ku+1:2*Ku) is the reverse.  The net flow is
      ## forward - reverse: positive -> arc in the forward direction,
      ## negative -> arc in the reverse direction.
      if (Ku > 0)
        net = flow(1:Ku) - flow(Ku+1:2*Ku);
        keep_fwd = net > 0;
        keep_rev = net < 0;
        gf_src = [ss_end(keep_fwd); tt_end(keep_rev)];
        gf_dst = [tt_end(keep_fwd); ss_end(keep_rev)];
        gf_w   = [ net(keep_fwd); -net(keep_rev)];
      else
        gf_src = zeros (0, 1);
        gf_dst = zeros (0, 1);
        gf_w   = zeros (0, 1);
      endif

      ## Build GF as a digraph so flow direction is captured.
      if (! isempty (gf_src))
        if (! isempty (G.nodenames_))
          GF = digraph (gf_src, gf_dst, gf_w, G.nodenames_);
        else
          GF = digraph (gf_src, gf_dst, gf_w, N);
        endif
      else
        if (! isempty (G.nodenames_))
          GF = digraph (zeros (0, 1), zeros (0, 1), zeros (0, 1), ...
                        G.nodenames_);
        else
          GF = digraph (N);
        endif
      endif

      if (nargout <= 2)
        return;
      endif

      cs_idx = find (reach_s);
      ct_idx = find (! reach_s);

      if (return_names)
        if (isempty (cs_idx))
          cs = cell (0, 1);
        else
          cs = G.nodenames_(cs_idx);
          cs = cs(:);
        endif
        if (isempty (ct_idx))
          ct = cell (0, 1);
        else
          ct = G.nodenames_(ct_idx);
          ct = ct(:);
        endif
      else
        cs = double (cs_idx(:));
        ct = double (ct_idx(:));
      endif

    endfunction

    function c = centrality (G, type, varargin)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{c} =} centrality (@var{G}, @var{type})
      ## Return the centrality of each node in the undirected graph
      ## @var{G}.  @var{type} is a character row vector
      ## (case-insensitive) selecting the centrality measure.
      ##
      ## Supported types:
      ## @table @code
      ## @item "degree"
      ## The number of edges incident to each node, counting a
      ## self-loop twice (MATLAB convention).
      ## @item "closeness"
      ## Closeness centrality, computed as
      ## @math{(N-1) / sum_{j != i} d(i, j)} where @math{d} is the
      ## all-pairs shortest-path matrix returned by
      ## @code{distances}.  Unreachable pairs contribute @code{Inf}
      ## to the sum so disconnected nodes receive a centrality of
      ## zero.  Stored edge weights are used when @var{G} is
      ## weighted (BFS is used otherwise).
      ## @item "betweenness"
      ## Betweenness centrality, the number of unordered pair
      ## shortest paths passing through each node:
      ## @math{c(v) = sum_{s < t, v != s, v != t} sigma_{s, t}(v) / sigma_{s, t}}
      ## where @math{sigma_{s, t}} is the number of shortest paths
      ## from @math{s} to @math{t}.  Computed with Brandes' algorithm
      ## using unweighted (BFS) shortest paths; stored edge weights
      ## and parallel edges are ignored by the default call.
      ## @item "pagerank"
      ## PageRank centrality, computed by power iteration on the
      ## row-stochastic transition matrix with a damping factor
      ## (@qcode{"FollowProbability"}) and uniform-teleportation
      ## treatment of dangling nodes.  Stored edge weights are
      ## honoured.  Recognised Name-Value options:
      ## @table @code
      ## @item "FollowProbability"
      ## Real scalar in @code{[0, 1]}, default @code{0.85}.
      ## @item "MaxIterations"
      ## Positive integer scalar, default @code{100}.
      ## @item "Tolerance"
      ## Non-negative finite real scalar, default @code{1e-4}.
      ## @end table
      ## @item "eigenvector"
      ## Eigenvector centrality: the principal (Perron) eigenvector
      ## of the weighted adjacency matrix, L1-normalised so the
      ## entries sum to @code{1}.  Computed by power iteration with
      ## an identity shift to guarantee convergence on bipartite
      ## graphs.  Only defined for an undirected graph; on a
      ## @code{digraph} use @code{"pagerank"}, @code{"hubs"}, or
      ## @code{"authorities"} instead.
      ## @end table
      ##
      ## Two Name-Value options provide custom per-edge weight vectors
      ## (length @code{numedges (@var{G})}) for the distance and
      ## iterative centralities:
      ##
      ## @table @code
      ## @item "Cost"
      ## Positive per-edge costs used for shortest-path-based
      ## @code{"closeness"} and @code{"betweenness"}.
      ## @item "Importance"
      ## Non-negative per-edge importances used for
      ## @code{"pagerank"} and @code{"eigenvector"}.
      ## @end table
      ##
      ## The directed-only types @code{"indegree"},
      ## @code{"outdegree"}, @code{"incloseness"} and
      ## @code{"outcloseness"} are not defined for an undirected
      ## graph.  The result is a column vector of length
      ## @code{numnodes (@var{G})}.
      ## @seealso{graph, degree, distances, centrality}
      ## @end deftypefn

      if (nargin < 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to centrality: expected 2 arguments");
      endif

      if (! ischar (type) || (! isempty (type) && ! isrow (type)))
        error ("Octave:invalid-input-arg", ...
               "centrality: TYPE must be a character row vector");
      endif

      if (isempty (type))
        error ("Octave:invalid-input-arg", ...
               "centrality: TYPE must not be empty");
      endif

      ## Only the types whose helpers take options can pass varargin
      ## through; every other TYPE must reject trailing arguments here
      ## so the user gets a clear "no options supported" error instead
      ## of a confusing downstream failure.  US-CT07 adds 'Cost' to
      ## closeness/betweenness family and 'Importance' to the
      ## iterative family (pagerank/eigenvector/hubs/authorities); the
      ## digraph-only types are whitelisted here too so the "only
      ## defined for digraph" error still reaches users even when they
      ## supplied Name-Value options.
      opts_accepting_types = {"closeness", "outcloseness", "incloseness", ...
                              "betweenness", "pagerank", "eigenvector", ...
                              "hubs", "authorities"};
      if (! any (strcmp (lower (type), opts_accepting_types)) ...
          && ! isempty (varargin))
        error ("Octave:invalid-input-arg", ...
               "centrality: no name-value options are supported for TYPE '%s'", ...
               type);
      endif

      switch (lower (type))
        case "degree"
          c = G.degree ();
        case {"indegree", "outdegree"}
          error ("Octave:invalid-input-arg", ...
                 ["centrality: TYPE '%s' is only defined for a ", ...
                  "digraph; use 'degree' for an undirected graph"], type);
        case "closeness"
          c = __centrality_closeness__ (G, "out", varargin{:});
        case "betweenness"
          c = __centrality_betweenness__ (G, varargin{:});
        case "pagerank"
          c = __centrality_pagerank__ (G, varargin{:});
        case "eigenvector"
          c = __centrality_eigenvector__ (G, varargin{:});
        case {"incloseness", "outcloseness", "hubs", "authorities"}
          error ("Octave:invalid-input-arg", ...
                 ["centrality: TYPE '%s' is only defined for a ", ...
                  "digraph"], type);
        otherwise
          error ("Octave:invalid-input-arg", ...
                 "centrality: unknown TYPE '%s'", type);
      endswitch

    endfunction

    function tf = isisomorphic (G1, G2)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{tf} =} isisomorphic (@var{G1}, @var{G2})
      ## Return @code{true} if the undirected graphs @var{G1} and
      ## @var{G2} are isomorphic, @code{false} otherwise.
      ##
      ## Both arguments must be @code{graph} objects; calling this
      ## method with a mix of @code{graph} and @code{digraph} inputs
      ## raises an error.  Node names and edge weights are ignored;
      ## only the undirected adjacency structure (including edge
      ## multiplicities for a multigraph and self-loops) determines
      ## the result.  The underlying search uses the VF2 algorithm.
      ## @seealso{graph, isomorphism}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to isisomorphic: expected 2 arguments");
      endif

      if (! isa (G2, "graph"))
        error ("Octave:invalid-input-arg", ...
               ["isisomorphic: G1 and G2 must be of the same class; ", ...
                "G1 is a graph but G2 is not"]);
      endif

      A1 = adjacency (G1);
      A2 = adjacency (G2);
      [~, tf] = __isomorphism_vf2__ (A1, A2, false);
      tf = logical (tf);

    endfunction

    function disp (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {} disp (@var{G})
      ## Print a concise, human-readable summary of the graph @var{G}:
      ## a header line reporting the node and edge counts, followed
      ## by the first few edges (at most 10) when any are present.
      ## The header uses singular/plural forms for 1-node and 1-edge
      ## graphs to match MATLAB's conventions.  Edges are printed in
      ## lexicographic order with @code{EndNode1 <= EndNode2}; when
      ## @var{G} has node names the names are printed instead of
      ## numeric indices.  A trailing continuation line reports any
      ## edges that were elided past the 10-row limit.
      ## @seealso{graph, display, numedges, numnodes}
      ## @end deftypefn

      if (nargin != 1)
        print_usage ();
      endif

      N = numnodes (G);
      M = numedges (G);
      if (N == 1)
        n_word = "node";
      else
        n_word = "nodes";
      endif
      if (M == 1)
        e_word = "edge";
      else
        e_word = "edges";
      endif

      if (M == 0)
        printf ("  graph with %d %s and %d %s.\n", N, n_word, M, e_word);
        return;
      endif

      printf ("  graph with %d %s and %d %s:\n\n", N, n_word, M, e_word);

      ## Show at most the first 10 edges.  Truncation is reported on a
      ## trailing continuation line so the output stays short on large
      ## graphs while still giving a readable preview.
      max_shown = 10;
      n_show = min (M, max_shown);
      e = G.Edges;

      if (! isempty (G.nodenames_))
        src_labels = G.nodenames_(e.EndNodes(1:n_show, 1));
        dst_labels = G.nodenames_(e.EndNodes(1:n_show, 2));
      else
        src_labels = arrayfun (@(x) sprintf ("%d", x), ...
                               e.EndNodes(1:n_show, 1), ...
                               "UniformOutput", false);
        dst_labels = arrayfun (@(x) sprintf ("%d", x), ...
                               e.EndNodes(1:n_show, 2), ...
                               "UniformOutput", false);
      endif
      src_labels = src_labels(:);
      dst_labels = dst_labels(:);

      col1_w = max ([cellfun("numel", src_labels); numel("EndNode1")]);
      col2_w = max ([cellfun("numel", dst_labels); numel("EndNode2")]);

      has_w = isfield (e, "Weight");

      if (has_w)
        printf ("    %-*s    %-*s    %s\n", ...
                col1_w, "EndNode1", col2_w, "EndNode2", "Weight");
      else
        printf ("    %-*s    %-*s\n", col1_w, "EndNode1", col2_w, "EndNode2");
      endif

      for ii = 1:n_show
        if (has_w)
          printf ("    %-*s    %-*s    %g\n", ...
                  col1_w, src_labels{ii}, col2_w, dst_labels{ii}, ...
                  e.Weight(ii));
        else
          printf ("    %-*s    %-*s\n", ...
                  col1_w, src_labels{ii}, col2_w, dst_labels{ii});
        endif
      endfor

      if (M > n_show)
        remaining = M - n_show;
        if (remaining == 1)
          r_word = "edge";
        else
          r_word = "edges";
        endif
        printf ("    ... (%d more %s)\n", remaining, r_word);
      endif

    endfunction

  endmethods

endclassdef

## Local helper: construct K default rows matching the element type of
## the existing edge-attribute column @var{col}.  Used by the addedge
## method to extend edge-attribute columns for the new edges.
function r = graph_default_edge_rows (col, K)

  cols = size (col, 2);
  if (K == 0)
    r = col(1:0, :);
    return;
  endif
  if (iscellstr (col))
    r = repmat ({""}, K, cols);
  elseif (iscell (col))
    r = cell (K, cols);
  elseif (islogical (col))
    r = false (K, cols);
  elseif (isnumeric (col))
    r = zeros (K, cols, class (col));
  elseif (ischar (col))
    r = repmat (' ', K, cols);
  else
    error ("Octave:invalid-input-arg", ...
           "addedge: cannot extend edge-attribute column of class %s", ...
           class (col));
  endif

endfunction

## Helper: build a symmetric sparse adjacency from (s, t[, w]).
## For off-diagonal edges, store the weight at both (s, t) and (t, s).
## For self-loops, store the weight once at (i, i).  Rejects duplicate
## unordered endpoint pairs unless @var{skip_dup_check} is true (useful
## when the caller already validated uniqueness, e.g., the EdgeTable
## branch which needs the dup-check sparse for a different purpose).
function [A, hw] = build_adj (s, t, w, N, have_weights, skip_dup_check)

  if (nargin < 6)
    skip_dup_check = false;
  endif

  m = numel (s);

  if (! skip_dup_check)
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

## BIST — US-C13: graph(ET) with numeric EndNodes only (unweighted).
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! G = graph (ET);
%! assert (class (G), "graph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (! isfield (G.Edges, "Weight"));

## BIST — US-C13: graph(ET) with Weight column round-trips.
%!test
%! ET.EndNodes = [1 2; 2 3; 1 3];
%! ET.Weight = [10; 20; 30];
%! G = graph (ET);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 30; 20]);

## BIST — US-C13: row-vector Weight normalized to column.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! ET.Weight = [5 10];
%! G = graph (ET);
%! assert (G.Edges.Weight, [5; 10]);

## BIST — US-C13: edges in the EdgeTable are re-sorted into lex order;
## Weight follows its edge.
%!test
%! ET.EndNodes = [3 1; 2 3; 1 2];
%! ET.Weight = [30; 20; 10];
%! G = graph (ET);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 30; 20]);

## BIST — US-C13: unordered input pairs normalized to (smaller, bigger).
%!test
%! ET.EndNodes = [2 1; 3 2];
%! ET.Weight = [10; 20];
%! G = graph (ET);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [10; 20]);

## BIST — US-C13: extra numeric edge column preserved and reordered.
%!test
%! ET.EndNodes = [3 1; 1 2; 2 3];
%! ET.Weight = [30; 10; 20];
%! ET.Capacity = [300; 100; 200];
%! G = graph (ET);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [10; 30; 20]);
%! assert (G.Edges.Capacity, [100; 300; 200]);

## BIST — US-C13: extra cellstr edge column preserved and reordered.
%!test
%! ET.EndNodes = [3 1; 1 2; 2 3];
%! ET.Label = {"c"; "a"; "b"};
%! G = graph (ET);
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Label, {"a"; "c"; "b"});

## BIST — US-C13: multiple extra edge columns preserved simultaneously.
%!test
%! ET.EndNodes = [1 2; 2 3; 1 3];
%! ET.Weight = [1; 2; 3];
%! ET.Name = {"e1"; "e2"; "e3"};
%! ET.Cost = [5; 10; 15];
%! G = graph (ET);
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 1 3; 2 3]);
%! assert (E.Weight, [1; 3; 2]);
%! assert (E.Name, {"e1"; "e3"; "e2"});
%! assert (E.Cost, [5; 15; 10]);

## BIST — US-C13: extra edge columns also work on unweighted tables.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! ET.Kind = {"in"; "out"};
%! G = graph (ET);
%! assert (! isfield (G.Edges, "Weight"));
%! assert (G.Edges.Kind, {"in"; "out"});

## BIST — US-C13: graph(ET, NT) — NT.Name sets the node names.
%!test
%! ET.EndNodes = [1 2; 2 3; 1 3];
%! ET.Weight = [1; 2; 3];
%! NT.Name = {"alpha"; "beta"; "gamma"};
%! G = graph (ET, NT);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"alpha"; "beta"; "gamma"});
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (G.Edges.Weight, [1; 3; 2]);

## BIST — US-C13: NT can provide isolated trailing nodes (N > max endpoint).
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Name = {"a"; "b"; "c"; "d"; "e"};
%! G = graph (ET, NT);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});

## BIST — US-C13: extra node columns preserved.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! G = graph (ET, NT);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Nodes.Size, [10; 20; 30]);

## BIST — US-C13: multiple extra node columns preserved.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! NT.Kind = {"x"; "y"; "z"};
%! G = graph (ET, NT);
%! assert (G.Nodes.Size, [10; 20; 30]);
%! assert (G.Nodes.Kind, {"x"; "y"; "z"});

## BIST — US-C13: NT without Name field — node count inferred from column
## length; Nodes.Name stays empty.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Size = [10; 20; 30];
%! G = graph (ET, NT);
%! assert (numnodes (G), 3);
%! assert (G.Nodes.Name, cell (0, 1));
%! assert (G.Nodes.Size, [10; 20; 30]);

## BIST — US-C13: cellstr EndNodes without NT infers names from first
## appearance in EndNodes.
%!test
%! ET.EndNodes = {"a", "b"; "b", "c"; "c", "a"};
%! G = graph (ET);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.EndNodes, [1 2; 1 3; 2 3]);

## BIST — US-C13: cellstr EndNodes with NT looks up in NT.Name.
%!test
%! ET.EndNodes = {"x", "y"; "y", "z"};
%! NT.Name = {"x"; "y"; "z"; "w"};
%! G = graph (ET, NT);
%! assert (numnodes (G), 4);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"x"; "y"; "z"; "w"});
%! assert (G.Edges.EndNodes, [1 2; 2 3]);

## BIST — US-C13: cellstr EndNodes with weights and extra columns.
%!test
%! ET.EndNodes = {"a", "b"; "b", "c"};
%! ET.Weight = [1.5; 2.5];
%! ET.Note = {"hi"; "lo"};
%! G = graph (ET);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.Weight, [1.5; 2.5]);
%! assert (G.Edges.Note, {"hi"; "lo"});

## BIST — US-C13: round-trip an existing graph via its Edges+Nodes.
%!test
%! G1 = graph ([1 2 3], [2 3 1], [10 20 30], {"a", "b", "c"});
%! G2 = graph (G1.Edges, G1.Nodes);
%! assert (numnodes (G2), numnodes (G1));
%! assert (numedges (G2), numedges (G1));
%! assert (G2.Edges.EndNodes, G1.Edges.EndNodes);
%! assert (G2.Edges.Weight, G1.Edges.Weight);
%! assert (G2.Nodes.Name, G1.Nodes.Name);

## BIST — US-C13: round-trip with isolated named nodes.
%!test
%! G1 = graph ([1 2], [2 3], [5 10], {"a", "b", "c", "d"});
%! G2 = graph (G1.Edges, G1.Nodes);
%! assert (numnodes (G2), 4);
%! assert (numedges (G2), 2);
%! assert (G2.Nodes.Name, {"a"; "b"; "c"; "d"});
%! assert (G2.Edges.Weight, [5; 10]);

## BIST — US-C13: empty edge table yields empty graph.
%!test
%! ET.EndNodes = zeros (0, 2);
%! G = graph (ET);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C13: empty edge table with NT yields N isolated nodes.
%!test
%! ET.EndNodes = zeros (0, 2);
%! NT.Name = {"p"; "q"; "r"};
%! G = graph (ET, NT);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);
%! assert (G.Nodes.Name, {"p"; "q"; "r"});

## BIST — US-C13: single-edge table.
%!test
%! ET.EndNodes = [1 2];
%! ET.Weight = 7;
%! G = graph (ET);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 1);
%! assert (G.Edges.Weight, 7);

## BIST — US-C13: self-loop in ET is preserved (single row in EndNodes).
%!test
%! ET.EndNodes = [1 1; 2 2];
%! G = graph (ET);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 1; 2 2]);

## BIST — US-C13: ET must be a struct with EndNodes field.
%!error <EndNodes> graph (struct ("Weight", [1; 2]))

## BIST — US-C13: EndNodes with wrong number of columns rejected.
%!error <two columns> graph (struct ("EndNodes", [1 2 3; 4 5 6]))
%!error <two columns> graph (struct ("EndNodes", [1; 2; 3]))

## BIST — US-C13: 3-D EndNodes rejected.
%!error <EndNodes> graph (struct ("EndNodes", ones (2, 2, 2)))

## BIST — US-C13: EndNodes of disallowed type rejected.
%!error <EndNodes> graph (struct ("EndNodes", true (2, 2)))

## BIST — US-C13: Weight row count must match EndNodes.
%!error <Weight> graph (struct ("EndNodes", [1 2; 2 3], "Weight", [1; 2; 3]))

## BIST — US-C13: Non-EndNodes/Weight columns must have matching row count.
%!error <Capacity> graph (struct ("EndNodes", [1 2; 2 3], "Capacity", [1; 2; 3]))

## BIST — US-C13: NT.Name must be cellstr.
%!error <Name> graph (struct ("EndNodes", [1 2]), struct ("Name", [1 2]))

## BIST — US-C13: NT.Name with duplicates rejected.
%!error <unique> ...
%! graph (struct ("EndNodes", [1 2]), struct ("Name", {{"a"; "a"}}))

## BIST — US-C13: Numeric EndNodes out of range (index > numnodes from NT).
%!error <exceed> ...
%! graph (struct ("EndNodes", [1 3]), struct ("Name", {{"a"; "b"}}))

## BIST — US-C13: cellstr endpoint not found in NT.Name rejected.
%!error <not found> ...
%! graph (struct ("EndNodes", {{"a", "c"}}), struct ("Name", {{"a"; "b"}}))

## BIST — US-C13: Inconsistent NT column lengths rejected.
%!error <length> ...
%! graph (struct ("EndNodes", [1 2]), ...
%!        struct ("Name", {{"a"; "b"}}, "Size", 1))

## BIST — US-C13: Non-scalar struct ET rejected.
%!error <scalar struct> graph (struct ("EndNodes", {[1 2], [2 3]}))

## BIST — US-C13: Duplicate edges in ET rejected.
%!error <duplicate> ...
%! graph (struct ("EndNodes", [1 2; 1 2]))

## BIST — US-C13: Undirected duplicate — (1,2) and (2,1) are the same edge.
%!error <duplicate> ...
%! graph (struct ("EndNodes", [1 2; 2 1]))

## BIST — US-C13: NT provided without Name but mismatched column lengths
## rejected.
%!error <length> ...
%! graph (struct ("EndNodes", [1 2]), ...
%!        struct ("Size", [1; 2], "Kind", {{"a"; "b"; "c"}}))

## BIST — US-C14: G.Nodes returns a struct on every constructor form.
%!test
%! assert (isstruct (graph ().Nodes));
%! assert (isstruct (graph (3).Nodes));
%! assert (isstruct (graph ([1 2], [2 3]).Nodes));
%! assert (isstruct (graph ([0 1; 1 0]).Nodes));
%! assert (isstruct (graph ([1 2], [2 3], [10 20], {"a","b","c"}).Nodes));

## BIST — US-C14: G.Edges returns a struct on every constructor form.
%!test
%! assert (isstruct (graph ().Edges));
%! assert (isstruct (graph (3).Edges));
%! assert (isstruct (graph ([1 2], [2 3]).Edges));
%! assert (isstruct (graph ([0 1; 1 0]).Edges));
%! assert (isstruct (graph ([1 2], [2 3], [10 20], {"a","b","c"}).Edges));

## BIST — US-C14: G.Nodes.Name is always present, always a column
## cellstr (empty cell(0,1) when unnamed, populated cellstr otherwise).
%!test
%! G = graph ();
%! assert (isfield (G.Nodes, "Name"));
%! assert (iscellstr (G.Nodes.Name));
%! assert (G.Nodes.Name, cell (0, 1));
%! G = graph (3);
%! assert (iscellstr (G.Nodes.Name));
%! assert (G.Nodes.Name, cell (0, 1));
%! G = graph ([1 2], [2 3], [10 20], {"a","b","c"});
%! assert (iscellstr (G.Nodes.Name));
%! assert (G.Nodes.Name, {"a"; "b"; "c"});

## BIST — US-C14: G.Edges.EndNodes is always m-by-2 numeric, even on a
## truly empty graph (0 nodes).  This closes a shape-consistency gap
## where find() on a 0-by-0 sparse matrix previously returned 0-by-0
## arrays, leaking a 0-by-0 EndNodes.
%!test
%! G = graph ();
%! assert (isfield (G.Edges, "EndNodes"));
%! assert (isnumeric (G.Edges.EndNodes));
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = graph (0);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = graph (3);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = graph ([], []);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = graph ([], [], [], 0);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = graph (sparse (0, 0));
%! assert (size (G.Edges.EndNodes), [0 2]);

## BIST — US-C14: EndNodes is numeric indices even when endpoints came
## in as strings (via the EdgeTable constructor, which supports cellstr
## EndNodes with first-appearance name inference).
%!test
%! ET = struct ("EndNodes", {{"a","b"; "b","c"}});
%! G = graph (ET);
%! assert (isnumeric (G.Edges.EndNodes));
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});

## BIST — US-C14: Weight appears only when the graph was built with
## explicit weights.
%!test
%! Gu = graph ([1 2], [2 3]);
%! assert (isfield (Gu.Edges, "EndNodes"));
%! assert (! isfield (Gu.Edges, "Weight"));
%! Gw = graph ([1 2], [2 3], [10 20]);
%! assert (isfield (Gw.Edges, "EndNodes"));
%! assert (isfield (Gw.Edges, "Weight"));
%! assert (iscolumn (Gw.Edges.Weight));
%! assert (Gw.Edges.Weight, [10; 20]);

## BIST — US-C14: on a weighted graph with zero edges, Weight is still
## present as an m-by-1 (0-by-1) column.
%!test
%! G = graph (sparse (3, 3));
%! assert (isfield (G.Edges, "Weight"));
%! assert (size (G.Edges.Weight), [0 1]);
%! assert (size (G.Edges.EndNodes), [0 2]);

## BIST — US-C14: fieldnames(G.Edges) order is EndNodes -> Weight ->
## extras (in the order the EdgeTable declared them).
%!test
%! ET = struct ("EndNodes", [1 2; 2 3], "Weight", [10; 20], ...
%!              "Label", {{"a"; "b"}});
%! G = graph (ET);
%! assert (fieldnames (G.Edges), {"EndNodes"; "Weight"; "Label"});

## BIST — US-C14: fieldnames(G.Edges) on an unweighted graph with extras
## omits Weight.
%!test
%! ET = struct ("EndNodes", [1 2; 2 3], "Kind", {{"solid"; "dashed"}});
%! G = graph (ET);
%! assert (fieldnames (G.Edges), {"EndNodes"; "Kind"});

## BIST — US-C14: fieldnames(G.Nodes) order is Name -> extras.
%!test
%! NT = struct ("Name", {{"x"; "y"; "z"}}, "Size", [1; 2; 3], ...
%!              "Tag", {{"A"; "B"; "C"}});
%! ET = struct ("EndNodes", [1 2; 2 3]);
%! G = graph (ET, NT);
%! assert (fieldnames (G.Nodes), {"Name"; "Size"; "Tag"});

## BIST — US-C14: G.Nodes is read-only (SetAccess=private).
%!test
%! G = graph ([1 2], [2 3], [10 20]);
%! fail ("G.Nodes = struct ();", "private access");

## BIST — US-C14: G.Edges is read-only (SetAccess=private).
%!test
%! G = graph ([1 2], [2 3], [10 20]);
%! fail ("G.Edges = struct ();", "private access");

## BIST — US-C14: reading G.Edges twice yields the same struct
## (deterministic, idempotent).
%!test
%! G = graph ([1 3 2], [2 1 3], [10 20 30]);
%! assert (isequal (G.Edges, G.Edges));

## BIST — US-C14: reading G.Nodes twice yields the same struct.
%!test
%! G = graph ([1 2], [2 3], [10 20], {"a","b","c"});
%! assert (isequal (G.Nodes, G.Nodes));

## BIST — US-C14: dynamic field access G.("Nodes") / G.("Edges") works
## and equals the static form.
%!test
%! G = graph ([1 2], [2 3], [10 20], {"a","b","c"});
%! assert (isequal (G.("Nodes"), G.Nodes));
%! assert (isequal (G.("Edges"), G.Edges));

## BIST — US-C14: a fully-featured graph (named + weighted + extra
## edge and node columns) exposes every column via G.Nodes and G.Edges.
%!test
%! ET = struct ("EndNodes", {{"a","b"; "b","c"; "c","a"}}, ...
%!              "Weight", [1; 2; 3], ...
%!              "Label", {{"ab"; "bc"; "ca"}});
%! NT = struct ("Name", {{"a"; "b"; "c"}}, "Size", [10; 20; 30]);
%! G = graph (ET, NT);
%! N = G.Nodes;
%! E = G.Edges;
%! assert (N.Name, {"a"; "b"; "c"});
%! assert (N.Size, [10; 20; 30]);
%! assert (E.EndNodes, [1 2; 1 3; 2 3]);   # undirected: lex (min, max)
%! assert (E.Weight, [1; 3; 2]);            # weights follow their edges
%! assert (E.Label, {"ab"; "ca"; "bc"});

## BIST — US-C14: isolated named nodes appear in G.Nodes even with zero
## edges, and G.Edges.EndNodes is still 0-by-2.
%!test
%! NT = struct ("Name", {{"x"; "y"; "z"}});
%! ET = struct ("EndNodes", zeros (0, 2));
%! G = graph (ET, NT);
%! assert (G.Nodes.Name, {"x"; "y"; "z"});
%! assert (size (G.Edges.EndNodes), [0 2]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);

## BIST — US-C14: property-driven round-trip Gx = graph(G.Edges, G.Nodes)
## preserves both Edges and Nodes identically.
%!test
%! ET = struct ("EndNodes", [1 2; 2 3; 1 3], ...
%!              "Weight", [10; 20; 30], ...
%!              "Tag", {{"e1"; "e2"; "e3"}});
%! NT = struct ("Name", {{"p"; "q"; "r"}}, "Rank", [1; 2; 3]);
%! G1 = graph (ET, NT);
%! G2 = graph (G1.Edges, G1.Nodes);
%! assert (isequal (G1.Edges, G2.Edges));
%! assert (isequal (G1.Nodes, G2.Nodes));

## BIST — US-C14: adjacency-constructed graph always has a Weight column
## (matrix form implies weighted, MATLAB parity).
%!test
%! G = graph ([0 1 0; 1 0 1; 0 1 0]);
%! assert (fieldnames (G.Edges), {"EndNodes"; "Weight"});

## BIST — US-C14: self-loops count once in G.Edges for undirected graph.
%!test
%! G = graph ([1 2 3], [2 3 3]);   # edge (2,3) and self-loop (3,3)
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 2 3; 3 3]);
%! assert (numedges (G), 3);

## BIST — US-C15: disp on the default (empty) graph reports 0 nodes
## and 0 edges and does not error.
%!test <*C15>
%! G = graph ();
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 0 nodes and 0 edges', 'once')));
%! ## Must NOT match the digraph header (leading "digraph").
%! assert (isempty (regexp (s, '\<digraph\>', 'once')));

## BIST — US-C15: disp on an N-node edgeless graph reports N nodes
## and 0 edges.
%!test <*C15>
%! G = graph (5);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 5 nodes and 0 edges', 'once')));

## BIST — US-C15: disp on a small weighted graph reports the correct
## counts in the header.
%!test <*C15>
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 3 nodes and 3 edges', 'once')));

## BIST — US-C15: singular word forms for 1 node and 1 edge.
%!test <*C15>
%! G = graph (1);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 1 node and 0 edges', 'once')));

## BIST — US-C15: singular word form for exactly 1 edge.
%!test <*C15>
%! G = graph (1, 2);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 2 nodes and 1 edge', 'once')));

## BIST — US-C15: disp on a graph with many edges shows the first few
## and reports the remaining count as a continuation line.
%!test <*C15>
%! ## Star K_{1,20}: 20 edges on 21 nodes.
%! s_in = ones (1, 20);
%! t_in = 2:21;
%! G = graph (s_in, t_in);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 21 nodes and 20 edges', 'once')));
%! assert (! isempty (regexp (s, 'more', 'once')));
%! assert (! isempty (regexp (s, '1\s+2', 'once')));

## BIST — US-C15: disp on a named graph prints node names, not
## numeric indices.
%!test <*C15>
%! G = graph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 3 nodes and 3 edges', 'once')));
%! assert (! isempty (strfind (s, "alpha")));
%! assert (! isempty (strfind (s, "beta")));
%! assert (! isempty (strfind (s, "gamma")));

## BIST — US-C15: disp shows a Weight column when weights are present.
%!test <*C15>
%! G = graph ([1 2], [2 3], [11 22]);
%! s = evalc ("disp (G)");
%! assert (! isempty (strfind (s, "Weight")));
%! assert (! isempty (regexp (s, '\<11\>', 'once')));
%! assert (! isempty (regexp (s, '\<22\>', 'once')));

## BIST — US-C15: disp omits the Weight column when unweighted.
%!test <*C15>
%! G = graph ([1 2], [2 3]);
%! s = evalc ("disp (G)");
%! assert (isempty (strfind (s, "Weight")));

## BIST — US-C15: disp on a graph lists self-loops once in the edge
## table (mirrors the undirected G.Edges semantics).
%!test <*C15>
%! G = graph ([1 2 3], [2 3 3]);   # edge (2,3) and self-loop (3,3)
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'graph with 3 nodes and 3 edges', 'once')));

## BIST — US-C15: display (G) includes a "G =" assignment prefix and
## contains the header string.
%!test <*C15>
%! G = graph ([1 2], [2 3]);
%! s = evalc ("display (G)");
%! assert (! isempty (strfind (s, "G =")));
%! assert (! isempty (regexp (s, 'graph with 3 nodes and 2 edges', 'once')));

## BIST — US-C15: disp writes to stdout (captured non-empty via evalc).
%!test <*C15>
%! G = graph ();
%! s = evalc ("disp (G)");
%! assert (! isempty (s));

## BIST — US-C15: disp on the truly empty graph ends with a period
## (no edges section to follow).
%!test <*C15>
%! G = graph ();
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, '0 edges\.\s*$', 'once')));
