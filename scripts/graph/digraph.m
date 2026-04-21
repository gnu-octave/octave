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
  ## @deftypefnx {} {@var{G} =} digraph (@var{EdgeTable})
  ## @deftypefnx {} {@var{G} =} digraph (@var{EdgeTable}, @var{NodeTable})
  ## @deftypefnx {} {@var{G} =} digraph (@dots{}, "omitselfloops")
  ## @deftypefnx {} {@var{G} =} digraph (@dots{}, "multigraph")
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
  ## With a single struct @var{EdgeTable}, build a digraph from the
  ## fields of the struct.  @var{EdgeTable} must have an
  ## @code{EndNodes} field (an @code{m}-by-2 numeric matrix of node
  ## indices or a cell array of strings), may have a @code{Weight}
  ## field (a length-@code{m} numeric vector), and may have any number
  ## of additional columns, which are preserved as extra edge
  ## attributes on the resulting digraph.  Edges are re-sorted into
  ## lexicographic @code{(source, destination)} order and every extra
  ## column is reordered to match.  Duplicate @code{(source,
  ## destination)} pairs are rejected unless the caller also passes
  ## the trailing @qcode{'multigraph'} flag (see below).
  ##
  ## With a second struct @var{NodeTable}, the node set is taken from
  ## @var{NodeTable}.  A @code{Name} field (a cell array of unique
  ## strings) is stored on @code{@var{G}.Nodes.Name}; any other
  ## columns are preserved as extra node attributes.  When
  ## @code{EndNodes} is a cell array of strings, each entry is looked
  ## up in @code{@var{NodeTable}.Name} (if provided) or in a name
  ## list inferred from first appearance in @code{EndNodes}.
  ## Until Octave has a built-in @code{table} class, this struct form
  ## stands in for MATLAB's @code{table}.
  ##
  ## Any of the forms above accepts a trailing string flag
  ## @qcode{'omitselfloops'} (case-insensitive).  When present, every
  ## self-loop edge (i.e.@: an edge whose source and destination node
  ## coincide) is dropped after the rest of the graph is built.  Any
  ## extra edge-attribute columns supplied via the @code{EdgeTable}
  ## form are filtered by the same mask so their row count remains in
  ## sync with the surviving edges.  Node names and node-attribute
  ## columns are unaffected.
  ##
  ## A trailing string flag @qcode{'multigraph'} (case-insensitive)
  ## permits parallel edges: duplicate @code{(source, destination)}
  ## pairs are accepted and every instance contributes its own edge
  ## row with its own weight.  @code{numedges (@var{G})} counts
  ## parallel edges individually and
  ## @code{ismultigraph (@var{G})} returns true when any pair of
  ## nodes has more than one edge between them.  Without this flag,
  ## duplicate edges are rejected with an error.  The
  ## @qcode{'multigraph'} and @qcode{'omitselfloops'} flags may be
  ## supplied together in either order.
  ##
  ## @code{digraph} is a value class: every mutator returns a new object,
  ## leaving the input unchanged.
  ##
  ## @strong{Properties}:
  ##
  ## @code{@var{G}.Nodes} is a struct standing in for MATLAB's @code{table}.
  ## It always has a @code{Name} field, a column cell array of strings
  ## giving each node's name.  For digraphs constructed without names the
  ## field is an empty @code{cell (0, 1)}.  Any columns supplied through
  ## the @var{NodeTable} form are preserved as extra fields (in the order
  ## they were declared).
  ##
  ## @code{@var{G}.Edges} is a struct standing in for MATLAB's @code{table}.
  ## It always has an @code{EndNodes} field, an @code{m}-by-2 numeric
  ## matrix whose row @var{i} is the @code{[source, destination]} pair
  ## of edge @var{i} in lexicographic order.  When the digraph was built
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
  ##
  ## ET.EndNodes = [1 2; 2 3; 3 1];
  ## ET.Weight   = [10; 20; 30];
  ## ET.Label    = @{"a"; "b"; "c"@};
  ## NT.Name     = @{"x"; "y"; "z"@};
  ## G = digraph (ET, NT);  # EdgeTable + NodeTable form
  ## G.Edges.Label          # ==> @{"a"; "b"; "c"@}
  ## G.Nodes.Name           # ==> @{"x"; "y"; "z"@}
  ##
  ## G = digraph ([1 2 3 4], [1 2 4 5], [10 20 30 40], "omitselfloops");
  ## numedges (G)           # ==> 2 (self-loops 1->1 and 2->2 dropped)
  ## G.Edges.EndNodes       # ==> [3 4; 4 5]
  ##
  ## G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
  ## numedges (G)           # ==> 3 (parallel 1->2 edges preserved)
  ## ismultigraph (G)       # ==> true
  ## G.Edges.EndNodes       # ==> [1 2; 1 2; 2 3]
  ## G.Edges.Weight         # ==> [10; 20; 30]
  ## @end group
  ## @end example
  ##
  ## @seealso{graph, numnodes, numedges, ismultigraph, addnode, addedge, rmnode, rmedge, reordernodes, subgraph, flipedge, successors, predecessors, neighbors, indegree, outdegree, findnode, findedge, edgecount, inedges, outedges, adjacency, incidence, laplacian, bfsearch, dfsearch, conncomp, biconncomp, condensation, toposort}
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

    ## Extra edge-attribute columns supplied by the user via the
    ## @code{digraph(ET)} or @code{digraph(ET, NT)} EdgeTable form.
    ## Each field is stored in lexicographic @code{(source, destination)}
    ## edge order so that @code{get.Edges} can return it directly.
    ## Weight is @emph{not} stored here (it lives in @code{adj_}).
    edge_attrs_ = struct ();

    ## Extra node-attribute columns supplied by the user via the
    ## NodeTable form.  Each field is stored in node-index order.
    ## Name is @emph{not} stored here (it lives in @code{nodenames_}).
    node_attrs_ = struct ();

    ## When true, this digraph was constructed with the
    ## @qcode{'multigraph'} flag and uses edge-list storage to permit
    ## parallel edges.  In that mode @code{adj_} is an empty N-by-N
    ## sparse placeholder (its size still gives @code{numnodes}) and
    ## the edges live in @code{mg_endnodes_} / @code{mg_weights_}.
    ## When false, @code{adj_} is the edge-carrying sparse matrix
    ## (simple-graph mode) and the @code{mg_*} arrays stay empty.
    is_multigraph_ = false;

    ## Lex-sorted @code{(source, destination)} pairs for multigraph
    ## storage.  Duplicates are adjacent.  Used only when
    ## @code{is_multigraph_} is true.  Stable sort preserves input
    ## order within each duplicate group.
    mg_endnodes_ = zeros (0, 2);

    ## Per-edge weights for multigraph storage, a column vector in the
    ## same row order as @code{mg_endnodes_}.  Used only when
    ## @code{is_multigraph_} and @code{has_weights_} are both true.
    mg_weights_ = zeros (0, 1);
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

      ## Pre-process the trailing @qcode{'omitselfloops'} (US-C09) and
      ## @qcode{'multigraph'} (US-C10) flags.  Each trailing char-row
      ## argument matching either flag (case-insensitive) is popped
      ## from the argument list and recorded so the constructor can
      ## route through multigraph storage and/or drop self-loops after
      ## the main build step.  The flags may appear in either order.
      ## Using local @var{args} / @var{nargs} shadows the built-in
      ## @code{varargin} / @code{nargin} so the existing dispatch
      ## branches keep their original shape.
      args = varargin;
      nargs = numel (args);
      omit_loops = false;
      is_multigraph = false;
      while (nargs >= 1 && ischar (args{end}) && isrow (args{end}))
        last = args{end};
        if (strcmpi (last, "omitselfloops"))
          omit_loops = true;
        elseif (strcmpi (last, "multigraph"))
          is_multigraph = true;
        else
          break;
        endif
        args(end) = [];
        nargs = numel (args);
      endwhile

      if (nargs == 0)
        ## Default constructor: empty graph.  Property defaults apply.
        G.is_multigraph_ = is_multigraph;
        ## Fall through to post-processing (which is a no-op on empty).
      elseif ((nargs == 1 && isstruct (args{1})) ...
              || (nargs == 2 && isstruct (args{1}) ...
                  && isstruct (args{2})))
        ## EdgeTable (and optional NodeTable) constructor.
        ## digraph (ET) or digraph (ET, NT).  ET is a scalar struct
        ## with an EndNodes field (numeric m-by-2 or cellstr m-by-2)
        ## and an optional Weight field; any other fields become extra
        ## edge-attribute columns.  NT is a scalar struct with an
        ## optional Name field; any other fields become extra
        ## node-attribute columns.  Edges are re-sorted into
        ## lexicographic (source, destination) order and every extra
        ## column is reordered to match.
        ET = args{1};
        have_nt = (nargs == 2);
        if (have_nt)
          NT = args{2};
        endif

        if (! isscalar (ET))
          error ("Octave:invalid-input-arg", ...
                 "digraph: EdgeTable must be a scalar struct");
        endif
        if (have_nt && ! isscalar (NT))
          error ("Octave:invalid-input-arg", ...
                 "digraph: NodeTable must be a scalar struct");
        endif
        if (! isfield (ET, "EndNodes"))
          error ("Octave:invalid-input-arg", ...
                 "digraph: EdgeTable must have an EndNodes field");
        endif

        EN = ET.EndNodes;
        if (! (isnumeric (EN) || iscellstr (EN)))
          error ("Octave:invalid-input-arg", ...
                 ["digraph: EndNodes must be a numeric matrix or ", ...
                  "a cell array of strings"]);
        endif
        if (ndims (EN) != 2 || size (EN, 2) != 2)
          error ("Octave:invalid-input-arg", ...
                 ["digraph: EndNodes must be a 2-D matrix with ", ...
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
                   ["digraph: numeric EndNodes entries must be ", ...
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
                   "digraph: Weight must be a numeric real vector");
          endif
          if (! (isvector (w_vec) || isempty (w_vec)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: Weight must be a vector");
          endif
          w_vec = double (w_vec(:));
          if (numel (w_vec) != m)
            error ("Octave:invalid-input-arg", ...
                   ["digraph: Weight length must match the number ", ...
                    "of rows in EndNodes"]);
          endif
          if (any (isnan (w_vec)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: Weight must not contain NaN");
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
                   ["digraph: EdgeTable column %s length must ", ...
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
                       ["digraph: NodeTable columns must all ", ...
                        "have the same length"]);
              endif
            endfor
          endif
          if (isfield (NT, "Name"))
            nm = NT.Name;
            if (! iscellstr (nm))
              error ("Octave:invalid-input-arg", ...
                     ["digraph: NodeTable Name must be a cell ", ...
                      "array of strings"]);
            endif
            nm = nm(:);
            if (numel (nm) != numel (unique (nm)))
              error ("Octave:invalid-input-arg", ...
                     ["digraph: NodeTable Name must contain ", ...
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
                     ["digraph: NodeTable column %s length must ", ...
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
                     ["digraph: EndNodes indices must not exceed ", ...
                      "the NodeTable node count"]);
            endif
          else
            N = max (max (s_idx), max (t_idx));
          endif
        endif

        if (is_multigraph)
          ## Multigraph storage: sort input rows lex-stably so duplicate
          ## (s, t) pairs stay adjacent in input order, and carry Weight
          ## plus every extra edge column along via the same permutation.
          if (m > 0)
            [EN, ord] = sortrows ([s_idx, t_idx]);
            G.mg_endnodes_ = EN;
            if (have_weights)
              G.mg_weights_ = w_vec(ord);
              G.has_weights_ = true;
            endif
            ef2 = fieldnames (e_attrs);
            for ii = 1:numel (ef2)
              fn_i = ef2{ii};
              e_attrs.(fn_i) = e_attrs.(fn_i)(ord, :);
            endfor
          endif
          G.adj_ = sparse (N, N);
          G.is_multigraph_ = true;
        else
          ## Build a sparse index matrix that simultaneously:
          ##   * detects duplicate edges -- any (s, t) pair appearing
          ##     twice in the input will accumulate into a single cell,
          ##     so nnz(p) < m;
          ##   * encodes the input -> lex-order permutation in its values
          ##     (find (p.') returns them in lex order).
          ## Using the index sequence 1:m (not weights) avoids a false
          ## duplicate report when a user-supplied weight is zero.
          if (m > 0)
            p = sparse (s_idx, t_idx, 1:m, N, N);
            if (nnz (p) != m)
              error ("Octave:invalid-input-arg", ...
                     ["digraph: EdgeTable contains duplicate edges; ", ...
                      "parallel edges require the 'multigraph' flag"]);
            endif
            ef2 = fieldnames (e_attrs);
            if (! isempty (ef2))
              [~, ~, perm] = find (p.');
              for ii = 1:numel (ef2)
                fn_i = ef2{ii};
                e_attrs.(fn_i) = e_attrs.(fn_i)(perm, :);
              endfor
            endif
          endif

          ## Build adj_ and commit state.  Weight is NOT permuted: it
          ## will be stored via sparse (s, t, w), which places each
          ## weight at its (s(i), t(i)) cell; get.Edges then retrieves
          ## them in lex order automatically.
          if (m > 0)
            if (have_weights)
              G.adj_ = sparse (s_idx, t_idx, w_vec, N, N);
              G.has_weights_ = true;
            else
              G.adj_ = sparse (s_idx, t_idx, 1, N, N);
            endif
          else
            G.adj_ = sparse (N, N);
          endif
        endif
        G.nodenames_ = nodenames_out;
        G.edge_attrs_ = e_attrs;
        G.node_attrs_ = n_attrs;

      elseif (nargs == 1)
        arg1 = args{1};
        if (isnumeric (arg1) && isscalar (arg1))
          ## Scalar numeric input: node count N.
          if (! (isreal (arg1) && isfinite (arg1) && arg1 >= 0 ...
                 && arg1 == fix (arg1)))
            error ("Octave:invalid-input-arg", ...
                   "digraph: N must be a non-negative integer scalar");
          endif
          N = double (arg1);
          G.adj_ = sparse (N, N);
          G.is_multigraph_ = is_multigraph;
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
          else
            ## Dense path: sparsify.  double() handles int* / logical.
            A = sparse (double (A));
          endif
          N = size (A, 1);
          ## Non-empty adjacency always carries a Weight column (matrix
          ## form implies weighted, MATLAB parity).  0x0 stays empty and
          ## unweighted.
          if (N > 0)
            G.has_weights_ = true;
          endif
          if (is_multigraph)
            ## Multigraph mode: pull nonzeros out of A in lex order and
            ## store them in mg_endnodes_/mg_weights_.  An adjacency
            ## matrix cannot express parallel edges so ismultigraph
            ## still returns false; the flag only affects storage mode.
            if (nnz (A) > 0)
              [dst, src, w] = find (A.');
              G.mg_endnodes_ = [src, dst];
              G.mg_weights_ = w;
            endif
            G.adj_ = sparse (N, N);
            G.is_multigraph_ = true;
          else
            G.adj_ = A;
          endif
        else
          error ("Octave:invalid-input-arg", ...
                 ["digraph: single-argument input must be a ", ...
                  "non-negative integer scalar or a real square ", ...
                  "adjacency matrix"]);
        endif
      elseif (nargs == 2 && iscellstr (args{2}))
        ## Adjacency-matrix + nodenames constructor:
        ## digraph (A, NODENAMES).  A must be a real square numeric or
        ## logical 2-D matrix; NODENAMES must be a cellstr of unique
        ## strings whose length equals size (A, 1).  Semantics otherwise
        ## mirror the US-C06 adjacency path (sparse preserved, weights
        ## drawn from A(i,j)).
        A = args{1};
        nn = args{2};
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
        else
          A = sparse (double (A));
        endif
        N_an = size (A, 1);
        if (N_an > 0)
          G.has_weights_ = true;
        endif
        if (is_multigraph)
          if (nnz (A) > 0)
            [dst, src, w] = find (A.');
            G.mg_endnodes_ = [src, dst];
            G.mg_weights_ = w;
          endif
          G.adj_ = sparse (N_an, N_an);
          G.is_multigraph_ = true;
        else
          G.adj_ = A;
        endif
        G.nodenames_ = nn;
      elseif (nargs == 2 || nargs == 3)
        ## Edge-list constructor: digraph (s, t) or digraph (s, t, w).
        s = args{1};
        t = args{2};
        have_weights = (nargs == 3);
        if (have_weights)
          w = args{3};
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
          if (is_multigraph)
            ## Multigraph storage: stable lex sort so duplicates stay
            ## adjacent; adj_ becomes an N-by-N size-only placeholder.
            [EN, ord] = sortrows ([s, t]);
            G.mg_endnodes_ = EN;
            if (have_weights)
              G.mg_weights_ = w(ord);
              G.has_weights_ = true;
            endif
            G.adj_ = sparse (N, N);
            G.is_multigraph_ = true;
          else
            ## Simple-graph storage: reject duplicate (s, t) pairs up
            ## front so sparse accumulation cannot silently merge them.
            m = numel (s);
            p = sparse (s, t, 1:m, N, N);
            if (nnz (p) != m)
              error ("Octave:invalid-input-arg", ...
                     ["digraph: duplicate edges in S and T; ", ...
                      "parallel edges require the 'multigraph' flag"]);
            endif
            if (have_weights)
              G.adj_ = sparse (s, t, w, N, N);
              G.has_weights_ = true;
            else
              G.adj_ = sparse (s, t, 1, N, N);
            endif
          endif
        elseif (is_multigraph)
          ## Empty edges with 'multigraph' flag: set storage mode and
          ## empty arrays.
          G.is_multigraph_ = true;
          if (have_weights)
            G.has_weights_ = true;
          endif
        endif
      elseif (nargs == 4)
        ## Four-argument constructor.  Dispatch on the type of the
        ## fourth argument:
        ##   cellstr           -> digraph (s, t, w, nodenames)
        ##   numeric scalar    -> digraph (s, t, w, N)
        ## Any other shape is rejected.
        s = args{1};
        t = args{2};
        w = args{3};
        arg4 = args{4};

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
            if (is_multigraph)
              G.is_multigraph_ = true;
              if (have_weights)
                G.has_weights_ = true;
              endif
            endif
          elseif (is_multigraph)
            ## Stable lex sort for multigraph storage.
            [EN, ord] = sortrows ([s_idx, t_idx]);
            G.mg_endnodes_ = EN;
            if (have_weights)
              G.mg_weights_ = w(ord);
              G.has_weights_ = true;
            endif
            G.adj_ = sparse (N, N);
            G.is_multigraph_ = true;
          else
            ## Reject duplicate (s, t) pairs.
            m = numel (s_idx);
            p = sparse (s_idx, t_idx, 1:m, N, N);
            if (nnz (p) != m)
              error ("Octave:invalid-input-arg", ...
                     ["digraph: duplicate edges in S and T; ", ...
                      "parallel edges require the 'multigraph' flag"]);
            endif
            if (have_weights)
              G.adj_ = sparse (s_idx, t_idx, w, N, N);
              G.has_weights_ = true;
            else
              G.adj_ = sparse (s_idx, t_idx, 1, N, N);
            endif
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
            if (is_multigraph)
              G.is_multigraph_ = true;
              if (have_weights)
                G.has_weights_ = true;
              endif
            endif
          elseif (is_multigraph)
            [EN, ord] = sortrows ([s, t]);
            G.mg_endnodes_ = EN;
            if (have_weights)
              G.mg_weights_ = w(ord);
              G.has_weights_ = true;
            endif
            G.adj_ = sparse (N, N);
            G.is_multigraph_ = true;
          else
            m = numel (s);
            p = sparse (s, t, 1:m, N, N);
            if (nnz (p) != m)
              error ("Octave:invalid-input-arg", ...
                     ["digraph: duplicate edges in S and T; ", ...
                      "parallel edges require the 'multigraph' flag"]);
            endif
            if (have_weights)
              G.adj_ = sparse (s, t, w, N, N);
              G.has_weights_ = true;
            else
              G.adj_ = sparse (s, t, 1, N, N);
            endif
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

      ## US-C09 post-processing: drop every self-loop edge (i, i) when
      ## the caller passed the trailing @qcode{'omitselfloops'} flag.
      ## Extra edge-attribute columns are filtered by the same mask so
      ## their row count stays in sync with the remaining edges.
      if (omit_loops)
        if (G.is_multigraph_)
          ## Multigraph path: mg_endnodes_ is already lex-sorted, so
          ## the self-loop mask can be applied directly to it, to
          ## mg_weights_, and to every extra edge column without any
          ## reordering.
          if (! isempty (G.mg_endnodes_))
            keep = (G.mg_endnodes_(:, 1) != G.mg_endnodes_(:, 2));
            if (any (! keep))
              G.mg_endnodes_ = G.mg_endnodes_(keep, :);
              if (G.has_weights_)
                G.mg_weights_ = G.mg_weights_(keep);
              endif
              efn = fieldnames (G.edge_attrs_);
              for ii = 1:numel (efn)
                fn_i = efn{ii};
                G.edge_attrs_.(fn_i) = G.edge_attrs_.(fn_i)(keep, :);
              endfor
            endif
          endif
        else
          N = size (G.adj_, 1);
          if (N > 0 && nnz (G.adj_) > 0)
            [r, c, v] = find (G.adj_);
            keep = (r != c);
            if (any (! keep))
              ## find(adj_) walks column-major, so (r, c) arrives in
              ## (dst, src) order.  edge_attrs_ are stored in lex
              ## (src, dst) order -- match them by sorting (r, c) as
              ## rows.  One sort on an nnz-by-2 integer matrix beats a
              ## second find on the transpose.
              efn = fieldnames (G.edge_attrs_);
              if (! isempty (efn))
                [~, lex_perm] = sortrows ([r, c]);
                keep_lex = keep(lex_perm);
                for ii = 1:numel (efn)
                  fn_i = efn{ii};
                  G.edge_attrs_.(fn_i) = G.edge_attrs_.(fn_i)(keep_lex, :);
                endfor
              endif
              G.adj_ = sparse (r(keep), c(keep), v(keep), N, N);
            endif
          endif
        endif
      endif

    endfunction

    function e = get.Edges (G)

      if (G.is_multigraph_)
        ## Multigraph storage: edges already lex-sorted, with duplicates
        ## adjacent.  The property defaults (zeros(0,2) / zeros(0,1))
        ## give the correct edgeless shape, so no empty guard needed.
        e.EndNodes = G.mg_endnodes_;
        if (G.has_weights_)
          e.Weight = G.mg_weights_;
        endif
      else
        ## Simple-graph storage: extract edges in lexicographic
        ## (source, destination) order.  find(A.') iterates A
        ## column-by-column of the transpose, which corresponds to
        ## iterating rows of A (i.e. sources) in outer order and
        ## within-row columns (destinations) in inner order.
        [dst, src, w] = find (G.adj_.');
        ## The (:) coercion normalises the shape to m-by-1 even when
        ## @code{adj_} is 0-by-0 (where find returns 0-by-0 arrays),
        ## so @code{EndNodes} is reliably m-by-2 and @code{Weight} is
        ## m-by-1 across every constructor form -- MATLAB parity.
        e.EndNodes = [src(:), dst(:)];
        if (G.has_weights_)
          e.Weight = w(:);
        endif
      endif
      ## Merge any extra edge-attribute columns supplied via the
      ## EdgeTable constructor.  Stored in lex-order already.
      efn = fieldnames (G.edge_attrs_);
      for ii = 1:numel (efn)
        e.(efn{ii}) = G.edge_attrs_.(efn{ii});
      endfor

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
      ## Return the number of nodes in the digraph @var{G}.
      ## @seealso{digraph, numedges}
      ## @end deftypefn

      n = size (G.adj_, 1);

    endfunction

    function m = numedges (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{m} =} numedges (@var{G})
      ## Return the number of edges in the digraph @var{G}.  For a
      ## multigraph, parallel edges count individually.
      ## @seealso{digraph, numnodes, ismultigraph}
      ## @end deftypefn

      if (G.is_multigraph_)
        m = size (G.mg_endnodes_, 1);
      else
        m = nnz (G.adj_);
      endif

    endfunction

    function tf = ismultigraph (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{tf} =} ismultigraph (@var{G})
      ## Return true if the digraph @var{G} contains parallel edges
      ## between the same ordered pair of nodes; return false
      ## otherwise.  A digraph built without the @qcode{'multigraph'}
      ## flag (or built with the flag but without any duplicate
      ## @code{(source, destination)} pairs) yields false.
      ## @seealso{digraph, numedges}
      ## @end deftypefn

      if (! G.is_multigraph_ || isempty (G.mg_endnodes_))
        tf = false;
      else
        m = size (G.mg_endnodes_, 1);
        u = unique (G.mg_endnodes_, "rows");
        tf = (size (u, 1) != m);
      endif

    endfunction

    function H = addnode (G, newnodes)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} addnode (@var{G}, @var{N})
      ## @deftypefnx {} {@var{H} =} addnode (@var{G}, @var{NodeNames})
      ## @deftypefnx {} {@var{H} =} addnode (@var{G}, @var{NodeTable})
      ## Append nodes to the digraph @var{G} and return the new digraph
      ## @var{H}.  See @code{help addnode} for the full description of
      ## the three call forms.  New nodes have no incident edges, so the
      ## multigraph storage (when present) is preserved as-is.
      ## @seealso{digraph, addedge, rmnode, rmedge, numnodes, findnode}
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
      ## Append edges to the digraph @var{G} and return the new digraph
      ## @var{H}.  See @code{help addedge} for the full description of
      ## the three call forms.  Endpoints that refer to node names not
      ## already present in @var{G} cause new nodes to be appended.
      ## Parallel edges require the @qcode{'multigraph'} flag on the
      ## original constructor.
      ## @seealso{digraph, addnode, rmnode, rmedge, numedges, findedge}
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
      ## edges.  The helper does not track edge attributes (only node
      ## attributes), so we handle that here.
      eattrs_out = G.edge_attrs_;
      efn = fieldnames (eattrs_out);
      for ii = 1:numel (efn)
        fn_i = efn{ii};
        col = eattrs_out.(fn_i);
        eattrs_out.(fn_i) = [col; digraph_default_edge_rows(col, m_new)];
      endfor

      H = G;
      H.nodenames_ = names_out;
      H.node_attrs_ = nattrs_out;
      H.has_weights_ = hw_out;
      H.edge_attrs_ = eattrs_out;

      if (G.is_multigraph_)
        ## Multigraph storage: append new (s, t) pairs to mg_endnodes_
        ## and new weights (if any) to mg_weights_.  Re-sort lex-stably
        ## so duplicate (s, t) pairs stay adjacent in input order.
        ## Extend the adj_ placeholder to the new node count.
        new_en = [s_idx(:), t_idx(:)];
        all_en = [G.mg_endnodes_; new_en];
        if (hw_out)
          if (G.has_weights_)
            old_w = G.mg_weights_;
          else
            ## Old unweighted multigraph being promoted: backfill ones.
            old_w = ones (size (G.mg_endnodes_, 1), 1);
          endif
          all_w = [old_w; w_vec];
          [srt_en, ord] = sortrows (all_en);
          H.mg_endnodes_ = srt_en;
          H.mg_weights_ = all_w(ord);
        else
          [srt_en, ~] = sortrows (all_en);
          H.mg_endnodes_ = srt_en;
          H.mg_weights_ = zeros (0, 1);
        endif
        H.adj_ = sparse (N_new, N_new);

      else
        ## Simple-graph storage.  Start from the existing adj_ and
        ## resize to the new N if needed; then scatter the new edges.
        A = G.adj_;
        if (N_new > Nold)
          A(N_new, N_new) = 0;
        endif

        if (m_new > 0)
          ## Duplicate detection: no two new edges at the same (s, t),
          ## and no new edge collides with an existing entry.  Use an
          ## m_new-vs-(new) sparse build (1:m values so zero weight does
          ## not trip the check).
          p_new = sparse (s_idx, t_idx, 1:m_new, N_new, N_new);
          if (nnz (p_new) != m_new)
            error ("Octave:invalid-input-arg", ...
                   ["addedge: duplicate edges in the input to ", ...
                    "addedge; parallel edges require the ", ...
                    "'multigraph' flag"]);
          endif
          ## Check against existing entries: intersection nonzero -> conflict.
          if (Nold > 0)
            Aprev = A;
            if (N_new > Nold)
              Aprev(N_new, N_new) = 0;   # pad to same size for mask op
            endif
            conflict = p_new & (Aprev != 0);
            if (nnz (conflict) > 0)
              error ("Octave:invalid-input-arg", ...
                     ["addedge: edge already exists in G; parallel ", ...
                      "edges require the 'multigraph' flag"]);
            endif
          endif

          ## Scatter new edges.
          if (hw_out)
            vals_new = w_vec;
          else
            vals_new = ones (m_new, 1);
          endif
          A = A + sparse (s_idx, t_idx, vals_new, N_new, N_new);
        endif

        H.adj_ = A;
      endif

    endfunction

    function H = rmnode (G, nodes)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} rmnode (@var{G}, @var{nodes})
      ## Remove one or more nodes (and their incident edges) from the
      ## digraph @var{G} and return the resulting digraph @var{H}.  See
      ## @code{help rmnode} for the full description.  Surviving nodes
      ## are reindexed compactly into @code{1:(numnodes (G) - k)}; node
      ## names, node-attribute columns, and edge-attribute columns are
      ## filtered to match.  The @qcode{'multigraph'} flag is preserved.
      ## @seealso{digraph, addnode, rmedge, addedge, numnodes, findnode}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to rmnode: expected 2 arguments");
      endif

      ## Resolve NODES into a column of validated indices.  Accepts
      ## numeric, char row, cellstr, [] or {}.
      rm_idx = __resolve_node_list__ (G, nodes, "rmnode");

      Nold = size (G.adj_, 1);

      ## Compute survivor mask for existing edges (needed to filter
      ## edge_attrs_ and multigraph edge storage).
      keep_mask = true (Nold, 1);
      if (! isempty (rm_idx))
        keep_mask(rm_idx) = false;
      endif

      if (G.is_multigraph_)
        ## Multigraph: filter mg_endnodes_ rows where both endpoints
        ## survive; remap indices using cumulative keep position.
        if (isempty (G.mg_endnodes_))
          edge_survive = false (0, 1);
        else
          edge_survive = keep_mask(G.mg_endnodes_(:, 1)) ...
                       & keep_mask(G.mg_endnodes_(:, 2));
        endif
      else
        ## Simple-graph: iterate existing edges in lex (src, dst) order
        ## matching get.Edges.
        if (nnz (G.adj_) == 0)
          edge_survive = false (0, 1);
        else
          [dst, src] = find (G.adj_.');
          src = src(:); dst = dst(:);
          edge_survive = keep_mask(src) & keep_mask(dst);
        endif
      endif

      ## Filter the edge-attribute columns by edge_survive.
      eattrs_out = G.edge_attrs_;
      efn = fieldnames (eattrs_out);
      for ii = 1:numel (efn)
        col = eattrs_out.(efn{ii});
        eattrs_out.(efn{ii}) = col(edge_survive, :);
      endfor

      ## Filter node-level state (adjacency, names, node_attrs_).
      H = G;
      [H.adj_, H.nodenames_, H.node_attrs_] = ...
        __rmnode_impl__ (G.adj_, G.nodenames_, G.node_attrs_, rm_idx);
      H.edge_attrs_ = eattrs_out;

      if (G.is_multigraph_)
        ## Remap surviving mg_endnodes_ to the compacted index space
        ## and resize the adj_ placeholder to the new node count.
        Nnew = size (H.adj_, 1);
        idx_map = zeros (Nold, 1);
        idx_map(keep_mask) = 1:Nnew;
        if (any (edge_survive))
          new_en = [idx_map(G.mg_endnodes_(edge_survive, 1)), ...
                    idx_map(G.mg_endnodes_(edge_survive, 2))];
          H.mg_endnodes_ = new_en;
          if (G.has_weights_)
            H.mg_weights_ = G.mg_weights_(edge_survive);
          else
            H.mg_weights_ = zeros (0, 1);
          endif
        else
          H.mg_endnodes_ = zeros (0, 2);
          H.mg_weights_ = zeros (0, 1);
        endif
        H.adj_ = sparse (Nnew, Nnew);
      endif

    endfunction

    function H = rmedge (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} rmedge (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{H} =} rmedge (@var{G}, @var{edgeIdx})
      ## Remove edges from the digraph @var{G} and return the resulting
      ## digraph @var{H}.  See @code{help rmedge} for the full
      ## description of the two call forms.  For a multigraph digraph,
      ## @code{rmedge (@var{G}, @var{s}, @var{t})} removes every
      ## parallel edge from @var{s}(i) to @var{t}(i).  Node count,
      ## node names, and node-attribute columns are preserved;
      ## edge-attribute columns are filtered to match the surviving
      ## edges.
      ## @seealso{digraph, addedge, rmnode, addnode, numedges, findedge}
      ## @end deftypefn

      if (nargin < 2 || nargin > 3)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to rmedge: expected 2 or 3 arguments");
      endif

      edge_survive = __rmedge_impl__ (G, varargin{:});
      N = size (G.adj_, 1);

      ## Filter edge-attribute columns by edge_survive mask.
      eattrs_out = G.edge_attrs_;
      efn = fieldnames (eattrs_out);
      for ii = 1:numel (efn)
        col = eattrs_out.(efn{ii});
        eattrs_out.(efn{ii}) = col(edge_survive, :);
      endfor

      H = G;
      H.edge_attrs_ = eattrs_out;

      if (G.is_multigraph_)
        ## Filter the parallel-edge storage.
        if (isempty (G.mg_endnodes_))
          H.mg_endnodes_ = zeros (0, 2);
        else
          H.mg_endnodes_ = G.mg_endnodes_(edge_survive, :);
        endif
        if (G.has_weights_)
          if (isempty (G.mg_weights_))
            H.mg_weights_ = zeros (0, 1);
          else
            H.mg_weights_ = G.mg_weights_(edge_survive);
          endif
        else
          H.mg_weights_ = zeros (0, 1);
        endif
        ## Keep the adj_ placeholder sized to the node count.
        H.adj_ = sparse (N, N);
      else
        ## Simple-graph mode: rebuild adj_ from surviving edges.  The
        ## edge iteration order here must match get.Edges (which uses
        ## find (adj_.')), otherwise the survive mask would be applied
        ## to a different ordering than the helper used.
        if (nnz (G.adj_) == 0)
          H.adj_ = sparse (N, N);
        else
          [dst, src, w] = find (G.adj_.');
          src = src(:); dst = dst(:); w = w(:);
          if (any (edge_survive))
            H.adj_ = sparse (src(edge_survive), dst(edge_survive), ...
                             w(edge_survive), N, N);
          else
            H.adj_ = sparse (N, N);
          endif
        endif
      endif

    endfunction

    function H = reordernodes (G, order)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} reordernodes (@var{G}, @var{order})
      ## Permute the nodes of the digraph @var{G} according to
      ## @var{order} and return the reordered digraph @var{H}.  See
      ## @code{help reordernodes} for the full description.  Node
      ## @code{i} of @var{H} is node @code{@var{order}(i)} of @var{G};
      ## the adjacency matrix of @var{H} is
      ## @code{adjacency (@var{G})(@var{order}, @var{order})}.  Node
      ## names, node-attribute columns, and edge-attribute columns are
      ## renumbered to match; the @qcode{'multigraph'} flag is
      ## preserved.
      ## @seealso{digraph, graph, subgraph, rmnode, addnode}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to reordernodes: expected 2 arguments");
      endif

      ## Resolve ORDER into a column of validated indices.  Accepts
      ## numeric, char row, cellstr, [] or {}.  The helper produces
      ## errors with regexes "invalid node index", "not found",
      ## "no node names", or "numeric index array" depending on the
      ## class of the invalid input.
      perm = __resolve_node_list__ (G, order, "reordernodes");

      N = size (G.adj_, 1);

      ## Validate that PERM is a permutation of 1:N.  The helper
      ## already validated that each entry is in 1:N; the remaining
      ## checks are length and uniqueness.
      if (numel (perm) != N || numel (unique (perm)) != N)
        error ("Octave:invalid-input-arg", ...
               ["digraph: reordernodes: ORDER must be a permutation ", ...
                "of 1:numnodes (G)"]);
      endif

      ## Compute the inverse permutation: inv_perm(perm(i)) = i.
      inv_perm = zeros (N, 1);
      inv_perm(perm) = 1:N;

      H = G;
      [H.adj_, H.nodenames_, H.node_attrs_] = ...
        __reordernodes_impl__ (G.adj_, G.nodenames_, G.node_attrs_, perm);

      ## Reorder edge-level storage to match the new adjacency's
      ## iteration order (get.Edges).
      if (G.is_multigraph_)
        ## Apply the inverse permutation to the stored (src, dst)
        ## pairs, then re-sort lex-stably.  Duplicates stay adjacent
        ## in input order (stable sort).
        m = size (G.mg_endnodes_, 1);
        if (m == 0)
          H.mg_endnodes_ = zeros (0, 2);
          H.mg_weights_ = zeros (0, 1);
          ## Preserve the placeholder adj_ size.
          H.adj_ = sparse (N, N);
        else
          new_en = [inv_perm(G.mg_endnodes_(:, 1)), ...
                    inv_perm(G.mg_endnodes_(:, 2))];
          [srt_en, p_edge] = sortrows (new_en);
          H.mg_endnodes_ = srt_en;
          if (G.has_weights_)
            H.mg_weights_ = G.mg_weights_(p_edge);
          else
            H.mg_weights_ = zeros (0, 1);
          endif
          H.adj_ = sparse (N, N);
        endif

        ## Edge-attribute columns follow the same stable sort
        ## permutation as mg_endnodes_.
        if (m > 0)
          efn = fieldnames (G.edge_attrs_);
          for ii = 1:numel (efn)
            col = G.edge_attrs_.(efn{ii});
            H.edge_attrs_.(efn{ii}) = col(p_edge, :);
          endfor
        endif
      else
        ## Simple-graph mode: iterate existing edges in lex (src, dst)
        ## order matching get.Edges (find(adj_.')), apply inv_perm to
        ## both endpoints, and compute the stable sort permutation to
        ## reorder the edge-attribute columns.
        if (nnz (G.adj_) == 0)
          ## Nothing to do: H.adj_ already set by the helper;
          ## edge_attrs_ has zero rows and follows trivially.
        else
          [dst_old, src_old] = find (G.adj_.');
          src_old = src_old(:); dst_old = dst_old(:);
          new_src = inv_perm(src_old);
          new_dst = inv_perm(dst_old);
          [~, p_edge] = sortrows ([new_src, new_dst]);
          efn = fieldnames (G.edge_attrs_);
          for ii = 1:numel (efn)
            col = G.edge_attrs_.(efn{ii});
            H.edge_attrs_.(efn{ii}) = col(p_edge, :);
          endfor
        endif
      endif

    endfunction

    function H = subgraph (G, nodes)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} subgraph (@var{G}, @var{nodes})
      ## Return the subgraph of the digraph @var{G} induced by the node
      ## subset @var{nodes}.  See @code{help subgraph} for the full
      ## description.  Only edges with @emph{both} endpoints in
      ## @var{nodes} are retained.  Nodes appear in @var{H} in the order
      ## given by @var{nodes}; node names, node-attribute columns,
      ## and edge-attribute columns are carried over.  For a multigraph,
      ## parallel edges between two surviving endpoints are preserved.
      ## @seealso{digraph, rmnode, reordernodes, addnode, numnodes, findnode}
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
                 ["digraph: subgraph: logical mask NODES must have ", ...
                  "length numnodes (G)"]);
        endif
        keep_idx = find (nodes(:));
      else
        keep_idx = __resolve_node_list__ (G, nodes, "subgraph");
      endif

      if (numel (unique (keep_idx)) != numel (keep_idx))
        error ("Octave:invalid-input-arg", ...
               "digraph: subgraph: NODES must be unique");
      endif

      Nnew = numel (keep_idx);

      ## Build an N-by-1 map: original index -> new index (or 0 if the
      ## node was dropped).  Used to both test survival and remap
      ## endpoints for surviving edges.
      idx_map = zeros (N, 1);
      idx_map(keep_idx) = 1:Nnew;

      ## Compute per-edge survival and new-order permutation from the
      ## class's edge iteration order (matching get.Edges).
      if (G.is_multigraph_)
        if (isempty (G.mg_endnodes_))
          edge_survive = false (0, 1);
          p_edge = zeros (0, 1);
          new_en = zeros (0, 2);
        else
          s_old = G.mg_endnodes_(:, 1);
          t_old = G.mg_endnodes_(:, 2);
          edge_survive = (idx_map(s_old) > 0) & (idx_map(t_old) > 0);
          if (any (edge_survive))
            new_en_raw = [idx_map(s_old(edge_survive)), ...
                          idx_map(t_old(edge_survive))];
            [new_en, p_edge] = sortrows (new_en_raw);
          else
            new_en = zeros (0, 2);
            p_edge = zeros (0, 1);
          endif
        endif
      else
        ## Simple-graph mode: iterate original edges via find(adj_.') to
        ## get (src, dst) in original lex order -- same as get.Edges.
        if (nnz (G.adj_) == 0)
          edge_survive = false (0, 1);
          p_edge = zeros (0, 1);
        else
          [dst_old, src_old] = find (G.adj_.');
          src_old = src_old(:); dst_old = dst_old(:);
          edge_survive = (idx_map(src_old) > 0) & (idx_map(dst_old) > 0);
          if (any (edge_survive))
            new_src = idx_map(src_old(edge_survive));
            new_dst = idx_map(dst_old(edge_survive));
            [~, p_edge] = sortrows ([new_src, new_dst]);
          else
            p_edge = zeros (0, 1);
          endif
        endif
      endif

      ## Filter edge-attribute columns by survive mask + reorder by
      ## the permutation.  survived_rows holds the rows in old lex
      ## order; applying p_edge gives new lex order.
      eattrs_out = struct ();
      efn = fieldnames (G.edge_attrs_);
      for ii = 1:numel (efn)
        col = G.edge_attrs_.(efn{ii});
        survived = col(edge_survive, :);
        eattrs_out.(efn{ii}) = survived(p_edge, :);
      endfor

      ## Filter node-level state (adjacency, names, node_attrs_).
      H = G;
      [H.adj_, H.nodenames_, H.node_attrs_] = ...
        __subgraph_impl__ (G.adj_, G.nodenames_, G.node_attrs_, keep_idx);
      H.edge_attrs_ = eattrs_out;

      if (G.is_multigraph_)
        ## Replace the adj_ placeholder with the new Nnew-by-Nnew
        ## empty sparse (multigraph stores real edges in mg_*).
        H.adj_ = sparse (Nnew, Nnew);
        H.mg_endnodes_ = new_en;
        if (G.has_weights_)
          if (any (edge_survive))
            surv_weights = G.mg_weights_(edge_survive);
            H.mg_weights_ = surv_weights(p_edge);
          else
            H.mg_weights_ = zeros (0, 1);
          endif
        else
          H.mg_weights_ = zeros (0, 1);
        endif
      endif

    endfunction

    function H = flipedge (G, edgeIdx)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} flipedge (@var{G})
      ## @deftypefnx {} {@var{H} =} flipedge (@var{G}, @var{edgeIdx})
      ## Reverse the direction of edges in the digraph @var{G} and
      ## return the resulting digraph @var{H}.  See @code{help flipedge}
      ## for the full description.  With one argument, every edge is
      ## reversed (adjacency matrix is transposed).  With two arguments,
      ## only the edges at indices @var{edgeIdx} are reversed; duplicate
      ## indices are silently deduplicated.  Self-loops are unaffected.
      ## In simple-graph mode, it is an error for the flip to create a
      ## duplicate directed edge with an existing one; in multigraph
      ## mode parallel edges are allowed.  Edge weights and
      ## edge-attribute columns follow their edges in the reversed
      ## graph.  Node count, node names, and node-attribute columns are
      ## preserved.
      ## @seealso{digraph, rmedge, addedge, reordernodes, subgraph}
      ## @end deftypefn

      if (nargin < 1 || nargin > 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to flipedge: expected 1 or 2 arguments");
      endif

      N = size (G.adj_, 1);
      M = numedges (G);

      ## Determine the flip mask (length M, true => reverse that edge).
      if (nargin == 1)
        flip_mask = true (M, 1);
      else
        if (! isnumeric (edgeIdx) || ! isreal (edgeIdx))
          error ("Octave:invalid-input-arg", ...
                 "digraph: flipedge: edgeIdx must be a real numeric array");
        endif
        v = double (edgeIdx)(:);
        if (! isempty (v))
          if (any (! isfinite (v)) || any (v < 1) || any (v > M) ...
              || any (v != fix (v)))
            error ("Octave:invalid-input-arg", ...
                   ["digraph: flipedge: invalid edge index (must be a ", ...
                    "positive integer in the range 1:numedges (G), ", ...
                    "or out of range)"]);
          endif
        endif
        flip_mask = false (M, 1);
        if (! isempty (v))
          flip_mask(unique (v)) = true;
        endif
      endif

      H = G;

      if (G.is_multigraph_)
        ## Multigraph: flip rows of mg_endnodes_ where flip_mask is set,
        ## then re-sort stably.  edge_attrs_ and mg_weights_ follow the
        ## permutation.
        if (M == 0)
          ## Nothing to flip.
          return;
        endif

        new_en = G.mg_endnodes_;
        if (any (flip_mask))
          new_en(flip_mask, :) = new_en(flip_mask, [2, 1]);
        endif
        [srt_en, p_edge] = sortrows (new_en);
        H.mg_endnodes_ = srt_en;
        if (G.has_weights_)
          H.mg_weights_ = G.mg_weights_(p_edge);
        else
          H.mg_weights_ = zeros (0, 1);
        endif
        ## Permute edge-attribute rows to match.
        efn = fieldnames (G.edge_attrs_);
        for ii = 1:numel (efn)
          col = G.edge_attrs_.(efn{ii});
          H.edge_attrs_.(efn{ii}) = col(p_edge, :);
        endfor
        ## Keep the placeholder adj_ size.
        H.adj_ = sparse (N, N);

      else
        ## Simple-graph mode.
        if (M == 0)
          ## Edgeless: flipedge is a no-op.
          return;
        endif

        ## Iterate edges in the canonical order get.Edges uses:
        ## find(adj_.') yields (src, dst) sorted lex.
        [dst_old, src_old, w] = find (G.adj_.');
        src_old = src_old(:); dst_old = dst_old(:); w = w(:);

        new_src = src_old;
        new_dst = dst_old;
        ## Swap endpoints for flipped edges.  Self-loops are invariant.
        if (any (flip_mask))
          tmp = new_src(flip_mask);
          new_src(flip_mask) = new_dst(flip_mask);
          new_dst(flip_mask) = tmp;
        endif

        ## Detect duplicate (src, dst) pairs in the result -- sparse()
        ## would silently sum such entries, which is not what we want.
        pairs = [new_src, new_dst];
        if (rows (unique (pairs, "rows")) != rows (pairs))
          error ("Octave:invalid-input-arg", ...
                 ["digraph: flipedge: flipping the requested edges ", ...
                  "would create duplicate directed edges; use a ", ...
                  "multigraph to allow parallel edges"]);
        endif

        H.adj_ = sparse (new_src, new_dst, w, N, N);

        ## Permute edge-attribute rows to match new lex order.  The
        ## new lex order corresponds to the stable sort of
        ## (new_src, new_dst).
        efn = fieldnames (G.edge_attrs_);
        if (! isempty (efn))
          [~, p_edge] = sortrows (pairs);
          for ii = 1:numel (efn)
            col = G.edge_attrs_.(efn{ii});
            H.edge_attrs_.(efn{ii}) = col(p_edge, :);
          endfor
        endif
      endif

    endfunction

    function s = successors (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{s} =} successors (@var{G}, @var{nodeID})
      ## Return the destinations of out-edges from @var{nodeID} in the
      ## digraph @var{G}.  @var{nodeID} is a scalar node identifier --
      ## either a numeric index in @code{1:numnodes (@var{G})} or a
      ## node name (char row vector or 1-element cellstr).  The return
      ## type matches the input type (numeric in / numeric out, string
      ## in / cellstr out).  For a multigraph, each parallel edge
      ## contributes one entry, so duplicate destinations are possible.
      ## @seealso{digraph, predecessors, neighbors, outedges}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to successors: expected 2 arguments");
      endif

      [n, return_names] = __resolve_single_node__ (G, nodeID, "successors");

      if (G.is_multigraph_)
        mask = (G.mg_endnodes_(:, 1) == n);
        idx = G.mg_endnodes_(mask, 2);
      else
        idx = find (G.adj_(n, :));
        idx = idx(:);
      endif

      if (return_names)
        s = G.nodenames_(idx);
        s = s(:);
      else
        s = double (idx);
      endif

    endfunction

    function p = predecessors (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{p} =} predecessors (@var{G}, @var{nodeID})
      ## Return the sources of in-edges into @var{nodeID} in the
      ## digraph @var{G}.  @var{nodeID} is a scalar node identifier --
      ## either a numeric index in @code{1:numnodes (@var{G})} or a
      ## node name (char row vector or 1-element cellstr).  The return
      ## type matches the input type (numeric in / numeric out, string
      ## in / cellstr out).  For a multigraph, each parallel edge
      ## contributes one entry, so duplicate sources are possible.
      ## @seealso{digraph, successors, neighbors, inedges}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to predecessors: expected 2 arguments");
      endif

      [n, return_names] = __resolve_single_node__ (G, nodeID, "predecessors");

      if (G.is_multigraph_)
        mask = (G.mg_endnodes_(:, 2) == n);
        idx = G.mg_endnodes_(mask, 1);
      else
        idx = find (G.adj_(:, n));
        idx = idx(:);
      endif

      if (return_names)
        p = G.nodenames_(idx);
        p = p(:);
      else
        p = double (idx);
      endif

    endfunction

    function nb = neighbors (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{n} =} neighbors (@var{G}, @var{nodeID})
      ## Return the nodes adjacent to @var{nodeID} in the digraph
      ## @var{G}, ignoring edge direction.  This is the union of the
      ## successors and predecessors of @var{nodeID}.  @var{nodeID} is a
      ## scalar node identifier -- either a numeric index in
      ## @code{1:numnodes (@var{G})} or a node name (char row vector or
      ## 1-element cellstr).  The return type matches the input type
      ## (numeric in / numeric out, string in / cellstr out).  A
      ## self-loop at @var{nodeID} contributes @var{nodeID} to the
      ## result once.  For a multigraph, each parallel edge between
      ## @var{nodeID} and another node contributes one entry, so
      ## duplicate neighbours are possible.
      ## @seealso{digraph, successors, predecessors, indegree, outdegree}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to neighbors: expected 2 arguments");
      endif

      [n, return_names] = __resolve_single_node__ (G, nodeID, "neighbors");

      if (G.is_multigraph_)
        ## Edges are stored as an m-by-2 list in lex order.  Partition
        ## the edges incident to N into (i) self-loops, stored as a row
        ## [N, N]; (ii) out-only edges [N, x] with x != N; (iii) in-only
        ## edges [x, N] with x != N.  Each parallel edge contributes one
        ## neighbour; a self-loop contributes N exactly once.
        en = G.mg_endnodes_;
        m1 = (en(:, 1) == n);            # edges starting at N
        m2 = (en(:, 2) == n);            # edges ending at N
        self_mask = m1 & m2;
        out_only = m1 & ! m2;            # [N, x], x != N
        in_only  = m2 & ! m1;            # [x, N], x != N
        idx = [en(out_only, 2); en(in_only, 1); ...
               repmat(n, nnz (self_mask), 1)];
        idx = sort (idx);
      else
        ## Simple digraph: union of out-neighbours and in-neighbours,
        ## sorted ascending.  @code{unique} returns a sorted column
        ## vector; a self-loop at N appears in both finds and collapses
        ## to a single entry.
        out_idx = find (G.adj_(n, :));
        in_idx  = find (G.adj_(:, n));
        idx = unique ([out_idx(:); in_idx(:)]);
      endif

      idx = idx(:);

      if (return_names)
        nb = G.nodenames_(idx);
        nb = nb(:);
      else
        nb = double (idx);
      endif

    endfunction

    function d = indegree (G, nodeIDs)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{d} =} indegree (@var{G})
      ## @deftypefnx {} {@var{d} =} indegree (@var{G}, @var{nodeIDs})
      ## Return the in-degrees of nodes in the digraph @var{G}.
      ## With one argument, return a @code{numnodes (G)}-by-1 column
      ## vector of edge-end counts.  With two arguments, return the
      ## in-degrees of the specified nodes, preserving the shape of
      ## @var{nodeIDs}.  Self-loops contribute 1; for a multigraph,
      ## each parallel edge is counted individually.
      ## @seealso{digraph, outdegree, degree, predecessors}
      ## @end deftypefn

      if (nargin < 1 || nargin > 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to indegree: expected 1 or 2 arguments");
      endif

      N = numnodes (G);
      if (G.is_multigraph_)
        en = G.mg_endnodes_;
        if (isempty (en))
          all_d = zeros (N, 1);
        else
          all_d = accumarray (en(:, 2), 1, [N, 1]);
        endif
      else
        if (N == 0)
          all_d = zeros (0, 1);
        else
          ## spones coerces nonzero entries to 1 so weighted graphs
          ## report edge counts, not weight sums.  sum (..., 1) is a
          ## row vector; (:) forces a column.
          all_d = full (sum (spones (G.adj_), 1))(:);
        endif
      endif

      if (nargin == 1)
        d = all_d;
        return;
      endif

      [idx, out_shape] = __resolve_node_list__ (G, nodeIDs, "indegree");
      d = reshape (all_d(idx), out_shape);

    endfunction

    function d = outdegree (G, nodeIDs)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{d} =} outdegree (@var{G})
      ## @deftypefnx {} {@var{d} =} outdegree (@var{G}, @var{nodeIDs})
      ## Return the out-degrees of nodes in the digraph @var{G}.
      ## With one argument, return a @code{numnodes (G)}-by-1 column
      ## vector of edge-start counts.  With two arguments, return the
      ## out-degrees of the specified nodes, preserving the shape of
      ## @var{nodeIDs}.  Self-loops contribute 1; for a multigraph,
      ## each parallel edge is counted individually.
      ## @seealso{digraph, indegree, degree, successors}
      ## @end deftypefn

      if (nargin < 1 || nargin > 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to outdegree: expected 1 or 2 arguments");
      endif

      N = numnodes (G);
      if (G.is_multigraph_)
        en = G.mg_endnodes_;
        if (isempty (en))
          all_d = zeros (N, 1);
        else
          all_d = accumarray (en(:, 1), 1, [N, 1]);
        endif
      else
        if (N == 0)
          all_d = zeros (0, 1);
        else
          all_d = full (sum (spones (G.adj_), 2))(:);
        endif
      endif

      if (nargin == 1)
        d = all_d;
        return;
      endif

      [idx, out_shape] = __resolve_node_list__ (G, nodeIDs, "outdegree");
      d = reshape (all_d(idx), out_shape);

    endfunction

    function idx = findnode (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{idx} =} findnode (@var{G}, @var{nodeID})
      ## Return the numeric node indices corresponding to @var{nodeID}
      ## in the digraph @var{G}.  Numeric inputs are validated and
      ## returned with shape preserved.  A char row vector is looked up
      ## as a single node name and returns a scalar (0 if not found).
      ## A cell array of strings is looked up element-wise and returns
      ## a column vector of indices (0 for any missing name).  This
      ## method matches MATLAB's findnode semantics: missing names
      ## yield 0, not an error.
      ## @seealso{digraph, findedge, numnodes, neighbors}
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
      ## Look up edges of the digraph @var{G}.  See @code{help findedge}
      ## for the full description of the three supported call forms.
      ## @seealso{digraph, findnode, numedges}
      ## @end deftypefn

      if (nargin < 1 || nargin > 3)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to findedge: expected 1, 2, or 3 arguments");
      endif

      ## Delegate to the shared private helper.  Request the matching
      ## number of outputs so the helper can return single-output
      ## (m-by-2 matrix / column idx) or two-output (separate columns)
      ## forms consistently.
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
      ## Count edges between node pairs in the digraph @var{G}.  The
      ## pair @code{(@var{s}(i), @var{t}(i))} is treated as an ordered
      ## pair; the reverse orientation is not counted.  For a multigraph
      ## digraph (see @code{ismultigraph}) the result counts parallel
      ## edges.  Returns a scalar for scalar inputs and a column vector
      ## otherwise.  See @code{help edgecount} for the full description.
      ## @seealso{digraph, findedge, numedges, ismultigraph}
      ## @end deftypefn

      if (nargin != 3)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to edgecount: expected 3 arguments");
      endif

      n = __edgecount_impl__ (G, varargin{1}, varargin{2});

    endfunction

    function [eid, nid] = outedges (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{eid} =} outedges (@var{G}, @var{nodeID})
      ## @deftypefnx {} {[@var{eid}, @var{nid}] =} outedges (@var{G}, @var{nodeID})
      ## Return the indices of the out-edges of @var{nodeID} in the
      ## digraph @var{G}.  @var{eid} is a column vector of edge indices
      ## (1-based) into @code{@var{G}.Edges}; with two outputs, @var{nid}
      ## is the column of destination node identifiers (numeric when
      ## @var{nodeID} was numeric, cellstr when it was a name).  See
      ## @code{help outedges} for the full description.
      ## @seealso{digraph, inedges, successors, findedge, outdegree}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to outedges: expected 2 arguments");
      endif

      [n, return_names] = __resolve_single_node__ (G, nodeID, "outedges");

      ## Use the public Edges.EndNodes which is always lex-sorted
      ## (src, dst) for both simple and multigraph storage.  This keeps
      ## the edge-index convention consistent with findedge.
      E = G.Edges.EndNodes;
      if (isempty (E))
        eid = zeros (0, 1);
        nid_idx = zeros (0, 1);
      else
        mask = (E(:, 1) == n);
        eid = find (mask);
        eid = eid(:);
        nid_idx = E(mask, 2);
      endif

      if (return_names)
        nid = G.nodenames_(nid_idx);
        nid = nid(:);
        if (isempty (nid_idx))
          ## Force the empty-cellstr shape to [0 1] for parity with the
          ## numeric path (some Octave versions return cell(1,0) here).
          nid = cell (0, 1);
        endif
      else
        nid = double (nid_idx);
      endif

    endfunction

    function [eid, nid] = inedges (G, nodeID)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{eid} =} inedges (@var{G}, @var{nodeID})
      ## @deftypefnx {} {[@var{eid}, @var{nid}] =} inedges (@var{G}, @var{nodeID})
      ## Return the indices of the in-edges of @var{nodeID} in the
      ## digraph @var{G}.  @var{eid} is a column vector of edge indices
      ## (1-based) into @code{@var{G}.Edges}; with two outputs, @var{nid}
      ## is the column of source node identifiers (numeric when
      ## @var{nodeID} was numeric, cellstr when it was a name).  See
      ## @code{help inedges} for the full description.
      ## @seealso{digraph, outedges, predecessors, findedge, indegree}
      ## @end deftypefn

      if (nargin != 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to inedges: expected 2 arguments");
      endif

      [n, return_names] = __resolve_single_node__ (G, nodeID, "inedges");

      ## Use the public Edges.EndNodes which is always lex-sorted
      ## (src, dst) for both simple and multigraph storage.  This keeps
      ## the edge-index convention consistent with findedge.
      E = G.Edges.EndNodes;
      if (isempty (E))
        eid = zeros (0, 1);
        nid_idx = zeros (0, 1);
      else
        mask = (E(:, 2) == n);
        eid = find (mask);
        eid = eid(:);
        nid_idx = E(mask, 1);
      endif

      if (return_names)
        nid = G.nodenames_(nid_idx);
        nid = nid(:);
        if (isempty (nid_idx))
          ## Force the empty-cellstr shape to [0 1] for parity with the
          ## numeric path (some Octave versions return cell(1,0) here).
          nid = cell (0, 1);
        endif
      else
        nid = double (nid_idx);
      endif

    endfunction

    function A = adjacency (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{A} =} adjacency (@var{G})
      ## @deftypefnx {} {@var{A} =} adjacency (@var{G}, @qcode{"weighted"})
      ## @deftypefnx {} {@var{A} =} adjacency (@var{G}, @var{W})
      ## Return the sparse adjacency matrix of the digraph @var{G}.
      ## The one-input form returns a binary (0/1) matrix (or an edge-
      ## count matrix for a multigraph); @qcode{"weighted"} uses the
      ## stored edge weights; a numeric vector @var{W} of length
      ## @code{numedges (@var{G})} provides custom per-edge weights.
      ## See @code{help adjacency} for the full description.
      ## @seealso{digraph, incidence, numedges, ismultigraph}
      ## @end deftypefn

      if (nargin < 1 || nargin > 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to adjacency: expected 1 or 2 arguments");
      endif

      N = size (G.adj_, 1);
      if (G.is_multigraph_)
        M = size (G.mg_endnodes_, 1);
      else
        M = nnz (G.adj_);
      endif

      if (nargin == 1)
        ## Binary / count form: for multigraph, sparse(s, t, 1, N, N)
        ## accumulates parallel edges into counts; for simple storage,
        ## spones(adj_) collapses weights to 1/0.
        if (G.is_multigraph_)
          if (M == 0)
            A = sparse (N, N);
          else
            src = G.mg_endnodes_(:, 1);
            dst = G.mg_endnodes_(:, 2);
            A = sparse (src, dst, 1, N, N);
          endif
        else
          if (N == 0)
            A = sparse (0, 0);
          else
            A = spones (G.adj_);
          endif
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
        if (G.is_multigraph_)
          if (M == 0)
            A = sparse (N, N);
          else
            src = G.mg_endnodes_(:, 1);
            dst = G.mg_endnodes_(:, 2);
            if (G.has_weights_)
              w = G.mg_weights_;
            else
              w = ones (M, 1);
            endif
            A = sparse (src, dst, w, N, N);
          endif
        else
          if (N == 0)
            A = sparse (0, 0);
          elseif (G.has_weights_)
            A = G.adj_;
          else
            A = spones (G.adj_);
          endif
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
      if (G.is_multigraph_)
        src = G.mg_endnodes_(:, 1);
        dst = G.mg_endnodes_(:, 2);
      else
        ## Lex-sorted (src, dst) pairs matching G.Edges order.
        E = G.Edges.EndNodes;
        src = E(:, 1);
        dst = E(:, 2);
      endif
      A = sparse (src, dst, w, N, N);

    endfunction

    function I = incidence (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{I} =} incidence (@var{G})
      ## Return the sparse incidence matrix of the digraph @var{G}.
      ## Column @math{k} of @var{I} has @code{-1} at the source-row and
      ## @code{+1} at the destination-row of edge @math{k}.  Self-loop
      ## edges produce an all-zero column.  See @code{help incidence}
      ## for the full description.
      ## @seealso{digraph, adjacency, numedges, numnodes}
      ## @end deftypefn

      if (nargin != 1)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to incidence: expected 1 argument");
      endif

      N = size (G.adj_, 1);
      if (G.is_multigraph_)
        src = G.mg_endnodes_(:, 1);
        dst = G.mg_endnodes_(:, 2);
        M = numel (src);
      else
        M = nnz (G.adj_);
        if (M == 0)
          I = sparse (N, 0);
          return;
        endif
        E = G.Edges.EndNodes;
        src = E(:, 1);
        dst = E(:, 2);
      endif

      if (M == 0)
        I = sparse (N, 0);
        return;
      endif

      ## Skip self-loop columns (must have exactly two entries per
      ## column, so self-loops contribute no sparse entries).
      k = (1:M)';
      keep = (src != dst);
      s_k = src(keep);
      d_k = dst(keep);
      c_k = k(keep);
      rows = [s_k; d_k];
      cols = [c_k; c_k];
      vals = [-ones(numel(c_k), 1); ones(numel(c_k), 1)];
      I = sparse (rows, cols, vals, N, M);

    endfunction

    function L = laplacian (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{L} =} laplacian (@var{G})
      ## The graph Laplacian is not defined on the directed
      ## @code{digraph} class; this method always raises an error.  Use
      ## @code{laplacian} on a @code{graph} object instead.  See
      ## @code{help laplacian} for the full description of the
      ## undirected case.
      ## @seealso{graph, digraph, adjacency, incidence}
      ## @end deftypefn

      error ("Octave:invalid-input-arg", ...
             "laplacian: not defined for a digraph; laplacian requires an undirected graph");

    endfunction

    function v = bfsearch (G, s, events, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{v} =} bfsearch (@var{G}, @var{s})
      ## @deftypefnx {} {@var{v} =} bfsearch (@var{G}, @var{s}, @var{event})
      ## @deftypefnx {} {@var{T} =} bfsearch (@var{G}, @var{s}, @var{events})
      ## @deftypefnx {} {@var{T} =} bfsearch (@dots{}, @qcode{"Restart"}, @var{tf})
      ## @deftypefnx {} {@var{T} =} bfsearch (@dots{}, @qcode{"EdgeColors"}, @var{tf})
      ## Perform a breadth-first search of the digraph @var{G} starting
      ## at node @var{s} and return nodes (or edges, or a full event
      ## log) in BFS order.  Out-edges are followed (source ->
      ## destination).  When a node has multiple unvisited out-neighbours
      ## they are visited in ascending order of node index (MATLAB
      ## parity tie-break).  Nodes not reachable from @var{s} are
      ## omitted; parallel edges in a multigraph are collapsed (each
      ## neighbour is enqueued at most once).
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
      ## are tagged @qcode{"tree"} (for @code{edgetonew}) or
      ## @qcode{"cross"} (for @code{edgetodiscovered} and
      ## @code{edgetofinished}); node events get @qcode{""}.  Requires
      ## the @var{events} argument to be @qcode{"allevents"} or a cell
      ## array of event names.
      ## @end itemize
      ## @seealso{digraph, dfsearch, successors, predecessors}
      ## @end deftypefn

      if (nargin < 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to bfsearch: expected at least 2 arguments");
      endif

      [src, ~] = __resolve_single_node__ (G, s, "bfsearch");

      ## Build a binary / count adjacency.  adjacency(G) with no
      ## second argument returns spones(adj_) for simple storage and
      ## a count matrix for a multigraph -- both have nonzeros exactly
      ## where edges exist, which is all BFS needs.
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
      ## Perform a depth-first search of the digraph @var{G} starting
      ## at node @var{s} and return nodes (or edges, or a full event
      ## log) in DFS order.  Out-edges are followed (source ->
      ## destination).  When a node has multiple unvisited out-neighbours
      ## they are visited in ascending order of node index (MATLAB
      ## parity tie-break).  Nodes not reachable from @var{s} are
      ## omitted; parallel edges in a multigraph are collapsed (each
      ## neighbour is visited at most once).
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
      ## (target on the DFS stack), while @qcode{"edgetofinished"}
      ## marks a @emph{cross} or @emph{forward edge} (target already
      ## finished).
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
      ## are tagged @qcode{"tree"} (@code{edgetonew}), @qcode{"back"}
      ## (@code{edgetodiscovered}), @qcode{"forward"}
      ## (@code{edgetofinished} to a descendant), or @qcode{"cross"}
      ## (@code{edgetofinished} otherwise); node events get @qcode{""}.
      ## Requires the @var{events} argument to be @qcode{"allevents"} or
      ## a cell array of event names.
      ## @end itemize
      ## @seealso{digraph, bfsearch, successors, predecessors}
      ## @end deftypefn

      if (nargin < 2)
        error ("Octave:invalid-fun-call", ...
               "Invalid call to dfsearch: expected at least 2 arguments");
      endif

      [src, ~] = __resolve_single_node__ (G, s, "dfsearch");

      ## Build a binary / count adjacency.  adjacency(G) with no
      ## second argument returns spones(adj_) for simple storage and
      ## a count matrix for a multigraph -- both have nonzeros exactly
      ## where edges exist, which is all DFS needs.
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
      ## @deftypefnx {} {@var{bins} =} conncomp (@var{G}, @qcode{"Type"}, @var{type})
      ## @deftypefnx {} {@var{bins} =} conncomp (@dots{}, @qcode{"OutputForm"}, @var{form})
      ## Compute the connected components of the digraph @var{G}.
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
      ## @qcode{"Type"} is either @qcode{"weak"} (default) or
      ## @qcode{"strong"}.  @qcode{"weak"} treats the digraph as
      ## undirected for component discovery; @qcode{"strong"} returns
      ## the strongly connected components via Tarjan's algorithm, so
      ## two nodes share a label iff there is a directed path from each
      ## to the other.
      ## @item
      ## @qcode{"OutputForm"} is either @qcode{"vector"} (default) or
      ## @qcode{"cell"}.  @qcode{"vector"} returns the @var{bins} row
      ## vector described above; @qcode{"cell"} returns a cell array
      ## @var{C} of length equal to the number of components, where
      ## @code{@var{C}@{k@}} is a sorted column vector of the node
      ## indices belonging to the @math{k}-th component.
      ## @end itemize
      ## @seealso{digraph, bfsearch, dfsearch}
      ## @end deftypefn

      opts = __conncomp_parse_opts__ (true, varargin);

      A = adjacency (G);
      if (strcmp (opts.type, "weak"))
        bins = __conncomp_weak__ (A);
      else
        bins = __conncomp_strong__ (A);
      endif

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
      ## @deftypefn {} {@var{bins} =} biconncomp (@var{G})
      ## Biconnected components are not defined on the directed
      ## @code{digraph} class; this method always raises an error.  Use
      ## @code{biconncomp} on a @code{graph} object instead.  See
      ## @code{help biconncomp} for the full description of the
      ## undirected case.
      ## @seealso{graph, digraph, conncomp}
      ## @end deftypefn

      error ("Octave:invalid-input-arg", ...
             "biconncomp: not defined for a digraph; biconncomp requires an undirected graph");

    endfunction

    function C = condensation (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{C} =} condensation (@var{G})
      ## Compute the graph condensation of the digraph @var{G}.
      ##
      ## The condensation @var{C} is a @code{digraph} in which every
      ## node represents one strongly connected component (SCC) of
      ## @var{G} and there is an edge from node @math{i} to node
      ## @math{j} whenever at least one edge of @var{G} goes from a
      ## node in SCC @math{i} to a node in SCC @math{j} (with
      ## @math{i \neq j}).  Parallel cross-SCC edges are merged; if
      ## @var{G} has edge weights, the result's edge weights are the
      ## sums of the original weights across the merged edges.
      ##
      ## @var{C} always represents a DAG: self-loops and within-SCC
      ## edges are dropped, and SCC numbering follows
      ## @code{conncomp (@var{G}, @qcode{"Type"}, @qcode{"strong"})},
      ## so the SCC containing the smallest unlabelled index always
      ## receives the next unused label.
      ##
      ## @var{C}'s @code{Nodes} struct carries a @code{Component}
      ## column that lists, for each new node, the original members of
      ## the corresponding SCC.  If @var{G} has named nodes each
      ## @code{Component@{k@}} is a column cellstr of name strings;
      ## otherwise each is a column vector of original node indices.
      ## @seealso{digraph, conncomp}
      ## @end deftypefn

      N = numnodes (G);

      ## Build Component column.  Even for N == 0 we set an empty
      ## 0x1 cell so C.Nodes.Component is always present.
      has_names = ! isempty (G.nodenames_);

      if (N == 0)
        comp = cell (0, 1);
        K = 0;
        bins = zeros (1, 0);
      else
        A = adjacency (G);
        bins = __conncomp_strong__ (A);
        K = max (bins);
        comp = cell (K, 1);
        for k = 1:K
          members = find (bins == k);
          if (has_names)
            comp{k} = G.nodenames_(members)(:);
          else
            comp{k} = double (members(:));
          endif
        endfor
      endif

      ## Build edge list of the new digraph.  Map each edge (s, t) of
      ## G to (bins(s), bins(t)), drop within-SCC edges, and sum
      ## parallel weights via sparse() duplicate-accumulation.
      E = G.Edges.EndNodes;
      m = size (E, 1);
      have_w = G.has_weights_;

      new_s = zeros (0, 1);
      new_t = zeros (0, 1);
      new_w = zeros (0, 1);
      if (m > 0 && K > 0)
        bs = double (bins(E(:, 1)));
        bt = double (bins(E(:, 2)));
        bs = bs(:);
        bt = bt(:);
        keep = bs != bt;
        if (any (keep))
          new_s = bs(keep);
          new_s = new_s(:);
          new_t = bt(keep);
          new_t = new_t(:);
          if (have_w)
            w_all = G.Edges.Weight;
            wk = w_all(keep);
            new_w = wk(:);
            ## sparse() accumulates values for duplicate (row, col)
            ## pairs -- exactly the weight-sum behaviour we want.
            Asum = sparse (new_s, new_t, new_w, K, K);
            [r, c, v] = find (Asum);
            new_s = r(:);
            new_t = c(:);
            new_w = v(:);
          else
            ## Unweighted: dedupe (row, col) pairs.
            [EN_u, ~, ~] = unique ([new_s, new_t], "rows");
            new_s = EN_u(:, 1);
            new_t = EN_u(:, 2);
            new_w = zeros (size (new_s, 1), 1);
          endif
        endif
      endif

      ## Build the result via the EdgeTable + NodeTable constructor.
      if (have_w)
        ET = struct ("EndNodes", [new_s, new_t], "Weight", new_w);
      else
        ET = struct ("EndNodes", [new_s, new_t]);
      endif
      NT.Component = comp;

      C = digraph (ET, NT);

    endfunction

    function n = toposort (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{n} =} toposort (@var{G})
      ## @deftypefnx {} {@var{n} =} toposort (@var{G}, @qcode{"Order"}, @var{order})
      ## Return a topological ordering of the directed acyclic graph
      ## @var{G}.
      ##
      ## @var{G} must represent a DAG; if any cycle exists (including
      ## any self-loop), an error is raised.  The result @var{n} is a
      ## @code{1}-by-@code{numnodes (@var{G})} row vector of node
      ## indices such that, for every edge @var{s}->@var{t},
      ## @code{find (@var{n} == @var{s}) < find (@var{n} == @var{t})}.
      ##
      ## The optional Name-Value @qcode{"Order"} option selects the
      ## tie-break rule (case-insensitive name and value).  Both
      ## @qcode{"stable"} (the default) and @qcode{"lexicographic"}
      ## return the lexicographically smallest valid topological
      ## order, i.e.@: whenever multiple nodes have no remaining
      ## incoming edges, the one with the smallest index is emitted
      ## first.
      ## @seealso{digraph, isdag, conncomp, condensation}
      ## @end deftypefn

      order = __toposort_parse_opts__ (varargin);

      N = numnodes (G);
      if (N == 0)
        n = zeros (1, 0);
        return;
      endif

      ## Build an N-by-N edge-count matrix directly from Edges.EndNodes
      ## so that both simple digraphs and multigraphs are handled
      ## uniformly: parallel edges contribute their count to the
      ## destination's indegree and each must be "consumed" before the
      ## destination becomes available.  Self-loops count twice here
      ## (once as out-edge, once as in-edge at the same node), so any
      ## node with a self-loop has indeg >= 1 and can never reach 0
      ## through Kahn's algorithm -- exactly the cycle-detection signal
      ## we want.
      E = G.Edges.EndNodes;
      m = size (E, 1);
      if (m == 0)
        indeg = zeros (N, 1);
        A_cnt = sparse (N, N);
      else
        A_cnt = sparse (double (E(:, 1)), double (E(:, 2)), 1, N, N);
        indeg = full (sum (A_cnt, 1)).';
      endif

      ## Kahn's algorithm with a sorted "ready" list.  find() returns
      ## sorted ascending indices, so we keep zero_list sorted ascending
      ## and always pop its first element (smallest index) -- that is
      ## the lex-smallest topological order.
      zero_list = find (indeg == 0);
      zero_list = zero_list(:);        # column

      n = zeros (1, N);
      pos = 0;

      while (! isempty (zero_list))
        u = zero_list(1);
        zero_list(1) = [];
        pos = pos + 1;
        n(pos) = u;

        ## Decrement indegrees of u's successors.
        succ = find (A_cnt(u, :));
        for v = succ
          mult = full (A_cnt(u, v));
          indeg(v) = indeg(v) - mult;
          if (indeg(v) == 0)
            ## Insert v into zero_list keeping ascending order.
            if (isempty (zero_list) || v > zero_list(end))
              zero_list = [zero_list; v];
            elseif (v < zero_list(1))
              zero_list = [v; zero_list];
            else
              ip = find (zero_list > v, 1);
              zero_list = [zero_list(1:ip-1); v; zero_list(ip:end)];
            endif
          endif
        endfor
      endwhile

      if (pos < N)
        error ("Octave:invalid-input-arg", ...
               "toposort: G is not a DAG; topological sort requires an acyclic digraph");
      endif

      ## Silence the unused-variable lint for 'order' -- stable and
      ## lexicographic currently share the same behaviour, but parsing
      ## the option still fully validates it (rejects unknown names or
      ## values).
      assert (any (strcmp (order, {"stable", "lexicographic"})));

    endfunction

    function tf = isdag (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{tf} =} isdag (@var{G})
      ## Return @code{true} if the digraph @var{G} is a directed
      ## acyclic graph (DAG), and @code{false} otherwise.
      ##
      ## The result @var{tf} is a scalar logical.  @code{true} means
      ## @var{G} contains no directed cycle (a self-loop @math{n ->
      ## n} counts as a cycle).  The empty digraph and any edgeless
      ## digraph are DAGs.
      ## @seealso{digraph, toposort, conncomp, condensation}
      ## @end deftypefn

      N = numnodes (G);
      if (N == 0)
        tf = true;
        return;
      endif

      ## Go through adjacency(G) rather than G.adj_ directly: for
      ## multigraph digraphs the edges are stored in side arrays and
      ## @code{adj_} is empty.  The one-input form of @code{adjacency}
      ## produces a binary-ish sparse matrix (parallel edges accumulate
      ## as counts, but only zero-vs-nonzero matters for cycle
      ## detection).
      A = adjacency (G);

      ## A self-loop is a one-node cycle, so a nonzero diagonal
      ## immediately rules out DAG-ness.
      if (any (diag (A) != 0))
        tf = false;
        return;
      endif

      ## Any strongly connected component of size >= 2 contains a
      ## directed cycle; the converse also holds for simple
      ## digraphs on a single vertex only when there is a self-loop
      ## (already handled above).  So: DAG iff every SCC is a
      ## singleton and no self-loop exists.
      bins = __conncomp_strong__ (A);
      comp_sizes = accumarray (bins(:), 1);
      tf = logical (all (comp_sizes <= 1));

    endfunction

    function H = transclosure (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} transclosure (@var{G})
      ## Return the transitive closure of the digraph @var{G}.
      ##
      ## The result @var{H} is a @code{digraph} on the same node set
      ## as @var{G}: an edge @math{i \to j} is present in @var{H}
      ## whenever there is a directed path of length at least one from
      ## @math{i} to @math{j} in @var{G} with @math{i \ne j}.
      ## Self-loops and parallel edges are not present in @var{H}; the
      ## result is a simple digraph.  Node names (if any) are
      ## preserved.  Edge weights are not preserved.
      ## @seealso{digraph, transreduction, condensation, conncomp}
      ## @end deftypefn

      N = numnodes (G);
      has_names = ! isempty (G.nodenames_);

      if (N == 0)
        ## Preserve the empty-with-names case too (numnodes==0 but
        ## a possible 0x1 name cell).
        H = digraph ();
        return;
      endif

      ## Use adjacency(G) (binary form) so multigraph parallel edges
      ## and edge weights are collapsed to a boolean relation: the
      ## only thing that matters for reachability is whether any
      ## directed edge exists.
      A = adjacency (G);
      R = double (A != 0);

      ## Warshall's algorithm for transitive closure.  After iteration
      ## k, R(i, j) is nonzero iff there is a directed path from i to
      ## j using intermediate vertices only from @math{\{1, ..., k\}}.
      ## The outer product @code{col * row} adds the paths that go
      ## through vertex k; @code{spones} collapses the running
      ## accumulation back to a 0/1 pattern so the matrix stays a
      ## boolean relation.
      for k = 1:N
        col = R(:, k);
        row = R(k, :);
        if (nnz (col) > 0 && nnz (row) > 0)
          R = spones (R + col * row);
        endif
      endfor

      ## Extract (source, destination) edge list.  Drop self-loops
      ## unconditionally: MATLAB's transclosure never emits self-loops,
      ## even when @var{G} has self-loops or cycles that reach back to
      ## the origin.
      [s, t] = find (R);
      s = s(:);
      t = t(:);
      keep = (s != t);
      s = s(keep);
      t = t(keep);

      ## Rebuild the digraph preserving the original node count and
      ## names.  Pass @code{[]} for weights so the result is unweighted
      ## (MATLAB parity: transclosure does not preserve edge weights).
      if (has_names)
        H = digraph (s, t, [], G.nodenames_);
      else
        H = digraph (s, t, [], N);
      endif

    endfunction

    function H = transreduction (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {@var{H} =} transreduction (@var{G})
      ## Return the transitive reduction of the directed acyclic graph
      ## @var{G}.
      ##
      ## @var{G} must be a @code{digraph} object that is a DAG;
      ## calling @code{transreduction} on a cyclic digraph raises an
      ## error.  The result @var{H} is a @code{digraph} on the same
      ## node set as @var{G} with the fewest edges that preserve the
      ## reachability relation: an edge @math{i \to j} is present in
      ## @var{H} iff it is present in @var{G} and there is no other
      ## directed path from @math{i} to @math{j} in @var{G}.  For a
      ## DAG the transitive reduction is unique.  Parallel edges and
      ## self-loops collapse in the output so @var{H} is always a
      ## simple digraph.  Node names (if any) are preserved; edge
      ## weights are not.
      ## @seealso{digraph, transclosure, isdag, condensation, conncomp}
      ## @end deftypefn

      N = numnodes (G);
      has_names = ! isempty (G.nodenames_);

      if (N == 0)
        ## Preserve the empty digraph exactly: empty relation, no
        ## names, no edges.  Matches transclosure's empty handling.
        H = digraph ();
        return;
      endif

      ## Require DAG input.  Reuse the @code{isdag} method so we pick
      ## up its handling of self-loops and multigraph side arrays.
      if (! isdag (G))
        error ("Octave:invalid-input-arg", ...
               "transreduction: G must be a directed acyclic graph (DAG)");
      endif

      ## Use adjacency(G) (binary form) so multigraph parallel edges
      ## are collapsed to a boolean relation: the reduction is defined
      ## on reachability, which ignores multiplicity.
      A = adjacency (G);
      Abin = double (A != 0);

      ## Warshall's algorithm for transitive closure.  R[i, j] is
      ## nonzero iff there is a directed path of length >= 1 from i
      ## to j in G.  For a DAG, R has no nonzero diagonal entries.
      R = Abin;
      for k = 1:N
        col = R(:, k);
        row = R(k, :);
        if (nnz (col) > 0 && nnz (row) > 0)
          R = spones (R + col * row);
        endif
      endfor

      ## An edge (i, j) of G is redundant iff there is another path
      ## i -> k -> ... -> j of length >= 2.  The product Abin * R
      ## computes exactly those length-at-least-two paths: the (i, j)
      ## entry of Abin * R counts intermediate vertices k such that
      ## A[i, k] = 1 and R[k, j] = 1.  (For a DAG, k = j contributes
      ## nothing because R[j, j] = 0.)
      P = spones (Abin * R);

      ## Keep edge (i, j) iff Abin[i, j] = 1 and P[i, j] = 0.  Express
      ## this without densifying by subtracting the intersection:
      ## Tr = Abin - (Abin .* P) leaves 1 where Abin=1 and P=0.
      Tr = Abin - (Abin .* P);

      [s, t] = find (Tr);
      s = s(:);
      t = t(:);

      ## Rebuild the digraph preserving the original node count and
      ## names.  Pass @code{[]} for weights so the result is
      ## unweighted (MATLAB parity: transreduction does not preserve
      ## edge weights; reachability is a boolean relation).
      if (has_names)
        H = digraph (s, t, [], G.nodenames_);
      else
        H = digraph (s, t, [], N);
      endif

    endfunction

    function H = simplify (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{H} =} simplify (@var{G})
      ## @deftypefnx {} {@var{H} =} simplify (@var{G}, @var{method})
      ## @deftypefnx {} {@var{H} =} simplify (@var{G}, @dots{}, @qcode{"omitselfloops"})
      ## @deftypefnx {} {@var{H} =} simplify (@var{G}, @dots{}, @var{Name}, @var{Value})
      ## Return a simplified copy of the digraph @var{G}: parallel edges
      ## are collapsed into a single edge, and (optionally) self-loops
      ## are dropped.  Parallel-edge weights are aggregated with
      ## @var{method} (default @qcode{"sum"}; also accepts
      ## @qcode{"mean"}, @qcode{"min"}, @qcode{"max"}).  Unweighted input
      ## produces unweighted output.  The optional trailing
      ## @qcode{"omitselfloops"} flag drops self-loops; the Name-Value
      ## option @qcode{"SelfLoops"} accepts @qcode{"keep"} or
      ## @qcode{"discard"}.  The Name-Value option
      ## @qcode{"AggregationVariables"} is a synonym for @var{method}.
      ## Node names (when present) are preserved.
      ## @seealso{digraph, ismultigraph, numedges}
      ## @end deftypefn

      [method, omit_loops] = __simplify_parse_opts__ (varargin);

      N = numnodes (G);
      has_names = ! isempty (G.nodenames_);

      if (N == 0)
        ## Preserve the empty digraph exactly: no nodes, no edges.
        H = digraph ();
        return;
      endif

      ## Extract every edge as (src, dst, weight).  Multigraph side
      ## arrays hold duplicates as separate rows; simple storage has
      ## already deduplicated at construction time.  For unweighted
      ## digraphs we synthesise per-edge weights of 1 so the aggregation
      ## pipeline is uniform -- the result is still built with
      ## @code{[]} weights further down when the input was unweighted.
      if (G.is_multigraph_)
        src = G.mg_endnodes_(:, 1);
        dst = G.mg_endnodes_(:, 2);
        if (G.has_weights_)
          w = G.mg_weights_;
        else
          w = ones (numel (src), 1);
        endif
      else
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
      endif

      ## Drop self-loops before aggregating so the aggregation bins do
      ## not include them.  When @code{omit_loops} is false this branch
      ## is a no-op.
      if (omit_loops && ! isempty (src))
        mask = (src != dst);
        src = src(mask);
        dst = dst(mask);
        w = w(mask);
      endif

      if (isempty (src))
        src_new = zeros (0, 1);
        dst_new = zeros (0, 1);
        wnew = zeros (0, 1);
      else
        ## Encode each ordered pair as a single integer key so
        ## @code{unique} and @code{accumarray} can group parallel edges
        ## in a single pass.  The encoding is reversible because
        ## @code{N} bounds every destination index.  The pair_id values
        ## are themselves sorted lexicographically by (src, dst), so
        ## @code{unique} returns the canonical output order directly.
        src = double (src(:));
        dst = double (dst(:));
        w = double (w(:));
        pair_id = (src - 1) .* N + dst;
        [upair, ~, pair_idx] = unique (pair_id);
        upair = upair(:);
        pair_idx = pair_idx(:);
        switch (method)
          case "sum"
            wnew = accumarray (pair_idx, w);
          case "mean"
            wnew = accumarray (pair_idx, w, [], @mean);
          case "min"
            wnew = accumarray (pair_idx, w, [], @min);
          case "max"
            wnew = accumarray (pair_idx, w, [], @max);
          otherwise
            ## Parser guarantees one of the above; this is a safety net.
            error ("Octave:invalid-input-arg", ...
                   "simplify: internal error -- unknown method '%s'", method);
        endswitch
        wnew = wnew(:);
        dst_new = mod (upair - 1, N) + 1;
        src_new = (upair - dst_new) ./ N + 1;
      endif

      ## Rebuild the output digraph preserving the original node count
      ## and names.  Unweighted input stays unweighted (pass @code{[]}
      ## for weights); weighted input carries the aggregated weights.
      if (G.has_weights_)
        if (has_names)
          H = digraph (src_new, dst_new, wnew, G.nodenames_);
        else
          H = digraph (src_new, dst_new, wnew, N);
        endif
      else
        if (has_names)
          H = digraph (src_new, dst_new, [], G.nodenames_);
        else
          H = digraph (src_new, dst_new, [], N);
        endif
      endif

    endfunction

    function D = distances (G, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{D} =} distances (@var{G})
      ## @deftypefnx {} {@var{d} =} distances (@var{G}, @var{src})
      ## @deftypefnx {} {@var{d} =} distances (@var{G}, @var{src}, @var{tgt})
      ## @deftypefnx {} {@var{D} =} distances (@dots{}, @qcode{"Method"}, @var{method})
      ## Return shortest-path distances on the digraph @var{G}.
      ##
      ## With no extra arguments, return the all-pairs
      ## @code{numnodes (@var{G})}-by-@code{numnodes (@var{G})} distance
      ## matrix.  With @var{src}, return a
      ## @code{numel (@var{src})}-by-@code{numnodes (@var{G})} matrix
      ## whose @math{k}-th row is the shortest-path distance from
      ## @code{@var{src}(k)} to every node (so a scalar @var{src}
      ## produces a row vector of length @code{numnodes (@var{G})}).
      ## With both @var{src} and @var{tgt}, return a
      ## @code{numel (@var{src})}-by-@code{numel (@var{tgt})} submatrix
      ## (scalar when both arguments are scalar).
      ##
      ## The optional @qcode{"Method"} Name-Value pair chooses the
      ## algorithm: @qcode{"auto"} (default) uses BFS on unweighted
      ## digraphs, Dijkstra on digraphs with only non-negative weights,
      ## and Bellman-Ford when any weight is negative; @qcode{"unweighted"}
      ## ignores weights and uses BFS; @qcode{"positive"} uses Dijkstra
      ## (error on negative weight); @qcode{"mixed"} uses Bellman-Ford
      ## (handles negative weights; errors on a negative cycle); and
      ## @qcode{"acyclic"} uses a topological-order relaxation that
      ## requires @var{G} to be a DAG.
      ##
      ## @var{src} and @var{tgt} may be numeric node indices or node
      ## names (character row vector or cell array of strings) when
      ## @var{G} has node names; see @code{help distances} for details.
      ## @seealso{digraph, shortestpath, shortestpathtree, adjacency}
      ## @end deftypefn

      [positional, method] = __distances_parse_opts__ (varargin);

      have_src = (numel (positional) >= 1);
      have_tgt = (numel (positional) >= 2);

      N = numnodes (G);

      ## Build the weight matrix W(i, j) used by the shortest-path
      ## routines.  For a multigraph, parallel edges between the same
      ## (i, j) collapse to the minimum weight (MATLAB parity: the
      ## shortest path uses the cheapest parallel edge).  For simple
      ## storage, we carry either the weighted adj_ or its binary
      ## skeleton depending on has_weights_.
      if (G.is_multigraph_)
        src = G.mg_endnodes_(:, 1);
        dst = G.mg_endnodes_(:, 2);
        if (G.has_weights_)
          w = G.mg_weights_(:);
        else
          w = ones (numel (src), 1);
        endif
        if (isempty (src))
          W = sparse (N, N);
        else
          ## Collapse parallel edges to min weight per (src, dst).
          [pairs, ~, ic] = unique ([src(:), dst(:)], "rows");
          min_w = accumarray (ic, w, [], @min);
          W = sparse (pairs(:, 1), pairs(:, 2), min_w, N, N);
        endif
      else
        if (G.has_weights_)
          W = G.adj_;
        else
          W = spones (G.adj_);
        endif
      endif

      ## Resolve method = "auto" to a concrete choice based on the
      ## weighted-ness and sign of W's stored entries.
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

      ## Dispatch on method.  Each helper accepts an optional sources
      ## column vector; omitted/empty means all-pairs.
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
          ## For all-pairs, Johnson's algorithm is asymptotically
          ## faster than running Bellman-Ford from every source
          ## (@math{O (V^2 \log V + V E)} vs @math{O (V^2 E)}).
          ## When only a subset of sources is requested we keep the
          ## direct per-source Bellman-Ford path.
          if (have_src)
            D_src = __distances_bellman_ford__ (W, src_idx);
          else
            D_src = __distances_johnson__ (W);
          endif
        case "acyclic"
          if (! isdag (G))
            error ("Octave:invalid-input-arg", ...
                   "distances: 'acyclic' Method requires G to be a DAG");
          endif
          topo = toposort (G);
          if (have_src)
            D_src = __distances_dag__ (W, topo, src_idx);
          else
            D_src = __distances_dag__ (W, topo);
          endif
        otherwise
          ## Parser guarantees a valid name; this is a safety net.
          error ("Octave:invalid-input-arg", ...
                 "distances: internal error -- unknown method '%s'", method);
      endswitch

      if (! have_src)
        ## All-pairs form: D_src is N-by-N.  Preserve 0-by-0 on empty.
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
      ## @var{t} of the digraph @var{G}.
      ##
      ## With one output argument, return only the node path @var{P}
      ## (a row vector of node identifiers traversed from @var{s} to
      ## @var{t}).  With two outputs, also return the total distance
      ## @var{d} along @var{P}.  With three outputs, also return
      ## @var{edgepath}, a row vector of indices into
      ## @code{@var{G}.Edges} identifying the traversed edges.
      ##
      ## When @var{s} and @var{t} are both numeric @var{P} is a
      ## numeric row vector; when either is a name @var{P} is a
      ## @code{1}-by-@var{k} cell array of strings.  When
      ## @code{@var{s} == @var{t}}, @var{P} is the 1-element vector
      ## @code{[@var{s}]}, @var{d} is @code{0} and @var{edgepath} is
      ## @code{1}-by-@code{0}.
      ##
      ## When @var{t} is not reachable from @var{s}, the outputs are
      ## empty: @var{P} is a @code{1}-by-@code{0} empty vector
      ## (numeric or cellstr following the input type), @var{d} is
      ## @code{Inf}, and @var{edgepath} is a @code{1}-by-@code{0}
      ## empty vector.
      ##
      ## The optional @qcode{"Method"} Name-Value pair chooses the
      ## algorithm: @qcode{"auto"} (default) picks Dijkstra when all
      ## weights are non-negative and Bellman-Ford when any weight is
      ## negative; @qcode{"positive"} forces Dijkstra (error on
      ## negative weight); @qcode{"mixed"} forces Bellman-Ford
      ## (handles negative weights; errors on a negative cycle
      ## reachable from @var{s}).
      ##
      ## For a digraph with parallel edges, the cheapest of the
      ## parallel edges connecting each pair of endpoints is used.
      ## Self-loops do not influence the path.
      ## @seealso{digraph, distances, shortestpathtree, allpaths}
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif

      method = __shortestpath_parse_method__ ("shortestpath", varargin);

      ## Resolve source and target node identifiers.  __resolve_single_node__
      ## accepts numeric indices, char row names, or 1-element
      ## cellstrs, validates against numnodes(G), and reports whether
      ## the input was a name (so we can decide the output type).
      [s_idx, s_by_name] = __resolve_single_node__ (G, s, "shortestpath");
      [t_idx, t_by_name] = __resolve_single_node__ (G, t, "shortestpath");

      ## Return cellstr paths when either endpoint was given by name
      ## (MATLAB parity).  This only applies when the digraph has
      ## node names; __resolve_single_node__ already rejects names
      ## on unnamed digraphs.
      return_names = s_by_name || t_by_name;

      N = numnodes (G);

      ## Build the weight matrix W used by the shortest-path search.
      ## For a multigraph, parallel edges between the same (i, j)
      ## collapse to the minimum weight; for a simple graph we carry
      ## either the weighted adj_ or its binary skeleton depending on
      ## has_weights_.
      if (G.is_multigraph_)
        src_e = G.mg_endnodes_(:, 1);
        dst_e = G.mg_endnodes_(:, 2);
        if (G.has_weights_)
          w_e = G.mg_weights_(:);
        else
          w_e = ones (numel (src_e), 1);
        endif
        if (isempty (src_e))
          W = sparse (N, N);
        else
          [pairs, ~, ic] = unique ([src_e(:), dst_e(:)], "rows");
          min_w = accumarray (ic, w_e, [], @min);
          W = sparse (pairs(:, 1), pairs(:, 2), min_w, N, N);
        endif
      else
        if (G.has_weights_)
          W = G.adj_;
        else
          W = spones (G.adj_);
        endif
      endif

      ## Resolve 'auto' to a concrete method based on weight signs.
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
          ## Parser guarantees a valid name; safety net.
          error ("Octave:invalid-input-arg", ...
                 "shortestpath: internal error -- unknown method '%s'", ...
                 method);
      endswitch
      ## path_idx is a column vector; row form is the public shape.
      path_idx = path_idx(:).';

      ## Convert node indices to either numeric or cellstr form.
      if (return_names)
        if (isempty (path_idx))
          P = cell (1, 0);
        else
          P = G.nodenames_(path_idx);
          ## Ensure row shape regardless of the stored cellstr shape.
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

      ## Compute edge indices along the path.  numel(path_idx) <= 1
      ## yields an empty edgepath row.
      k = numel (path_idx);
      if (k <= 1)
        edgepath = zeros (1, 0);
        return;
      endif

      src_pairs = path_idx(1:k-1);
      dst_pairs = path_idx(2:k);
      if (G.is_multigraph_)
        ## For a multigraph, several parallel edges may connect the
        ## same (src, dst) pair; the path's distance was computed
        ## using the cheapest of them, so edgepath must return the
        ## index of that cheapest edge (so the standard identity
        ## @code{sum (G.Edges.Weight(ep)) == d} holds).
        edgepath = zeros (1, k - 1);
        E = G.mg_endnodes_;
        if (G.has_weights_)
          w_all = G.mg_weights_(:);
        else
          w_all = ones (size (E, 1), 1);
        endif
        for ii = 1:(k - 1)
          mask = (E(:, 1) == src_pairs(ii)) & (E(:, 2) == dst_pairs(ii));
          cand_idx = find (mask);
          if (isempty (cand_idx))
            error ("Octave:internal-error", ...
                   "shortestpath: internal error -- missing edge (%d, %d)", ...
                   src_pairs(ii), dst_pairs(ii));
          endif
          [~, j] = min (w_all(cand_idx));
          edgepath(ii) = cand_idx(j);
        endfor
      else
        ep = __findedge_impl__ (G, 1, src_pairs(:), dst_pairs(:));
        edgepath = ep(:).';
      endif

    endfunction

    function TR = shortestpathtree (G, s, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{TR} =} shortestpathtree (@var{G}, @var{s})
      ## @deftypefnx {} {@var{TR} =} shortestpathtree (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{TR} =} shortestpathtree (@dots{}, @qcode{"OutputForm"}, @var{form})
      ## @deftypefnx {} {@var{TR} =} shortestpathtree (@dots{}, @qcode{"Method"}, @var{method})
      ## Return a single-source shortest path tree rooted at node
      ## @var{s} of the digraph @var{G}.
      ##
      ## With the two-argument form, the tree covers every node
      ## reachable from @var{s}.  With the three-argument form,
      ## @var{t} is a list of target nodes and the tree is pruned to
      ## only edges on some shortest path from @var{s} to a target.
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
      ## @qcode{"positive"} forces Dijkstra (error on negative weight);
      ## @qcode{"mixed"} forces Bellman-Ford (handles negative weights;
      ## errors on a negative cycle).
      ## @seealso{digraph, shortestpath, distances, allpaths}
      ## @end deftypefn

      if (nargin < 2)
        print_usage ();
      endif

      [s_idx, s_by_name] = __resolve_single_node__ (G, s, ...
                                                   "shortestpathtree");

      N = numnodes (G);

      ## Build the weight matrix used by Dijkstra (same rules as
      ## shortestpath): multigraph -> collapse parallel edges to min
      ## weight; simple graph -> adj_ or its 0/1 skeleton.
      if (G.is_multigraph_)
        src_e = G.mg_endnodes_(:, 1);
        dst_e = G.mg_endnodes_(:, 2);
        if (G.has_weights_)
          w_e = G.mg_weights_(:);
        else
          w_e = ones (numel (src_e), 1);
        endif
        if (isempty (src_e))
          W = sparse (N, N);
        else
          [pairs, ~, ic] = unique ([src_e(:), dst_e(:)], "rows");
          min_w = accumarray (ic, w_e, [], @min);
          W = sparse (pairs(:, 1), pairs(:, 2), min_w, N, N);
        endif
      else
        if (G.has_weights_)
          W = G.adj_;
        else
          W = spones (G.adj_);
        endif
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
      ## the digraph @var{G}.
      ##
      ## A simple path is a path that does not visit any node more
      ## than once.  Self-loops are ignored.  The returns @var{P},
      ## @var{d}, and @var{edgepaths} are all column cell / column
      ## numeric arrays; each path is a row vector.
      ##
      ## Optional Name-Value pairs @qcode{"MaxPathLength"} and
      ## @qcode{"MinPathLength"} restrict the total weight of
      ## returned paths.  For unweighted digraphs each edge weight is
      ## @code{1}, so these bounds act on the number of edges on the
      ## path.
      ##
      ## For a multigraph, each parallel edge produces a distinct
      ## path in the output (identical node sequence, distinct entry
      ## in @var{edgepaths}).
      ## @seealso{digraph, shortestpath, shortestpathtree, allcycles}
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
      ## Return all elementary directed cycles of the digraph @var{G}.
      ##
      ## A cycle is a closed directed walk in which no node is repeated
      ## (except that the walk starts and ends at the same node).  A
      ## self-loop on a node is a 1-cycle; a pair of opposite directed
      ## edges between two nodes is a 2-cycle.  For a multigraph each
      ## parallel edge contributes a separate cycle.
      ##
      ## Each cycle is reported exactly once with its smallest node
      ## first.  See @code{allcycles} for the full description, the
      ## returned shapes, and the @qcode{"MaxNumCycles"},
      ## @qcode{"MinCycleLength"}, @qcode{"MaxCycleLength"} options.
      ## @seealso{digraph, allpaths, conncomp, isdag}
      ## @end deftypefn

      [cycles, edgecycles] = __allcycles_impl__ (G, varargin{:});

    endfunction

    function mf = maxflow (G, s, t, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{mf} =} maxflow (@var{G}, @var{s}, @var{t})
      ## @deftypefnx {} {@var{mf} =} maxflow (@var{G}, @var{s}, @var{t}, @var{algorithm})
      ## Return the maximum flow value @var{mf} from node @var{s} to
      ## node @var{t} in the digraph @var{G}.
      ##
      ## @var{s} and @var{t} are scalar node identifiers (a positive
      ## integer index, a char row vector naming a node, or a
      ## 1-element cell array of strings).  Edge weights are
      ## interpreted as capacities and must be non-negative; when
      ## @var{G} is unweighted every edge has capacity @code{1}.
      ## Parallel edges sum their capacities.  Self-loops do not
      ## contribute to any @math{s-t} flow.  When @code{@var{s} ==
      ## @var{t}} or when @var{t} is not reachable from @var{s} along
      ## edges with positive capacity, @var{mf} is @code{0}.
      ##
      ## The optional @var{algorithm} argument selects the solver
      ## (case-insensitive): @qcode{"augmentpath"} (default) uses the
      ## Edmonds-Karp implementation of Ford-Fulkerson;
      ## @qcode{"searchtrees"} uses a dual-search-tree augmenting-path
      ## method that grows one BFS tree from @var{s} and another
      ## backward from @var{t} and augments along the shortest joining
      ## path.  Both algorithms return the same flow value.
      ## @seealso{digraph, mincut, shortestpath, distances}
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif

      algorithm = __maxflow_parse_algorithm__ (varargin);

      [s_idx, ~] = __resolve_single_node__ (G, s, "maxflow");
      [t_idx, ~] = __resolve_single_node__ (G, t, "maxflow");

      N = numnodes (G);

      ## Build an edge list (uu, vv, caps) of directed arcs.  Parallel
      ## edges in a multigraph are kept as distinct arcs so the
      ## algorithm sums their capacities implicitly.  For a simple
      ## digraph the adjacency matrix gives one weight per (u,v)
      ## cell, so we extract the edge list directly from adj_.
      if (G.is_multigraph_)
        uu = G.mg_endnodes_(:, 1);
        vv = G.mg_endnodes_(:, 2);
        if (G.has_weights_)
          caps = G.mg_weights_(:);
        else
          caps = ones (numel (uu), 1);
        endif
      else
        if (G.has_weights_)
          [uu_v, vv_v, caps_v] = find (G.adj_);
        else
          [uu_v, vv_v] = find (G.adj_);
          caps_v = ones (numel (uu_v), 1);
        endif
        uu = uu_v(:);
        vv = vv_v(:);
        caps = caps_v(:);
      endif

      ## Validate capacities up front so the error messages are
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
      ## Return the minimum @math{s}-@math{t} cut in the digraph
      ## @var{G}.  By the max-flow min-cut theorem @var{mf} equals
      ## @code{maxflow (@var{G}, @var{s}, @var{t})}.  When additional
      ## outputs are requested, @var{GF} is a digraph of the
      ## flow-carrying arcs (weights = flow values), and @var{cs} /
      ## @var{ct} partition the nodes into the source and sink sides
      ## of the minimum cut.
      ## @seealso{digraph, maxflow, shortestpath, distances}
      ## @end deftypefn

      if (nargin < 3)
        print_usage ();
      endif

      [s_idx, s_by_name] = __resolve_single_node__ (G, s, "mincut");
      [t_idx, t_by_name] = __resolve_single_node__ (G, t, "mincut");
      return_names = s_by_name || t_by_name;

      N = numnodes (G);

      ## Build an edge list (uu, vv, caps) of directed arcs.  Parallel
      ## edges in a multigraph are kept distinct so the algorithm sums
      ## their capacities implicitly.  Matches maxflow's edge-list
      ## construction so the two methods agree on mf.
      if (G.is_multigraph_)
        uu = G.mg_endnodes_(:, 1);
        vv = G.mg_endnodes_(:, 2);
        if (G.has_weights_)
          caps = G.mg_weights_(:);
        else
          caps = ones (numel (uu), 1);
        endif
      else
        if (G.has_weights_)
          [uu_v, vv_v, caps_v] = find (G.adj_);
        else
          [uu_v, vv_v] = find (G.adj_);
          caps_v = ones (numel (uu_v), 1);
        endif
        uu = uu_v(:);
        vv = vv_v(:);
        caps = caps_v(:);
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

      ## Run Edmonds-Karp with the multi-output form to recover per-arc
      ## flows and the source side of the min cut.
      [mf, flow, reach_s] = ...
          __maxflow_edmonds_karp__ (uu, vv, caps, N, s_idx, t_idx);

      if (nargout <= 1)
        return;
      endif

      ## Build GF: digraph on the same node set containing only the
      ## flow-carrying arcs (flow > 0), with flow values as weights.
      ## For a multigraph, parallel arcs carrying flow are preserved
      ## (each as its own arc in GF); for a simple digraph the
      ## find(adj_) result never has parallel arcs to begin with.
      keep = flow > 0;
      if (any (keep))
        gf_src = uu(keep);
        gf_dst = vv(keep);
        gf_w   = flow(keep);
        mg_opt = {};
        if (G.is_multigraph_)
          mg_opt = {"multigraph"};
        endif
        if (! isempty (G.nodenames_))
          GF = digraph (gf_src, gf_dst, gf_w, G.nodenames_, mg_opt{:});
        else
          GF = digraph (gf_src, gf_dst, gf_w, N, mg_opt{:});
        endif
      else
        ## No flow-carrying arcs -- GF is an edgeless digraph with the
        ## same node structure as G.
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

      ## cs = nodes reachable from s in residual graph (source side).
      ## ct = all other nodes (sink side).
      cs_idx = find (reach_s);
      ct_idx = find (! reach_s);

      ## Match input type: if either endpoint was given by name,
      ## return names (MATLAB parity).
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
      ## Return the centrality of each node in the digraph @var{G}.
      ## @var{type} is a character row vector (case-insensitive)
      ## selecting the centrality measure.
      ##
      ## Supported types:
      ## @table @code
      ## @item "indegree"
      ## The number of incoming edges per node.
      ## @item "outdegree"
      ## The number of outgoing edges per node.
      ## @item "outcloseness"
      ## Closeness centrality using outgoing shortest-path distances,
      ## @math{(N-1) / sum_{j != i} d(i, j)} where @math{d(i, j)} is
      ## the shortest path from node @math{i} to node @math{j}.
      ## Unreachable pairs contribute @code{Inf}, so nodes that can
      ## not reach the rest of the digraph receive a centrality of
      ## zero.  Stored edge weights are used for the distance
      ## computation when @var{G} is weighted.
      ## @item "incloseness"
      ## Closeness centrality using incoming shortest-path
      ## distances, @math{(N-1) / sum_{j != i} d(j, i)}.
      ## @item "closeness"
      ## Alias for @code{"outcloseness"} on a digraph.
      ## @end table
      ##
      ## The undirected @code{"degree"} type is not defined for a
      ## digraph.  The result is a column vector of length
      ## @code{numnodes (@var{G})}.
      ## @seealso{digraph, indegree, outdegree, distances, centrality}
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

      if (! isempty (varargin))
        error ("Octave:invalid-input-arg", ...
               "centrality: no name-value options are supported for TYPE '%s'", ...
               type);
      endif

      ## Future stories (US-CT03 betweenness, US-CT04 pagerank,
      ## US-CT05 eigenvector, US-CT06 hits, US-CT07
      ## Cost/Importance weights) will extend this switch.
      switch (lower (type))
        case "indegree"
          c = G.indegree ();
        case "outdegree"
          c = G.outdegree ();
        case "degree"
          error ("Octave:invalid-input-arg", ...
                 ["centrality: TYPE 'degree' is only defined for an ", ...
                  "undirected graph; use 'indegree' or 'outdegree' ", ...
                  "for a digraph"]);
        case {"closeness", "outcloseness"}
          c = __centrality_closeness__ (G, "out");
        case "incloseness"
          c = __centrality_closeness__ (G, "in");
        case {"betweenness", "pagerank", "eigenvector", ...
              "hubs", "authorities"}
          error ("Octave:invalid-input-arg", ...
                 "centrality: TYPE '%s' is not yet implemented", type);
        otherwise
          error ("Octave:invalid-input-arg", ...
                 "centrality: unknown TYPE '%s'", type);
      endswitch

    endfunction

    function disp (G)

      ## -*- texinfo -*-
      ## @deftypefn {} {} disp (@var{G})
      ## Print a concise, human-readable summary of the digraph
      ## @var{G}: a header line reporting the node and edge counts,
      ## followed by the first few edges (at most 10) when any are
      ## present.  The header uses singular/plural forms for 1-node
      ## and 1-edge graphs to match MATLAB's conventions.  Edges are
      ## printed in lexicographic (source, destination) order; when
      ## @var{G} has node names the names are printed instead of
      ## numeric indices.  A trailing continuation line reports any
      ## edges that were elided past the 10-row limit.
      ## @seealso{digraph, display, numedges, numnodes}
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
        printf ("  digraph with %d %s and %d %s.\n", N, n_word, M, e_word);
        return;
      endif

      printf ("  digraph with %d %s and %d %s:\n\n", N, n_word, M, e_word);

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
      ## Ensure column orientation so cellfun below operates on a
      ## column vector regardless of whether nodenames_ was a row or
      ## column cellstr.
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
## the existing edge-attribute column @var{col}.  Mirrors the node-column
## default in __addedge_impl__; kept local here because the digraph
## class method extends edge-attribute columns before delegating edge
## storage to the helper.
function r = digraph_default_edge_rows (col, K)

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

## BIST — US-C08: digraph(ET) with numeric EndNodes only (unweighted).
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! G = digraph (ET);
%! assert (class (G), "digraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (! isfield (G.Edges, "Weight"));

## BIST — US-C08: digraph(ET) with Weight column round-trips.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! G = digraph (ET);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — US-C08: row-vector Weight normalized to column.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! ET.Weight = [5 10];
%! G = digraph (ET);
%! assert (G.Edges.Weight, [5; 10]);

## BIST — US-C08: edges in the EdgeTable are re-sorted into lex order;
## Weight follows its edge.
%!test
%! ET.EndNodes = [3 1; 1 2; 2 3];
%! ET.Weight = [30; 10; 20];
%! G = digraph (ET);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — US-C08: extra numeric edge column preserved and reordered.
%!test
%! ET.EndNodes = [3 1; 1 2; 2 3];
%! ET.Weight = [30; 10; 20];
%! ET.Capacity = [300; 100; 200];
%! G = digraph (ET);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight, [10; 20; 30]);
%! assert (G.Edges.Capacity, [100; 200; 300]);

## BIST — US-C08: extra cellstr edge column preserved and reordered.
%!test
%! ET.EndNodes = [3 1; 1 2; 2 3];
%! ET.Label = {"c"; "a"; "b"};
%! G = digraph (ET);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Label, {"a"; "b"; "c"});

## BIST — US-C08: multiple extra edge columns preserved simultaneously.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [1; 2; 3];
%! ET.Name = {"e1"; "e2"; "e3"};
%! ET.Cost = [5; 10; 15];
%! G = digraph (ET);
%! E = G.Edges;
%! assert (E.Weight, [1; 2; 3]);
%! assert (E.Name, {"e1"; "e2"; "e3"});
%! assert (E.Cost, [5; 10; 15]);

## BIST — US-C08: extra edge columns also work on unweighted tables.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! ET.Kind = {"in"; "out"};
%! G = digraph (ET);
%! assert (! isfield (G.Edges, "Weight"));
%! assert (G.Edges.Kind, {"in"; "out"});

## BIST — US-C08: digraph(ET, NT) — NT.Name sets the node names.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [1; 2; 3];
%! NT.Name = {"alpha"; "beta"; "gamma"};
%! G = digraph (ET, NT);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"alpha"; "beta"; "gamma"});
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight, [1; 2; 3]);

## BIST — US-C08: NT can provide isolated trailing nodes (N > max endpoint).
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Name = {"a"; "b"; "c"; "d"; "e"};
%! G = digraph (ET, NT);
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});

## BIST — US-C08: extra node columns preserved.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! G = digraph (ET, NT);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Nodes.Size, [10; 20; 30]);

## BIST — US-C08: multiple extra node columns preserved.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! NT.Kind = {"x"; "y"; "z"};
%! G = digraph (ET, NT);
%! assert (G.Nodes.Size, [10; 20; 30]);
%! assert (G.Nodes.Kind, {"x"; "y"; "z"});

## BIST — US-C08: NT without Name field — node count inferred from column
## length; Nodes.Name stays empty.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT.Size = [10; 20; 30];
%! G = digraph (ET, NT);
%! assert (numnodes (G), 3);
%! assert (G.Nodes.Name, cell (0, 1));
%! assert (G.Nodes.Size, [10; 20; 30]);

## BIST — US-C08: cellstr EndNodes without NT infers names from first
## appearance in EndNodes.
%!test
%! ET.EndNodes = {"a", "b"; "b", "c"; "c", "a"};
%! G = digraph (ET);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);

## BIST — US-C08: cellstr EndNodes with NT looks up in NT.Name.
%!test
%! ET.EndNodes = {"x", "y"; "y", "z"};
%! NT.Name = {"x"; "y"; "z"; "w"};
%! G = digraph (ET, NT);
%! assert (numnodes (G), 4);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"x"; "y"; "z"; "w"});
%! assert (G.Edges.EndNodes, [1 2; 2 3]);

## BIST — US-C08: cellstr EndNodes with weights and extra columns.
%!test
%! ET.EndNodes = {"a", "b"; "b", "c"};
%! ET.Weight = [1.5; 2.5];
%! ET.Note = {"hi"; "lo"};
%! G = digraph (ET);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.Weight, [1.5; 2.5]);
%! assert (G.Edges.Note, {"hi"; "lo"});

## BIST — US-C08: round-trip an existing digraph via its Edges+Nodes.
%!test
%! G1 = digraph ([1 2 3], [2 3 1], [10 20 30], {"a", "b", "c"});
%! G2 = digraph (G1.Edges, G1.Nodes);
%! assert (numnodes (G2), numnodes (G1));
%! assert (numedges (G2), numedges (G1));
%! assert (G2.Edges.EndNodes, G1.Edges.EndNodes);
%! assert (G2.Edges.Weight, G1.Edges.Weight);
%! assert (G2.Nodes.Name, G1.Nodes.Name);

## BIST — US-C08: round-trip with isolated named nodes.
%!test
%! G1 = digraph ([1 2], [2 3], [5 10], {"a", "b", "c", "d"});
%! G2 = digraph (G1.Edges, G1.Nodes);
%! assert (numnodes (G2), 4);
%! assert (numedges (G2), 2);
%! assert (G2.Nodes.Name, {"a"; "b"; "c"; "d"});
%! assert (G2.Edges.Weight, [5; 10]);

## BIST — US-C08: empty edge table yields empty digraph.
%!test
%! ET.EndNodes = zeros (0, 2);
%! G = digraph (ET);
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C08: empty edge table with NT yields N isolated nodes.
%!test
%! ET.EndNodes = zeros (0, 2);
%! NT.Name = {"p"; "q"; "r"};
%! G = digraph (ET, NT);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);
%! assert (G.Nodes.Name, {"p"; "q"; "r"});

## BIST — US-C08: single-edge table.
%!test
%! ET.EndNodes = [1 2];
%! ET.Weight = 7;
%! G = digraph (ET);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 1);
%! assert (G.Edges.Weight, 7);

## BIST — US-C08: self-loop in ET is preserved.
%!test
%! ET.EndNodes = [1 1; 2 2];
%! G = digraph (ET);
%! assert (numnodes (G), 2);
%! assert (numedges (G), 2);

## BIST — US-C08: ET must be a struct with EndNodes field.
%!error <EndNodes> digraph (struct ("Weight", [1; 2]))

## BIST — US-C08: EndNodes with wrong number of columns rejected.
%!error <two columns> digraph (struct ("EndNodes", [1 2 3; 4 5 6]))
%!error <two columns> digraph (struct ("EndNodes", [1; 2; 3]))

## BIST — US-C08: 3-D EndNodes rejected.
%!error <EndNodes> digraph (struct ("EndNodes", ones (2, 2, 2)))

## BIST — US-C08: EndNodes of disallowed type rejected.
%!error <EndNodes> digraph (struct ("EndNodes", true (2, 2)))

## BIST — US-C08: Weight row count must match EndNodes.
%!error <Weight> digraph (struct ("EndNodes", [1 2; 2 3], "Weight", [1; 2; 3]))

## BIST — US-C08: Non-EndNodes/Weight columns must have matching row count.
%!error <Capacity> digraph (struct ("EndNodes", [1 2; 2 3], "Capacity", [1; 2; 3]))

## BIST — US-C08: NT.Name must be cellstr.
%!error <Name> digraph (struct ("EndNodes", [1 2]), struct ("Name", [1 2]))

## BIST — US-C08: NT.Name with duplicates rejected.
%!error <unique> ...
%! digraph (struct ("EndNodes", [1 2]), struct ("Name", {{"a"; "a"}}))

## BIST — US-C08: Numeric EndNodes out of range (index > numnodes from NT).
%!error <exceed> ...
%! digraph (struct ("EndNodes", [1 3]), struct ("Name", {{"a"; "b"}}))

## BIST — US-C08: cellstr endpoint not found in NT.Name rejected.
%!error <not found> ...
%! digraph (struct ("EndNodes", {{"a", "c"}}), struct ("Name", {{"a"; "b"}}))

## BIST — US-C08: Inconsistent NT column lengths rejected.
%!error <length> ...
%! digraph (struct ("EndNodes", [1 2]), ...
%!          struct ("Name", {{"a"; "b"}}, "Size", 1))

## BIST — US-C08: Non-scalar struct ET rejected.
%!error <scalar struct> digraph (struct ("EndNodes", {[1 2], [2 3]}))

## BIST — US-C08: Duplicate edges in ET (without 'multigraph' flag) rejected.
%!error <duplicate> ...
%! digraph (struct ("EndNodes", [1 2; 1 2]))

## BIST — US-C08: NT provided without Name but mismatched column lengths
## rejected.
%!error <length> ...
%! digraph (struct ("EndNodes", [1 2]), ...
%!          struct ("Size", [1; 2], "Kind", {{"a"; "b"; "c"}}))

## BIST — US-C09: digraph(s, t, 'omitselfloops') drops self-loops.
%!test
%! s = [1 2 3 4];
%! t = [1 2 3 5];
%! G = digraph (s, t, "omitselfloops");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 1);
%! assert (G.Edges.EndNodes, [4 5]);

## BIST — US-C09: digraph(s, t, w, 'omitselfloops') drops self-loop weights.
%!test
%! s = [1 2 3 4];
%! t = [1 2 4 5];
%! w = [10 20 30 40];
%! G = digraph (s, t, w, "omitselfloops");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [3 4; 4 5]);
%! assert (G.Edges.Weight, [30; 40]);

## BIST — US-C09: digraph(s, t, w, nodenames, 'omitselfloops').
%!test
%! G = digraph ([1 2 3], [1 3 1], [10 20 30], ...
%!              {"a", "b", "c"}, "omitselfloops");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.EndNodes, [2 3; 3 1]);
%! assert (G.Edges.Weight, [20; 30]);

## BIST — US-C09: digraph(s, t, w, N, 'omitselfloops') with isolated nodes.
%!test
%! G = digraph ([1 2 3], [1 3 1], [10 20 30], 10, "omitselfloops");
%! assert (numnodes (G), 10);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [2 3; 3 1]);
%! assert (G.Edges.Weight, [20; 30]);

## BIST — US-C09: digraph(A, 'omitselfloops') drops diagonal of adjacency.
%!test
%! A = [1 2 0; 0 5 3; 4 0 7];
%! G = digraph (A, "omitselfloops");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight, [2; 3; 4]);

## BIST — US-C09: digraph(A, nodenames, 'omitselfloops').
%!test
%! A = [1 2 0; 0 5 3; 4 0 7];
%! G = digraph (A, {"a", "b", "c"}, "omitselfloops");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (G.Edges.Weight, [2; 3; 4]);

## BIST — US-C09: sparse adjacency + 'omitselfloops' stays sparse.
%!test
%! A = sparse ([1 1 2 2], [1 2 2 3], [10 20 30 40], 3, 3);
%! G = digraph (A, "omitselfloops");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [20; 40]);

## BIST — US-C09: digraph(N, 'omitselfloops') is a no-op (no edges).
%!test
%! G = digraph (5, "omitselfloops");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 0);

## BIST — US-C09: digraph('omitselfloops') alone yields empty digraph.
%!test
%! G = digraph ("omitselfloops");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C09: digraph(ET, 'omitselfloops') drops self-loop edges.
%!test
%! ET.EndNodes = [1 1; 1 2; 2 2; 2 3];
%! ET.Weight = [10; 20; 30; 40];
%! G = digraph (ET, "omitselfloops");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Edges.Weight, [20; 40]);

## BIST — US-C09: digraph(ET, 'omitselfloops') filters extra edge columns.
%!test
%! ET.EndNodes = [1 1; 1 2; 2 2; 2 3];
%! ET.Weight = [10; 20; 30; 40];
%! ET.Label = {"loop1"; "ab"; "loop2"; "bc"};
%! G = digraph (ET, "omitselfloops");
%! assert (numedges (G), 2);
%! assert (G.Edges.Label, {"ab"; "bc"});

## BIST — US-C09: digraph(ET, NT, 'omitselfloops').
%!test
%! ET.EndNodes = [1 1; 1 2; 2 3];
%! NT.Name = {"x"; "y"; "z"};
%! G = digraph (ET, NT, "omitselfloops");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 2);
%! assert (G.Nodes.Name, {"x"; "y"; "z"});
%! assert (G.Edges.EndNodes, [1 2; 2 3]);

## BIST — US-C09: 'OmitSelfLoops' is case-insensitive.
%!test
%! G1 = digraph ([1 2], [1 3], "OmitSelfLoops");
%! G2 = digraph ([1 2], [1 3], "OMITSELFLOOPS");
%! assert (numedges (G1), 1);
%! assert (numedges (G2), 1);
%! assert (G1.Edges.EndNodes, [2 3]);
%! assert (G2.Edges.EndNodes, [2 3]);

## BIST — US-C09: no self-loops -> 'omitselfloops' is a no-op.
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3], [2 3 1], "omitselfloops");
%! assert (numnodes (G1), numnodes (G2));
%! assert (numedges (G1), numedges (G2));
%! assert (G1.Edges.EndNodes, G2.Edges.EndNodes);

## BIST — US-C09: all edges are self-loops -> empty edge set.
%!test
%! G = digraph ([1 2 3], [1 2 3], "omitselfloops");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);

## BIST — US-C09: empty edges + 'omitselfloops' is valid.
%!test
%! G = digraph ([], [], "omitselfloops");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);

## BIST — US-C09: empty edges with N + 'omitselfloops' preserves N.
%!test
%! G = digraph ([], [], [], 5, "omitselfloops");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 0);

## BIST — US-C09: scalar weight broadcast + 'omitselfloops'.
%!test
%! G = digraph ([1 2 3], [1 2 4], 2.5, "omitselfloops");
%! assert (numedges (G), 1);
%! assert (G.Edges.EndNodes, [3 4]);
%! assert (G.Edges.Weight, 2.5);

## BIST — US-C09: string endpoints + 'omitselfloops'.
%!test
%! G = digraph ({"a", "a", "b"}, {"a", "b", "c"}, [], ...
%!              {"a", "b", "c"}, "omitselfloops");
%! assert (numedges (G), 2);
%! assert (G.Edges.EndNodes, [1 2; 2 3]);

## BIST — US-C09: logical adjacency + 'omitselfloops'.
%!test
%! A = logical ([1 1 0; 0 1 1; 1 0 1]);
%! G = digraph (A, "omitselfloops");
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);

## BIST — US-C09: too many positional args after stripping the flag still
## trips the unsupported-nargs error.
%!error <unsupported> digraph (1, 2, 3, 4, 5, "omitselfloops")

## BIST — US-C09: an unrecognised trailing string is not stripped, and
## still reaches the existing edge-list validation which rejects it.
%!error digraph ([1 2], [1 3], "badflag")

## BIST — US-C10: without 'multigraph', duplicate (s, t) pairs in the
## edge-list form are rejected.
%!error <duplicate> digraph ([1 1], [2 2])
%!error <duplicate> digraph ([1 2 1], [2 3 2])
%!error <duplicate> digraph ([1 1 2], [2 2 3], [10 20 30])

## BIST — US-C10: without 'multigraph', duplicates under the (s, t, w, N)
## form are also rejected.
%!error <duplicate> digraph ([1 1], [2 2], [10 20], 5)

## BIST — US-C10: without 'multigraph', duplicates under the
## (s, t, w, nodenames) form are rejected.
%!error <duplicate> digraph ([1 1], [2 2], [10 20], {"a", "b", "c"})

## BIST — US-C10: digraph(s, t, 'multigraph') permits parallel edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (class (G), "digraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 1 2; 2 3]);

## BIST — US-C10: digraph(s, t, w, 'multigraph') preserves per-edge
## weights for parallel edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 1 2; 2 3]);
%! assert (E.Weight, [10; 20; 30]);

## BIST — US-C10: digraph(s, t, w, N, 'multigraph') respects the node
## count (isolated trailing nodes preserved).
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], 5, "multigraph");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — US-C10: digraph(s, t, w, nodenames, 'multigraph') handles
## parallel edges between named nodes.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], ...
%!              {"a", "b", "c"}, "multigraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — US-C10: parallel edges are sorted lex with duplicates adjacent
## regardless of input order.
%!test
%! G = digraph ([2 1 1 2], [3 2 2 3], [3 1 2 4], "multigraph");
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 1 2; 2 3; 2 3]);
%! assert (E.Weight, [1; 2; 3; 4]);

## BIST — US-C10: three parallel edges between the same pair.
%!test
%! G = digraph ([1 1 1], [2 2 2], [1 2 3], "multigraph");
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 1 2; 1 2]);
%! assert (G.Edges.Weight, [1; 2; 3]);

## BIST — US-C10: multigraph flag without actual duplicates -> simple
## graph shape but ismultigraph still returns false (MATLAB parity).
%!test
%! G = digraph ([1 2 3], [2 3 1], "multigraph");
%! assert (numedges (G), 3);
%! assert (! ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);

## BIST — US-C10: ismultigraph returns false on a regular (non-multigraph)
## digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (! ismultigraph (G));

## BIST — US-C10: ismultigraph returns false on the empty digraph.
%!test
%! G = digraph ();
%! assert (! ismultigraph (G));

## BIST — US-C10: ismultigraph returns false on digraph(N).
%!test
%! G = digraph (5);
%! assert (! ismultigraph (G));

## BIST — US-C10: digraph(A, 'multigraph') — adjacency input has no
## parallel edges, so ismultigraph returns false.
%!test
%! A = [0 1 0; 0 0 1; 1 0 0];
%! G = digraph (A, "multigraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (! ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);

## BIST — US-C10: digraph(A, nodenames, 'multigraph').
%!test
%! A = [0 1 0; 0 0 1; 1 0 0];
%! G = digraph (A, {"a", "b", "c"}, "multigraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});
%! assert (! ismultigraph (G));

## BIST — US-C10: digraph(ET, 'multigraph') permits duplicate EndNodes.
%!test
%! ET.EndNodes = [1 2; 1 2; 2 3];
%! ET.Weight = [10; 20; 30];
%! G = digraph (ET, "multigraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3]);
%! assert (G.Edges.Weight, [10; 20; 30]);

## BIST — US-C10: digraph(ET, NT, 'multigraph') preserves node names.
%!test
%! ET.EndNodes = [1 2; 1 2; 2 3];
%! ET.Weight = [10; 20; 30];
%! NT.Name = {"alpha"; "beta"; "gamma"};
%! G = digraph (ET, NT, "multigraph");
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! assert (G.Nodes.Name, {"alpha"; "beta"; "gamma"});
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3]);

## BIST — US-C10: digraph(ET, 'multigraph') with extra edge column and
## parallel edges — extra column stays in sync with the lex-sorted
## multigraph edges.
%!test
%! ET.EndNodes = [2 3; 1 2; 1 2];
%! ET.Weight = [30; 10; 20];
%! ET.Label = {"c"; "a"; "b"};
%! G = digraph (ET, "multigraph");
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3]);
%! assert (G.Edges.Weight, [10; 20; 30]);
%! assert (G.Edges.Label, {"a"; "b"; "c"});

## BIST — US-C10: 'Multigraph' is case-insensitive.
%!test
%! G1 = digraph ([1 1], [2 2], "Multigraph");
%! G2 = digraph ([1 1], [2 2], "MULTIGRAPH");
%! G3 = digraph ([1 1], [2 2], "multigraph");
%! assert (numedges (G1), 2);
%! assert (numedges (G2), 2);
%! assert (numedges (G3), 2);
%! assert (ismultigraph (G1));
%! assert (ismultigraph (G2));
%! assert (ismultigraph (G3));

## BIST — US-C10: digraph(N, 'multigraph') is an edgeless digraph with
## ismultigraph == false.
%!test
%! G = digraph (5, "multigraph");
%! assert (numnodes (G), 5);
%! assert (numedges (G), 0);
%! assert (! ismultigraph (G));

## BIST — US-C10: digraph('multigraph') alone yields the empty digraph.
%!test
%! G = digraph ("multigraph");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);
%! assert (! ismultigraph (G));

## BIST — US-C10: 'multigraph' + 'omitselfloops' compose (either order).
## Input pairs (1,2)(1,2)(2,3)(2,3)(3,3) → drop (3,3), keep two parallel
## (1,2) pairs and two parallel (2,3) pairs.
%!test
%! G = digraph ([1 1 2 2 3], [2 2 3 3 3], [1 2 3 4 5], ...
%!              "multigraph", "omitselfloops");
%! assert (numedges (G), 4);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3; 2 3]);
%! assert (G.Edges.Weight, [1; 2; 3; 4]);

%!test
%! G = digraph ([1 1 2 2 3], [2 2 3 3 3], [1 2 3 4 5], ...
%!              "omitselfloops", "multigraph");
%! assert (numedges (G), 4);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3; 2 3]);
%! assert (G.Edges.Weight, [1; 2; 3; 4]);

## BIST — US-C10: 'multigraph' + 'omitselfloops' with parallel self-loops
## (both self-loops dropped, parallel non-loop kept).
%!test
%! G = digraph ([1 1 2 2], [1 1 3 3], [10 20 30 40], ...
%!              "multigraph", "omitselfloops");
%! assert (numedges (G), 2);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [2 3; 2 3]);
%! assert (G.Edges.Weight, [30; 40]);

## BIST — US-C10: parallel edges with string endpoints and weights.
%!test
%! G = digraph ({"a", "a", "b"}, {"b", "b", "c"}, [1 2 3], ...
%!              {"a", "b", "c"}, "multigraph");
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 2; 1 2; 2 3]);
%! assert (G.Edges.Weight, [1; 2; 3]);

## BIST — US-C10: empty edges + 'multigraph' is a no-op.
%!test
%! G = digraph ([], [], "multigraph");
%! assert (numnodes (G), 0);
%! assert (numedges (G), 0);
%! assert (! ismultigraph (G));

## BIST — US-C10: parallel self-loops permitted with 'multigraph'.
%!test
%! G = digraph ([1 1 2], [1 1 2], [1 2 3], "multigraph");
%! assert (numedges (G), 3);
%! assert (ismultigraph (G));
%! assert (G.Edges.EndNodes, [1 1; 1 1; 2 2]);
%! assert (G.Edges.Weight, [1; 2; 3]);

## BIST — US-C10: unweighted multigraph has no Weight field.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! E = G.Edges;
%! assert (isfield (E, "EndNodes"));
%! assert (! isfield (E, "Weight"));

## BIST — US-C14: G.Nodes returns a struct on every constructor form.
%!test
%! assert (isstruct (digraph ().Nodes));
%! assert (isstruct (digraph (3).Nodes));
%! assert (isstruct (digraph ([1 2], [2 3]).Nodes));
%! assert (isstruct (digraph ([0 1; 0 0]).Nodes));
%! assert (isstruct (digraph ([1 2], [2 3], [10 20], {"a","b","c"}).Nodes));

## BIST — US-C14: G.Edges returns a struct on every constructor form.
%!test
%! assert (isstruct (digraph ().Edges));
%! assert (isstruct (digraph (3).Edges));
%! assert (isstruct (digraph ([1 2], [2 3]).Edges));
%! assert (isstruct (digraph ([0 1; 0 0]).Edges));
%! assert (isstruct (digraph ([1 2], [2 3], [10 20], {"a","b","c"}).Edges));

## BIST — US-C14: G.Nodes.Name is always present, always a column
## cellstr (empty cell(0,1) when unnamed, populated cellstr otherwise).
%!test
%! G = digraph ();
%! assert (isfield (G.Nodes, "Name"));
%! assert (iscellstr (G.Nodes.Name));
%! assert (G.Nodes.Name, cell (0, 1));
%! G = digraph (3);
%! assert (iscellstr (G.Nodes.Name));
%! assert (G.Nodes.Name, cell (0, 1));
%! G = digraph ([1 2], [2 3], [10 20], {"a","b","c"});
%! assert (iscellstr (G.Nodes.Name));
%! assert (G.Nodes.Name, {"a"; "b"; "c"});

## BIST — US-C14: G.Edges.EndNodes is always m-by-2 numeric, even on a
## truly empty digraph (0 nodes).  This closes a shape-consistency gap
## where find() on a 0-by-0 sparse matrix previously returned 0-by-0
## arrays, leaking a 0-by-0 EndNodes.
%!test
%! G = digraph ();
%! assert (isfield (G.Edges, "EndNodes"));
%! assert (isnumeric (G.Edges.EndNodes));
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = digraph (0);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = digraph (3);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = digraph ([], []);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = digraph ([], [], [], 0);
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = digraph (sparse (0, 0));
%! assert (size (G.Edges.EndNodes), [0 2]);

## BIST — US-C14: EndNodes is numeric indices even when endpoints came
## in as strings (via the EdgeTable constructor, which supports cellstr
## EndNodes with first-appearance name inference).
%!test
%! ET = struct ("EndNodes", {{"a","b"; "b","c"}});
%! G = digraph (ET);
%! assert (isnumeric (G.Edges.EndNodes));
%! assert (G.Edges.EndNodes, [1 2; 2 3]);
%! assert (G.Nodes.Name, {"a"; "b"; "c"});

## BIST — US-C14: Weight appears only when the digraph was built with
## explicit weights.
%!test
%! Gu = digraph ([1 2], [2 3]);
%! assert (isfield (Gu.Edges, "EndNodes"));
%! assert (! isfield (Gu.Edges, "Weight"));
%! Gw = digraph ([1 2], [2 3], [10 20]);
%! assert (isfield (Gw.Edges, "EndNodes"));
%! assert (isfield (Gw.Edges, "Weight"));
%! assert (iscolumn (Gw.Edges.Weight));
%! assert (Gw.Edges.Weight, [10; 20]);

## BIST — US-C14: on a weighted digraph with zero edges, Weight is still
## present as an m-by-1 (0-by-1) column.
%!test
%! G = digraph (sparse (3, 3));
%! assert (isfield (G.Edges, "Weight"));
%! assert (size (G.Edges.Weight), [0 1]);
%! assert (size (G.Edges.EndNodes), [0 2]);

## BIST — US-C14: fieldnames(G.Edges) order is EndNodes -> Weight ->
## extras (in the order the EdgeTable declared them).
%!test
%! ET = struct ("EndNodes", [1 2; 2 3], "Weight", [10; 20], ...
%!              "Label", {{"a"; "b"}});
%! G = digraph (ET);
%! assert (fieldnames (G.Edges), {"EndNodes"; "Weight"; "Label"});

## BIST — US-C14: fieldnames(G.Edges) on an unweighted digraph with
## extras omits Weight.
%!test
%! ET = struct ("EndNodes", [1 2; 2 3], "Kind", {{"solid"; "dashed"}});
%! G = digraph (ET);
%! assert (fieldnames (G.Edges), {"EndNodes"; "Kind"});

## BIST — US-C14: fieldnames(G.Nodes) order is Name -> extras.
%!test
%! NT = struct ("Name", {{"x"; "y"; "z"}}, "Size", [1; 2; 3], ...
%!              "Tag", {{"A"; "B"; "C"}});
%! ET = struct ("EndNodes", [1 2; 2 3]);
%! G = digraph (ET, NT);
%! assert (fieldnames (G.Nodes), {"Name"; "Size"; "Tag"});

## BIST — US-C14: G.Nodes is read-only (SetAccess=private).
%!test
%! G = digraph ([1 2], [2 3], [10 20]);
%! fail ("G.Nodes = struct ();", "private access");

## BIST — US-C14: G.Edges is read-only (SetAccess=private).
%!test
%! G = digraph ([1 2], [2 3], [10 20]);
%! fail ("G.Edges = struct ();", "private access");

## BIST — US-C14: reading G.Edges twice yields the same struct
## (deterministic, idempotent).
%!test
%! G = digraph ([1 3 2], [2 1 3], [10 20 30]);
%! assert (isequal (G.Edges, G.Edges));

## BIST — US-C14: reading G.Nodes twice yields the same struct.
%!test
%! G = digraph ([1 2], [2 3], [10 20], {"a","b","c"});
%! assert (isequal (G.Nodes, G.Nodes));

## BIST — US-C14: dynamic field access G.("Nodes") / G.("Edges") works
## and equals the static form.
%!test
%! G = digraph ([1 2], [2 3], [10 20], {"a","b","c"});
%! assert (isequal (G.("Nodes"), G.Nodes));
%! assert (isequal (G.("Edges"), G.Edges));

## BIST — US-C14: a fully-featured digraph (named + weighted + extra
## edge and node columns) exposes every column via G.Nodes and G.Edges.
%!test
%! ET = struct ("EndNodes", {{"a","b"; "b","c"; "c","a"}}, ...
%!              "Weight", [1; 2; 3], ...
%!              "Label", {{"ab"; "bc"; "ca"}});
%! NT = struct ("Name", {{"a"; "b"; "c"}}, "Size", [10; 20; 30]);
%! G = digraph (ET, NT);
%! N = G.Nodes;
%! E = G.Edges;
%! assert (N.Name, {"a"; "b"; "c"});
%! assert (N.Size, [10; 20; 30]);
%! assert (E.EndNodes, [1 2; 2 3; 3 1]);
%! assert (E.Weight, [1; 2; 3]);
%! assert (E.Label, {"ab"; "bc"; "ca"});

## BIST — US-C14: isolated named nodes appear in G.Nodes even with zero
## edges, and G.Edges.EndNodes is still 0-by-2.
%!test
%! NT = struct ("Name", {{"x"; "y"; "z"}});
%! ET = struct ("EndNodes", zeros (0, 2));
%! G = digraph (ET, NT);
%! assert (G.Nodes.Name, {"x"; "y"; "z"});
%! assert (size (G.Edges.EndNodes), [0 2]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 0);

## BIST — US-C14: property-driven round-trip Gx = digraph(G.Edges, G.Nodes)
## preserves both Edges and Nodes identically.
%!test
%! ET = struct ("EndNodes", [1 2; 2 3; 3 1], ...
%!              "Weight", [10; 20; 30], ...
%!              "Tag", {{"e1"; "e2"; "e3"}});
%! NT = struct ("Name", {{"p"; "q"; "r"}}, "Rank", [1; 2; 3]);
%! G1 = digraph (ET, NT);
%! G2 = digraph (G1.Edges, G1.Nodes);
%! assert (isequal (G1.Edges, G2.Edges));
%! assert (isequal (G1.Nodes, G2.Nodes));

## BIST — US-C14: adjacency-constructed digraph always has a Weight
## column (matrix form implies weighted, MATLAB parity).
%!test
%! G = digraph ([0 1 0; 0 0 1; 0 0 0]);
%! assert (fieldnames (G.Edges), {"EndNodes"; "Weight"});

## BIST — US-C14: G.Nodes on a multigraph has the expected Name field
## (empty cell(0,1) when unnamed, cellstr otherwise).
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (isfield (G.Nodes, "Name"));
%! assert (G.Nodes.Name, cell (0, 1));
%! Gn = digraph ([1 1 2], [2 2 3], [], {"a","b","c"}, "multigraph");
%! assert (Gn.Nodes.Name, {"a"; "b"; "c"});

## BIST — US-C14: G.Edges on a multigraph preserves parallel edges in
## lex order, duplicates adjacent.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! E = G.Edges;
%! assert (E.EndNodes, [1 2; 1 2; 2 3]);
%! assert (E.Weight, [10; 20; 30]);
%! assert (numedges (G), 3);

## BIST — US-C14: G.Edges.EndNodes is m-by-2 even on the empty multigraph.
%!test
%! G = digraph ("multigraph");
%! assert (size (G.Edges.EndNodes), [0 2]);
%! G = digraph (3, "multigraph");
%! assert (size (G.Edges.EndNodes), [0 2]);

## BIST — US-C15: disp on the default (empty) digraph reports 0 nodes
## and 0 edges and does not error.
%!test <*C15>
%! G = digraph ();
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 0 nodes and 0 edges', 'once')));

## BIST — US-C15: disp on an N-node edgeless digraph reports N nodes
## and 0 edges.
%!test <*C15>
%! G = digraph (5);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 5 nodes and 0 edges', 'once')));

## BIST — US-C15: disp on a small weighted digraph reports the correct
## node and edge counts in the header.
%!test <*C15>
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 3 nodes and 3 edges', 'once')));

## BIST — US-C15: singular word forms for 1 node and 1 edge (MATLAB parity).
%!test <*C15>
%! G = digraph (1);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 1 node and 0 edges', 'once')));

## BIST — US-C15: singular word form for exactly 1 edge.
%!test <*C15>
%! G = digraph (1, 2);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 2 nodes and 1 edge', 'once')));

## BIST — US-C15: disp on a digraph with many edges shows the first few
## and reports the remaining count as a continuation line.
%!test <*C15>
%! s_in = 1:20;
%! t_in = [2:20, 1];
%! G = digraph (s_in, t_in);
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 20 nodes and 20 edges', 'once')));
%! ## Continuation line reports the remaining edges beyond the first ten.
%! assert (! isempty (regexp (s, 'more', 'once')));
%! ## Must list at least one concrete edge row ("1   2" with spaces).
%! assert (! isempty (regexp (s, '1\s+2', 'once')));

## BIST — US-C15: disp on a named digraph prints node names (not
## numeric indices) in the edge rows.
%!test <*C15>
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 3 nodes and 3 edges', 'once')));
%! assert (! isempty (strfind (s, "alpha")));
%! assert (! isempty (strfind (s, "beta")));
%! assert (! isempty (strfind (s, "gamma")));

## BIST — US-C15: disp on a multigraph respects parallel-edge counts
## in the header.
%!test <*C15>
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, 'digraph with 3 nodes and 3 edges', 'once')));

## BIST — US-C15: disp shows a Weight column when weights are present.
%!test <*C15>
%! G = digraph ([1 2], [2 3], [11 22]);
%! s = evalc ("disp (G)");
%! assert (! isempty (strfind (s, "Weight")));
%! assert (! isempty (regexp (s, '\<11\>', 'once')));
%! assert (! isempty (regexp (s, '\<22\>', 'once')));

## BIST — US-C15: disp omits the Weight column when the digraph is
## unweighted.
%!test <*C15>
%! G = digraph ([1 2], [2 3]);
%! s = evalc ("disp (G)");
%! assert (isempty (strfind (s, "Weight")));

## BIST — US-C15: display (G) includes a "G =" assignment prefix and
## also contains the header string.
%!test <*C15>
%! G = digraph ([1 2], [2 3]);
%! s = evalc ("display (G)");
%! assert (! isempty (strfind (s, "G =")));
%! assert (! isempty (regexp (s, 'digraph with 3 nodes and 2 edges', 'once')));

## BIST — US-C15: disp writes to stdout (captured non-empty via evalc).
%!test <*C15>
%! G = digraph ();
%! s = evalc ("disp (G)");
%! assert (! isempty (s));

## BIST — US-C15: disp on the truly empty digraph ends with a period
## (no edges section to follow), not a colon.
%!test <*C15>
%! G = digraph ();
%! s = evalc ("disp (G)");
%! assert (! isempty (regexp (s, '0 edges\.\s*$', 'once')));
