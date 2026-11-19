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
## @deftypefn {} {[@var{nc1}, @var{nc2}, @var{ec1}, @var{ec2}] =} __isomorphism_parse_opts__ (@var{G1}, @var{G2}, @var{directed}, @var{opts})
## Private helper for the @code{isomorphism} class methods: parse the
## trailing name-value pairs in @var{opts} (a cell array, typically
## @code{varargin}) and compute the per-node and per-edge color
## matrices consumed by @code{__isomorphism_vf2__}.
##
## Recognised options (all cellstr or char scalar values):
## @itemize @bullet
## @item @qcode{"NodeVariables"}: one or more variable names found in
## both @code{G1.Nodes} and @code{G2.Nodes}.  Entries in the node
## tables must match up at every mapped pair of nodes.
## @item @qcode{"EdgeVariables"}: one or more variable names found in
## both @code{G1.Edges} and @code{G2.Edges}.  Entries in the edge
## tables must match up at every mapped pair of edges.  Not supported
## for multigraphs (raises an error).
## @end itemize
##
## When an option is absent or its value is the empty cell
## @code{@{@}}, the corresponding color arrays are returned as
## @code{[]} (disabling that check in VF2).
## @seealso{isomorphism, __isomorphism_vf2__}
## @end deftypefn

function [nc1, nc2, ec1, ec2] = __isomorphism_parse_opts__ (G1, G2, ...
                                                            directed, opts)

  nc1 = [];
  nc2 = [];
  ec1 = [];
  ec2 = [];

  if (nargin != 4)
    error ("Octave:invalid-fun-call", ...
           "__isomorphism_parse_opts__: expected 4 arguments");
  endif

  if (! iscell (opts))
    error ("Octave:invalid-input-arg", ...
           "__isomorphism_parse_opts__: OPTS must be a cell array");
  endif

  if (mod (numel (opts), 2) != 0)
    error ("Octave:invalid-input-arg", ...
           ["isomorphism: trailing arguments must be name-value ", ...
            "pairs"]);
  endif

  node_vars = {};
  edge_vars = {};
  for ii = 1 : 2 : numel (opts)
    key = opts{ii};
    val = opts{ii + 1};
    if (! (ischar (key) && isrow (key)))
      error ("Octave:invalid-input-arg", ...
             "isomorphism: option names must be character strings");
    endif
    switch (lower (key))
      case "nodevariables"
        node_vars = normalize_var_spec (val, "NodeVariables");
      case "edgevariables"
        edge_vars = normalize_var_spec (val, "EdgeVariables");
      otherwise
        error ("Octave:invalid-input-arg", ...
               "isomorphism: unknown option '%s'", key);
    endswitch
  endfor

  ## ---- Node colors -------------------------------------------------
  if (! isempty (node_vars))
    N1 = numnodes (G1);
    N2 = numnodes (G2);
    NT1 = G1.Nodes;
    NT2 = G2.Nodes;
    combined = cell (1, numel (node_vars));
    for k = 1 : numel (node_vars)
      vname = node_vars{k};
      if (! isfield (NT1, vname))
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: NodeVariables: '%s' is not a field ", ...
                "of G1.Nodes"], vname);
      endif
      if (! isfield (NT2, vname))
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: NodeVariables: '%s' is not a field ", ...
                "of G2.Nodes"], vname);
      endif
      v1 = NT1.(vname);
      v2 = NT2.(vname);
      if (size (v1, 1) != N1)
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: NodeVariables: G1.Nodes.%s has %d ", ...
                "rows but G1 has %d nodes"], ...
               vname, size (v1, 1), N1);
      endif
      if (size (v2, 1) != N2)
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: NodeVariables: G2.Nodes.%s has %d ", ...
                "rows but G2 has %d nodes"], ...
               vname, size (v2, 1), N2);
      endif
      if (! same_type (v1, v2))
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: NodeVariables: '%s' has ", ...
                "incompatible types in G1 and G2"], vname);
      endif
      combined{k} = [v1; v2];
    endfor
    colors = __combine_labels__ (combined, N1 + N2);
    nc1 = colors(1 : N1);
    nc2 = colors(N1 + 1 : end);
  endif

  ## ---- Edge colors -------------------------------------------------
  if (! isempty (edge_vars))
    if (ismultigraph (G1) || ismultigraph (G2))
      error ("Octave:invalid-input-arg", ...
             ["isomorphism: EdgeVariables is not supported for ", ...
              "multigraph inputs"]);
    endif
    N1 = numnodes (G1);
    N2 = numnodes (G2);
    ET1 = G1.Edges;
    ET2 = G2.Edges;
    M1 = size (ET1.EndNodes, 1);
    M2 = size (ET2.EndNodes, 1);
    if (M1 == 0 || M2 == 0)
      ## Caller's structural check will already reject M1 != M2; if
      ## both are zero, no per-edge color matrix is needed.
      if (M1 == 0 && M2 == 0)
        ec1 = sparse (N1, N1);
        ec2 = sparse (N2, N2);
        return;
      endif
    endif
    combined = cell (1, numel (edge_vars));
    for k = 1 : numel (edge_vars)
      vname = edge_vars{k};
      if (! isfield (ET1, vname))
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: EdgeVariables: '%s' is not a field ", ...
                "of G1.Edges"], vname);
      endif
      if (! isfield (ET2, vname))
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: EdgeVariables: '%s' is not a field ", ...
                "of G2.Edges"], vname);
      endif
      v1 = ET1.(vname);
      v2 = ET2.(vname);
      if (size (v1, 1) != M1)
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: EdgeVariables: G1.Edges.%s has %d ", ...
                "rows but G1 has %d edges"], ...
               vname, size (v1, 1), M1);
      endif
      if (size (v2, 1) != M2)
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: EdgeVariables: G2.Edges.%s has %d ", ...
                "rows but G2 has %d edges"], ...
               vname, size (v2, 1), M2);
      endif
      if (! same_type (v1, v2))
        error ("Octave:invalid-input-arg", ...
               ["isomorphism: EdgeVariables: '%s' has ", ...
                "incompatible types in G1 and G2"], vname);
      endif
      combined{k} = [v1; v2];
    endfor
    colors = __combine_labels__ (combined, M1 + M2);
    col1 = colors(1 : M1);
    col2 = colors(M1 + 1 : end);

    en1 = ET1.EndNodes;
    en2 = ET2.EndNodes;
    ec1 = build_edge_color_matrix (en1, col1, N1, directed);
    ec2 = build_edge_color_matrix (en2, col2, N2, directed);
  endif

endfunction


## Normalise a user-supplied variable spec (char scalar or cellstr)
## to a cellstr row.  Returns @{@} for an empty spec.
function out = normalize_var_spec (val, label)

  if (isempty (val))
    if (iscell (val) || ischar (val))
      out = {};
      return;
    endif
    error ("Octave:invalid-input-arg", ...
           "isomorphism: %s must be a character string or cellstr", ...
           label);
  endif

  if (ischar (val) && isrow (val))
    out = {val};
    return;
  endif

  if (iscellstr (val))
    out = val(:).';
    return;
  endif

  error ("Octave:invalid-input-arg", ...
         "isomorphism: %s must be a character string or cellstr", ...
         label);

endfunction


## Both v1 and v2 are column vectors.  Cellstr + cellstr OK; numeric
## + numeric OK; char + char OK.  Mixed -> false.
function tf = same_type (v1, v2)

  if (iscell (v1) && iscell (v2))
    tf = iscellstr (v1) && iscellstr (v2);
  elseif (ischar (v1) && ischar (v2))
    tf = true;
  elseif (isnumeric (v1) && isnumeric (v2))
    tf = true;
  elseif (islogical (v1) && islogical (v2))
    tf = true;
  else
    tf = false;
  endif

endfunction


## Build the N-by-N edge color matrix EC from EndNodes (m-by-2), a
## column of integer colors (m-by-1), and the directed flag.
##
## For a digraph, EC(src, dst) = color.  For a graph, the underlying
## adjacency is symmetric so EC(i, j) = EC(j, i) = color for i != j,
## and EC(i, i) = color for self-loops (stored once).
function EC = build_edge_color_matrix (en, col, N, directed)

  s = en(:, 1);
  t = en(:, 2);
  if (directed)
    EC = sparse (s, t, col, N, N);
  else
    self = (s == t);
    i_nodes = [s; t(! self)];
    j_nodes = [t; s(! self)];
    v = [col; col(! self)];
    EC = sparse (i_nodes, j_nodes, v, N, N);
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Basic NodeVariables parsing: char option name, char value.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! [nc1, nc2, ec1, ec2] = __isomorphism_parse_opts__ (G, G, true, ...
%!   {"NodeVariables", "Name"});
%! assert (size (nc1), [3, 1]);
%! assert (size (nc2), [3, 1]);
%! assert (nc1, nc2);
%! assert (isempty (ec1));
%! assert (isempty (ec2));

## Cellstr NodeVariables.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! [nc1, nc2] = __isomorphism_parse_opts__ (G, G, true, ...
%!   {"NodeVariables", {"Name"}});
%! assert (numel (nc1), 3);

## Empty NodeVariables cell -> nothing returned (all empty).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [nc1, nc2, ec1, ec2] = __isomorphism_parse_opts__ (G, G, true, ...
%!   {"NodeVariables", {}});
%! assert (isempty (nc1));
%! assert (isempty (nc2));
%! assert (isempty (ec1));
%! assert (isempty (ec2));

## EdgeVariables="Weight" on a simple digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! [nc1, nc2, ec1, ec2] = __isomorphism_parse_opts__ (G, G, true, ...
%!   {"EdgeVariables", "Weight"});
%! assert (isempty (nc1));
%! assert (size (ec1), [3, 3]);
%! assert (nnz (ec1), 3);
%! assert (full (ec1(1, 2)) != 0);   # edge 1->2 has nonzero color

## EdgeVariables on a multigraph -> error.
%!error <multigraph> ...
%! s = [1 1 2]; t = [2 2 3];
%! G = digraph (s, t, [1 2 3], "multigraph");
%! __isomorphism_parse_opts__ (G, G, true, {"EdgeVariables", "Weight"});

## Unknown option name -> error.
%!error <unknown option> ...
%! G = digraph ([1 2 3], [2 3 1]);
%! __isomorphism_parse_opts__ (G, G, true, {"NotAnOption", "foo"});

## Odd number of name-value args -> error.
%!error <name-value> ...
%! G = digraph ([1 2 3], [2 3 1]);
%! __isomorphism_parse_opts__ (G, G, true, {"NodeVariables"});

## NodeVariables not cellstr/char -> error.
%!error <NodeVariables> ...
%! G = digraph ([1 2 3], [2 3 1]);
%! __isomorphism_parse_opts__ (G, G, true, {"NodeVariables", 42});

## Unknown Node variable name -> error.
%!error <NodeVariables> ...
%! G = digraph ([1 2 3], [2 3 1]);
%! __isomorphism_parse_opts__ (G, G, true, {"NodeVariables", "NoSuch"});

## Row-length mismatch: empty Name column but nodes exist -> error.
%!error <NodeVariables> ...
%! G = digraph ([1 2 3], [2 3 1]);   # Name column is cell (0, 1)
%! G2 = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! __isomorphism_parse_opts__ (G, G2, true, {"NodeVariables", "Name"});

## Undirected case: ec matrix is symmetric.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! [~, ~, ec1, ~] = __isomorphism_parse_opts__ (G, G, false, ...
%!   {"EdgeVariables", "Weight"});
%! assert (issymmetric (ec1));

## Combined NodeVariables + EdgeVariables.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! NT.Name = {"a"; "b"; "c"};
%! G = digraph (ET, NT);
%! [nc1, nc2, ec1, ec2] = __isomorphism_parse_opts__ (G, G, true, ...
%!   {"NodeVariables", "Name", "EdgeVariables", "Weight"});
%! assert (! isempty (nc1));
%! assert (! isempty (ec1));
