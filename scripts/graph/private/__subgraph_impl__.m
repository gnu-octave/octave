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
## @deftypefn {} {[@var{adj_out}, @var{names_out}, @var{nattrs_out}] =} __subgraph_impl__ (@var{adj_in}, @var{names_in}, @var{nattrs_in}, @var{keep_idx})
## Private helper shared by the @code{subgraph} methods of the
## @code{graph} and @code{digraph} classes.
##
## Filter and permute the rows/columns of the @code{N}-by-@code{N}
## sparse adjacency @var{adj_in} so that the returned
## @code{numel (keep_idx)}-by-@code{numel (keep_idx)} sparse
## @var{adj_out} retains only the nodes listed in @var{keep_idx}, in the
## given order.  @var{keep_idx} is a column vector of unique positive
## integer indices in @code{1:N}.
##
## The node-name cellstr @var{names_in} is filtered/reordered the same
## way (or left empty if the graph was nameless), and each field of the
## node-attribute struct @var{nattrs_in} is sliced by rows.
##
## The helper does not touch edge-attribute storage
## (@code{edge_attrs_}, @code{has_weights_}, @code{mg_endnodes_},
## @code{mg_weights_}): those depend on the edge-representation choice
## made by the calling class, which handles per-edge filtering and
## reordering from a pre-computed survival mask and permutation.
##
## @seealso{subgraph, graph, digraph, __resolve_node_list__}
## @end deftypefn

function [adj_out, names_out, nattrs_out] = __subgraph_impl__ (adj_in, names_in, nattrs_in, keep_idx)

  if (nargin != 4)
    print_usage ();
  endif

  ## Slice and reorder adjacency.  For empty keep_idx the indexing
  ## yields a 0-by-0 sparse, matching the default property shape.
  adj_out = adj_in(keep_idx, keep_idx);

  ## Filter and reorder node names.  Preserve the "nameless" convention:
  ## empty input stays empty.
  if (isempty (names_in))
    names_out = names_in;
  else
    names_out = names_in(keep_idx);
    names_out = names_out(:);
  endif

  ## Filter and reorder each node-attribute column by row.
  nattrs_out = struct ();
  fn = fieldnames (nattrs_in);
  for ii = 1:numel (fn)
    col = nattrs_in.(fn{ii});
    nattrs_out.(fn{ii}) = col(keep_idx, :);
  endfor

endfunction


## ------------------------------------------------------------------
## Private-helper smoke tests.
## ------------------------------------------------------------------

## Keep a prefix: 3x3 adjacency -> 2x2 with filter only.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [A2, nm, at] = __subgraph_impl__ (A, {}, struct (), [1; 2]);
%! assert (size (A2), [2, 2]);
%! assert (isempty (nm));
%! ## Edges surviving from (1->2, 2->3, 3->1): only 1->2.
%! [r, c] = find (A2);
%! assert (sort ([r c], 1), [1 2]);

## Reorder: pick nodes in reversed order.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [A2, nm, at] = __subgraph_impl__ (A, {}, struct (), [3; 2; 1]);
%! assert (size (A2), [3, 3]);
%! ## Old edges 1->2, 2->3, 3->1 become new 3->2, 2->1, 1->3.
%! [r, c] = find (A2);
%! assert (sortrows ([r c]), [1 3; 2 1; 3 2]);

## Filter node names and attribute columns in keep-order.
%!test
%! A = sparse (3, 3);
%! names = {"a"; "b"; "c"};
%! nat.Size = [10; 20; 30];
%! [A2, nm, at] = __subgraph_impl__ (A, names, nat, [3; 1]);
%! assert (size (A2), [2, 2]);
%! assert (nm, {"c"; "a"});
%! assert (at.Size, [30; 10]);

## Empty keep_idx -> 0-by-0 sparse.
%!test
%! A = sparse (3, 3);
%! [A2, nm, at] = __subgraph_impl__ (A, {"a";"b";"c"}, struct(), zeros (0, 1));
%! assert (size (A2), [0, 0]);
%! assert (numel (nm), 0);

## All nodes in original order is an identity.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [A2, nm, at] = __subgraph_impl__ (A, {}, struct (), (1:3)');
%! assert (nnz (A2 - A), 0);

## Single node in list: 1x1 result.
%!test
%! A = sparse (3, 3);
%! nat.Tag = {"x"; "y"; "z"};
%! [A2, nm, at] = __subgraph_impl__ (A, {"a";"b";"c"}, nat, 2);
%! assert (size (A2), [1, 1]);
%! assert (nm, {"b"});
%! assert (at.Tag, {"y"});

## Nameless graph stays nameless after subgraph.
%!test
%! A = sparse (4, 4);
%! [A2, nm, at] = __subgraph_impl__ (A, {}, struct (), [1; 3]);
%! assert (isempty (nm));
