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
## @deftypefn {} {[@var{adj_out}, @var{names_out}, @var{nattrs_out}, @var{keep_mask}] =} __rmnode_impl__ (@var{adj_in}, @var{names_in}, @var{nattrs_in}, @var{rm_idx})
## Private helper shared by the @code{rmnode} methods of the
## @code{graph} and @code{digraph} classes.
##
## Drop the rows/columns of the @code{N}-by-@code{N} sparse adjacency
## @var{adj_in} whose indices are listed in @var{rm_idx} (a numeric
## column vector of unique-or-duplicated positive integer indices in
## @code{1:N}).  Returns the reduced adjacency @var{adj_out}, the
## filtered node-name cellstr @var{names_out} (unchanged shape when the
## input was nameless), the filtered node-attribute struct
## @var{nattrs_out} (each field sliced by rows), and the
## @code{N}-by-@code{1} logical keep-mask @var{keep_mask} that the
## caller can use to filter class-specific edge storage (simple
## adjacency survival mask for @code{graph} and simple-mode
## @code{digraph}, or multigraph edge-row survival for multigraph
## @code{digraph}).
##
## The helper does not touch edge-attribute storage
## (@code{edge_attrs_}, @code{has_weights_}, @code{mg_endnodes_},
## @code{mg_weights_}): those depend on the edge-representation choice
## made by the calling class, which uses @var{keep_mask} and its own
## edge iteration to compute a per-edge survivor mask.
##
## @seealso{rmnode, graph, digraph, __resolve_node_list__}
## @end deftypefn

function [adj_out, names_out, nattrs_out, keep_mask] = __rmnode_impl__ (adj_in, names_in, nattrs_in, rm_idx)

  if (nargin != 4)
    print_usage ();
  endif

  N = size (adj_in, 1);

  ## Build keep_mask (N-by-1 logical).  Empty rm_idx means "keep all".
  keep_mask = true (N, 1);
  if (! isempty (rm_idx))
    keep_mask(rm_idx) = false;
  endif

  ## Filter adjacency.  For N == 0 the indexing yields a 0-by-0 sparse,
  ## matching the default property shape.
  adj_out = adj_in(keep_mask, keep_mask);

  ## Filter node names.  Preserve the "nameless" convention: empty
  ## input stays empty (no auto-naming here).
  if (isempty (names_in))
    names_out = names_in;
  else
    names_out = names_in(keep_mask);
    names_out = names_out(:);
  endif

  ## Filter every node-attribute column by row.
  nattrs_out = struct ();
  fn = fieldnames (nattrs_in);
  for ii = 1:numel (fn)
    col = nattrs_in.(fn{ii});
    nattrs_out.(fn{ii}) = col(keep_mask, :);
  endfor

endfunction


## ------------------------------------------------------------------
## Private-helper smoke tests.
## ------------------------------------------------------------------

## Remove one node from a 3-node adjacency.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [A2, nm, at, km] = __rmnode_impl__ (A, {}, struct (), 2);
%! assert (size (A2), [2, 2]);
%! assert (km, [true; false; true]);
%! assert (isempty (nm));
%! ## Only the 3->1 edge survives; after compaction it is 2->1.
%! [r, c] = find (A2);
%! assert (sort ([r c], 1), [2 1]);

## Filter node names and attribute columns.
%!test
%! A = sparse (3, 3);
%! names = {"a"; "b"; "c"};
%! nat.Size = [10; 20; 30];
%! [A2, nm, at, km] = __rmnode_impl__ (A, names, nat, [1; 3]);
%! assert (size (A2), [1, 1]);
%! assert (nm, {"b"});
%! assert (at.Size, 20);
%! assert (km, [false; true; false]);

## Empty rm_idx is a no-op.
%!test
%! A = sparse (4, 4);
%! [A2, nm, at, km] = __rmnode_impl__ (A, {"a";"b";"c";"d"}, struct(), []);
%! assert (size (A2), [4, 4]);
%! assert (nm, {"a"; "b"; "c"; "d"});
%! assert (km, true (4, 1));

## Duplicates in rm_idx are idempotent.
%!test
%! A = sparse (4, 4);
%! [A2, nm, at, km] = __rmnode_impl__ (A, {}, struct(), [2; 2]);
%! assert (size (A2), [3, 3]);
%! assert (km, [true; false; true; true]);

## Remove all nodes leaves a 0-by-0 sparse.
%!test
%! A = sparse (3, 3);
%! [A2, nm, at, km] = __rmnode_impl__ (A, {"a";"b";"c"}, struct(), [1;2;3]);
%! assert (size (A2), [0, 0]);
%! assert (numel (nm), 0);
%! assert (km, [false; false; false]);

## Cellstr attribute column filters correctly.
%!test
%! A = sparse (3, 3);
%! nat.Tag = {"x"; "y"; "z"};
%! [A2, nm, at, km] = __rmnode_impl__ (A, {}, nat, 2);
%! assert (at.Tag, {"x"; "z"});
