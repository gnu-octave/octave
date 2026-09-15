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
## @deftypefn {} {[@var{adj_out}, @var{names_out}, @var{nattrs_out}] =} __reordernodes_impl__ (@var{adj_in}, @var{names_in}, @var{nattrs_in}, @var{perm})
## Private helper shared by the @code{reordernodes} methods of the
## @code{graph} and @code{digraph} classes.
##
## Permute the rows/columns of the @code{N}-by-@code{N} sparse
## adjacency @var{adj_in} according to @var{perm} (an @code{N}-by-@code{1}
## column vector of positive integer indices that is a permutation of
## @code{1:N}).  Returns @var{adj_out}, @var{names_out} (node names
## reordered; empty input stays empty), and @var{nattrs_out} (each
## node-attribute column sliced by row).
##
## The helper does not validate @var{perm}: the caller is responsible
## for verifying that @var{perm} is a valid permutation before
## delegating.
##
## The helper does not touch edge-attribute storage
## (@code{edge_attrs_}, @code{mg_endnodes_}, @code{mg_weights_}):
## those depend on the edge-representation choice made by the calling
## class, which re-sorts the edge rows according to the new adjacency's
## iteration order.
##
## @seealso{reordernodes, graph, digraph}
## @end deftypefn

function [adj_out, names_out, nattrs_out] = __reordernodes_impl__ (adj_in, names_in, nattrs_in, perm)

  if (nargin != 4)
    print_usage ();
  endif

  ## Permute adjacency.  For N == 0 the indexing yields a 0-by-0 sparse
  ## matching the default property shape; MATLAB parity.
  adj_out = adj_in(perm, perm);

  ## Permute node names.  Preserve the "nameless" convention: empty
  ## input stays empty (no auto-naming here).
  if (isempty (names_in))
    names_out = names_in;
  else
    names_out = names_in(perm);
    names_out = names_out(:);
  endif

  ## Permute every node-attribute column by row.
  nattrs_out = struct ();
  fn = fieldnames (nattrs_in);
  for ii = 1:numel (fn)
    col = nattrs_in.(fn{ii});
    nattrs_out.(fn{ii}) = col(perm, :);
  endfor

endfunction


## ------------------------------------------------------------------
## Private-helper smoke tests.
## ------------------------------------------------------------------

## Reorder a 3-node adjacency with a 3-cycle permutation.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! [A2, nm, at] = __reordernodes_impl__ (A, {}, struct (), [3; 1; 2]);
%! assert (size (A2), [3, 3]);
%! assert (isempty (nm));
%! ## Check adj_out(i, j) == adj_in(perm(i), perm(j)).
%! assert (full (A2), full (A([3 1 2], [3 1 2])));

## Reorder node names.
%!test
%! A = sparse (3, 3);
%! names = {"a"; "b"; "c"};
%! [A2, nm, at] = __reordernodes_impl__ (A, names, struct (), [3; 1; 2]);
%! assert (nm, {"c"; "a"; "b"});

## Reorder node-attribute columns.
%!test
%! A = sparse (3, 3);
%! nat.Size = [10; 20; 30];
%! [A2, nm, at] = __reordernodes_impl__ (A, {}, nat, [3; 1; 2]);
%! assert (at.Size, [30; 10; 20]);

## Identity permutation is a no-op.
%!test
%! A = sparse ([1 2 3], [2 3 1], [10 20 30], 3, 3);
%! names = {"a"; "b"; "c"};
%! nat.Size = [10; 20; 30];
%! [A2, nm, at] = __reordernodes_impl__ (A, names, nat, [1; 2; 3]);
%! assert (full (A2), full (A));
%! assert (nm, names);
%! assert (at.Size, [10; 20; 30]);

## Empty input (N == 0) yields empty output.
%!test
%! A = sparse (0, 0);
%! [A2, nm, at] = __reordernodes_impl__ (A, {}, struct (), zeros (0, 1));
%! assert (size (A2), [0, 0]);
%! assert (isempty (nm));

## Cellstr attribute column permutes correctly.
%!test
%! A = sparse (3, 3);
%! nat.Tag = {"x"; "y"; "z"};
%! [A2, nm, at] = __reordernodes_impl__ (A, {}, nat, [3; 2; 1]);
%! assert (at.Tag, {"z"; "y"; "x"});

## Numeric matrix attribute column permutes rows only.
%!test
%! A = sparse (3, 3);
%! nat.XY = [1 2; 3 4; 5 6];
%! [A2, nm, at] = __reordernodes_impl__ (A, {}, nat, [2; 3; 1]);
%! assert (at.XY, [3 4; 5 6; 1 2]);
