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

## MATLAB R2024a doc example: "Topological Order of DAG"
##   doc/digraph.toposort and doc/digraph.isdag pages.
##
## toposort returns one valid topological ordering of a DAG.
## isdag returns true when a topological ordering exists.

## A DAG: 1 -> 2 -> 4, 1 -> 3 -> 4, 4 -> 5.
G = digraph ([1 1 2 3 4], [2 3 4 4 5]);
assert (isdag (G));

order = toposort (G);
assert (numel (order), 5);
## 1 must come before 2 and 3; 4 before 5; 2 and 3 before 4.
pos = zeros (1, 5);
for k = 1:5
  pos(order(k)) = k;
endfor
assert (pos(1) < pos(2));
assert (pos(1) < pos(3));
assert (pos(2) < pos(4));
assert (pos(3) < pos(4));
assert (pos(4) < pos(5));

## A cycle kills the DAG property.
Gc = digraph ([1 2 3], [2 3 1]);
assert (! isdag (Gc));
