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

## MATLAB R2024a doc example: "Breadth-First Search"
##   doc/graph.bfsearch page.
##
## bfsearch (G, s) returns the nodes in BFS discovery order starting
## from s.  The first entry is always s itself.

## A tree-ish digraph:
##   1 -> 2 -> 4
##   1 -> 3 -> 5
##   3 -> 6
G = digraph ([1 1 2 3 3], [2 3 4 5 6]);
v = bfsearch (G, 1);
assert (v(1), 1);
## BFS discovers node 2, 3 before 4, 5, 6.
assert (sort (v(:))', [1 2 3 4 5 6]);
p2 = find (v == 2);
p4 = find (v == 4);
assert (p2 < p4, "BFS should discover 2 before 4");
