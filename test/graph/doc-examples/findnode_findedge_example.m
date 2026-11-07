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

## MATLAB R2024a doc example: "Find Nodes and Edges by Name or ID"
##   doc/graph.findnode and doc/graph.findedge pages.
##
## findnode (G, name) returns the numeric ID of a named node (0 when
## the name is not found).  findedge (G, s, t) returns the edge index
## (0 when the edge is absent).

nodenames = {"a", "b", "c", "d"};
s = {"a" "a" "b"};
t = {"b" "c" "c"};
w = [1 2 3];
G = digraph (s, t, w, nodenames);

## Name -> index round-trips.
assert (findnode (G, "a"), 1);
assert (findnode (G, "d"), 4);
assert (findnode (G, "nonexistent"), 0);

## Edge lookup returns the row index into Edges.
assert (findedge (G, 1, 2), 1);
assert (findedge (G, 2, 3), 3);
## Absent edge reports 0.
assert (findedge (G, 3, 4), 0);
