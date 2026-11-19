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

## MATLAB R2024a doc example: "Extract Subgraph"
##   doc/graph.subgraph page.
##
## subgraph (G, nodes) returns the graph induced by the selected node
## subset.  All edges whose endpoints are both in the subset are
## preserved.

G = graph ([1 1 2 3 4], [2 3 3 4 5]);

## Extract nodes {1, 2, 3}; the surviving edges are (1,2), (1,3), (2,3).
H = subgraph (G, [1, 2, 3]);
assert (numnodes (H), 3);
assert (numedges (H), 3);
assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);
