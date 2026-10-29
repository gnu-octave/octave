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

## MATLAB R2024a doc example: "Node Centrality"
##   doc/graph.centrality page.
##
## Use centrality (G, type) to rank nodes.  For a star graph centered
## at node 1, the central node dominates degree, closeness, and
## betweenness measures.

## Star graph with 5 leaves: center = node 1.
s = [1 1 1 1 1];
t = [2 3 4 5 6];
G = graph (s, t);

d = centrality (G, "degree");
assert (numel (d), 6);
## The center has maximal degree.
[~, idx] = max (d);
assert (idx, 1);

## Closeness of the center is also maximal.
c = centrality (G, "closeness");
[~, idx_c] = max (c);
assert (idx_c, 1);
