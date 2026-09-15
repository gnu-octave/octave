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

## MATLAB R2024a doc example: "Node Degrees"
##   doc/graph.degree, doc/digraph.indegree, doc/digraph.outdegree.
##
## For graph: degree (G) = number of incident edges per node (self-
## loops count as 2).  For digraph: indegree/outdegree split the
## count by direction.

## Undirected case.
G = graph ([1 1 2 3 4], [2 3 3 4 5]);
d = degree (G);
## Degrees: node 1 touches {2,3}; node 2 touches {1,3}; node 3 touches
## {1,2,4}; node 4 touches {3,5}; node 5 touches {4}.
assert (d(:), [2; 2; 3; 2; 1]);

## Directed case: 1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 4 -> 5.
D = digraph ([1 1 2 3 4], [2 3 3 4 5]);
assert (indegree (D)(:),  [0; 1; 2; 1; 1]);
assert (outdegree (D)(:), [2; 1; 1; 1; 0]);
