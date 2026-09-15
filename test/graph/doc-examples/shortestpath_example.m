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

## MATLAB R2024a doc example: "Find Shortest Path Between Two Nodes"
##   doc/graph.shortestpath page.
##
## [P, d] = shortestpath (G, s, t) returns the path P as a vector of
## node IDs and the total cost d.  Unweighted graphs measure d as the
## number of edges traversed.

## A simple path graph: 1 -- 2 -- 3 -- 4 -- 5.
G = graph ([1 2 3 4], [2 3 4 5]);
[P, d] = shortestpath (G, 1, 5);
assert (P, [1 2 3 4 5]);
assert (d, 4);

## With weights, the same path gets the sum of weights.
Gw = graph ([1 2 3 4 1], [2 3 4 5 3], [1 1 1 1 10]);
[Pw, dw] = shortestpath (Gw, 1, 5);
assert (Pw, [1 2 3 4 5]);
assert (dw, 4);   # four weight-1 edges cheaper than one 10-edge detour.
