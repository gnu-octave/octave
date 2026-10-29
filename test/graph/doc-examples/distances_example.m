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

## MATLAB R2024a doc example: "All Shortest Path Distances"
##   doc/graph.distances page.
##
## D = distances (G) returns the all-pairs shortest-path distance
## matrix.  For disconnected node pairs, D(i,j) is Inf.  On the
## diagonal, D(i,i) is 0.

## A path: 1 - 2 - 3 - 4 plus an isolated node 5.
G = graph ([1 2 3], [2 3 4]);
G = addnode (G, 1);        # node 5 isolated

D = distances (G);
assert (size (D), [5, 5]);
assert (diag (D), zeros (5, 1));
assert (D(1, 4), 3);
assert (D(2, 3), 1);
## Node 5 is disconnected from everything.
assert (all (isinf (D(5, 1:4))));
assert (all (isinf (D(1:4, 5))));
