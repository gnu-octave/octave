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

## MATLAB R2024a doc examples: "adjacency", "incidence", "laplacian"
##   doc/graph.adjacency, doc/graph.incidence, doc/graph.laplacian.
##
## Expose the three standard matrix representations of a graph:
## adjacency, node-edge incidence, and combinatorial Laplacian.

## Triangle 1-2-3.
G = graph ([1 2 3], [2 3 1]);

A = adjacency (G);
assert (issparse (A));
assert (size (A), [3, 3]);
assert (full (A), [0 1 1; 1 0 1; 1 1 0]);

I = incidence (G);
assert (size (I), [3, 3]);
## Each column of the incidence matrix sums to 0 for undirected
## graphs (two entries: +1 / -1 or two +1s depending on convention).
## Octave matches MATLAB's oriented +/-1 convention for undirected
## graphs where a consistent orientation is chosen.
assert (full (sum (abs (I), 1)), [2 2 2]);  # each edge touches exactly 2 nodes

L = laplacian (G);
## L = D - A: for a triangle, each degree is 2, so diag (L) = [2; 2; 2].
assert (full (diag (L)), [2; 2; 2]);
assert (full (sum (L, 2)), zeros (3, 1));
