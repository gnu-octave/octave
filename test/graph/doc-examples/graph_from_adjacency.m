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

## MATLAB R2024a doc example: "Graph from Adjacency Matrix"
##   doc/graph page.
##
## For undirected graphs, the adjacency matrix A must be symmetric or
## triangular.  graph (A) reads the lower (or upper) triangle to avoid
## double-counting edges.

A = [0 1 2 0;
     1 0 3 0;
     2 3 0 4;
     0 0 4 0];
G = graph (A);

assert (numnodes (G), 4);
assert (numedges (G), 4);
## Canonical lex-ordered EndNodes.
assert (G.Edges.EndNodes, [1 2; 1 3; 2 3; 3 4]);
assert (G.Edges.Weight, [1; 2; 3; 4]);
