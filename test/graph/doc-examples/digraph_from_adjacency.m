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

## MATLAB R2024a doc example: "Directed Graph from Adjacency Matrix"
##   doc/digraph page.
##
## An adjacency matrix A directly encodes edges and weights: node i
## has an edge to node j with weight A(i,j) when A(i,j) != 0.

A = [0 10 20  0;
     0  0  0  0;
     0  0  0 30;
     0  0  0  0];
G = digraph (A);

assert (numnodes (G), 4);
assert (numedges (G), 3);
assert (G.Edges.EndNodes, [1 2; 1 3; 3 4]);
assert (G.Edges.Weight, [10; 20; 30]);
