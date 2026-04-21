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

## MATLAB R2024a doc example: "Graph with Numeric Node IDs"
##   doc/graph page.
##
## Create an undirected graph from two parallel endpoint vectors.  For
## undirected graphs each (s, t) is treated as an unordered pair; the
## EndNodes table canonicalizes with the smaller index first.

s = [1 1 2 3];
t = [2 3 3 4];
G = graph (s, t);

assert (numnodes (G), 4);
assert (numedges (G), 4);
## Undirected edges are stored with min-first canonical orientation.
assert (G.Edges.EndNodes, [1 2; 1 3; 2 3; 3 4]);
