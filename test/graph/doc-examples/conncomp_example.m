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

## MATLAB R2024a doc example: "Connected Components"
##   doc/graph.conncomp page.
##
## conncomp (G) returns a component label per node.  Nodes in the
## same component share a label.

## Two disjoint cliques: {1,2,3} and {4,5}.
s = [1 2 1 4];
t = [2 3 3 5];
G = graph (s, t);

labels = conncomp (G);
assert (numel (labels), 5);
## Nodes 1, 2, 3 share a component; 4, 5 share another.
assert (labels(1), labels(2));
assert (labels(1), labels(3));
assert (labels(4), labels(5));
assert (labels(1) != labels(4));

## Two components total.
assert (max (labels), 2);
