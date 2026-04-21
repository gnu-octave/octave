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

## MATLAB R2024a doc example: "Reorder Graph Nodes"
##   doc/graph.reordernodes page.
##
## reordernodes (G, order) returns a graph with the nodes relabeled
## according to order: new node k corresponds to old node order(k).

nodenames = {"a", "b", "c", "d"};
s = {"a" "a" "b" "c"};
t = {"b" "c" "c" "d"};
w = [1 2 3 4];
G = graph (s, t, w, nodenames);
assert (G.Nodes.Name, {"a"; "b"; "c"; "d"});

## Reverse the order: new 1 <- old 4, ..., new 4 <- old 1.
H = reordernodes (G, [4 3 2 1]);
assert (H.Nodes.Name, {"d"; "c"; "b"; "a"});
assert (numedges (H), numedges (G));
