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

## MATLAB R2024a doc example: "Directed Graph with Node Names"
##   doc/digraph page.
##
## Create a directed graph with named nodes using the
## digraph (s, t, w, nodenames) constructor form so that cellstr
## endpoints are resolved by lookup and isolated nodes are preserved.

nodenames = {"a", "b", "c", "d"};
s = {"a" "a" "b" "b" "c"};
t = {"b" "c" "a" "d" "a"};
w = [1 1 1 1 1];
G = digraph (s, t, w, nodenames);

assert (numnodes (G), 4);
assert (numedges (G), 5);
assert (G.Nodes.Name, {"a"; "b"; "c"; "d"});
## EndNodes get stored as resolved 1-based indices in lex order.
assert (G.Edges.EndNodes, [1 2; 1 3; 2 1; 2 4; 3 1]);
