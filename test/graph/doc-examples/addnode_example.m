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

## MATLAB R2024a doc example: "Add Nodes to Graph"
##   doc/graph.addnode page.
##
## addnode (G, N) adds N isolated numeric nodes.
## addnode (G, names) adds named nodes by cellstr.

## Start from an empty digraph with 3 nodes.
G = digraph (3);
H = addnode (G, 2);
assert (numnodes (H), 5);
assert (numnodes (G), 3);  # original unchanged

## Adding named nodes extends the Nodes.Name cellstr.
nodenames = {"a", "b", "c"};
G2 = digraph (sparse (3, 3), nodenames);
H2 = addnode (G2, {"d", "e"});
assert (numnodes (H2), 5);
assert (H2.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
