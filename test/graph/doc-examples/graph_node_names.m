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

## MATLAB R2024a doc example: "Graph with Node Names"
##   doc/graph page.
##
## Create an undirected graph with named nodes using the
## graph (s, t, w, nodenames) constructor form.

nodenames = {"New York", "Boston", "Chicago", "Miami"};
s = {"New York" "New York" "Boston" "Chicago"};
t = {"Boston" "Chicago" "Chicago" "Miami"};
w = [300 900 1000 1400];
G = graph (s, t, w, nodenames);

assert (numnodes (G), 4);
assert (numedges (G), 4);
assert (G.Nodes.Name, {"New York"; "Boston"; "Chicago"; "Miami"});
assert (G.Edges.Weight, [300; 900; 1000; 1400]);
