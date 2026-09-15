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

## MATLAB R2024a doc example: "Directed Graph from Numeric Node IDs"
##   doc/digraph page, first example.
##
## Create a directed graph whose edges are specified by two vectors of
## numeric node identifiers.  The node count is auto-derived from the
## largest node identifier appearing in s or t.

s = [1 1 2 2 3];
t = [2 3 1 4 1];
G = digraph (s, t);

assert (numnodes (G), 4);
assert (numedges (G), 5);
## EndNodes are lex-sorted by (src, dst).
assert (G.Edges.EndNodes, [1 2; 1 3; 2 1; 2 4; 3 1]);
