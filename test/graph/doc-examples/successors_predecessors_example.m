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

## MATLAB R2024a doc examples: "successors" and "predecessors"
##   doc/digraph.successors and doc/digraph.predecessors pages.
##
## For a directed graph the asymmetric neighbor lookups are exposed
## via successors (out-edges) and predecessors (in-edges).

G = digraph ([1 1 2 3 4 5], [2 3 3 4 5 6]);

## Successors of node 3 are {4}.
s3 = successors (G, 3);
assert (s3(:), 4);

## Predecessors of node 3 are {1, 2}.
p3 = predecessors (G, 3);
assert (sort (p3(:)), [1; 2]);

## Node 6 is a sink; it has no successors and one predecessor.
assert (isempty (successors (G, 6)));
assert (predecessors (G, 6), 5);
