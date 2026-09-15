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

## MATLAB R2024a doc example: "Find Neighbors in Graph"
##   doc/graph.neighbors page.
##
## neighbors (G, n) returns the nodes adjacent to n.  For undirected
## graphs the result is unioned over both incident directions.

G = graph ([1 1 2 3 4], [2 3 3 4 5]);

nbrs1 = neighbors (G, 1);
assert (sort (nbrs1(:)), [2; 3]);

nbrs3 = neighbors (G, 3);
assert (sort (nbrs3(:)), [1; 2; 4]);

nbrs5 = neighbors (G, 5);
assert (nbrs5(:), 4);
