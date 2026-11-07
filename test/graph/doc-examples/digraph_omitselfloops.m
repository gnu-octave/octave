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

## MATLAB R2024a doc example: "Specify 'omitselfloops' to ignore diagonal"
##   doc/digraph page.
##
## The 'omitselfloops' option drops edges of the form (i, i) during
## construction, regardless of which constructor form is used.

A = [1 1 0;
     0 1 1;
     0 0 1];
G = digraph (A, "omitselfloops");

assert (numnodes (G), 3);
## Diagonal entries are dropped, so only the two off-diagonals remain.
assert (numedges (G), 2);
assert (G.Edges.EndNodes, [1 2; 2 3]);
