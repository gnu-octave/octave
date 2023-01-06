########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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

## There are many corner cases for left division operator
%!assert (0 \ 1, Inf)
%!assert (0 \ single (1), single (Inf))
## FIXME: Should return Inf, but not coded correctly yet.
%#!assert (0 \ i, Inf)
%#!assert (0 \ single (i), single (Inf))

%!assert ([Inf, 0; 0, 0] \ [1; 1], zeros (2,1))
%!assert ([Inf, 0; 0, 0] \ single ([1; 1]), zeros (2,1, "single"))
%!assert ([Inf, 0; 0, 0] \ [i; 1], zeros (2,1))
%!assert ([Inf, 0; 0, 0] \ single ([i; 1]), zeros (2,1, "single"))

