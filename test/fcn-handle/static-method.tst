########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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

%!test <*51709>
%! fh = @pkga.pkgb.bug51709_a.smeth;
%! assert (fh (), "pkg bug51709_a");

%!test <*55975>
%! fh = @pkga.pkgb.bug51709_b.smeth;
%! assert (fh (), "pkg bug51709_b");

## Also test without function handle.
%!assert <*55975> (pkga.pkgb.bug51709_a.smeth (), "pkg bug51709_a");
%!assert (pkga.pkgb.bug51709_b.smeth (), "pkg bug51709_b")
