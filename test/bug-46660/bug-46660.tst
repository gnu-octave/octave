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

%!test <*46660>
%! a(1) = class_bug46660 ();
%! a(2) = class_bug46660 ();
%! a(2).x = 123;
%! assert (a(2).x, 123);
%! assert (isempty (a(2).y));
%!
%! # writing into a(2).y may not alter a(2).x
%! a(2).y = 321;
%! assert (a(2).x, 123);
%! assert (a(2).y, 321);
