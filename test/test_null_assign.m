## Copyright (C) 2008 Jaroslav Hajek
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%!test
%! a = 1:3; a(:) = []; assert (size (a), [0, 0])
%!test
%! a = 1:3; a(1:3) = []; assert (size (a), [1, 0])
%!test
%! a = (1:3).'; a(1:3) = []; assert (size (a), [0, 1])
%!test
%! a = ones (3); a(:,:) = []; assert (size (a), [0, 3])
%!test
%! a = ones (3); a(1:3,:) = []; assert (size (a), [0, 3])
%!test
%! a = ones (3); a(:,1:3) = []; assert (size (a), [3, 0])
%!test
%! a = ones (3); fail ("a(1:2,1:2) = []", ".*");
%!test
%! a = ones (3); fail ("a(1:3,1:3) = []", ".*");

