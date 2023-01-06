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

%!test <*58593>
%! obj = myclass1 ();
%! assert (obj.data (plus (minus (end,1), 1)), 1005);

%!test <*58593>
%! obj = myclass2 ();
%! assert (obj.alldata, 1001:1005);

%!test <*58593>
%! obj = myclass2 ();
%! assert (obj(end), 1004);

%!test <*58593>
%! obj = myclass2 ();
%! assert (obj.data(end), 1005);

%!test <*58593>
%! obj = myclass2 ();
%! obj.alldata = 1:5;
%! assert (obj.data, 1:5);

%!test <*58593>
%! obj = myclass2 ();
%! obj(end) = -1;
%! assert (obj.data, [1001:1003, -1, 1005]);

%!test <*58593>
%! obj = myclass2 ();
%! obj.data(end) = -1;
%! assert (obj.data, [1001:1004, -1]);

