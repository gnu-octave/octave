## Copyright (C) 2013-2016 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%!test
%! unwind_protect
%!   addpath dir1;
%!   [d1_r, d1_f1, d1_f2, d1_f3] = func1 (0);
%!   addpath dir2;
%!   [d2_r, d2_f1, d2_f2, d2_f3] = func1 (0);
%!   assert (d1_r, 0);
%!   assert (d2_r, 1);
%!   assert (d1_f1, "dir1/func1");
%!   assert (d1_f2, "dir1/func2");
%!   assert (d1_f3, "dir1/func3");
%!   assert (d2_f1, "dir2/func1");
%!   assert (d2_f2, "dir2/func2");
%!   assert (d2_f3, "dir2/func3");
%! unwind_protect_cleanup
%!   rmpath dir1;
%!   rmpath dir2;
%! end_unwind_protect
