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

%!function r = f1 ()
%!  ls = svd (1);
%!  r = eval ("ls -1;");
%!endfunction
%!function r = f2 ()
%!  [u,ls,v] = svd (1);
%!  r = eval ("ls -1;");
%!endfunction
%!function r = f3 (ls)
%!  r = eval ("ls -1;");
%!endfunction

%!test
%! ## Windows systems can't run "ls -1"
%! if (! ispc ())
%!   assert (f1 (), 0);
%!   assert (f2 (), 0);
%!   assert (ischar (f3 ()), true);
%!   assert (f3 (1), 0);
%! endif
