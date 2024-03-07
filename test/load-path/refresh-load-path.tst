########################################################################
##
## Copyright (C) 2024 The Octave Project Developers
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

%!test <*65406>
%! path_orig = path ();
%! pwd_orig = pwd ();
%! unwind_protect
%!   addpath (fullfile (pwd_orig, "in-load-path"));
%!   assert (load_path_fcn (), "in-load-path");
%!
%!   cd ("not-in-load-path");
%!   assert (load_path_fcn (), "not-in-load-path");
%! unwind_protect_cleanup
%!   path (path_orig);
%!   cd (pwd_orig);
%! end_unwind_protect
