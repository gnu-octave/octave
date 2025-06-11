########################################################################
##
## Copyright (C) 2024-2025 The Octave Project Developers
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

%!warning <shadows a built-in function>
%! path_orig = path ();
%! unwind_protect
%!   addpath (fullfile (pwd (), "shadowed-builtin"));
%! unwind_protect_cleanup
%!   path (path_orig);
%! end_unwind_protect

%!warning <shadows a core library function>
%! path_orig = path ();
%! orig_sys_path = __pathorig__ ();
%! unwind_protect
%!   ## Temporarily override sys_path so this test works even before Octave
%!   ## has been installed and __pathorig__ is empty by default.
%!    __pathorig__ (path_orig);
%!   addpath (fullfile (pwd (), "shadowed-corelib"));
%! unwind_protect_cleanup
%!   path (path_orig);
%!   __pathorig__ (orig_sys_path);
%! end_unwind_protect

## Test that no shadowing warning is emitted if a function with the same name
## as a builtin function is added inside a (different) +namespace.
%!test <*46849>
%! path_orig = path ();
%! lastwarn ("");
%! unwind_protect
%!   addpath (fullfile (pwd (), "namespace-builtin"));
%!   assert (lastwarn (), "");
%! unwind_protect_cleanup
%!   path (path_orig);
%! end_unwind_protect

## Test that no shadowing warning is emitted if a function with the same name
## as a function from the core library is added inside a (different) +namespace.
%!test <*46849>
%! path_orig = path ();
%! lastwarn ("");
%! unwind_protect
%!   addpath (fullfile (pwd (), "namespace-corelib"));
%!   assert (lastwarn (), "");
%! unwind_protect_cleanup
%!   path (path_orig);
%! end_unwind_protect
