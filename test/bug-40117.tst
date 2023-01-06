########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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

%!function __mktestfun_40117__ (file, varargin)
%!  unwind_protect
%!    fid = fopen (file, "w");
%!    fprintf (fid, "%s\n", varargin{:});
%!  unwind_protect_cleanup
%!    if (fid > 0)
%!      fclose (fid);
%!    endif
%!  end_unwind_protect
%!endfunction

%!test <*40117>
%! unwind_protect
%!   tmp_dir = tempname ();
%!   mkdir (tmp_dir);
%!   a_dir = fullfile (tmp_dir, "a");
%!   a_private_dir = fullfile (a_dir, "private");
%!   mkdir (a_dir);
%!   mkdir (a_private_dir);
%!   __mktestfun_40117__ (fullfile (a_dir, "main_40117.m"),
%!                        "function r = main_40117 ()",
%!                        "  r = p1_40117 ();",
%!                        "endfunction");
%!   __mktestfun_40117__ (fullfile (a_private_dir, "p1_40117.m"),
%!                        "function r = p1_40117 ()",
%!                        "  r = p2_40117 ();",
%!                        "endfunction");
%!   __mktestfun_40117__ (fullfile (a_private_dir, "p2_40117.m"),
%!                        "function r = p2_40117 ()",
%!                        "  r = 'a_p2_40117';",
%!                        "endfunction");
%!   addpath (a_dir);
%!   assert (main_40117 (), "a_p2_40117");
%!
%!   ## Update the secondary private function, attempting to avoid
%!   ## filesystem timestamp resolution problems.
%!   pause (1);
%!   __mktestfun_40117__ (fullfile (a_private_dir, "p2_40117.m"),
%!                        "function r = p2_40117 ()",
%!                        "  r = 'new function!';",
%!                        "endfunction");
%!
%!   ## Force new functions to be found.
%!   rehash ();
%!
%!   assert (main_40117 (), "new function!");
%! unwind_protect_cleanup
%!   rmpath (a_dir);
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir (tmp_dir, "s");
%! end_unwind_protect
