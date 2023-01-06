########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

## test for publish

%!test
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (! __have_feature__ ("QT_OFFSCREEN")
%!       || ! strcmp (graphics_toolkit (), "qt"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!
%!   scripts = dir ("test_script*.m");
%!   tmpDir = tempname ();
%!   mkdir (tmpDir);
%!   opts.outputDir = tmpDir;
%!   for fname = {scripts.name}
%!     publish (fname{1}, opts);
%!   endfor
%!   confirm_recursive_rmdir (false, "local");
%!   sts = rmdir (tmpDir, "s");
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## test for grabcode

%!test
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (! __have_feature__ ("QT_OFFSCREEN")
%!       || ! strcmp (graphics_toolkit (), "qt"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!   ## Create temporary directory
%!   tmpDir = tempname ();
%!   mkdir (tmpDir);
%!   opts.outputDir = tmpDir;
%!   ## Call publish and grabcode
%!   publish ("test_script.m", opts);
%!   str1 = fileread ("test_script.m");
%!   str2 = grabcode (fullfile (tmpDir, "test_script.html"));
%!   confirm_recursive_rmdir (false, "local");
%!   sts = rmdir (tmpDir, "s");
%!   ## Canonicalize strings
%!   str1 = strjoin (deblank (strsplit (str1, "\n")), "\n");
%!   str2 = strjoin (deblank (strsplit (str2, "\n")), "\n");
%!   assert (hash ("md5", str1), hash ("md5", str2));
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect
