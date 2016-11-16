## Copyright (C) 2016 Kai T. Ohlhus
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

## publish

%!test
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   set (0, "defaultfigurevisible", "off");
%!   if (ispc ())
%!     graphics_toolkit ("gnuplot");
%!   endif
%!   cases = dir ("test_script*.m");
%!   cases = strsplit (strrep ([cases.name], ".m", ".m\n"));
%!   for i = 1:length(cases)-1
%!     publish (cases{i});
%!   endfor
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir ("html", "s");
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## grabcode

%!test
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   set (0, "defaultfigurevisible", "off");
%!   if (ispc ())
%!     graphics_toolkit ("gnuplot");
%!   endif
%!   publish ("test_script.m");
%!   str1 = fileread ("test_script.m");
%!   str2 = grabcode ("html/test_script.html");
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir ("html", "s");
%!   # Canonicalize strings
%!   str1 = strjoin (deblank (strsplit (str1, "\n")), "\n");
%!   str2 = strjoin (deblank (strsplit (str2, "\n")), "\n");
%!   assert (hash ("md5", str1), hash ("md5", str2));
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

