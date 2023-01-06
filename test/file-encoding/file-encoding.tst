########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
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

## Note: The cache of dir_encoding from .oct-config files in the load path
## persists even after removing the folder from the load path.
## Explictily, delete it when removing the path.

## test file in current directory

%!assert (dir_encoding ("."), "windows-1252")
%!assert (dir_encoding (pwd ()), "windows-1252")

%!test
%! help_str = get_help_text ("test_CP1252");
%! ## The help text contains the string "ÄÖÜäöü ŠŽšž".  Compare to the
%! ## corresponding UTF-8 byte sequence to make sure this test is independent
%! ## of how this .tst file is interpreted.
%! ref_str = char ([195 132 195 150 195 156 195 164 195 182 195 188 32 ...
%!                  197 160 197 189 197 161 197 190]);
%! assert (strfind (help_str, ref_str));


## test file in load path (relative)

%!test
%! path_orig = path ();
%! unwind_protect
%!   addpath ("CP1251");
%!   assert (dir_encoding ("CP1251"), "windows-1251");
%!   assert (dir_encoding (fullfile (pwd (), "CP1251")), "windows-1251");
%! unwind_protect_cleanup
%!   path (path_orig);
%!   dir_encoding (canonicalize_file_name ("CP1251"), "delete");
%! end_unwind_protect

%!test
%! path_orig = path ();
%! unwind_protect
%!   addpath ("CP1251");
%!   help_str = get_help_text ("test_CP1251");
%!   ## The help text contains the string "ДЦЬдць ЉЋљћ".  Compare to the
%!   ## corresponding UTF-8 byte sequence to make sure this test is independent
%!   ## of how this .tst of how this .tst file is interpreted.
%!   ref_str = char ([208 148 208 166 208 172 208 180 209 134 209 140 32 ...
%!                    208 137 208 139 209 153 209 155]);
%!   assert (strfind (help_str, ref_str));
%! unwind_protect_cleanup
%!   path (path_orig);
%!   dir_encoding (canonicalize_file_name ("CP1251"), "delete");
%! end_unwind_protect


## test file in load path (absolute)

%!test
%! clear all;  # make sure files are re-parsed
%! path_orig = path ();
%! unwind_protect
%!   new_path = canonicalize_file_name ("CP1251");
%!   addpath (new_path);
%!   assert (dir_encoding (new_path), "windows-1251");
%!   assert (dir_encoding ("CP1251"), "windows-1251");
%!   assert (dir_encoding (fullfile (pwd (), "CP1251")), "windows-1251");
%! unwind_protect_cleanup
%!   path (path_orig);
%!   dir_encoding (canonicalize_file_name ("CP1251"), "delete");
%! end_unwind_protect

%!test
%! clear all;  # make sure files are re-parsed
%! path_orig = path ();
%! unwind_protect
%!   addpath (canonicalize_file_name ("CP1251"));
%!   help_str = get_help_text ("test_CP1251");
%!   ## The help text contains the string "ДЦЬдць ЉЋљћ".  Compare to the UTF-8
%!   ## byte sequence to make sure this test is independent of how this .tst
%!   ## file is interpreted.
%!   ref_str = char ([208 148 208 166 208 172 208 180 209 134 209 140 32 ...
%!                    208 137 208 139 209 153 209 155]);
%!   assert (strfind (help_str, ref_str));
%! unwind_protect_cleanup
%!   path (path_orig);
%!   dir_encoding (canonicalize_file_name ("CP1251"), "delete");
%! end_unwind_protect


## oruntests with file in current folder with .oct-config file
%!test <*62780>
%! ## wrap in "evalc" to suppress output to the log
%! evalc ('oruntests (".");');

## oruntests with file in different folder (not in load path) with
## "dir_encoding"
%!test <*62780>
%! unwind_protect
%!   dir_encoding (canonicalize_file_name ("CP1251"), "windows-1251");
%!   ## use "evalc" to suppress output to the log
%!   evalc ('oruntests ("CP1251");');
%! unwind_protect_cleanup
%!   dir_encoding (canonicalize_file_name ("CP1251"), "delete");
%! end_unwind_protect

## oruntests with file in different folder (not in load path) with
## "__mfile_encoding__"
%!test <*62780>
%! old_mfile_encoding = __mfile_encoding__ ("windows-1251");
%! unwind_protect
%!   ## use "evalc" to suppress output to the log
%!   evalc ('oruntests ("CP1251");');
%! unwind_protect_cleanup
%!   __mfile_encoding__ (old_mfile_encoding);
%! end_unwind_protect

## oruntests with file in different folder with .oct-config file (in load path)
%!test <*62780>
%! path_orig = path ();
%! unwind_protect
%!   addpath (canonicalize_file_name ("CP1251"));
%!   ## use "evalc" to suppress output to the log
%!   evalc ('oruntests ("CP1251");');
%! unwind_protect_cleanup
%!   path (path_orig);
%!   dir_encoding (canonicalize_file_name ("CP1251"), "delete");
%! end_unwind_protect
