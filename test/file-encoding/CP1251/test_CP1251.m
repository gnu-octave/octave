########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of the
## License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {} test_CP1251 ()
## Test function with some characters from CP1251
##
## ÄÖÜäöü ŠŽšž
##
## @end deftypefn

function test_CP1251 ()
  help ("test_CP1251");
endfunction

%!assert (double ("ÄÖÜäöü ŠŽšž"),
%!        [208 148 208 166 208 172 208 180 209 134 209 140 32 ...
%!         208 137 208 139 209 153 209 155])
