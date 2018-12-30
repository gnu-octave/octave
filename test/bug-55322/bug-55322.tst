## Copyright (C) 2018 Rik Wehbring
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

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hg = hggroup ();
%!   hl = line (hax, [0, 1], [1, 1], "parent", hax, "parent", hg);
%!   assert (get (hax, "children"), hg);
%!   assert (get (hg, "children"), hl);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   hg = hggroup ();
%!   hl = line ([0, 1], [1, 1], "tag", "parent", "color", "r");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
