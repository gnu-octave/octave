########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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

%!test <*35881>
%! global _tstvar_;  # Final test does "clear all" which removes this var.
%!
%! _tstvar_ = struct ("init1",-1, "a1",-1, "init2",-1, "a2",-1);
%! bug35881 (0);
%! assert (_tstvar_, struct ("init1",true, "a1",1, "init2",false, "a2",1));
%! clear -f bug35881
%!
%! _tstvar_ = struct ("init1",-1, "a1",-1, "init2",-1, "a2",-1);
%! bug35881 (1);
%! assert (_tstvar_, struct ("init1",true, "a1",1, "init2",false, "a2",1));
%!
%! _tstvar_ = struct ("init1",-1, "a1",-1, "init2",-1, "a2",-1);
%! fail ("bug35881 (2)", "'a' undefined near line");
