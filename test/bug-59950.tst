########################################################################
##
## Copyright (C) 2021-2023 The Octave Project Developers
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

## The following tests are for undocumented Matlab behavior.

%!shared x
%! x = 13;
%!assert <*59950>  (exist (''), 0)
%!assert <*59950>  (exist ([]), 0)
%!assert <*59950>  (exist ({}), 0)
%!assert <*59950>  (exist (1:0), 0)
%!assert <*59950>  (exist ('', 'var'), 0)
%!assert <*59950>  (exist ([], 'builtin'), 0)
%!assert <*59950>  (exist ({}, 'dir'), 0)
%!assert <*59950>  (exist (1:0, 'file'), 0)
%!error exist (containers.Map ())
%!error exist (containers.Map ())
%!assert <*59950>  (exist ('x', ''), 0)
%!assert <*59950>  (exist ('x', []), 0)
%!assert <*59950>  (exist ('x', {}), 0)
%!assert <*59950>  (exist ('x', struct ([])), 0)
%!error exist ('x', containers.Map ())
%!assert <*59950> (exist (containers.Map (), ''), 0)
%!assert <*59950> (exist (containers.Map (), []), 0)
%!assert <*59950> (exist (containers.Map (), {}), 0)
%!assert <*59950> (exist (containers.Map (), struct ([])), 0)
%!error exist (containers.Map (), containers.Map ())
