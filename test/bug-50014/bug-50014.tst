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

%!error <duplicate subfunction or nested function name>
%! duplicate_nested_function ()

%!assert <*50014> (duplicate_nested_in_subfunction_ok (), 3)

%!error <duplicate subfunction or nested function name>
%! duplicate_nested_parent_function ()

%!error <duplicate subfunction or nested function name>
%! duplicate_parent_nested2 ()

%!error <duplicate subfunction or nested function name>
%! duplicate_parent_nested_function ()

%!error <duplicate subfunction or nested function name>
%! duplicate_primary_nested_function ()

%!error <duplicate subfunction or nested function name>
%! duplicate_primary_subfunction_old_syntax ()

%!error <duplicate subfunction or nested function name>
%! duplicate_primary_subfunction ()

%!error <duplicate subfunction or nested function name>
%! duplicate_subfunction ()

%!error <duplicate subfunction or nested function name>
%! duplicate_subfunction_old_syntax ()

%!assert <*50014> (duplicate_subfunction_separate_scope_ok (), 3)
