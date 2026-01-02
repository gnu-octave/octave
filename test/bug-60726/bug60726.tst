########################################################################
##
## Copyright (C) 2025-2026 The Octave Project Developers
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

%%
%% Testing nargout when calling subsref for classdef classes
%%

%!test <60726>
%! x = class_bug60726;
%! x{1};
%! assert (ans, 1);

%!test <60726>
%! x = class_bug60726;
%! x.a;
%! assert (ans, 0);

%!test <60726>
%! x = class_bug60726;
%! x(1).a;
%! assert (ans, 1);

%!test <60726>
%! x = class_bug60726;
%! x{1}.a;
%! assert (ans, 1);

%%
%% Testing nargout when calling subsref for struct-based classes
%%

%!test <60726>
%! x = class2_bug60726;
%! x{1};
%! assert (ans, 1);

%!test <60726>
%! x = class2_bug60726;
%! x.a;
%! assert (ans, 0);

%!test <60726>
%! x = class2_bug60726;
%! x(1).a;
%! assert (ans, 1);

%!test <60726>
%! x = class2_bug60726;
%! x{1}.a;
%! assert (ans, 1);

