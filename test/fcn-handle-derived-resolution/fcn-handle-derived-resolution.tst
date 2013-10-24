## Copyright (C) 2012-2013 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%%  Test script for legacy OOP.
%%  Requires the path to contain the directory ctor-vs-method.
%%
%%  Note: This script and all classes are also intended to run
%%        in Matlab to test compatibility.  Don't break that!

%!shared
%! clear -classes

%!test
%! p = parent (7);
%! assert (numel (p), 7)

%!test
%! d = derived (13);
%! assert (numel (d), 13)

%!test
%! p = parent (11);
%! f = @numel;
%! assert (f (p), 11)

%!test
%! d = parent (21);
%! f = @numel;
%! assert (f (d), 21)

%!test
%! o(1) = other (13);
%! o(2) = other (42);
%! assert (getsize_loop (o), [13, 42])

%!test
%! o(1) = other (13);
%! o(2) = other (42);
%! assert (getsize_cellfun (o), [13, 42])

%!test
%! o(1) = other (13);
%! o(2) = other (42);
%! assert (getsize_arrayfun (o), [13, 42])
