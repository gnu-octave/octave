## Copyright (C) 2006-2015 John W. Eaton
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

################################################################################
## This file actually executes the tests on nested functions.
##
## It relies on the function files defined in the nest/ directory.
################################################################################

%!assert (recursive_nest (), 25)

%!assert (recursive_nest2 (), 20)

%!assert (recursive_nest3 (), 5)

%!assert (script_nest (), 5)

%!assert (arg_ret (), 10)

%!assert (arg_nest, 1)

%!assert (varg_nest (-1), 6)

%!assert (varg_nest2, 5)

%!test
%! scope0;

%!test
%! scope1 (1);

%!test
%! scope3;

%!assert (nest_eval ("x = 5;", "x = 6;"), 6);
%!assert (nest_eval ("x = 5;", "y = 6;"), 5);
%!assert (nest_eval ("x = -5; x = abs (x);", "y = 6;"), 5);

%!error <D' undefined near line 7> scope2
%!error <handles to nested functions are not yet supported> no_closure (0)
%!error <handles to nested functions are not yet supported> no_closure (1)
%!error <can not add variable "y" to a static workspace> nest_eval ("y = 5;", "")
%!error <can not add variable "y" to a static workspace> nest_eval ("y;", "")
