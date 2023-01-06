########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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

################################################################################
## This file actually executes the tests on nested functions.
##
## It relies on the function files defined in the nest/ directory.
################################################################################

%!test
%! assert (recursive_nest (), 25)
%! clear -global recursive_nest_inc;  # cleanup after test

%!assert (recursive_nest2 (), 20)

%!assert (recursive_nest3 (), 5)

## FIXME: The following test works in Matlab R2020b, but in Octave it never
##        worked.  The output of "script_nest" is unassigned. This got
##        revealed by fixing bug #58686.

%!test <*58691> assert (script_nest (), 5)

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

%!assert (nest_eval ("x = 5;", "x = 6;"), 6)

%!error <can not add variable "y" to a static workspace>
%! nest_eval ("x = 5;", "y = 6;");

%!error <can not add variable "y" to a static workspace>
%! nest_eval ("x = -5; x = abs (x);", "y = 6;")

%!test
%! f = no_closure (0);
%! assert (f("foo"), "nested foo");
%! assert (f("foo"), "nested foo");

%!test <*39257>
%! f = no_closure (1);
%! assert (f(), "nested");
%! assert (f("foo"), "nested foo");

%!error <D' undefined near line 7> scope2
%!error <can not add variable "y" to a static workspace> nest_eval ("y = 5;", "")
%!error <can not add variable "y" to a static workspace> nest_eval ("y;", "")

## Test the way that non-local variables referenced by nested functions
## work with function handles.

## FH1 and FH2 were created separately so will have distinct
## closure contexts.handles, FH3 is a copy of FH2 so they will
## share the same context.

%!test <*39257>
%! fh1 = nst1 (13);
%! fh2 = nst1 (13);
%! fh3 = fh2;
%!
%! assert (fh1 (), 13);
%! assert (fh2 (), 13);
%! assert (fh3 (), 13);
%!
%! assert (fh1 (42), 42);
%! assert (fh2 (), 13);
%! assert (fh3 (), 13);
%!
%! assert (fh2 (pi), pi);
%! assert (fh1 (), 42);
%! assert (fh3 (), pi);

## Similar to the test above, but with persistent variables.  These are
## stored in the function, not the closure context, so are shared among
## all handles whether they are created separately or copied.

%!test
%! fh1 = nst2 (13);
%! fh2 = nst2 (13);
%! fh3 = fh2;
%!
%! assert (fh1 (), 13);
%! assert (fh2 (), 13);
%! assert (fh3 (), 13);
%!
%! assert (fh1 (42), 42);
%! assert (fh2 (), 42);
%! assert (fh3 (), 42);
%!
%! assert (fh2 (pi), pi);
%! assert (fh1 (), pi);
%! assert (fh3 (), pi);

## And again with global variables.

%!test
%! fh1 = nst3 (13);
%! fh2 = nst3 (13);
%! fh3 = fh2;
%!
%! assert (fh1 (), 13);
%! assert (fh2 (), 13);
%! assert (fh3 (), 13);
%!
%! assert (fh1 (42), 42);
%! assert (fh2 (), 42);
%! assert (fh3 (), 42);
%!
%! assert (fh2 (pi), pi);
%! assert (fh1 (), pi);
%! assert (fh3 (), pi);
%!
%! clear -global g;  # cleanup after tests

## Test case from <https://stackoverflow.com/q/26238491/6579744>
%!test
%! f1 = counter ();
%! f2 = counter ();
%! observed = [f1(), f1(), f2(), f1(), f2()];
%! assert (observed, [1, 2, 1, 3, 2]);

## Test visibility of nested function from script called from parent.
%!assert (script_nest_2 (42), 84)
%!error script_nest_2 (0)

%!assert (bug_59989 (), 6)
