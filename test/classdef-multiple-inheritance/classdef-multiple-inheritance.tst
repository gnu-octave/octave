########################################################################
##
## Copyright (C) 2017-2026 The Octave Project Developers
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

%%  Test script for classdef OOP.
%%  Requires the path to contain the test classes.
%%
%%  Note: This script and all classes are also intended to run
%%        in MATLAB to test compatibility.  Don't break that!
%%
%%  The classes used for the tests reside in the
%%  test/classdef_multiple_inheritance with others in the test directory.
%%
%%  The classes provide the ability to test most of the major features
%%  of the classdef OOP facilities.  There are a number of classes, mostly
%%  kind of the same, that create a hierarchy.

%%  Basic classdef tests for multiple inheritance (bug #49973)
%!test
%! obj1 = child_from_base_1 ();
%! obj2 = child_from_base_1_and_2 ();
%! assert (class (obj1), 'child_from_base_1');
%! assert (class (obj2), "child_from_base_1_and_2");

# Diamond inheritance tests (bug #68227)

# class_bug68227_1A
# class_bug68227_1B < class_bug68227_1A
# class_bug68227_1C < class_bug68227_1A
# class_bug68227_1D < class_bug68227_1B < class_bug68227_1C
#
#          class_bug68227_*A
#                 ^
#                 |
#          -----------------
#          |               |
#          |               |
#   class_bug68227_*B  class_bug68227_*C
#          ^               ^
#          |               |
#          -----------------
#                 |
#          class_bug68227_*D
%!test
%! obj = class_bug68227_1D ();
%! assert (obj.testfunc (), 'success');

%!xtest
%! obj = class_bug68227_2D ();
%! assert (obj.testprop, 'success');

# class_bug68227_1A
# class_bug68227_1B < class_bug68227_1A
# class_bug68227_1C < class_bug68227_1A
# class_bug68227_1D < class_bug68227_1B < class_bug68227_1C
#
#          class_bug68227_*A
#                 ^
#                 |
#          -----------------
#          |               |
#          |               |
#   class_bug68227_*B  class_bug68227_*C
#          ^               ^
#          |               |
#          -----------------
#                 |
#          class_bug68227_*D
%!error <method testfunc conflicting>
%! obj = class_bug68227_3D ();


# testclass_1A
# testclass_1B
# testclass_1C < testclass_1A & testclass_1B
#
#   testclass_1A  testclass_1B
#          ^               ^
#          |               |
#          -----------------
#                 |
#              testclass_1C
%!error <method testfunc conflicting>
%! obj = testclass_1C ();

# testclass_2A
# testclass_2B
# testclass_2C < testclass_2A & testclass_2B
#
#   testclass_2A  testclass_2B
#          ^               ^
#          |               |
#          -----------------
#                 |
#              testclass_2C
#
# The class should be refused, as testclass_1C is above; Octave does not yet
# check its properties
%!xtest
%! fail ("testclass_2C ()", "property testprop conflicting");

# Cyclic inheritance hierarchies (error in all cases)



