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

## Diamond inheritance tests (bug #68227)

## class_bug68227_1A
## class_bug68227_1B < class_bug68227_1A
## class_bug68227_1C < class_bug68227_1A
## class_bug68227_1D < class_bug68227_1B < class_bug68227_1C
#
##          class_bug68227_*A
##                 ^
##                 |
##          -----------------
##          |               |
##          |               |
##   class_bug68227_*B  class_bug68227_*C
##          ^               ^
##          |               |
##          -----------------
##                 |
##          class_bug68227_*D
%!test
%! obj = class_bug68227_1D ();
%! assert (obj.testfunc (), 'success');

%!test <*68227>
%! obj = class_bug68227_2D ();
%! assert (obj.testprop, 'success');

## class_bug68227_1A
## class_bug68227_1B < class_bug68227_1A
## class_bug68227_1C < class_bug68227_1A
## class_bug68227_1D < class_bug68227_1B < class_bug68227_1C
#
##          class_bug68227_*A
##                 ^
##                 |
##          -----------------
##          |               |
##          |               |
##   class_bug68227_*B  class_bug68227_*C
##          ^               ^
##          |               |
##          -----------------
##                 |
##          class_bug68227_*D
## class_bug68227_3B overrides the testfunc that class_bug68227_3C inherits
## from class_bug68227_3A, so its testfunc is the one class_bug68227_3D gets.
%!test
%! mc = ?class_bug68227_3D;
%! meth = mc.MethodList(strcmp ({mc.MethodList.Name}, 'testfunc'));
%! cls = meth.DefiningClass;
%! assert (cls.Name, 'class_bug68227_3B');


## testclass_1A
## testclass_1B
## testclass_1C < testclass_1A & testclass_1B
#
##   testclass_1A  testclass_1B
##          ^               ^
##          |               |
##          -----------------
##                 |
##              testclass_1C
%!error <method testfunc: conflicting definitions>
%! obj = testclass_1C ();

## testclass_2A
## testclass_2B
## testclass_2C < testclass_2A & testclass_2B
#
##   testclass_2A  testclass_2B
##          ^               ^
##          |               |
##          -----------------
##                 |
##              testclass_2C
#
## The class is rejected when it is defined, as testclass_1C is above.
%!error <property testprop: conflicting definitions in classes 'testclass_2A' and 'testclass_2B'>
%! obj = testclass_2C ();

%!error <property testprop: conflicting definitions in classes 'testclass_2A' and 'testclass_2B'>
%! mc = meta.class.fromName ('testclass_2C');

## testclass_2D < testclass_2A & testclass_2B, declaring testprop
#
## A subclass cannot resolve the conflict by declaring the property again.
%!error <duplicate property 'testprop' in class 'testclass_2D'>
%! obj = testclass_2D ();

## testclass_3A, abstract property testprop
## testclass_3B, property testprop
## testclass_3C < testclass_3A & testclass_3B
## testclass_3D < testclass_3B & testclass_3A
## testclass_3E, abstract property testprop
## testclass_3F < testclass_3A & testclass_3E, declaring testprop
#
## A concrete property implements the abstract one of the same name, whichever
## superclass comes first.
%!test
%! obj = testclass_3C ();
%! assert (obj.testprop, 'success');

%!test
%! obj = testclass_3D ();
%! assert (obj.testprop, 'success');

%!test
%! mc = ?testclass_3C;
%! assert (mc.Abstract, false);
%! assert (numel (mc.PropertyList), 1);
%! cls = mc.PropertyList.DefiningClass;
%! assert (cls.Name, 'testclass_3B');

%!test
%! obj = testclass_3F ();
%! assert (obj.testprop, 'success');

## testclass_4A, private property testprop
## testclass_4B, protected property testprop
## testclass_4C < testclass_4A & testclass_3B
## testclass_4D < testclass_4B & testclass_3B
## testclass_4E < testclass_3A & testclass_4A
## testclass_4F < testclass_4A & testclass_3A
## testclass_4G < testclass_4A & testclass_2A & testclass_2B
#
## Matlab accepts testclass_4C and keeps both properties; Octave cannot.
%!error <Octave cannot inherit properties of the same name from superclasses 'testclass_4A' and 'testclass_3B' when one is private>
%! obj = testclass_4C ();

%!error <property testprop: conflicting definitions in classes 'testclass_4B' and 'testclass_3B'>
%! obj = testclass_4D ();

## A private property does not implement an abstract one, whichever superclass
## comes first.  Octave cannot keep both.
%!error <Octave cannot inherit properties of the same name from superclasses 'testclass_3A' and 'testclass_4A' when one is private>
%! mc = ?testclass_4E;

%!error <Octave cannot inherit properties of the same name from superclasses 'testclass_4A' and 'testclass_3A' when one is private>
%! mc = ?testclass_4F;

## The conflict is between the properties that are not private.
%!error <property testprop: conflicting definitions in classes 'testclass_2A' and 'testclass_2B'>
%! obj = testclass_4G ();

