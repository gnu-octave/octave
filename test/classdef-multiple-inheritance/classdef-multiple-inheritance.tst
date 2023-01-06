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
%! assert (class (obj1), 'child_from_base_1')
%! assert (class (obj2), "child_from_base_1_and_2")
