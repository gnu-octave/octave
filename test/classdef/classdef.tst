########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
%%  To Do:  This script tests to ensure that things done correctly work
%%          correctly.  It should also check that things done incorrectly
%%          error properly.
%%
%%  The classes used for the tests reside in the test/classdef with others
%%  in the test directory.
%%
%%  The classes provide the ability to test most of the major features
%%  of the classdef OOP facilities.  There are a number of classes, mostly
%%  kind of the same, that create a hierarchy.

%%  Basic classdef tests for value class
%!shared p, q, i, amt
%! q = foo_value_class ();
%! p = foo_value_class (4, 4*12, 50e3);
%! i = p.rate / (12 * 100);
%! amt = (p.principle * i) / (1 - (1 + i)^(-p.term));
%!assert (isempty (q.rate))
%!assert (isempty (q.principle))
%!assert (isempty (q.term))
%!assert (strcmp (class (p), 'foo_value_class'))
%!assert (p.term == 48)
%!assert (p.rate == 4.0)
%!assert (p.principle == 50e3)
%!assert (p.amount, amt, eps ())
%!assert (amount (p), amt, eps ())
%!test <55961>
%! assert (properties (p), {'rate'; 'term'; 'principle'});
%!test <*55858>
%! assert (methods (p), {'amount'; 'foo_value_class'});
%!assert (isempty (foo_value_class().rate))
%!error <property 'rate' is not constant> foo_value_class.rate
%!shared  # clear all shared variables for remainder of tests

%%  Static method and Constant Property
%!assert (foo_static_method_constant_property.radians_per_cycle == 2*pi)
%!assert (foo_static_method_constant_property().radians_per_cycle == 2*pi)
%!assert (foo_static_method_constant_property().pie == pi)
%!error <property 'frequency' is not constant> foo_static_method_constant_property.frequency
%!error <method 'cosine' is not static> foo_static_method_constant_property.cosine
%!test
%! obj = foo_static_method_constant_property;
%! obj.frequency = 10;
%! assert (obj.cosine (0.1), cos (2 * pi * 10 * 0.1), eps ());
%! assert (obj.sine (0.1), sin (2 * pi * 10 * 0.1), eps ());

%!test
%! obj = foo_method_changes_property_size (3);
%! obj = obj.move_element_to_end (2);
%! assert (isequal (obj.element, [1 3 2]));

%!error <parse error> plist_t1
%!assert (strcmp (class (plist_t2), 'plist_t2'))
%!assert (strcmp (class (plist_t3), 'plist_t3'))

%!test
%! obj = struct_wrapper ();
%! obj{'a'} = 1;
%! assert (obj{'a'} == 1);
%! obj{'bc'} = 2;
%! assert (obj{'bc'} == 2);
%! assert (isequal (obj{'a', 'bc'}, [1 2]));

%% Test for meta.class.fromName
%!test <*51935>
%! meta.class.fromName ("inputParser");

%% Do not change this to "containers.Map()".  This test is intended to
%% ensure that calling a function in a +package directory will work
%% properly.
%!test <*51715>
%! x = containers.Map;
%! assert (isobject (x));

%!assert <*52096> (isempty (meta.class.fromName ("__nonexi$tent_cl@$$__")))
%!assert <*52096> (isempty (meta.package.fromName ("__nonexi$tent_p@ck@ge__")))

%% Test overloaded subsref and subsasgn functions.
%% (bug #54783, bug #54966, and bug #55223)
%!test <*54783>
%! obj = foo_subsref_subsasgn (1);
%! obj(2) = 3;
%! assert (obj(2) == 3)
%! assert (obj{2} == 3)
%! assert (isequal (obj.x, [1 3 3 4]))
%! obj{2} = 4;
%! assert (obj(2) == 4)
%! assert (obj{2} == 4)
%! assert (isequal (obj.x, [1 4 3 4]))
%! obj(end) = 6;
%! assert (obj(end) == 6)
%! assert (obj{end} == 6)
%! assert (isequal (obj.x, [1 4 3 6]))
%! obj{end} = 8;
%! assert (obj(end) == 8)
%! assert (obj{end} == 8)
%! assert (isequal (obj.x, [1 4 3 8]))
%! obj.x = 1:4;
%! assert (isequal (obj.x, 1:4))
%! obj(1:3) = 7:9;
%! assert (isequal (obj(1:3), 7:9))
%! assert (isequal (obj.x, [7 8 9 4]))
%! obj(2:end) = 5:7;
%! assert (isequal (obj(2:end), 5:7))
%! assert (isequal (obj.x, [7 5 6 7]))

%!test <54966>
%! obj = foo_subsref_subsasgn (1);
%! obj{1:3} = 5:7;
%! assert (isequal ([obj{1:3}], 5:7))
%! assert (isequal (obj.x, [5 6 7 4]))
%! obj{2:end} = 7:9;
%! assert (isequal ([obj{2:end}], 7:9))
%! assert (isequal (obj.x, [5 7 8 9]))

%!test <*54783>
%! obj = foo_subsref_subsasgn (1);
%! obj.x(2) = 3;
%! assert (obj.x(2) == 3)
%! assert (obj.x{2} == 3)
%! assert (isequal (obj.x, [1 3 3 4]))
%! obj.x{2} = 4;
%! assert (obj.x(2) == 4)
%! assert (obj.x{2} == 4)
%! assert (isequal (obj.x, [1 4 3 4]))
%! obj.x(end) = 6;
%! assert (obj.x(end) == 6)
%! assert (obj.x{end} == 6)
%! assert (isequal (obj.x, [1 4 3 6]))
%! obj.x{end} = 8;
%! assert (obj.x(end) == 8)
%! assert (obj.x{end} == 8)
%! assert (isequal (obj.x, [1 4 3 8]))
%! obj.x = 1:4;
%! assert (isequal (obj.x, 1:4))
%! obj.x(1:3) = 7:9;
%! assert (isequal (obj.x(1:3), 7:9))
%! assert (isequal (obj.x, [7 8 9 4]))
%! obj.x(2:end) = 5:7;
%! assert (isequal (obj.x(2:end), 5:7))
%! assert (isequal (obj.x, [7 5 6 7]))

%!test <54966>
%! obj = foo_subsref_subsasgn (1);
%! obj.x{1:3} = 5:7;
%! assert (isequal ([obj.x{1:3}], 5:7))
%! assert (isequal (obj.x, [5 6 7 4]))
%! obj.x{2:end} = 7:9;
%! assert (isequal ([obj.x{2:end}], 7:9))
%! assert (isequal (obj.x, [5 7 8 9]))

%!test <*55223>
%! obj = foo_subsref_subsasgn (2);
%! obj{2}(2) = 3;
%! assert (obj{2}(2) == 3);
%! obj{2}{2} = 4;
%! assert (obj{2}{2} == 4);

%!test <54966>
%! obj = foo_subsref_subsasgn (2);
%! obj{1:2}(1:2) = ones (2);
%! assert (isequal (obj{1:2}(1:2), ones (2)));
%! obj{3:4}(3:4) = 4 * ones (2);
%! assert (isequal (obj{3:4}(3:4), 4 * ones (2)));

%!test <*52614>
%! A = class_bug52614A ();
%! assert (A.a, 1);
%! B = class_bug52614B ();
%! assert (B.a, 1);
%! assert (B.b, 2);

%!test <*55766>
%! x = class_bug55766 ();
%! props = {"notahiddentestprop"; "publictestprop"; "testprop"};
%! assert (properties (x), props);

%!test <*60763>
%! x = bug_60763 ();
%! assert (x.foobar (), {"some_property"});
%! assert (x.methods (), 42);

## test class with methods in @folder and in classdef definition
%!assert <*62802> (numel (methods ("class_bug62802")), 4)
