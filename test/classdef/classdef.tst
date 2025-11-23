########################################################################
##
## Copyright (C) 2013-2025 The Octave Project Developers
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

%!error <syntax error> plist_t1
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
%! props = {"testprop"; "publictestprop"; "notahiddentestprop"};
%! assert (properties (x), props);

%!test <*60763>
%! x = bug_60763 ();
%! assert (x.foobar (), {"some_property"});
%! assert (x.methods (), 42);

## test class with methods in @folder and in classdef definition
%!assert <*62802> (numel (methods ("class_bug62802")), 4)

## Test class with duplicate definitions for methods (A) and properties (B)
## This is bug #66930.
%!error <duplicate method>   A = class_bug66930A ([1 2 3], 3);
%!error <duplicate property> B = class_bug66930B ([1 2 3], 3);

## break closure cycles for classdef arrays
%!test <*67749>
%! fcn = cdef_array_break_cycles ();
%! result = fcn ([]);
%! assert (numel (result), 2);
%! assert (class (result), "foo_value_class");

## duplicate definitions of methods in parent classes (bug #50011)
%!error <method .* conflicting>
%! cls_50011 = class_bug50011_1 ();
%! cls_50011.m_c ();

## duplicate definitions of properties in parent classes (bug #50011)
%!error <property .* conflicting>
%! cls_50011 = class_bug50011_2 ();
%! cls_50011.m_c ();

## reshape array of value class objects
%!test <*65179>
%! obj(1,1) = value_class ();
%! obj(2,3) = value_class ();
%! assert (size (obj), [2, 3]);
%! assert (size (reshape (obj, [3, 2])), [3, 2]);

## reshape array of handle class objects
%!test <*65179>
%! obj1(1,1) = handle_class ();
%! obj1(2,3) = handle_class ();
%! assert (size (obj1), [2, 3]);
%! obj2 = reshape (obj1, [3, 2]);
%! assert (size (obj2), [3, 2]);
%! obj1(2,3).a = 42;
%! assert (obj2(3,2).a, 42);

## reshape array of value class objects with overloaded reshape function
%!test <*65179>
%! obj1(1,1) = value_class_reshape ();
%! obj1(2,3) = value_class_reshape ();
%! assert (size (obj1), [2, 3]);
%! obj2 = reshape (obj1, [3, 2]);
%! assert (size (obj2), [2, 3]);
%! assert (obj2(end).a, [3, 2]);

## reshape on scalar objects only works for all-one dimensions
%!test <*65179>
%! assert (size (reshape (value_class (), [1, 1])), [1, 1]);

%!error <cannot reshape scalar .* to .* array>
%! reshape (value_class (), [3, 2]);

## properties take precedence over methods with the same name
%!test <*67362>
%! obj = class_bug67362 ();
%! assert (obj.shared_name, 42);

## concatenation of value classes without conversion
%!test <*44665>
%! p = class_pair (3, 5);
%! p2 = class_pair (7, 4);
%! y = [p, [p2, p]];
%! assert (size (y), [1, 3]);
%! assert ([y.first; y.second], [3, 7, 3; 5, 4, 5]);
%! z = [y; p2, [p2, p]];
%! assert (size (z), [2, 3]);
%! assert ([z.first; z.second], [3, 7, 7, 7, 3, 3; 5, 4, 4, 4, 5, 5]);
%! v = [z, z; y, p, [p2, p]];
%! assert (size (v), [3, 6]);

## concatenation of value classes using functions
%!test <*44665>
%! p = class_pair (3, 5);
%! p2 = class_pair (7, 4);
%! assert( size (horzcat (p, p2, p)), [1, 3]);
%! assert( size (vertcat (p, p2, p)), [3, 1]);
%! assert( size (cat (4, p, p2)), [1, 1, 1, 2]);

## concatenation with handle class
%!test <*44665>
%! h = handle_class ();
%! h.a = 1;
%! h2 = handle_class ();
%! h2.a = 2;
%! arr = [h, h2, h];
%! assert (size (arr), [1, 3]);
%! assert ([arr(:).a], [1, 2, 1]);
%! h(1).a = 3;
%! assert ([arr(:).a], [3, 2, 3]);

## error message when concatenating incompatible classdefs, no constructors
%!error <cat: cannot convert from type "handle_class" to type "value_class">
%! v = value_class ();
%! h = handle_class ();
%! [ v, h ];

## previous test, but try type converting value class to handle class
%!error <cat: cannot convert from type "value_class" to type "handle_class">
%! v = value_class ();
%! h = handle_class ();
%! [ h, v ];

## error message when concatenating classdef and builtin-type, no constructor
%!error <cat: cannot convert from type "double" to type "value_class">
%! v = value_class ();
%! [ v, 3 ];

## previous test, but try type converting value class to handle class
%!error <cat: cannot convert from type "double" to type "handle_class">
%! h = handle_class ();
%! [ h, 3 ];

## concatenation of empty object with value class
%!test <*44665>
%! v = value_class ();
%! arr = [ [] v [] ];
%! arr = [ [] arr [] ];
%! assert (size(arr), [1 1]);

## previous test, but with handle class
%!test <*44665>
%! h = handle_class ();
%! arr = [ [] h [] ];
%! arr = [ [] arr [] ];
%! assert (size(arr), [1 1]);

## error message when concatenating incompatible classdefs, failed constructor
## FIXME: May change this error in the future to indicate that the type
## conversion was tried explicitly during a concatenation. Right now, this
## error is thrown in the function "attempt_type_conversion" in data.cc, which
## may not necessarily be called by only "cat".
%!error <foo_value_class constructor failed for class_pair_elem argument>
%! f = foo_value_class (4, 12, 20000);
%! p = class_pair_elem (5);
%! [ f, p ];

## classdef concatenation of value classes using conversion method of
## non-dominant class.
## Since neither class in this example has a defined dominance relation, the
## dominant class is the left-most class in the concatenation.
%!test <*44665>
%! y = [class_pair_elem(7), class_pair(3, 5)];
%! assert (class (y), 'class_pair_elem');
%! assert (size (y), [1, 2]);
%! assert ([y.value], [7, 3]);

## previous test, but with handle classes
%!test <*44665>
%! y = [class_pair_elem_handle(7), class_pair_handle(3, 5)];
%! assert (class (y), 'class_pair_elem_handle');
%! assert (size (y), [1, 2]);
%! assert ([y.value], [7, 3]);

## classdef concatenation using constructor of dominant class to convert other
## classdefs
%!test <*44665>
%! y = [class_pair(3, 5), class_pair_elem(7)];
%! assert (class (y), 'class_pair');
%! assert ([y.first; y.second], [3, 7; 5, 0]);

## pprevious test, but with handle classes
%!test <*44665>
%! y = [class_pair_handle(3, 5), class_pair_elem_handle(7)];
%! assert (class (y), 'class_pair_handle');
%! assert ([y.first; y.second], [3, 7; 5, 0]);

## classdef concatenation of value classes using constructor of dominant class
## to convert built-in types
%!test <*44665>
%! y = class_pair_elem (1);
%! y = [y, double(2)];
%! y = [y, single(3)];
%! y = [y, int8(4)];
%! y = [y, uint8(5)];
%! y = [y, int16(6)];
%! y = [y, uint16(7)];
%! y = [y, int32(8)];
%! y = [y, uint32(9)];
%! y = [y, 'a'];
%! y = [y, true];
%! ## Have to declare func handle separately to check equality
%! f = @(x) x + 1;
%! y = [y, f];
%! y = [y, struct("foo", 1)];
%! y = [y, {1, 2, 3}];
%! assert (class (y), 'class_pair_elem');
%! assert (size (y), [1, 14]);
%! assert (y(2).value, double (2));
%! assert (y(3).value, single (3));
%! assert (y(4).value, int8 (4));
%! assert (y(5).value, uint8 (5));
%! assert (y(6).value, int16 (6));
%! assert (y(7).value, uint16 (7));
%! assert (y(8).value, int32 (8));
%! assert (y(9).value, uint32 (9));
%! assert (y(10).value, 'a');
%! assert (y(11).value, true);
%! assert (y(12).value, f);
%! assert (y(13).value, struct ("foo", 1));
%! assert (y(14).value, {1, 2, 3});

## previous test, but with handle classes
%!test <*44665>
%! y = class_pair_elem_handle (1);
%! y = [y, double(2)];
%! y = [y, single(3)];
%! y = [y, int8(4)];
%! y = [y, uint8(5)];
%! y = [y, int16(6)];
%! y = [y, uint16(7)];
%! y = [y, int32(8)];
%! y = [y, uint32(9)];
%! y = [y, 'a'];
%! y = [y, true];
%! ## Have to declare func handle separately to check equality
%! f = @(x) x + 1;
%! y = [y, f];
%! y = [y, struct("foo", 1)];
%! y = [y, {1, 2, 3}];
%! assert (class (y), 'class_pair_elem_handle');
%! assert (size (y), [1, 14]);
%! assert (y(2).value, double (2));
%! assert (y(3).value, single (3));
%! assert (y(4).value, int8 (4));
%! assert (y(5).value, uint8 (5));
%! assert (y(6).value, int16 (6));
%! assert (y(7).value, uint16 (7));
%! assert (y(8).value, int32 (8));
%! assert (y(9).value, uint32 (9));
%! assert (y(10).value, 'a');
%! assert (y(11).value, true);
%! assert (y(12).value, f);
%! assert (y(13).value, struct ("foo", 1));
%! assert (y(14).value, {1, 2, 3});

## concatenation of value classdef (dominant arg) and old-style classes
%!test <*44665>
%! p = class_pair_elem (1);
%! o = osc (2);
%! arr = [ p, o ];
%! assert (class (arr), 'class_pair_elem');
%! assert (size (arr), [1 2]);

## concatenation of value classdef (dominant arg) and old-style classes
%!test <*44665>
%! p = class_pair_elem_handle (1);
%! o = osc (2);
%! arr = [ p, o ];
%! assert (class (arr), 'class_pair_elem_handle');
%! assert (size (arr), [1 2]);

## concatenation of old-style class (dominant arg) and value classdef
%!test <*44665>
%! p = class_pair_elem (1);
%! o = osc (2);
%! arr = [ o, p ];
%! assert (class (arr), 'osc');
%! assert (size (arr), [1 2]);

## concatenation of old-style class (dominant arg) and handle classdef
%!test <*44665>
%! p = class_pair_elem_handle (1);
%! o = osc (2);
%! arr = [ o, p ];
%! assert (class (arr), 'osc');
%! assert (size (arr), [1 2]);

## classdef array concatenation of value classes has value semantics
%!test <*44665>
%! p1 = class_pair_elem (1);
%! p2 = class_pair_elem (2);
%! arr = [p1, p2];
%! p1.value = 3;
%! ## Array elements should be independent copies (value semantics)
%! assert (arr(1).value, 1);
%! assert (arr(2).value, 2);
%! assert (p1.value, 3);

## classdef array concatenation of handle classes has handle semantics
%!test <*44665>
%! p1 = class_pair_elem_handle (1);
%! p2 = class_pair_elem_handle (2);
%! arr = [p1, p2];
%! p1.value = 3;
%! ## Array elements should be pointers to the same underlying object
%! assert (arr(1).value, 3);
%! assert (arr(2).value, 2);
%! assert (p1.value, 3);

## concatenation edge cases - single object operations
%!test <*44665>
%! p1 = class_pair (1, 2);
%! ## Concatenating single object should preserve dimensions
%! result = [p1];
%! assert (size (result), [1, 1]);
%! assert (result.first, 1);
%! ## Test assignment with a 1x1 object -- should work the same as a scalar
%! result.first = 2;
%! assert (result.first, 2);
%! ## Using cat with single object
%! result2 = cat (1, p1);
%! assert (size (result2), [1, 1]);
%! assert (result2.first, 1);

## previous test, but with handle classes
%!test <*44665>
%! h = class_pair_handle (1, 2);
%! ## Concatenating single object should preserve dimensions
%! result = [h];
%! assert (size (result), [1, 1]);
%! assert (result.first, 1);
%! ## Test assignment with a 1x1 object -- should work the same as a scalar
%! result.first = 2;
%! assert (result.first, 2);
%! ## Using cat with single object
%! result2 = cat (1, h);
%! assert (size (result2), [1, 1]);
%! assert (result2.first, 2);

## Concatenation with overloaded "cat" method, value class
%!test <*44665>
%! o1 = overloaded_cat_class ();
%! o1.data = 1;
%! o2 = overloaded_cat_class ();
%! o2.data = 2;
%! arr = horzcat(o1, o2);
%! # See "overloaded_cat_class.m" for specifics how cat is implemented
%! assert (size (arr), [1, 1]);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, [1, 2]);
%! arr = vertcat (arr, arr);
%! assert (size (arr), [1, 1]);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, [1, 2; 1, 2]);
%! arr = cat (3, arr, arr);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, cat (3, [1, 2; 1, 2], [1, 2; 1, 2]));

## previous test, but with handle classes
%!test <*44665>
%! o1 = overloaded_cat_class_handle ();
%! o1.data = 1;
%! o2 = overloaded_cat_class_handle ();
%! o2.data = 2;
%! arr = horzcat (o1, o2);
%! # See "overloaded_cat_class.m" for specifics how cat is implemented
%! assert (size (arr), [1, 1]);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, [1, 2]);
%! arr = vertcat (arr, arr);
%! assert (size (arr), [1, 1]);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, [1, 2; 1, 2]);
%! arr = cat (3, arr, arr);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, cat (3, [1, 2; 1, 2], [1, 2; 1, 2]));

## Test concatenation of classdef that overloads horzcat and vertcat, but not cat
## See "overloaded_horzcat_vertcat_class.m" for more specifics
%!test <*44665>
%! o1 = overloaded_horzcat_vertcat_class ();
%! o1.data = 1;
%! o2 = overloaded_horzcat_vertcat_class ();
%! o2.data = 2;
%! arr = horzcat(o1, o2);
%! assert (size (arr), [1, 1]);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, [1, 2]);
%! o1.data = 1;
%! o2.data = 2;
%! arr = vertcat (o1, o2);
%! assert (size (arr), [1, 1]);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, [1; 2]);
%! o1.data = 1;
%! o2.data = 2;
%! arr = [o1, o2; o1, o2];
%! assert (size (arr), [1, 1]);
%! assert (numel ({arr.data}), 1);
%! assert (arr.data, [1, 2; 1, 2]);
%! o1.data = 1;
%! o2.data = 2;
%! arr = cat (3, o1, o2);
%! assert (size (arr), [1, 1, 2]);
%! assert (numel ({arr.data}), 2);
%! assert (cat (3, arr.data), cat (3, 1, 2));

## previous test, but with handle classes
%!test <*44665>
%! o1 = overloaded_horzcat_vertcat_class_handle ();
%! o1.data = 1;
%! o2 = overloaded_horzcat_vertcat_class_handle ();
%! o2.data = 2;
%! arr_horzcat = horzcat (o1, o2);
%! assert (size (arr_horzcat), [1, 1]);
%! assert (numel ({arr_horzcat.data}), 1);
%! assert (arr_horzcat.data, [1, 2]);
%! o1.data = 1;
%! o2.data = 2;
%! arr_vertcat = vertcat (o1, o2);
%! assert (size (arr_vertcat), [1, 1]);
%! assert (numel ({arr_vertcat.data}), 1);
%! assert (arr_vertcat.data, [1; 2]);
%! o1.data = 1;
%! o2.data = 2;
%! arr_matrix = [o1, o2; o1, o2];
%! assert (size (arr_matrix), [1, 1]);
%! assert (numel ({arr_matrix.data}), 1);
%! assert (arr_matrix.data, [1, 2, 2; 1, 2, 2]);
%! o1.data = 1;
%! o2.data = 2;
%! arr = cat (3, o1, o2);
%! assert (size (arr), [1, 1, 2]);
%! assert (numel ({arr.data}), 2);
%! assert (cat (3, arr.data), cat (3, 1, 2));

# selection of dominant class when concatenating classdefs
# FIXME: This relies on the "InferiorClasses" attribute being supported.
%!test <44665>
%! p1 = class_pair_elem (1);
%! p2 = class_pair_elem_dominant (2);
%! arr = [ p1, p2 ];
%! assert (class (arr), 'class_pair_elem_dominant');

## previous test, but with handle classes
%!test <44665>
%! p1 = class_pair_elem_handle (1);
%! p2 = class_pair_elem_dominant_handle (2);
%! arr = [ p1, p2 ];
%! assert (class (arr), 'class_pair_elem_dominant_handle');

## deep copy (on write) of value class arrays
%!test <*54028>
%! arr1 = value_class ();
%! arr1.a = 1;
%! arr1(2) = arr1;
%! arr2 = arr1;
%! arr2(1).a = 2;
%! assert (arr1(1).a, 1);

## same thing with handle class arrays
%!test
%! arr1 = handle_class ();
%! arr1.a = 1;
%! arr1(2) = arr1;
%! arr2 = arr1;
%! arr2(1).a = 2;
%! assert (arr1(1).a, 2);
