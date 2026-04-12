########################################################################
##
## Copyright (C) 2013-2026 The Octave Project Developers
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
%%  FIXME: This script tests to ensure that things done correctly work
%%         correctly.  It should also check that things done incorrectly
%%         error properly.
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
%! assert (obj(2) == 3);
%! assert (obj{2} == 3);
%! assert (isequal (obj.x, [1 3 3 4]));
%! obj{2} = 4;
%! assert (obj(2) == 4);
%! assert (obj{2} == 4);
%! assert (isequal (obj.x, [1 4 3 4]));
%! obj(end) = 6;
%! assert (obj(end) == 6);
%! assert (obj{end} == 6);
%! assert (isequal (obj.x, [1 4 3 6]));
%! obj{end} = 8;
%! assert (obj(end) == 8);
%! assert (obj{end} == 8);
%! assert (isequal (obj.x, [1 4 3 8]));
%! obj.x = 1:4;
%! assert (isequal (obj.x, 1:4));
%! obj(1:3) = 7:9;
%! assert (isequal (obj(1:3), 7:9));
%! assert (isequal (obj.x, [7 8 9 4]));
%! obj(2:end) = 5:7;
%! assert (isequal (obj(2:end), 5:7));
%! assert (isequal (obj.x, [7 5 6 7]));

%!test <*54966>
%! obj = foo_subsref_subsasgn (1);
%! obj{1:3} = 5:7;
%! assert (isequal ([obj{1:3}], 5:7));
%! assert (isequal (obj.x, [5 6 7 4]));
%! obj{2:end} = 7:9;
%! assert (isequal ([obj{2:end}], 7:9));
%! assert (isequal (obj.x, [5 7 8 9]));
%! fail ("[obj{1:3}] = 5:7;", "invalid number of output arguments");

%!test <*54783>
%! obj = foo_subsref_subsasgn (1);
%! obj.x(2) = 3;
%! assert (obj.x(2) == 3);
%! assert (obj.x{2} == 3);
%! assert (isequal (obj.x, [1 3 3 4]));
%! obj.x{2} = 4;
%! assert (obj.x(2) == 4);
%! assert (obj.x{2} == 4);
%! assert (isequal (obj.x, [1 4 3 4]));
%! obj.x(end) = 6;
%! assert (obj.x(end) == 6);
%! assert (obj.x{end} == 6);
%! assert (isequal (obj.x, [1 4 3 6]));
%! obj.x{end} = 8;
%! assert (obj.x(end) == 8);
%! assert (obj.x{end} == 8);
%! assert (isequal (obj.x, [1 4 3 8]));
%! obj.x = 1:4;
%! assert (isequal (obj.x, 1:4));
%! obj.x(1:3) = 7:9;
%! assert (isequal (obj.x(1:3), 7:9));
%! assert (isequal (obj.x, [7 8 9 4]));
%! obj.x(2:end) = 5:7;
%! assert (isequal (obj.x(2:end), 5:7));
%! assert (isequal (obj.x, [7 5 6 7]));

%!test <*54966>
%! obj = foo_subsref_subsasgn (1);
%! obj.x{1:3} = 5:7;
%! assert (isequal ([obj.x{1:3}], 5:7));
%! assert (isequal (obj.x, [5 6 7 4]));
%! obj.x{2:end} = 7:9;
%! assert (isequal ([obj.x{2:end}], 7:9));
%! assert (isequal (obj.x, [5 7 8 9]));
%! fail ("[obj.x{1:3}] = 5:7;", "invalid number of output arguments");

%!test <*60723>
%! # Check that numel is not called in subsasgn, simple assignment
%! obj = class_bug60723A;
%! obj.c = 20;
%! assert (obj.c, 20);

%!test <60723>
%! # Check that numel is not called in subsasgn, multi-assignment
%! obj = class_bug60723A ();
%! [obj.c] = 20;
%! assert (obj.c, 20);

%!test <60723>
%! # Check that numel is not called in subsref
%! obj = class_bug60723B ();
%! assert ([obj{1:5}], repelem (5, 5));
%! assert ([obj(1:5).a], repelem (5, 5));

%!test <*60723>
%! obj = class_bug60723C ();
%! assert ([obj{1:end}], repelem (4, 4));
%! [a, b] = obj.x{1:end};

%!test <*55223>
%! obj = foo_subsref_subsasgn (2);
%! obj{2}(2) = 3;
%! assert (obj{2}(2) == 3);
%! obj{2}{2} = 4;
%! assert (obj{2}{2} == 4);

%!test <*54966>
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

## Multi-output constructor tests (bug #67845).

## two-output constructor, no input args
%!test <*67845>
%! [obj, val] = class_bug67845A ();
%! assert (isa (obj, "class_bug67845A"));
%! assert (val, 0);

## previous test, but with handle classes
%!test <*67845>
%! [obj, val] = class_bug67845A_handle ();
%! assert (isa (obj, "class_bug67845A_handle"));
%! assert (val, 0);
%! obj2 = obj;
%! assert (obj2.prop, 0);

## two-output constructor with input args
%!test <*67845>
%! [obj, val] = class_bug67845A (5);
%! assert (isa (obj, "class_bug67845A"));
%! assert (val, 5);

## previous test, but with handle classes
%!test <*67845>
%! [obj, val] = class_bug67845A_handle (5);
%! assert (isa (obj, "class_bug67845A_handle"));
%! assert (val, 5);
%! obj2 = obj;
%! obj.prop = 10;
%! assert (obj2.prop, 10);

## single-output call to two-output-declared constructor
%!test <*67845>
%! obj = class_bug67845A ();
%! assert (isa (obj, "class_bug67845A"));
%! assert (obj.prop, 0);

## previous test, but with handle classes
%!test <*67845>
%! obj = class_bug67845A_handle ();
%! assert (isa (obj, "class_bug67845A_handle"));
%! assert (obj.prop, 0);

## double-output with implicit superclass constructor
%!test <*67845>
%! [obj, val] = class_bug67845B ();
%! assert (isa (obj, "class_bug67845B"));
%! assert (isa (obj, "class_bug67845A"));
%! assert (obj.prop, 0);

## previous test, but with handle classes
%!test <*67845>
%! [obj, status] = class_bug67845B_handle ();
%! assert (isa (obj, "class_bug67845B_handle"));
%! assert (isa (obj, "class_bug67845B_handle"));
%! assert (obj.prop, 0);

## quad-output constructor with zero input args
%!test <*67845>
%! [obj, val1, val2, val3, val4] = class_bug67845C ();
%! assert (isa (obj, 'class_bug67845C'));
%! assert (val1, 1);
%! assert (val2, 2);
%! assert (val3, 3);
%! assert (val4, 4);

## previous test, but with handle classes
%!test <*67845>
%! [obj, val1, val2, val3, val4] = class_bug67845C_handle ();
%! assert (isa (obj, 'class_bug67845C_handle'));
%! assert (val1, 1);
%! assert (val2, 2);
%! assert (val3, 3);
%! assert (val4, 4);

## quad-output constructor with two input args
%!test <*67845>
%! [obj, val1, val2, val3, val4] = class_bug67845C ('5', {3});
%! assert (isa (obj, 'class_bug67845C'));
%! assert (val1, '5');
%! assert (val2, {3});
%! assert (val3, 3);
%! assert (val4, 4);

## previous test, but with handle classes
%!test <*67845>
%! [obj, val1, val2, val3, val4] = class_bug67845C_handle ('5', {3});
%! assert (isa (obj, 'class_bug67845C_handle'));
%! assert (val1, '5');
%! assert (val2, {3});
%! assert (val3, 3);
%! assert (val4, 4);

## quad-output constructor with four input args
%!test <*67845>
%! ## Have to declare func handle separately to check equality
%! f = @(x) x + 1;
%! [obj, val1, val2, val3, val4] = ...
%!   class_bug67845C ('5', [10, 11; 12, 13], {15}, f);
%! assert (isa (obj, 'class_bug67845C'));
%! assert (val1, '5');
%! assert (val2, [10, 11; 12, 13]);
%! assert (val3, {15});
%! assert (val4, f);

## previous test, but with handle classes
%!test <*67845>
%! ## Have to declare func handle separately to check equality
%! f = @(x) x + 1;
%! [obj, val1, val2, val3, val4] = ...
%!   class_bug67845C_handle ('5', [10, 11; 12, 13], {15}, f);
%! assert (isa (obj, 'class_bug67845C_handle'));
%! assert (val1, '5');
%! assert (val2, [10, 11; 12, 13]);
%! assert (val3, {15});
%! assert (val4, f);

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

## sizeof tests (bug #55810)

## sizeof a value classdef with no values inside properties
%!test <*55810>
%! obj = value_class ();
%! assert (sizeof (obj), 0)

## previous test, but with a classdef array
%!test <*55810>
%! obj(2, 2) = value_class ();
%! assert (sizeof (obj), 0)

## sizeof a value classdef with clearly defined value sizes inside properties
%!test <*55810>
%! obj = value_class ();
%! obj.a = uint8 (1);
%! obj.b = uint16 (1);
%! obj.c = uint32 (1);
%! obj.d = uint64 (1);
%! true_size = sum ([sizeof(obj.a), sizeof(obj.b), ...
%!   sizeof(obj.c), sizeof(obj.d)]);
%! assert (sizeof (obj), true_size);

## previous test, but with a classdef array
%!test <*55810>
%! obj(2, 2) = value_class ();
%! for i = 1:numel (obj)
%!   obj(i).a = uint8 (1);
%!   obj(i).b = uint16 (1);
%!   obj(i).c = uint32 (1);
%!   obj(i).d = uint64 (1);
%! endfor
%! true_size = sum ([sizeof([obj.a]), sizeof([obj.b]), ...
%!   sizeof([obj.c]), sizeof([obj.d])]);
%! assert (sizeof (obj), true_size);

## sizeof a value classdef that contains a classdef
%!test <*55810>
%! obj = value_class ();
%! obj_inner = value_class ();
%! obj_inner.a = uint8 (1);
%! obj_inner.b = uint16 (1);
%! obj_inner.c = uint32 (1);
%! obj_inner.d = uint64 (1);
%! obj.a = obj_inner;
%! true_size = sum ([sizeof(obj.a), sizeof(obj.b), ...
%!   sizeof(obj.c), sizeof(obj.d)]);
%! assert (sizeof (obj), true_size);

## previous test, but with a classdef array
%!test <*55810>
%! obj(2, 2) = value_class ();
%! obj_inner = value_class ();
%! obj_inner.a = uint8 (1);
%! obj_inner.b = uint16 (1);
%! obj_inner.c = uint32 (1);
%! obj_inner.d = uint64 (1);
%! for i = 1:numel (obj)
%!   obj(i).a = obj_inner;
%! endfor
%! true_size = sum ([sizeof([obj.a]), sizeof([obj.b]), ...
%!   sizeof([obj.c]), sizeof([obj.d])]);
%! assert (sizeof (obj), true_size);

## sizeof a value classdef that is a subclass of another
%!test <*55810>
%! obj = value_class_subclass ();
%! obj.a = uint8 (1);
%! obj.b = uint16 (1);
%! obj.c = uint32 (1);
%! obj.d = uint64 (1);
%! true_size = sum ([sizeof(obj.a), sizeof(obj.b), ...
%!   sizeof(obj.c), sizeof(obj.d)]);
%! assert (sizeof (obj), true_size);
%! obj.z = uint64 (1);
%! true_size = true_size + sizeof (obj.z);
%! assert (sizeof (obj), true_size);

## overloaded sizeof method
%!test <*55810>
%! obj = overloaded_sizeof_class ();
%! assert (sizeof (obj), -5);  # the retval of the overloaded method
%! obj.a = uint8 (1);
%! assert (sizeof (obj), -5);
%! assert (builtin ('sizeof', obj), 1);

## sizeof a handle class
%!test <*55810>
%! obj = handle_class ();
%! [computer, maxsize] = computer ();
%! if (maxsize == 2^(63))  # if machine is 64-bit:
%!   assert (sizeof (obj), 8);  # size is the machine word
%! endif

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

## subsasgn of classdef array with fewer ()-indices than array dimensions
%!test <*65179>
%! obj(2,3,2) = value_class ();
%! obj(12).a = 42;
%! assert (size (obj), [2, 3, 2]);
%! assert (obj(2,3,2).a, 42);
%! obj(2,4).a = 33;
%! assert (size (obj), [2, 3, 2]);
%! assert (obj(2,1,2).a, 33);

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
## conversion was tried explicitly during a concatenation.  Right now, this
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

## Test classdef permutation

## scalar permute with value class - should return scalar unchanged
%!test <*65179>
%! obj = foo_value_class (4, 48, 50000);
%! result = permute (obj, [1, 2]);  # permute ignores out-of-bounds dims on scalars
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);

## previous test, but with handle classes
%!test <*65179>
%! obj = foo_handle_class (4, 48, 50000);
%! result = permute (obj, [1, 2]);  # permute ignores out-of-bounds dims on scalars
%! assert (isequal (obj.rate, result.rate));
%! obj.rate = 5;  # stress test handle semantics
%! assert (isequal (obj.rate, result.rate));
%! assert (isequal (obj.term, result.term));
%! assert (isequal (obj.principle, result.principle));

## scalar permute with value class and extra dim - should return scalar unchanged
%!test <*65179>
%! obj = foo_value_class (3, 36, 25000);
%! result = permute (obj, [2, 1, 3]);  # permute ignores extra ndims on scalars
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);

## previous test, but with handle classes
%!test <*65179>
%! obj = foo_handle_class (3, 36, 25000);
%! result = permute (obj, [2, 1, 3]);  # permute ignores extra ndims on scalars
%! assert (isequal (obj.rate, result.rate));
%! obj.rate = 4;  # stress test handle semantics
%! assert (isequal (obj.rate, result.rate));
%! assert (isequal (obj.term, result.term));
%! assert (isequal (obj.principle, result.principle));

## array permute - 2-D transpose
%!test <*65179>
%! obj1 = foo_value_class (4, 48, 50000);
%! obj2 = foo_value_class (3, 36, 25000);
%! obj3 = foo_value_class (5, 60, 75000);
%! obj4 = foo_value_class (6, 72, 10000);
%! arr = [obj1, obj2; obj3, obj4];
%! result = permute (arr, [2, 1]);
%! assert (size (result), [2, 2]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(1,2).rate, arr(2,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(2,2).rate, arr(2,2).rate);

## previous test, but with handle classes
%!test <*65179>
%! obj1 = foo_handle_class (4, 48, 50000);
%! obj2 = foo_handle_class (3, 36, 25000);
%! obj3 = foo_handle_class (5, 60, 75000);
%! obj4 = foo_handle_class (6, 72, 10000);
%! arr = [obj1, obj2; obj3, obj4];
%! result = permute (arr, [2, 1]);
%! assert (size (result), [2, 2]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(1,2).rate, arr(2,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(2,2).rate, arr(2,2).rate);
%! arr(1,1).rate = 7;  # stress test handle semantics
%! assert (result(1,1).rate, arr(1,1).rate);

## 3-D array permute
%!test <*65179>
%! obj1 = foo_value_class (4, 48, 50000);
%! obj2 = foo_value_class (3, 36, 25000);
%! obj3 = foo_value_class (5, 60, 75000);
%! obj4 = foo_value_class (6, 72, 10000);
%! arr = cat (3, [obj1; obj2], [obj3; obj4]);  # 2x1x2 array
%! result = permute (arr, [2, 1, 3]);  # 1x2x2 array
%! assert (size (result), [1, 2, 2]);
%! assert (result(1,1,1).rate, arr(1,1,1).rate);
%! assert (result(1,2,1).rate, arr(2,1,1).rate);
%! assert (result(1,1,2).rate, arr(1,1,2).rate);
%! assert (result(1,2,2).rate, arr(2,1,2).rate);

## previous test, but with handle classes
%!test <*65179>
%! obj1 = foo_handle_class (4, 48, 50000);
%! obj2 = foo_handle_class (3, 36, 25000);
%! obj3 = foo_handle_class (5, 60, 75000);
%! obj4 = foo_handle_class (6, 72, 10000);
%! arr = cat (3, [obj1; obj2], [obj3; obj4]);  # 2x1x2 array
%! result = permute (arr, [2, 1, 3]);  # 1x2x2 array
%! assert (size (result), [1, 2, 2]);
%! assert (result(1,1,1).rate, arr(1,1,1).rate);
%! assert (result(1,2,1).rate, arr(2,1,1).rate);
%! assert (result(1,1,2).rate, arr(1,1,2).rate);
%! assert (result(1,2,2).rate, arr(2,1,2).rate);
%! arr(1,1,1).rate = 7;  # stress test handle semantics
%! assert (result(1,1,1).rate, arr(1,1,1).rate);

## test overloaded "permute" method
## see "overloaded_permute_class.m" for more specifics
%!test <*65179>
%! obj1 = overloaded_permute_class;
%! obj2 = overloaded_permute_class;
%! obj3 = overloaded_permute_class;
%! obj4 = overloaded_permute_class;
%! arr = [obj1, obj2, obj3, obj4];
%! result = permute (arr, [2, 1]);
%! assert (size (result), [1, 4]);  # unchanged
%! result(1).data = 5;
%! assert (obj1.data, []);  # stress test value semantics
%! ## Make it into a 3-D array
%! arr = cat (3, arr, arr);
%! result = permute (arr, [1, 2, 3]);
%! assert (size (result), [2, 1, 4]);  # [1, 4, 2] -> [2, 1, 4]

## previous test, but with handle classes
%!test <*65179>
%! obj1 = overloaded_permute_class_handle;
%! obj2 = overloaded_permute_class_handle;
%! obj3 = overloaded_permute_class_handle;
%! obj4 = overloaded_permute_class_handle;
%! arr = [obj1, obj2, obj3, obj4];
%! result = permute (arr, [2, 1]);
%! assert (size (result), [1, 4])  # unchanged
%! result(1).data = 5;
%! assert (obj1.data, 5);  # stress test handle semantics
%! ## Make it into a 3-D array
%! arr = cat (3, arr, arr);
%! result = permute (arr, [1, 2, 3]);
%! assert (size (result), [2, 1, 4]);  # [1, 4, 2] -> [2, 1, 4]

## Test classdef transpose and ctranspose

## scalar transpose - should return scalar unchanged
%!test
%! obj = foo_value_class (4, 48, 50000);
%! result = transpose (obj);
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 4);  # stress test value semantics

## previous test, but with handle classes
%!test
%! obj = foo_handle_class (4, 48, 50000);
%! result = transpose (obj);
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 2);  # stress test handle semantics

## scalar transpose operator - should return scalar unchanged
%!test
%! obj = foo_value_class (4, 48, 50000);
%! result = obj.';  # Test .' operator
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 4);  # stress test value semantics

## previous test, but with handle classes
%!test
%! obj = foo_handle_class (4, 48, 50000);
%! result = obj.';  # Test .' operator
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 2);  # stress test handle semantics

## scalar ctranspose - should work same as transpose for classdef
%!test
%! obj = foo_value_class (4, 48, 50000);
%! result = ctranspose (obj);
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 4);  # stress test value semantics

## previous test, but with handle classes
%!test
%! obj = foo_handle_class (4, 48, 50000);
%! result = ctranspose (obj);
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 2);  # stress test handle semantics

## scalar ctranspose operator - should work same as transpose for classdef
%!test
%! obj = foo_value_class (4, 48, 50000);
%! result = obj';  # Test ' operator
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 4);  # stress test value semantics

## previous test, but with handle classes
%!test
%! obj = foo_handle_class (4, 48, 50000);
%! result = obj';  # Test ' operator
%! assert (obj.rate, result.rate);
%! assert (obj.term, result.term);
%! assert (obj.principle, result.principle);
%! assert (size (result), [1, 1]);
%! obj.rate = 2;
%! assert (result.rate, 2);  # stress test handle semantics

## 1-D array transpose, dims [1 3] -> dims[3 1], value classes
%!test
%! obj1 = foo_value_class (4, 48, 50000);
%! obj2 = foo_value_class (3, 36, 25000);
%! obj3 = foo_value_class (5, 60, 75000);
%! arr = [obj1, obj2, obj3];  # 1x3 row vector
%! result = transpose (arr);
%! assert (size (result), [3, 1]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(3,1).rate, arr(1,3).rate);

## previous test, but with handle classes
%!test
%! obj1 = foo_handle_class (4, 48, 50000);
%! obj2 = foo_handle_class (3, 36, 25000);
%! obj3 = foo_handle_class (5, 60, 75000);
%! arr = [obj1, obj2, obj3];  # 1x3 row vector
%! result = transpose (arr);
%! assert (size (result), [3, 1]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(3,1).rate, arr(1,3).rate);
%! arr(1,1).rate = 6;  # stress test handle semantics
%! assert (result(1,1).rate, arr(1,1).rate);

## 1-D array transpose, dims [3 1] -> [1 3], value classes
%!test
%! obj1 = foo_value_class (4, 48, 50000);
%! obj2 = foo_value_class (3, 36, 25000);
%! obj3 = foo_value_class (5, 60, 75000);
%! arr = [obj1; obj2; obj3];  # 3x1 column vector
%! result = arr.';
%! assert (size (result), [1, 3]);
%! assert (result(1).rate, obj1.rate);
%! assert (result(2).rate, obj2.rate);
%! assert (result(3).rate, obj3.rate);

## previous test, but with handle classes
%!test
%! obj1 = foo_handle_class (4, 48, 50000);
%! obj2 = foo_handle_class (3, 36, 25000);
%! obj3 = foo_handle_class (5, 60, 75000);
%! arr = [obj1; obj2; obj3];  # 3x1 column vector
%! result = arr.';
%! assert (size (result), [1, 3]);
%! assert (result(1).rate, obj1.rate);
%! assert (result(2).rate, obj2.rate);
%! assert (result(3).rate, obj3.rate);
%! arr(1).rate = 6;  # stress test handle semantics
%! assert (result(1).rate, obj1.rate);

## 2-D array transpose, dims [2 2] -> [2 2]
%!test
%! obj1 = foo_value_class (4, 48, 50000);
%! obj2 = foo_value_class (3, 36, 25000);
%! obj3 = foo_value_class (5, 60, 75000);
%! obj4 = foo_value_class (2, 24, 40000);
%! arr = [obj1, obj2; obj3, obj4];  # 2x2 matrix
%! result = transpose (arr);
%! assert( size (result), [2, 2]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(1,2).rate, arr(2,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(2,2).rate, arr(2,2).rate);

## previous test, but with handle classes
%!test
%! obj1 = foo_handle_class (4, 48, 50000);
%! obj2 = foo_handle_class (3, 36, 25000);
%! obj3 = foo_handle_class (5, 60, 75000);
%! obj4 = foo_handle_class (2, 24, 40000);
%! arr = [obj1, obj2; obj3, obj4];  # 2x2 matrix
%! result = transpose (arr);
%! assert( size (result), [2, 2]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(1,2).rate, arr(2,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(2,2).rate, arr(2,2).rate);
%! arr(1,1).rate = 6;  # stress test handle semantics
%! assert (result(1,1).rate, obj1.rate);

## 2-D array transpose, dims [2 3] -> [3 2]
%!test
%! obj1 = foo_value_class (1, 12, 10000);
%! obj2 = foo_value_class (2, 24, 20000);
%! obj3 = foo_value_class (3, 36, 30000);
%! obj4 = foo_value_class (4, 48, 40000);
%! obj5 = foo_value_class (5, 60, 50000);
%! obj6 = foo_value_class (6, 72, 60000);
%! arr = [obj1, obj2, obj3; obj4, obj5, obj6];  # 2x3 matrix
%! result = arr';
%! assert (size (result), [3, 2]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(1,2).rate, arr(2,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(2,2).rate, arr(2,2).rate);
%! assert (result(3,1).rate, arr(1,3).rate);
%! assert (result(3,2).rate, arr(2,3).rate);

## previous test, but with handle classes
%!test
%! obj1 = foo_handle_class (1, 12, 10000);
%! obj2 = foo_handle_class (2, 24, 20000);
%! obj3 = foo_handle_class (3, 36, 30000);
%! obj4 = foo_handle_class (4, 48, 40000);
%! obj5 = foo_handle_class (5, 60, 50000);
%! obj6 = foo_handle_class (6, 72, 60000);
%! arr = [obj1, obj2, obj3; obj4, obj5, obj6];  # 2x3 matrix
%! result = arr';
%! assert (size (result), [3, 2]);
%! assert (result(1,1).rate, arr(1,1).rate);
%! assert (result(1,2).rate, arr(2,1).rate);
%! assert (result(2,1).rate, arr(1,2).rate);
%! assert (result(2,2).rate, arr(2,2).rate);
%! assert (result(3,1).rate, arr(1,3).rate);
%! assert (result(3,2).rate, arr(2,3).rate);
%! arr(1,1).rate = 6;  # stress test handle semantics
%! assert (result(1,1).rate, obj1.rate);

## ctranspose on arrays - should work same as transpose
%!test
%! obj1 = foo_value_class (4, 48, 50000);
%! obj2 = foo_value_class (3, 36, 25000);
%! arr = [obj1, obj2];
%! result1 = transpose (arr);
%! result2 = ctranspose (arr);
%! assert (size (result1), size (result2));
%! assert (result1(1).rate, result2(1).rate);
%! assert (result1(2).rate, result2(2).rate);

## previous test, but with handle classes
%!test
%! obj1 = foo_handle_class (4, 48, 50000);
%! obj2 = foo_handle_class (3, 36, 25000);
%! arr = [obj1, obj2];
%! result1 = transpose (arr);
%! result2 = ctranspose (arr);
%! assert (size (result1), size (result2));
%! assert (result1(1).rate, result2(1).rate);
%! assert (result1(2).rate, result2(2).rate);

## double transpose returns original
%!test
%! obj1 = foo_value_class (4, 48, 50000);
%! obj2 = foo_value_class (3, 36, 25000);
%! obj3 = foo_value_class (5, 60, 75000);
%! obj4 = foo_value_class (2, 24, 40000);
%! arr = [obj1, obj2; obj3, obj4];
%! result = transpose (transpose (arr));
%! assert (size (result), size (arr));
%! assert ([result.rate], [arr.rate])

## previous test, but with handle classes
%!test
%! obj1 = foo_handle_class (4, 48, 50000);
%! obj2 = foo_handle_class (3, 36, 25000);
%! obj3 = foo_handle_class (5, 60, 75000);
%! obj4 = foo_handle_class (2, 24, 40000);
%! arr = [obj1, obj2; obj3, obj4];
%! result = transpose (transpose (arr));
%! assert (size (result), size (arr));
%! assert ([result.rate], [arr.rate])

## test if transpose/ctranspose throws an error for classdef arrays of dim >2
##
## this test has four parts to it
## *.1) (value classes) show that direct function call throws an error
## *.2) (handle classes) show that direct function call throws an error
## *.3) (value classes) show that operator usage throws an error
## *.4) (handle classes) show that operator usage throws an error

## 1.1) (value classes) show that direct function call "transpose" throws an error
%!error <transpose not defined for N-D objects of value_class class>
%! arr(1,1,3) = value_class ();
%! result = transpose (arr);

## 1.2) (handle classes) show that direct function call "transpose" throws an error
%!error <transpose not defined for N-D objects of handle_class class>
%! arr(1,1,3) = handle_class ();
%! result = transpose (arr);

## 1.3) (value classes) show that operator "transpose" usage throws an error
%!error <transpose not defined for N-D objects of value_class class>
%! arr(1,1,3) = value_class ();
%! result = arr.';

## 1.4) (handle classes) show that operator "transpose" usage throws an error
%!error <transpose not defined for N-D objects of handle_class class>
%! arr(1,1,3) = handle_class ();
%! result = arr.';

## 2.1) (value classes) show that direct function call "ctranspose" throws an error
%!error <ctranspose not defined for N-D objects of value_class class>
%! arr(1,1,3) = value_class ();
%! result = ctranspose (arr);

## 2.2) (handle classes) show that direct function call "ctranspose" throws an error
%!error <ctranspose not defined for N-D objects of handle_class class>
%! arr(1,1,3) = handle_class ();
%! result = ctranspose (arr);

## 2.3) (value classes) show that operator "ctranspose" usage throws an error
%!error <ctranspose not defined for N-D objects of value_class class>
%! arr(1,1,3) = value_class ();
%! result = arr';

## 2.4) (handle classes) show that operator "ctranspose" usage throws an error
%!error <ctranspose not defined for N-D objects of handle_class class>
%! arr(1,1,3) = handle_class ();
%! result = arr';

## test overloaded "transpose" method for value classes
##
## see "overloaded_transpose_class.m" for more specifics
##
## NOTE: "overloaded_transpose_class" does not overload "ctranspose"
## likewise with the handle class version
##
## this test has three parts to it
## 1.1) test direct call to "transpose", value classes
%!test
%! obj1 = overloaded_transpose_class ();
%! obj2 = overloaded_transpose_class ();
%! obj3 = overloaded_transpose_class ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = transpose (arr);
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 1.2) test direct call to "transpose", handle classes
%!test
%! obj1 = overloaded_transpose_class_handle ();
%! obj2 = overloaded_transpose_class_handle ();
%! obj3 = overloaded_transpose_class_handle ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = transpose (arr);
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 2.1) test that the .' operator calls the overloaded method, value classes
%!test
%! obj1 = overloaded_transpose_class ();
%! obj2 = overloaded_transpose_class ();
%! obj3 = overloaded_transpose_class ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = arr.';
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 2.2) test that the .' operator calls the overloaded method, handle classes
%!test
%! obj1 = overloaded_transpose_class_handle ();
%! obj2 = overloaded_transpose_class_handle ();
%! obj3 = overloaded_transpose_class_handle ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = arr.';
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 3.1) test that the ' operator does NOT call the overloaded method, value classes
%!test
%! obj1 = overloaded_transpose_class ();
%! obj2 = overloaded_transpose_class ();
%! obj3 = overloaded_transpose_class ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = arr';
%! assert (size (result), [3, 1]);  # regular ctranspose
%! assert (result(1,1).data, [1, 2, 3]);
%! assert (result(2,1).data, [4, 5, 6]);
%! assert (result(3,1).data, [7, 8, 9]);

## 3.2) test that the ' operator does NOT call the overloaded method, handle classes
%!test
%! obj1 = overloaded_transpose_class_handle ();
%! obj2 = overloaded_transpose_class_handle ();
%! obj3 = overloaded_transpose_class_handle ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = arr';
%! assert (size (result), [3, 1]);  # regular ctranspose
%! assert (result(1,1).data, [1, 2, 3]);
%! assert (result(2,1).data, [4, 5, 6]);
%! assert (result(3,1).data, [7, 8, 9]);

## test overloaded "ctranspose" method for value classes
##
## see "overloaded_ctranspose_class.m" for more specifics
##
## NOTE: "overloaded_ctranspose_class" does not overload "transpose"
## likewise with the handle class version
##
## this test has three parts to it
## 1.1) test direct call to "ctranspose", value classes
%!test
%! obj1 = overloaded_ctranspose_class ();
%! obj2 = overloaded_ctranspose_class ();
%! obj3 = overloaded_ctranspose_class ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = ctranspose (arr);
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 1.2) test direct call to "ctranspose", handle classes
%!test
%! obj1 = overloaded_ctranspose_class_handle ();
%! obj2 = overloaded_ctranspose_class_handle ();
%! obj3 = overloaded_ctranspose_class_handle ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = ctranspose (arr);
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 2.1) test that the .' operator calls the overloaded method, value classes
%!test
%! obj1 = overloaded_ctranspose_class ();
%! obj2 = overloaded_ctranspose_class ();
%! obj3 = overloaded_ctranspose_class ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = arr';
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 2.2) test that the .' operator calls the overloaded method, handle classes
%!test
%! obj1 = overloaded_ctranspose_class_handle ();
%! obj2 = overloaded_ctranspose_class_handle ();
%! obj3 = overloaded_ctranspose_class_handle ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = ctranspose (arr);
%! assert (size (result), [1, 3]);  # unchanged
%! assert (result(1,1).data, [1; 2; 3]);
%! assert (result(1,2).data, [4; 5; 6]);
%! assert (result(1,3).data, [7; 8; 9]);

## 3.1) test that the .' operator does NOT call the overloaded method, value classes
%!test
%! obj1 = overloaded_ctranspose_class ();
%! obj2 = overloaded_ctranspose_class ();
%! obj3 = overloaded_ctranspose_class ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = arr.';
%! assert (size (result), [3, 1]);  # regular transpose
%! assert (result(1,1).data, [1, 2, 3]);
%! assert (result(2,1).data, [4, 5, 6]);
%! assert (result(3,1).data, [7, 8, 9]);

## 3.2) test that the .' operator does NOT call the overloaded method, handle classes
%!test
%! obj1 = overloaded_ctranspose_class_handle ();
%! obj2 = overloaded_ctranspose_class_handle ();
%! obj3 = overloaded_ctranspose_class_handle ();
%! obj1.data = [1, 2, 3];
%! obj2.data = [4, 5, 6];
%! obj3.data = [7, 8, 9];
%! arr = [obj1, obj2, obj3];
%! result = arr.';
%! assert (size (result), [3, 1]);  # regular transpose
%! assert (result(1,1).data, [1, 2, 3]);
%! assert (result(2,1).data, [4, 5, 6]);
%! assert (result(3,1).data, [7, 8, 9]);

## make sure that default transpose doesn't call overridden permute
##
## Default transpose is essentially the same thing as "permute(A, [2, 1])".
## We want to make sure that default transpose does not call to this overrided
## permute function (which will essentially run "permute(A, [1, 2])").
%!test
%! obj1 = overloaded_permute_class ();
%! obj2 = overloaded_permute_class ();
%! obj3 = overloaded_permute_class ();
%! arr = [obj1, obj2, obj3];
%! result = arr.';
%! assert (size (result), [3, 1]);  # regular transpose

# same as previous test, but for handle classes
%!test
%! obj1 = overloaded_permute_class_handle ();
%! obj2 = overloaded_permute_class_handle ();
%! obj3 = overloaded_permute_class_handle ();
%! arr = [obj1, obj2, obj3];
%! result = arr.';
%! assert (size (result), [3, 1]);  # regular transpose

## same as last two tests, but testing "ctranspose"
%!test
%! obj1 = overloaded_permute_class ();
%! obj2 = overloaded_permute_class ();
%! obj3 = overloaded_permute_class ();
%! arr = [obj1, obj2, obj3];
%! result = arr';
%! assert (size (result), [3, 1]);  # regular ctranspose

## same as previous test, but for handle classes
%!test
%! obj1 = overloaded_permute_class_handle ();
%! obj2 = overloaded_permute_class_handle ();
%! obj3 = overloaded_permute_class_handle ();
%! arr = [obj1, obj2, obj3];
%! result = arr';
%! assert (size (result), [3, 1]);  # regular ctranspose

## Test classdef resize method

## test resize method, scalar input and scalar output
%!test
%! obj = value_class ();
%! obj.a = 1;
%! result = resize (obj, 1);
%! assert (size (result), [1, 1]);
%! assert (obj.a, 1);
%! result.a = 2;  # stress test value semantics
%! assert (obj.a, 1);

## previous test, but with handle classes
%!test
%! obj = handle_class ();
%! obj.a = 1;
%! result = resize (obj, 1);
%! assert (size (result), [1, 1]);
%! assert (obj.a, 1);
%! result.a = 2;  # stress test handle semantics
%! assert (obj.a, 2);

## test resize method, scalar input with constructor that requires arguments
##
## Rationale: resizing an input array to be larger requires the classdef to
## implement a default (no-argument) constructor.  We want to ensure that the
## default constructor is not required when the output array is smaller or
## equal in size to the input array.
##
%!test
%! obj = class_pair_elem (1);
%! result = resize (obj, 1);
%! assert (size (result), [1, 1]);
%! assert (obj.value, 1);
%! result.value = 2;  # stress test value semantics
%! assert (obj.value, 1);

## previous test, but with handle classes
%!test
%! obj = class_pair_elem_handle (1);
%! result = resize (obj, 1);
%! assert (size (result), [1, 1]);
%! assert (obj.value, 1);
%! result.value = 2;  # stress test handle semantics
%! assert (obj.value, 2);

## test resize method to make empty 0x0 arrays
%!test
%! obj = value_class ();
%! result = resize (obj, 0);
%! assert (size (result), [0, 0]);
%! assert (class (result), 'value_class');

## previous test, but with handle classes
%!test
%! obj = handle_class ();
%! result = resize (obj, 0);
%! assert (size (result), [0, 0]);
%! assert (class (result), 'handle_class');

## test resize method to make empty 0x1 arrays
%!test
%! obj = value_class ();
%! result = resize (obj, 0, 1);
%! assert (size (result), [0, 1]);
%! assert (class (result), 'value_class');

## previous test, but with handle classes
%!test
%! obj = handle_class ();
%! result = resize (obj, 0, 1);
%! assert (size (result), [0, 1]);
%! assert (class (result), 'handle_class');

## test resize method to make empty 1x0 arrays
%!test
%! obj = value_class ();
%! result = resize (obj, 1, 0);
%! assert (size (result), [1, 0]);
%! assert (class (result), 'value_class');

## previous test, but with handle classes
%!test
%! obj = handle_class ();
%! result = resize (obj, 1, 0);
%! assert (size (result), [1, 0]);
%! assert (class (result), 'handle_class');

## test resize method, scalar input and array 1x3 output, default constructor
%!test
%! obj = value_class ();
%! obj.a = 1;
%! result = resize (obj, 1, 3);
%! assert (size (result), [1, 3]);
%! assert (result(1).a, 1);
%! assert (result(2).a, []);
%! assert (result(3).a, []);

## previous test, but with handle classes
%!test
%! obj = handle_class ();
%! obj.a = 1;
%! result = resize (obj, 1, 3);
%! assert (size (result), [1, 3]);
%! assert (result(1).a, 1);
%! assert (result(2).a, []);
%! assert (result(3).a, []);
%! result(2).a = 2;  # each entry should be a unique handle object
%! assert (result(1).a, 1);
%! assert (result(2).a, 2);
%! assert (result(3).a, []);

## test resize method, scalar input and array 3x1 output, default constructor
%!test
%! obj = value_class ();
%! obj.a = 1;
%! result = resize (obj, 3, 1);
%! assert (size (result), [3, 1]);
%! assert (result(1).a, 1);
%! assert (result(2).a, []);
%! assert (result(3).a, []);

## previous test, but with handle classes
%!test
%! obj = handle_class ();
%! obj.a = 1;
%! result = resize (obj, 3, 1);
%! assert (size (result), [3, 1]);
%! assert (result(1).a, 1);
%! assert (result(2).a, []);
%! assert (result(3).a, []);
%! result(2).a = 2;  # each entry should be a unique handle object
%! assert (result(1).a, 1);
%! assert (result(2).a, 2);
%! assert (result(3).a, []);

## test resize method, scalar input and array 3x3 output, default constructor
%!test
%! obj = value_class ();
%! obj.a = 1;
%! result = resize (obj, 3, 3);
%! assert (size (result), [3, 3]);
%! assert (result(1,1).a, 1);
%! assert (result(2,2).a, []);
%! assert (result(3,3).a, []);

## previous test, but with handle classes
%!test
%! obj = handle_class ();
%! obj.a = 1;
%! result = resize (obj, 3, 3);
%! assert (size (result), [3, 3]);
%! assert (result(1,1).a, 1);
%! assert (result(2,2).a, []);
%! assert (result(3,3).a, []);
%! result(2,2).a = 2;  # each entry should be a unique handle object
%! assert (result(1,1).a, 1);
%! assert (result(2,2).a, 2);
%! assert (result(3,3).a, []);

## test resize method, array 2x2 input and array 4x4 output, default constructor
%!test
%! arr = [ value_class(), value_class() ; value_class(), value_class() ];
%! arr(1,1).a = 1;
%! arr(1,2).a = 2;
%! arr(2,1).a = 3;
%! arr(2,2).a = 4;
%! result = resize (arr, 4, 4);
%! assert (size (result), [4, 4]);
%! assert (result(1,1).a, 1);
%! assert (result(1,2).a, 2);
%! assert (result(2,1).a, 3);
%! assert (result(2,2).a, 4);
%! assert (result(3,3).a, []);
%! assert (result(4,4).a, []);

## previous test, but with handle classes
%!test
%! arr = [ handle_class(), handle_class() ; handle_class(), handle_class() ];
%! arr(1,1).a = 1;
%! arr(1,2).a = 2;
%! arr(2,1).a = 3;
%! arr(2,2).a = 4;
%! result = resize (arr, 4, 4);
%! assert (size (result), [4, 4]);
%! assert (result(1,1).a, 1);
%! assert (result(1,2).a, 2);
%! assert (result(2,1).a, 3);
%! assert (result(2,2).a, 4);
%! assert (result(3,3).a, []);
%! assert (result(4,4).a, []);
%! result(3,3).a = 10;
%! assert (result(1,1).a, 1);
%! assert (result(1,2).a, 2);
%! assert (result(2,1).a, 3);
%! assert (result(2,2).a, 4);
%! assert (result(3,3).a, 10);
%! assert (result(4,4).a, []);

## Additional test resize of classdef objects
## Using built-in classdef (inputParser()).

%!test
%! ## Test resize of scalar classdef object to array
%! obj = inputParser ();  # use classdef with implementation in core Octave
%! obj.KeepUnmatched = true;
%! arr = resize (obj, [2, 3]);
%! assert (size (arr), [2, 3]);
%! assert (arr(1,1).KeepUnmatched, true);
%! ## New elements should be default-constructed
%! assert (arr(1,2).KeepUnmatched, false);

%!test
%! ## Resize to empty
%! obj = inputParser ();
%! arr = resize (obj, [0, 5]);
%! assert (size (arr), [0, 5]);
%! assert (numel (arr), 0);

%!test
%! ## Resize scalar to scalar
%! obj = inputParser ();
%! obj.CaseSensitive = true;
%! obj2 = resize (obj, [1, 1]);
%! assert (size (obj2), [1, 1]);
%! assert (obj2.CaseSensitive, true);

%!test
%! ## Resize to 3-D
%! obj = inputParser ();
%! arr = resize (obj, [2, 2, 2]);
%! assert (size (arr), [2, 2, 2]);
%! assert (numel (arr), 8);

## test resize method, scalar input and array 1x3 output, no default constructor
## FIXME: provide a proper error message when the default construction of a
## classdef object fails.  Right now, the error message is something like
## <'value' undefined near line 7, column 20>, but that is the actual failure
## point in the .m file code and not the general gist of what went wrong.
%!error
%! obj = class_pair_elem (1);
%! result = resize (obj, 1, 3);

## test resize method, array 1x2 input and array 1x3 output, no default constructor
%!error
%! obj = [ class_pair_elem(1); class_pair_elem(2) ];
%! result = resize (obj, 1, 3);

## arr(i) = [] deletion syntax tests (bug #55983).

## test arr(i) = [] deletion syntax on single index positions
%!test <*55983>
%! arr(5) = value_class ();
%! arr(1).a = 1;
%! arr(2).a = 2;
%! arr(3).a = 3;
%! arr(4).a = 4;
%! arr(5).a = 5;
%! arr(4) = [];
%! assert (size (arr), [1, 4]);
%! assert ([arr.a], [1, 2, 3, 5]);
%! arr(1:2) = [];
%! assert (size (arr), [1, 2]);
%! assert ([arr.a], [3, 5]);

## previous test, but with handle classes
%!test <*55983>
%! arr(5) = handle_class ();
%! arr(1).a = 1;
%! arr(2).a = 2;
%! arr(3).a = 3;
%! arr(4).a = 4;
%! arr(5).a = 5;
%! arr(4) = [];
%! assert (size (arr), [1, 4]);
%! assert ([arr.a], [1, 2, 3, 5]);
%! arr(1:2) = [];
%! assert (size (arr), [1, 2]);
%! assert ([arr.a], [3, 5]);

## test arr(i) = [] deletion syntax on multiple index positions
%!test <*55983>
%! arr(5,5) = value_class ();
%! for i = 1:numel (arr)
%!   arr(i).a = i;
%! endfor
%! arr(:,4) = [];
%! assert (size (arr), [5, 4]);
%! assert ([arr(:,1).a], [1, 2, 3, 4, 5]);
%! assert ([arr(:,2).a], [6, 7, 8, 9, 10]);
%! assert ([arr(:,3).a], [11, 12, 13, 14, 15]);
%! assert ([arr(:,4).a], [21, 22, 23, 24, 25]);
%! arr(3:4,:) = [];
%! assert (size (arr), [3, 4]);
%! assert ([arr(:,1).a], [1, 2, 5]);
%! assert ([arr(:,2).a], [6, 7, 10]);
%! assert ([arr(:,3).a], [11, 12, 15]);
%! assert ([arr(:,4).a], [21, 22, 25]);

## previous test, but with handle classes
%!test <*55983>
%! arr(5,5) = handle_class ();
%! for i = 1:numel (arr)
%!   arr(i).a = i;
%! endfor
%! arr(:,4) = [];
%! assert (size (arr), [5, 4]);
%! assert ([arr(:,1).a], [1, 2, 3, 4, 5]);
%! assert ([arr(:,2).a], [6, 7, 8, 9, 10]);
%! assert ([arr(:,3).a], [11, 12, 13, 14, 15]);
%! assert ([arr(:,4).a], [21, 22, 23, 24, 25]);
%! arr(3:4,:) = [];
%! assert (size (arr), [3, 4]);
%! assert ([arr(:,1).a], [1, 2, 5]);
%! assert ([arr(:,2).a], [6, 7, 10]);
%! assert ([arr(:,3).a], [11, 12, 15]);
%! assert ([arr(:,4).a], [21, 22, 25]);

## test arr(i) = [] fails if more than one non-colon index is supplied
%!error <a null assignment can only have one non-colon index>
%! arr(2,2) = value_class ();
%! arr(2,2) = [];

## previous test, but for handle classes
%!error <a null assignment can only have one non-colon index>
%! arr(2,2) = value_class ();
%! arr(2,2) = [];

## test that array deletion syntax on entire array returns a 0x0 object,
## value classes
%!test <*55983>
%! arr(2) = value_class ();
%! arr(:) = [];
%! assert (size (arr), [0, 0]);
%! assert (class (arr), 'value_class');

## previous test, but with handle classes
%!test <*55983>
%! arr(2) = handle_class ();
%! arr(:) = [];
%! assert (size (arr), [0, 0]);
%! assert (class (arr), 'handle_class');

## test that array deletion syntax on row vector returns a 1x0 object,
## value classes
%!test <*55983>
%! arr(2) = value_class ();
%! arr(2) = [];
%! arr(1) = [];
%! assert (size (arr), [1, 0]);
%! assert (class (arr), 'value_class');

## previous test, but with handle classes
%!test <*55983>
%! arr(2) = handle_class ();
%! arr(2) = [];
%! arr(1) = [];
%! assert (size (arr), [1, 0]);
%! assert (class (arr), 'handle_class');

## test that array deletion syntax on column vector returns a 0x1 object,
## value classes
%!test <*55983>
%! arr(2,1) = value_class ();
%! arr(2,:) = [];
%! arr(1,:) = [];
%! assert (size (arr), [0, 1]);
%! assert (class (arr), 'value_class');

## previous test, but with handle classes
%!test <*55983>
%! arr(2,1) = handle_class ();
%! arr(2,:) = [];
%! arr(1,:) = [];
%! assert (size (arr), [0, 1]);
%! assert (class (arr), 'handle_class');

## "all" and "any" tests

## error on calling "all" on a classdef with no overloaded "all" method
%!error <wrong type argument 'object'>
%! obj = value_class ();
%! all (obj);

## previous test, but with handle classes
%!error <wrong type argument 'object'>
%! obj = handle_class ();
%! all (obj);

## error on calling "all" on a classdef ARRAY with no overloaded "all" method
%!error <wrong type argument 'object'>
%! obj(1, 2) = value_class ();
%! all (obj);

## previous test, but with handle classes
%!error <wrong type argument 'object'>
%! obj(1, 2) = handle_class ();
%! all (obj);

## call "all" method when present inside a classdef
%!test
%! obj = overloaded_all_class ();
%! assert (all (obj), -1);

## previous test, but with handle classes
%!test
%! obj = overloaded_all_class_handle ();
%! assert (all (obj), -1);

## error on calling "any" on a classdef with no overloaded "any" method
%!error <wrong type argument 'object'>
%! obj = value_class ();
%! any (obj);

## previous test, but with handle classes
%!error <wrong type argument 'object'>
%! obj = handle_class ();
%! any (obj);

## error on calling "any" on a classdef ARRAY with no overloaded "any" method
%!error <wrong type argument 'object'>
%! obj(1, 2) = value_class ();
%! any (obj);

## previous test, but with handle classes
%!error <wrong type argument 'object'>
%! obj(1, 2) = handle_class ();
%! any (obj);

## call "any" method when present inside a classdef
%!test
%! obj = overloaded_any_class ();
%! assert (any (obj), -1);

## previous test, but with handle classes
%!test
%! obj = overloaded_any_class_handle ();
%! assert (any (obj), -1);

## save classdef object with overloaded subsref in struct
%!test <67894>
%! file_format = {'-binary', '-float-binary', '-text', '-v6'};
%! for i_format = 1:numel (file_format)
%!   S.obj = overloaded_subsref ();
%!   S.obj.a = 1;
%!   savefile = tempname ();
%!   unwind_protect
%!     save (file_format{i_format}, savefile, 'S');
%!     clear S;
%!     load (savefile);
%!     assert (S.obj.a, 1);
%!   unwind_protect_cleanup
%!     delete ([savefile, '*']);
%!   end_unwind_protect
%! endfor

%!testif HAVE_HDF5 <67894>
%! file_format = {'-hdf5', '-float-hdf5'};
%! for i_format = 1:numel (file_format)
%!   S.obj = overloaded_subsref ();
%!   S.obj.a = 1;
%!   savefile = tempname ();
%!   unwind_protect
%!     save (file_format{i_format}, savefile, 'S');
%!     clear S;
%!     load (savefile);
%!     assert (S.obj.a, 1);
%!   unwind_protect_cleanup
%!     delete ([savefile, '*']);
%!   end_unwind_protect
%! endfor

%!testif HAVE_ZLIB <67894>
%! file_format = {'-v7'};
%! for i_format = 1:numel (file_format)
%!   S.obj = overloaded_subsref ();
%!   S.obj.a = 1;
%!   savefile = tempname ();
%!   unwind_protect
%!     save (file_format{i_format}, savefile, 'S');
%!     clear S;
%!     load (savefile);
%!     assert (S.obj.a, 1);
%!   unwind_protect_cleanup
%!     delete ([savefile, '*']);
%!   end_unwind_protect
%! endfor

## save classdef object with overloaded subsref in cell
%!test <67894>
%! file_format = {'-binary', '-float-binary', '-text', '-v6'};
%! for i_format = 1:numel (file_format)
%!   C{1} = overloaded_subsref ();
%!   C{1}.a = 1;
%!   savefile = tempname ();
%!   unwind_protect
%!     save (file_format{i_format}, savefile, 'C');
%!     clear C;
%!     load (savefile);
%!     assert (C{1}.a, 1);
%!   unwind_protect_cleanup
%!     delete ([savefile, '*']);
%!   end_unwind_protect
%! endfor

%!testif HAVE_HDF5 <67894>
%! file_format = {'-hdf5', '-float-hdf5'};
%! for i_format = 1:numel (file_format)
%!   C{1} = overloaded_subsref ();
%!   C{1}.a = 1;
%!   savefile = tempname ();
%!   unwind_protect
%!     save (file_format{i_format}, savefile, 'C');
%!     clear C;
%!     load (savefile);
%!     assert (C{1}.a, 1);
%!   unwind_protect_cleanup
%!     delete ([savefile, '*']);
%!   end_unwind_protect
%! endfor

%!testif HAVE_ZLIB <67894>
%! file_format = {'-v7'};
%! for i_format = 1:numel (file_format)
%!   C{1} = overloaded_subsref ();
%!   C{1}.a = 1;
%!   savefile = tempname ();
%!   unwind_protect
%!     save (file_format{i_format}, savefile, 'C');
%!     clear C;
%!     load (savefile);
%!     assert (C{1}.a, 1);
%!   unwind_protect_cleanup
%!     delete ([savefile, '*']);
%!   end_unwind_protect
%! endfor

%!test <*59775>
%! m = class_bug59775A ([1, 2, 3]);
%! assert ([m.A], [1, 1, 1]);

%!test <*59775>
%! m = class_bug59775B ([1, 2, 3]);
%! assert ([m.A], [1, 2, 3]);

%!test <*59775>
%! m = class_bug59775B ([1, 3, 5; 2, 4, 6]);
%! assert ([m.A], [1, 2, 3, 4, 5, 6]);
