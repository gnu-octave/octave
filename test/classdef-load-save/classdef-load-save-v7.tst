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

## No constructor, ConstructOnLoad = false, no loadobj/saveobj
%!testif HAVE_ZLIB
%! obj = regular_class ();
%! obj.a = 1;
%! obj.b = "Regular Class";
%! obj.c = [[1, 2], [3, 4]];
%! obj.d = struct ("a", 1, "b", 2);
%! obj.e = {1, "Regular Class"};
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%!   assert (obj.b, "Regular Class");
%!   assert (obj.c, [[1, 2], [3, 4]]);
%!   assert (obj.d, struct ("a", 1, "b", 2));
%!   assert (obj.e, {1, "Regular Class"});
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Constructor defined, ConstructOnLoad = false, no loadobj/saveobj
## (ensures that constructor is NOT called on load)
%!testif HAVE_ZLIB
%! obj = regular_class_with_constructor ();
%! obj.a = [];
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, no loadobj/saveobj, nested object
%!testif HAVE_ZLIB
%! obj = regular_class ();
%! obj.a = regular_class_with_constructor ();
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (class (obj.a), "regular_class_with_constructor");
%!   assert (obj.a.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, no loadobj/saveobj, nested object inside a struct
%!testif HAVE_ZLIB  <*45833>
%! obj = regular_class ();
%! s.obj_field = regular_class_with_constructor ();
%! obj.a = s;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (class (obj.a.obj_field), "regular_class_with_constructor");
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, no loadobj/saveobj, nested object inside struct array
%!testif HAVE_ZLIB  <*45833>
%! obj = regular_class ();
%! s(2,3).obj_field = regular_class_with_constructor ();
%! obj.a = s;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (class (obj.a(2,3).obj_field), "regular_class_with_constructor");
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, no loadobj/saveobj, nested object inside cell array
%!testif HAVE_ZLIB  <*45833>
%! obj = regular_class ();
%! c{2,3}.obj_field = regular_class_with_constructor ();
%! obj.a = c;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (class (obj.a{2,3}.obj_field), "regular_class_with_constructor");
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Vector of value class objects.
%!testif HAVE_ZLIB
%! obj = regular_class;
%! obj.a = 1;
%! obj(2) = regular_class;
%! obj(2).a = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj(1).a, 1);
%!   assert (obj(2).a, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Matrix of value class objects.
%!testif HAVE_ZLIB  <*65179>
%! obj = regular_class;
%! obj.a = 1;
%! obj(2,3) = regular_class;
%! obj(2,3).a = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj(1).a, 1);
%!   assert (obj(6).a, 2);
%!   assert (size (obj), [2, 3]);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, saveobj returns an object, no loadobj
%!testif HAVE_ZLIB
%! obj = saveobj_obj_class ();
%! obj.a = 1;
%! obj.b = 3;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert(obj.a, 2);
%!   assert(obj.b, 3);
%!   assert(obj.c, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, saveobj returns a struct, no loadobj
%!testif HAVE_ZLIB
%! obj = saveobj_struct_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert(obj.a, []);
%!   assert(obj.b, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, loadobj is defined, no saveobj
%!testif HAVE_ZLIB
%! obj = loadobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, loadobj is defined, no saveobj
## Class definition changes between saving and loading the object
%!testif HAVE_ZLIB  <*67414>
%! clear classes;
%! obj = loadobj_changed_class ();
%! obj.a = 0;
%! savefile = fullfile (tempdir (), "oct-changed-class.sav");
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, "object");
%!   clear obj;
%!   ## change to directory with classdef with renamed property name
%!   cd ("changed-class");
%!   clear classes;  # includes "clear all"
%!   savefile = fullfile (tempdir (), "oct-changed-class.sav");
%!   load (savefile);
%!   assert (obj.a, "changed-struct");
%! unwind_protect_cleanup
%!   delete (savefile);
%!   [~, curr_dir] = fileparts (pwd ());
%!   if (strcmp (curr_dir, "changed-class"))
%!     cd ..
%!   endif
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, loadobj is defined, saveobj is defined
%!testif HAVE_ZLIB
%! obj = loadobj_saveobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Constructor defined, ConstructOnLoad = false, custom return type for loadobj/saveobj
%!testif HAVE_ZLIB
%! obj = custom_saveobj ();
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, "abcde");
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, undefined return variable from saveobj (should return default initialized classdef)
%!testif HAVE_ZLIB
%! obj = invalid_saveobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, []);  # fails in MATLAB
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, undefined return variable from saveobj (should emit a warning)
%!testif HAVE_ZLIB
%! obj = invalid_saveobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! lastwarn ("");
%! unwind_protect
%!   save ('-v7', savefile, 'obj');
%!   assert (! isempty (regexp (lastwarn (), "saveobj.*does not return.*value", "once")));
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Handle class, no constructor, ConstructOnLoad = false, no loadobj/saveobj
%!testif HAVE_ZLIB
%! obj1 = regular_handle_class ();
%! obj2 = obj1;
%! obj2.a = 1;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj1', 'obj2');
%! unwind_protect
%!   clear obj1 obj2;
%!   load (savefile);
%!   obj2.b = 2;
%!   assert (obj1.b, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Handle class, no constructor, ConstructOnLoad = false, no loadobj/saveobj, circular references
%!testif HAVE_ZLIB
%! obj = regular_handle_class ();
%! obj.a = regular_handle_class ();
%! obj.c = 1;
%! obj.a.b = obj;
%! obj.a.d = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.c, 1);
%!   obj.c = 3;
%!   assert (obj.a.b.c, 3);
%!   assert (obj.a.d, 2);
%!   obj.a.d = 4;
%!   assert (obj.a.b.a.d, 4);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Handle class, no constructor, ConstructOnLoad = false, no loadobj/saveobj, vector
%!testif HAVE_ZLIB
%! obj = regular_handle_class ();
%! obj(2) = regular_handle_class ();
%! obj(3) = obj(1);
%! obj(1).a = 1;
%! obj(2).a = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj(1).a, 1);
%!   assert (obj(2).a, 2);
%!   obj(1).a = 3;
%!   assert (obj(3).a, 3);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Constructor, ConstructOnLoad = false, Transient property
%!testif HAVE_ZLIB
%! obj = transient_property_class ();
%! obj.a = 1;
%! obj.transient_property = 6;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%!   ## Transient property should not be saved and loaded
%!   assert (obj.transient_property, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Constructor, ConstructOnLoad = true, Transient property, no loadobj/saveobj
%!testif HAVE_ZLIB
%! obj = regular_class_construct_on_load ();
%! obj.a = 10;
%! obj.f = 16;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 10);
%!   assert (obj.f, 6);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Overloaded subsref is not called on save (bug #67894)
%!testif HAVE_ZLIB <*67894>
%! obj = overloaded_subsref_class (1);
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (raw_a (obj), 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Overloaded subsref is not called on save of an object array (bug #67894)
%!testif HAVE_ZLIB <*67894>
%! obj = [overloaded_subsref_class(1), overloaded_subsref_class(2), ...
%!        overloaded_subsref_class(3)];
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (size (obj), [1, 3]);
%!   assert (raw_a (obj), [1, 2, 3]);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Overloaded subsref is not called on save of a nested object (bug #67894)
%!testif HAVE_ZLIB <*67894>
%! obj = overloaded_subsref_class (overloaded_subsref_class (2));
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (raw_a (raw_a (obj)), 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Overloaded subsref is not called on save of objects in a cell and a
## struct held by a property (bug #67894)
%!testif HAVE_ZLIB <*67894>
%! obj = overloaded_subsref_class ({overloaded_subsref_class(2), ...
%!                                  struct("s", overloaded_subsref_class (3))});
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   c = raw_a (obj);
%!   assert (raw_a (c{1}), 2);
%!   assert (raw_a (c{2}.s), 3);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Overloaded subsasgn is not called on load of an object array (bug #67894)
%!testif HAVE_ZLIB <*67894>
%! obj = [overloaded_subsasgn_class(1), overloaded_subsasgn_class(2), ...
%!        overloaded_subsasgn_class(3)];
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (size (obj), [1, 3]);
%!   assert ([obj.a], [1, 2, 3]);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## save -append adds an object to a file without objects
%!testif HAVE_ZLIB
%! x = 1;
%! obj = regular_class ();
%! obj.a = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'x');
%! unwind_protect
%!   save ('-v7', '-append', savefile, 'obj');
%!   clear x obj;
%!   load (savefile);
%!   assert (x, 1);
%!   assert (obj.a, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## save -append adds a variable to a file with an object
%!testif HAVE_ZLIB
%! obj = regular_class ();
%! obj.a = 1;
%! x = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   save ('-v7', '-append', savefile, 'x');
%!   clear obj x;
%!   load (savefile);
%!   assert (obj.a, 1);
%!   assert (x, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## save -append adds an object to a file with an object of the same class
%!testif HAVE_ZLIB
%! obj1 = regular_class ();
%! obj1.a = 1;
%! obj2 = regular_class ();
%! obj2.a = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj1');
%! unwind_protect
%!   save ('-v7', '-append', savefile, 'obj2');
%!   clear obj1 obj2;
%!   load (savefile);
%!   assert (obj1.a, 1);
%!   assert (obj2.a, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## save -append three times, with a nested object, a handle object saved
## twice in one call, and an object array
%!testif HAVE_ZLIB
%! obj = regular_class ();
%! obj.a = regular_class ();
%! obj.a.a = 1;
%! h = regular_handle_class ();
%! h.a = 5;
%! g = h;
%! arr = [regular_class(), regular_class()];
%! arr(2).a = 20;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   save ('-v7', '-append', savefile, 'h', 'g');
%!   save ('-v7', '-append', savefile, 'arr');
%!   clear obj h g arr;
%!   load (savefile);
%!   assert (obj.a.a, 1);
%!   assert (h.a, 5);
%!   g.a = 6;
%!   assert (h.a, 6);
%!   assert (size (arr), [1, 2]);
%!   assert (arr(2).a, 20);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Object array whose first element holds a nested object
%!testif HAVE_ZLIB
%! obj = [regular_class(), regular_class(), regular_class()];
%! obj(1).a = regular_class ();
%! obj(1).a.a = regular_class ();
%! obj(1).a.a.a = 1;
%! obj(2).b = 2;
%! obj(3).b = 3;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj(1).a.a.a, 1);
%!   assert (obj(2).a, []);
%!   assert (obj(2).b, 2);
%!   assert (obj(3).b, 3);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Object array whose first element holds an object array
%!testif HAVE_ZLIB
%! inner = [regular_class(), regular_class()];
%! inner(2).b = 12;
%! obj = [regular_class(), regular_class()];
%! obj(1).a = inner;
%! obj(2).b = 2;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (size (obj(1).a), [1, 2]);
%!   assert (obj(1).a(2).b, 12);
%!   assert (obj(2).a, []);
%!   assert (obj(2).b, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Protected and private properties are restored on load
%!testif HAVE_ZLIB
%! obj = set_values (private_property_class (), 1, 2, 3);
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (get_values (obj), {1, 2, 3});
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Protected and private properties are restored on load of an object array
%!testif HAVE_ZLIB
%! obj1 = set_values (private_property_class (), 1, 2, 3);
%! obj2 = set_values (private_property_class (), 4, 5, 6);
%! obj = [obj1, obj2];
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (get_values (obj(1)), {1, 2, 3});
%!   assert (get_values (obj(2)), {4, 5, 6});
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## loadobj receives an object when the saved properties, private ones
## included, match the class
%!testif HAVE_ZLIB
%! obj = set_values (private_property_loadobj_class (), 1, 2, 3);
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (class (obj), "private_property_loadobj_class");
%!   assert (get_values (obj), {1, 2, 3});
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## containers.Map objects, whose data is in private properties
%!testif HAVE_ZLIB
%! m = containers.Map ({"a", "b"}, {1, "two"});
%! n = containers.Map ([1, 2], {[3, 4], {5}});
%! savefile = tempname ();
%! save ('-v7', savefile, 'm', 'n');
%! unwind_protect
%!   clear m n;
%!   load (savefile);
%!   assert (m.keys (), {"a", "b"});
%!   assert (m("b"), "two");
%!   assert (n.KeyType, "double");
%!   assert (n(1), [3, 4]);
%!   assert (n(2), {5});
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect
