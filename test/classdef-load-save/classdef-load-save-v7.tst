########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
%!   assert (class(obj.a), "regular_class_with_constructor");
%!   assert (obj.a.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, no loadobj/saveobj, nested object inside a struct
%!testif HAVE_ZLIB  <45833>
%! obj = regular_class ();
%! s.obj_field = regular_class_with_constructor ();
%! obj.a = s;
%! savefile = tempname ();
%! save ('-v7', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (class(obj.a.obj_field), "regular_class_with_constructor");
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
