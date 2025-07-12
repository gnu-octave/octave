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
%!test
%! obj = regular_class ();
%! obj.a = 1;
%! obj.b = "Regular Class";
%! obj.c = [[1, 2], [3, 4]];
%! obj.d = struct ("a", 1, "b", 2);
%! obj.e = {1, "Regular Class"};
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
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
%!test
%! obj = regular_class_with_constructor ();
%! obj.a = [];
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, no loadobj/saveobj, nested object
%!test
%! obj = regular_class ();
%! obj.a = regular_class_with_constructor ();
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (class(obj.a), "regular_class_with_constructor");
%!   assert (obj.a.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Vector of value class objects.
%!test
%! obj = regular_class;
%! obj.a = 1;
%! obj(2) = regular_class;
%! obj(2).a = 2;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj(1).a, 1);
%!   assert (obj(2).a, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Matrix of value class objects.
%!test <65179>
%! obj = regular_class;
%! obj.a = 1;
%! obj(2,3) = regular_class;
%! obj(2,3).a = 2;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj(1).a, 1);
%!   assert (obj(6).a, 2);
%!   assert (size (obj), [2, 3]);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, saveobj return a struct, no loadobj
%!test
%! obj = saveobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert(obj.a, 1);
%!   assert(obj.b, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, loadobj is defined, no saveobj
%!test
%! obj = loadobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, loadobj is defined, saveobj is defined
%!test
%! obj = loadobj_saveobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Constructor defined, ConstructOnLoad = false, custom return type for loadobj/saveobj
%!test
%! obj = custom_saveobj ();
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, "abcde");
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, undefined return variable from saveobj (should return default initialized classdef)
%!test
%! obj = invalid_saveobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## No constructor, ConstructOnLoad = false, undefined return variable from saveobj (should emit a warning)
%!warning <saveobj.*does not return.*value>
%! obj = invalid_saveobj_class ();
%! obj.a = 1;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! delete (savefile);


## The following tests are either known failure points, or the appropriate
## functionality hasn't been implemented yet.

%% Constructor, ConstructOnLoad = true, no loadobj/saveobj
%!test <45833>
%! obj = regular_class_construct_on_load ();
%! obj.a = [];
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Handle class, no constructor, ConstructOnLoad = false, no loadobj/saveobj
## Note: File format for saving handle classes needs to save the appropriate metadata
%!test <45833>
%! obj1 = regular_handle_class ();
%! obj2 = obj1;
%! obj2.a = 1;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj1', 'obj2');
%! unwind_protect
%!   clear obj1 obj2;
%!   load (savefile);
%!   obj2.b = 2;
%!   assert (obj1.b, 2);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect

## Constructor, ConstructOnLoad = false, Transient property
%!test <45833>
%! obj = transient_property_class ();
%! obj.a = 1;
%! obj.transient_property = 6;
%! savefile = tempname ();
%! save ('-text', savefile, 'obj');
%! unwind_protect
%!   clear obj;
%!   load (savefile);
%!   assert (obj.a, 1);
%!   ## Transient property should not be saved and loaded
%!   assert (obj.transient_property, []);
%! unwind_protect_cleanup
%!   delete (savefile);
%! end_unwind_protect
