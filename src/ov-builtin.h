/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_builtin_h)
#define octave_builtin_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "ov-fcn.h"
#include "ov-typeinfo.h"

class octave_value;
class octave_value_list;

// Builtin functions.

class
octave_builtin : public octave_function
{
public:

  typedef octave_value_list (*fcn) (const octave_value_list&, int);

  octave_builtin (fcn ff, const string& nm = string (),
		  const string& ds = string ())
    : octave_function (nm, ds), f (ff) { }

  ~octave_builtin (void) { }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  octave_function *function_value (bool) { return this; }

  octave_value_list do_index_op (int nargout, const octave_value_list& args);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  octave_builtin (void);

  octave_builtin (const octave_builtin& m);

  // A pointer to the actual function.
  fcn f;

  // For custom memory management.
  static octave_allocator allocator;

  // Type id of list objects, set by register_type().
  static int t_id;

  // Type name of list objects, defined in ov-list.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
