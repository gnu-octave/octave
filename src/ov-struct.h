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

#if !defined (octave_struct_h)
#define octave_struct_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-alloc.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Data structures.

class
octave_struct : public octave_base_value
{
public:

  octave_struct (void)
    : octave_base_value () { }

  octave_struct (const Octave_map& m)
    : octave_base_value (), map (m) { }

  octave_struct (const octave_struct& s)
    : octave_base_value (), map (s.map) { }

  ~octave_struct (void) { }

  octave_value *clone (void) { return new octave_struct (*this); }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  octave_value
  do_struct_elt_index_op (const string& nm, const octave_value_list& idx,
			  bool silent);

  octave_value do_struct_elt_index_op (const string& nm, bool silent);

  octave_lvalue struct_elt_ref (octave_value *parent, const string& nm);

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return false; }

  bool is_map (void) const { return true; }

  Octave_map map_value (void) const { return map; }

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // The associative array used to manage the structure data.
  Octave_map map;

  // For custom memory management.
  static octave_allocator allocator;

  // Type id of struct objects, set by register_type().
  static int t_id;

  // Type name of struct objects, defined in ov-struct.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
