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

#if !defined (octave_list_h)
#define octave_list_h 1

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
#include "oct-obj.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class tree_walker;

// Lists.

class
octave_list : public octave_base_value
{
public:

  octave_list (void)
    : octave_base_value () { }

  octave_list (const octave_value_list& l)
    : octave_base_value (), lst (l) { }

  octave_list (const octave_list& l)
    : octave_base_value (), lst (l.lst) { }

  ~octave_list (void) { }

  octave_value *clone (void) { return new octave_list (*this); }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  octave_value do_index_op (const octave_value_list& idx);

  void assign (const octave_value_list& idx, const octave_value& rhs);

  int length (void) const { return lst.length (); }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_list (void) const { return true; }

  octave_value_list list_value (void) const { return lst; }

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // The list of Octave values.
  octave_value_list lst;

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
