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

#if !defined (octave_magic_colon_h)
#define octave_magic_colon_h 1

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// A type to represent `:' as used for indexing.

class
octave_magic_colon : public octave_base_value
{
public:

  octave_magic_colon (void)
    : octave_base_value () { }

  octave_magic_colon (const octave_magic_colon&)
    : octave_base_value () { }

  ~octave_magic_colon (void) { }

  octave_value *clone (void) { return new octave_magic_colon (*this); }

  idx_vector index_vector (void) const { return idx_vector (':'); }

  bool is_defined (void) const { return true; }

  bool is_magic_colon (void) const { return true; }

  bool valid_as_scalar_index (void) const { return true; }

  bool valid_as_zero_index (void) const { return false; }

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // Type id of magic colon objects, set by register_type().
  static int t_id;

  // Type name of magic colon objects, defined in ov-colon.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
