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

#if !defined (octave_bool_h)
#define octave_bool_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "lo-utils.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Real scalar values.

class
octave_bool : public octave_base_value
{
public:

  octave_bool (void)
    : octave_base_value (), scalar (false) { }

  octave_bool (bool b)
    : octave_base_value (), scalar (b) { }

  octave_bool (const octave_bool& s)
    : octave_base_value (), scalar (s.scalar) { }

  ~octave_bool (void) { }

  octave_value *clone (void) { return new octave_bool (*this); }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  type_conv_fcn numeric_conversion_function (void) const;

  octave_value index (const octave_value_list& idx) const;

  idx_vector index_vector (void) const { return idx_vector (scalar); }

  int rows (void) const { return 1; }
  int columns (void) const { return 1; }

  bool is_defined (void) const { return true; }
  bool is_real_scalar (void) const { return true; }

  octave_value all (void) const { return scalar; }
  octave_value any (void) const { return scalar; }

  bool is_real_type (void) const { return true; }
  bool is_scalar_type (void) const { return true; }
  bool is_numeric_type (void) const { return true; }

  bool valid_as_scalar_index (void) const { return scalar; }

  bool valid_as_zero_index (void) const { return ! scalar; }

  bool is_true (void) const { return scalar; }

  double double_value (bool = false) const { return scalar; }

  double scalar_value (bool = false) const { return scalar; }

  Matrix matrix_value (bool = false) const { return Matrix (1, 1, scalar); }

  Complex complex_value (bool = false) const { return scalar; }

  ComplexMatrix complex_matrix_value (bool = false) const
    { return  ComplexMatrix (1, 1, Complex (scalar)); }

  bool bool_value (void) const { return scalar; }

  boolMatrix bool_matrix_value (void) const
    { return boolMatrix (1, 1, scalar); }

  octave_value not (void) const { return octave_value (! scalar); }

  octave_value uminus (void) const { return octave_value (- (double) scalar); }

  octave_value transpose (void) const { return octave_value (scalar); }

  octave_value hermitian (void) const { return octave_value (scalar); }

  octave_value convert_to_str (void) const;

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // The value of this scalar.
  bool scalar;

  // For custom memory management.
  static octave_allocator allocator;

  // Type id of bool objects, set by register_type().
  static int t_id;

  // Type name of bool objects, defined in ov-bool.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
