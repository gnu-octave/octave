/*

Copyright (C) 1996 John W. Eaton

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
#include "oct-map.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Real scalar values.

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

#if 0
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  octave_value struct_elt_val (const string& nm) const;

  octave_value& struct_elt_ref (const string& nm);

  bool is_defined (void) const { return true; }

  bool is_map (void) const { return true; }

#if 0
  double double_value (bool) const
  Matrix matrix_value (bool frc_str_conv = false) const;
  Complex complex_value (bool frc_str_conv = false) const;
  ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const;
  charMatrix char_matrix_value (bool frc_str_conv = false) const;
  charMatrix all_strings (void) const;
  string string_value (void) const;
  Range range_value (void) const
#endif

  Octave_map map_value (void) const { return map; }

#if 0
  octave_value& lookup_map_element (const string& name,
				    bool insert = false,
				    bool silent = false);

  octave_value& lookup_map_element (SLList<string>& name,
				    bool insert = false,
				    bool silent = false);

  ColumnVector vector_value (bool frc_str_conv = false,
			     bool frc_vec_conv = false) const;

  ComplexColumnVector
  complex_vector_value (bool frc_str_conv = false,
			bool frc_vec_conv = false) const;

  octave_value convert_to_str (void) const;

  void convert_to_row_or_column_vector (void);

  void maybe_mutate (void);
#endif

  void print (ostream& os);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

#if 0
  // Binary and unary operations.

  friend octave_value do_binary_op (octave_value& a, octave_value& b,
				    tree_expression::type t);

  friend octave_value do_unary_op (octave_value& a,
				   tree_expression::type t);

  // We want to eliminate this.

  constant_type const_type (void) const { return type_tag; }

  // We want to get rid of these too:

  void force_numeric (bool frc_str_conv = false);
  octave_value make_numeric (bool frc_str_conv = false) const;

  // But not this.

  void convert_to_matrix_type (bool make_complex);

  // Indexing and assignment.

  void clear_index (void);

  // void set_index (double d);
  void set_index (const Range& r);
  void set_index (const ColumnVector& v);
  void set_index (const Matrix& m);
  void set_index (char c);

  void set_index (const octave_value_list& args,
		  bool rhs_is_complex = false);

  octave_value do_index (const octave_value_list& args);

  void maybe_widen (constant_type t);

  void assign (octave_value& rhs, const octave_value_list& args);

  bool print_as_scalar (void);

  bool print_as_structure (void);
#endif

private:

  Octave_map map;

  static int t_id;

  static const string t_name;

#if 0
  // For custom memory management.
  // XXX FIXME XXX -- maybe this should be inherited (use void* and cast).

  octave_base_value *freeptr;
#endif
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
