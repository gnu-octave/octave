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

#if !defined (octave_base_value_h)
#define octave_base_value_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// A base value type, so that derived types only have to redefine what
// they need (if they are derived from octave_base_value instead of
// octave_value).

class
octave_base_value : public octave_value
{
public:

  octave_base_value (void)
    : octave_value (octave_xvalue ()) { }

  octave_base_value (const octave_base_value&)
    : octave_value (octave_xvalue ()) { }

  ~octave_base_value (void) { }

  octave_value *clone (void) { return new octave_base_value (*this); }

  type_conv_fcn numeric_conversion_function (void) const
    { return static_cast<type_conv_fcn> (0); }

  octave_value *try_narrowing_conversion (void)
    { return static_cast<octave_value *> (0); }

  octave_value do_index_op (const octave_value_list& idx);

  octave_value_list do_index_op (int nargout, const octave_value_list& idx);

  idx_vector index_vector (void) const;

  octave_value
  do_struct_elt_index_op (const string& nm, const octave_value_list& idx,
			  bool silent);

  octave_value do_struct_elt_index_op (const string& nm, bool silent);

  octave_lvalue struct_elt_ref (octave_value *parent, const string& nm);

  int rows (void) const;
  int columns (void) const;

  int length (void) const;

  bool is_defined (void) const { return false; }

  bool is_real_scalar (void) const { return false; }

  bool is_real_matrix (void) const { return false; }

  bool is_complex_scalar (void) const { return false; }

  bool is_complex_matrix (void) const { return false; }

  bool is_char_matrix (void) const { return false; }

  bool is_string (void) const { return false; }

  bool is_range (void) const { return false; }

  bool is_map (void) const { return false; }

  bool is_file (void) const { return false; }

  bool is_list (void) const { return false; }

  bool is_magic_colon (void) const { return false; }

  bool is_all_va_args (void) const { return false; }

  octave_value all (void) const { return 0.0; }

  octave_value any (void) const { return 0.0; }

  bool is_real_type (void) const { return false; }

  bool is_complex_type (void) const { return false; }

  // Would be nice to get rid of the next four functions:

  bool is_scalar_type (void) const { return false; }

  bool is_matrix_type (void) const { return false; }

  bool is_numeric_type (void) const { return false; }

  bool valid_as_scalar_index (void) const { return false; }

  bool valid_as_zero_index (void) const { return false; }

  bool is_true (void) const { return false; }

  bool is_empty (void) const
    { return (rows () == 0 || columns () == 0); }

  bool is_zero_by_zero (void) const
    { return (rows () == 0 && columns () == 0); }

  bool is_constant (void) const { return false; }

  bool is_function (void) const { return false; }

  int int_value (bool = false, bool = false) const;

  int nint_value (bool = false) const;

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  charMatrix char_matrix_value (bool = false) const;

  string_vector all_strings (void) const;

  string string_value (void) const;

  Range range_value (void) const;

  Octave_map map_value (void) const;

  octave_stream *stream_value (void) const;

  int stream_number (void) const;

  octave_function *function_value (bool silent);

  octave_value_list list_value (void) const;

  bool bool_value (void) const;

  boolMatrix bool_matrix_value (void) const;

  octave_value convert_to_str (void) const;

  void convert_to_row_or_column_vector (void);

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // Type id of base value objects, set by register_type().
  static int t_id;

  // Type name of base value objects, defined in ov-base.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
