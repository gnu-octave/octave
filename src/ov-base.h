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

// Real scalar values.

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

#if 0
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  type_conv_fcn numeric_conversion_function (void) const
    { return (type_conv_fcn) 0; }

  octave_value *try_narrowing_conversion (void)
    { return (octave_value *) 0; }

  octave_value index (const octave_value_list& idx) const;

  idx_vector index_vector (void) const;

  octave_value struct_elt_val (const string& nm, bool silent) const;

  octave_value& struct_elt_ref (const string& nm);

  int rows (void) const { return -1; }

  int columns (void) const { return -1; }

  bool is_defined (void) const { return false; }

  bool is_real_scalar (void) const { return false; }

  bool is_real_matrix (void) const { return false; }

  bool is_complex_scalar (void) const { return false; }

  bool is_complex_matrix (void) const { return false; }

  bool is_char_matrix (void) const { return false; }

  bool is_string (void) const { return false; }

  bool is_range (void) const { return false; }

  bool is_map (void) const { return false; }

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

  double double_value (bool) const;

  Matrix matrix_value (bool frc_str_conv = false) const;

  Complex complex_value (bool frc_str_conv = false) const;

  ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const;

  charMatrix char_matrix_value (bool frc_str_conv = false) const;

  charMatrix all_strings (void) const;

  string string_value (void) const;

  Range range_value (void) const;

  Octave_map map_value (void) const;

  octave_value not (void) const;

  octave_value uminus (void) const;

  octave_value transpose (void) const;

  octave_value hermitian (void) const;

  void increment (void);

  void decrement (void);

  octave_value convert_to_str (void) const;

  void convert_to_row_or_column_vector (void);

  void print (ostream& os, bool pr_as_read_syntax = false);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  static int t_id;

  static const string t_name;
};

#endif

extern void install_base_type_conversions (void);

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
