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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_base_value_h)
#define octave_base_value_h 1

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov.h"
#include "ov-typeinfo.h"

class Cell;
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

  octave_value *clone (void) const { return new octave_base_value (*this); }
  octave_value *empty_clone (void) const { return new octave_base_value (); }

  type_conv_fcn numeric_conversion_function (void) const
    { return static_cast<type_conv_fcn> (0); }

  octave_value squeeze (void) const;

  octave_value *try_narrowing_conversion (void)
    { return static_cast<octave_value *> (0); }

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  octave_value do_index_op (const octave_value_list& idx, int resize_ok);

  octave_value do_index_op (const octave_value_list& idx)
    { return do_index_op (idx, 0); }

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx);

  idx_vector index_vector (void) const;

  octave_value subsasgn (const std::string& type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs);

  dim_vector dims (void) const { return dim_vector (-1, -1); }

  octave_idx_type numel (void) const { return dims ().numel (); }

  octave_idx_type capacity (void) const { return numel (); }

  size_t byte_size (void) const { return 0; }

  octave_value reshape (const dim_vector&) const;

  octave_value permute (const Array<int>& vec, bool = false) const;

  octave_value resize (const dim_vector&) const;

  bool is_defined (void) const { return false; }

  bool is_cell (void) const { return false; }

  bool is_real_scalar (void) const { return false; }

  bool is_real_matrix (void) const { return false; }

  bool is_real_nd_array (void) const { return false; }

  bool is_complex_scalar (void) const { return false; }

  bool is_complex_matrix (void) const { return false; }

  bool is_bool_matrix (void) const { return false; }

  bool is_char_matrix (void) const { return false; }

  bool is_string (void) const { return false; }

  bool is_sq_string (void) const { return false; }

  bool is_range (void) const { return false; }

  bool is_map (void) const { return false; }

  bool is_streamoff (void) const { return false; }

  bool is_cs_list (void) const { return false; }

  bool is_list (void) const { return false; }

  bool is_magic_colon (void) const { return false; }

  bool is_all_va_args (void) const { return false; }

  octave_value all (int = 0) const { return 0.0; }

  octave_value any (int = 0) const { return 0.0; }

  bool is_bool_type (void) const { return false; }

  bool is_real_type (void) const { return false; }

  bool is_complex_type (void) const { return false; }

  // Would be nice to get rid of the next four functions:

  bool is_scalar_type (void) const { return false; }

  bool is_matrix_type (void) const { return false; }

  bool is_numeric_type (void) const { return false; }

  bool valid_as_scalar_index (void) const { return false; }

  bool valid_as_zero_index (void) const { return false; }

  bool is_true (void) const { return false; }

  bool is_zero_by_zero (void) const
    { return (rows () == 0 && columns () == 0); }

  bool is_constant (void) const { return false; }

  bool is_function_handle (void) const { return false; }

  bool is_inline_function (void) const { return false; }

  bool is_function (void) const { return false; }

  bool is_builtin_function (void) const { return false; }

  bool is_dld_function (void) const { return false; }

  short int short_value (bool = false, bool = false) const;

  unsigned short int ushort_value (bool = false, bool = false) const;

  int int_value (bool = false, bool = false) const;

  unsigned int uint_value (bool = false, bool = false) const;

  int nint_value (bool = false) const;

  long int long_value (bool = false, bool = false) const;

  unsigned long int ulong_value (bool = false, bool = false) const;

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Cell cell_value (void) const;

  Matrix matrix_value (bool = false) const;

  NDArray array_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  ComplexNDArray complex_array_value (bool = false) const;

  bool bool_value (void) const;

  boolMatrix bool_matrix_value (void) const;

  boolNDArray bool_array_value (void) const;

  charMatrix char_matrix_value (bool force = false) const;

  charNDArray char_array_value (bool = false) const;

  SparseMatrix sparse_matrix_value (bool = false) const;

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  SparseBoolMatrix sparse_bool_matrix_value (bool = false) const;

  octave_int8 int8_scalar_value (void) const;

  octave_int16 int16_scalar_value (void) const;

  octave_int32 int32_scalar_value (void) const;

  octave_int64 int64_scalar_value (void) const;

  octave_uint8 uint8_scalar_value (void) const;

  octave_uint16 uint16_scalar_value (void) const;

  octave_uint32 uint32_scalar_value (void) const;

  octave_uint64 uint64_scalar_value (void) const;

  int8NDArray int8_array_value (void) const;

  int16NDArray int16_array_value (void) const;

  int32NDArray int32_array_value (void) const;

  int64NDArray int64_array_value (void) const;

  uint8NDArray uint8_array_value (void) const;

  uint16NDArray uint16_array_value (void) const;

  uint32NDArray uint32_array_value (void) const;

  uint64NDArray uint64_array_value (void) const;

  string_vector all_strings (bool pad = false, bool force = false) const;

  std::string string_value (bool force = false) const;

  Range range_value (void) const;

  Octave_map map_value (void) const;

  string_vector map_keys (void) const;

  std::streamoff streamoff_value (void) const;

  streamoff_array streamoff_array_value (void) const;

  octave_function *function_value (bool silent = false);

  octave_user_function *user_function_value (bool silent = false);

  octave_fcn_handle *fcn_handle_value (bool silent = false);

  octave_fcn_inline *fcn_inline_value (bool silent = false);

  octave_value_list list_value (void) const;

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void convert_to_row_or_column_vector (void);

  bool print_as_scalar (void) const { return false; }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  void print_info (std::ostream& os, const std::string& prefix) const;

  bool save_ascii (std::ostream& os, bool& infnan_warned,
			   bool strip_nan_and_inf);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap, 
		    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  bool save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (hid_t loc_id, const char *name, bool have_h5giterate_bug);
#endif

  int write (octave_stream& os, int block_size,
	     oct_data_conv::data_type output_type, int skip,
	     oct_mach_info::float_format flt_fmt) const;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
