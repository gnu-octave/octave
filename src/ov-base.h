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
#include <list>
#include <string>

#if defined (HAVE_HDF5)
#include <hdf5.h>
#endif

#include "Range.h"
#include "mx-base.h"
#include "str-vec.h"

#include "error.h"

class Cell;
class streamoff_array;
class Octave_map;
class octave_value;
class octave_value_list;
class octave_stream;
class octave_streamoff;
class octave_function;
class octave_user_function;
class octave_fcn_handle;
class octave_fcn_inline;
class octave_value_list;
class octave_lvalue;

class tree_walker;

// T_ID is the type id of struct objects, set by register_type().
// T_NAME is the type name of struct objects.
#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2()

#define DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(virtual)

#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(VIRTUAL) \
  public: \
    VIRTUAL int type_id (void) const { return t_id; } \
    VIRTUAL std::string type_name (void) const { return t_name; } \
    VIRTUAL std::string class_name (void) const { return c_name; } \
    static int static_type_id (void) { return t_id; } \
    static std::string static_type_name (void) { return t_name; } \
    static std::string static_class_name (void) { return c_name; } \
    static void register_type (void); \
 \
  private: \
    static int t_id; \
    static const std::string t_name; \
    static const std::string c_name;


#define DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(t, n, c) \
  int t::t_id (-1); \
  const std::string t::t_name (n); \
  const std::string t::c_name (c); \
  void t::register_type (void) \
    { \
      t_id = octave_value_typeinfo::register_type (t::t_name, \
						   t::c_name, \
						   octave_value (new t ())); \
    }

// A base value type, so that derived types only have to redefine what
// they need (if they are derived from octave_base_value instead of
// octave_value).

class
octave_base_value
{
public:

  typedef octave_base_value * (*type_conv_fcn) (const octave_base_value&);

  friend class octave_value;

  octave_base_value (void) : count (1) { }

  octave_base_value (const octave_base_value&) { }

  virtual ~octave_base_value (void) { }

  virtual octave_base_value *
  clone (void) const { return new octave_base_value (*this); }

  virtual octave_base_value *
  empty_clone (void) const { return new octave_base_value (); }

  virtual type_conv_fcn
  numeric_conversion_function (void) const
    { return static_cast<type_conv_fcn> (0); }

  virtual octave_value squeeze (void) const;

  virtual octave_base_value *try_narrowing_conversion (void) { return 0; }

  virtual octave_value
  subsref (const std::string& type,
	   const std::list<octave_value_list>& idx);

  virtual octave_value_list
  subsref (const std::string& type,
	   const std::list<octave_value_list>& idx,
	   int nargout);

  virtual octave_value
  do_index_op (const octave_value_list& idx, int resize_ok);

  virtual octave_value
  do_index_op (const octave_value_list& idx);

  virtual octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx);

  virtual octave_value
  subsasgn (const std::string& type,
	    const std::list<octave_value_list>& idx,
	    const octave_value& rhs);

  virtual idx_vector index_vector (void) const;

  virtual dim_vector dims (void) const { return dim_vector (-1, -1); }

  octave_idx_type rows (void) const
    {
      dim_vector dv = dims ();

      return (dv.length () > 0) ? dv(0) : -1;
    }

  octave_idx_type columns (void) const
    {
      dim_vector dv = dims ();

      return (dv.length () > 1) ? dv(1) : -1;
    }

  virtual int ndims (void) const;

  virtual octave_idx_type numel (void) const { return dims ().numel (); }

  virtual octave_idx_type capacity (void) const { return numel (); }

  virtual size_t byte_size (void) const { return 0; }

  virtual octave_idx_type nnz (void) const;

  virtual octave_idx_type nzmax (void) const;

  virtual octave_value reshape (const dim_vector&) const;

  virtual octave_value permute (const Array<int>& vec, bool = false) const;

  virtual octave_value resize (const dim_vector&, bool fill = false) const;

  virtual MatrixType matrix_type (void) const;

  virtual MatrixType matrix_type (const MatrixType& typ) const;

  virtual bool is_defined (void) const { return false; }

  bool is_empty (void) const { return numel () == 0; }

  virtual bool is_cell (void) const { return false; }

  virtual bool is_real_scalar (void) const { return false; }

  virtual bool is_real_matrix (void) const { return false; }

  virtual bool is_real_nd_array (void) const { return false; }

  virtual bool is_complex_scalar (void) const { return false; }

  virtual bool is_complex_matrix (void) const { return false; }

  virtual bool is_bool_matrix (void) const { return false; }

  virtual bool is_char_matrix (void) const { return false; }

  virtual bool is_string (void) const { return false; }

  virtual bool is_sq_string (void) const { return false; }

  virtual bool is_range (void) const { return false; }

  virtual bool is_map (void) const { return false; }

  virtual bool is_streamoff (void) const { return false; }

  virtual bool is_cs_list (void) const { return false; }

  virtual bool is_list (void) const { return false; }

  virtual bool is_magic_colon (void) const { return false; }

  virtual bool is_all_va_args (void) const { return false; }

  virtual octave_value all (int = 0) const;

  virtual octave_value any (int = 0) const;

  virtual bool is_bool_type (void) const { return false; }

  virtual bool is_real_type (void) const { return false; }

  virtual bool is_complex_type (void) const { return false; }

  // Would be nice to get rid of the next four functions:

  virtual bool is_scalar_type (void) const { return false; }

  virtual bool is_matrix_type (void) const { return false; }

  virtual bool is_numeric_type (void) const { return false; }

  virtual bool is_sparse_type (void) const { return false; }

  virtual bool valid_as_scalar_index (void) const { return false; }

  virtual bool valid_as_zero_index (void) const { return false; }

  virtual bool is_true (void) const { return false; }

  virtual bool is_constant (void) const { return false; }

  virtual bool is_function_handle (void) const { return false; }

  virtual bool is_inline_function (void) const { return false; }

  virtual bool is_function (void) const { return false; }

  virtual bool is_builtin_function (void) const { return false; }

  virtual bool is_dld_function (void) const { return false; }

  virtual short int short_value (bool = false, bool = false) const;

  virtual unsigned short int ushort_value (bool = false, bool = false) const;

  virtual int int_value (bool = false, bool = false) const;

  virtual unsigned int uint_value (bool = false, bool = false) const;

  virtual int nint_value (bool = false) const;

  virtual long int long_value (bool = false, bool = false) const;

  virtual unsigned long int ulong_value (bool = false, bool = false) const;

  virtual double double_value (bool = false) const;

  virtual double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  virtual Cell cell_value (void) const;

  virtual Matrix matrix_value (bool = false) const;

  virtual NDArray array_value (bool = false) const;

  virtual Complex complex_value (bool = false) const;

  virtual ComplexMatrix complex_matrix_value (bool = false) const;

  virtual ComplexNDArray complex_array_value (bool = false) const;

  virtual bool bool_value (void) const;

  virtual boolMatrix bool_matrix_value (void) const;

  virtual boolNDArray bool_array_value (void) const;

  virtual charMatrix char_matrix_value (bool force = false) const;

  virtual charNDArray char_array_value (bool = false) const;

  virtual SparseMatrix sparse_matrix_value (bool = false) const;

  virtual SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  virtual SparseBoolMatrix sparse_bool_matrix_value (bool = false) const;

  virtual octave_int8 int8_scalar_value (void) const;

  virtual octave_int16 int16_scalar_value (void) const;

  virtual octave_int32 int32_scalar_value (void) const;

  virtual octave_int64 int64_scalar_value (void) const;

  virtual octave_uint8 uint8_scalar_value (void) const;

  virtual octave_uint16 uint16_scalar_value (void) const;

  virtual octave_uint32 uint32_scalar_value (void) const;

  virtual octave_uint64 uint64_scalar_value (void) const;

  virtual int8NDArray int8_array_value (void) const;

  virtual int16NDArray int16_array_value (void) const;

  virtual int32NDArray int32_array_value (void) const;

  virtual int64NDArray int64_array_value (void) const;

  virtual uint8NDArray uint8_array_value (void) const;

  virtual uint16NDArray uint16_array_value (void) const;

  virtual uint32NDArray uint32_array_value (void) const;

  virtual uint64NDArray uint64_array_value (void) const;

  virtual string_vector all_strings (bool pad = false) const;

  virtual std::string string_value (bool force = false) const;

  virtual Range range_value (void) const;

  virtual Octave_map map_value (void) const;

  virtual string_vector map_keys (void) const;

  virtual std::streamoff streamoff_value (void) const;

  virtual streamoff_array streamoff_array_value (void) const;

  virtual octave_function *function_value (bool silent = false);

  virtual octave_user_function *user_function_value (bool silent = false);

  virtual octave_fcn_handle *fcn_handle_value (bool silent = false);

  virtual octave_fcn_inline *fcn_inline_value (bool silent = false);

  virtual octave_value_list list_value (void) const;

  virtual octave_value convert_to_str (bool pad = false, bool force = false,
				       char type = '"') const;
  virtual octave_value
  convert_to_str_internal (bool pad, bool force, char type) const;

  virtual void convert_to_row_or_column_vector (void);

  virtual bool print_as_scalar (void) const { return false; }

  virtual void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  virtual void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  virtual bool
  print_name_tag (std::ostream& os, const std::string& name) const;

  virtual void
  print_with_name (std::ostream& output_buf, const std::string& name, 
		   bool print_padding = true) const;

  virtual void print_info (std::ostream& os, const std::string& prefix) const;

  virtual bool save_ascii (std::ostream& os, bool& infnan_warned,
			   bool strip_nan_and_inf);

  virtual bool load_ascii (std::istream& is);

  virtual bool save_binary (std::ostream& os, bool& save_as_floats);

  virtual bool load_binary (std::istream& is, bool swap, 
			    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  virtual bool
  save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  virtual bool
  load_hdf5 (hid_t loc_id, const char *name, bool have_h5giterate_bug);
#endif

  virtual int
  write (octave_stream& os, int block_size,
	 oct_data_conv::data_type output_type, int skip,
	 oct_mach_info::float_format flt_fmt) const;

protected:

  // This should only be called for derived types.

  octave_value numeric_assign (const std::string& type,
			       const std::list<octave_value_list>& idx,
			       const octave_value& rhs);

  void reset_indent_level (void) const
    { curr_print_indent_level = 0; }

  void increment_indent_level (void) const
    { curr_print_indent_level += 2; }

  void decrement_indent_level (void) const
    { curr_print_indent_level -= 2; }

  int current_print_indent_level (void) const
    { return curr_print_indent_level; }

  void indent (std::ostream& os) const;

  void newline (std::ostream& os) const;

  void reset (void) const;

  // A reference count.
  int count;

private:

  static int curr_print_indent_level;
  static bool beginning_of_line;

  DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
