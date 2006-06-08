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

#if !defined (octave_value_h)
#define octave_value_h 1

#include <cstdlib>

#include <iostream>
#include <string>
#include <list>

#if defined (HAVE_HDF5)
#include <hdf5.h>
#endif

#include "Range.h"
#include "data-conv.h"
#include "idx-vector.h"
#include "mach-info.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "oct-time.h"
#include "str-vec.h"

class Cell;
class streamoff_array;
class Octave_map;
class octave_stream;
class octave_streamoff;
class octave_function;
class octave_user_function;
class octave_fcn_handle;
class octave_fcn_inline;
class octave_value_list;
class octave_lvalue;

#include "ov-base.h"

// Constants.

class octave_value;

class
octave_value
{
public:

  enum unary_op
  {
    op_not,
    op_uplus,
    op_uminus,
    op_transpose,
    op_hermitian,
    op_incr,
    op_decr,
    num_unary_ops,
    unknown_unary_op
  };

  enum binary_op
  {
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_pow,
    op_ldiv,
    op_lshift,
    op_rshift,
    op_lt,
    op_le,
    op_eq,
    op_ge,
    op_gt,
    op_ne,
    op_el_mul,
    op_el_div,
    op_el_pow,
    op_el_ldiv,
    op_el_and,
    op_el_or,
    op_struct_ref,
    num_binary_ops,
    unknown_binary_op
  };

  enum assign_op
  {
    op_asn_eq,
    op_add_eq,
    op_sub_eq,
    op_mul_eq,
    op_div_eq,
    op_ldiv_eq,
    op_pow_eq,
    op_lshift_eq,
    op_rshift_eq,
    op_el_mul_eq,
    op_el_div_eq,
    op_el_ldiv_eq,
    op_el_pow_eq,
    op_el_and_eq,
    op_el_or_eq,
    num_assign_ops,
    unknown_assign_op
  };

  static std::string unary_op_as_string (unary_op);

  static std::string binary_op_as_string (binary_op);

  static std::string assign_op_as_string (assign_op);

  static octave_value empty_conv (const std::string& type,
				  const octave_value& rhs = octave_value ());

  enum magic_colon { magic_colon_t };

  octave_value (void);
  octave_value (short int i);
  octave_value (unsigned short int i);
  octave_value (int i);
  octave_value (unsigned int i);
  octave_value (long int i);
  octave_value (unsigned long int i);

  // FIXME -- these are kluges.  They turn into doubles
  // internally, which will break for very large values.  We just use
  // them to store things like 64-bit ino_t, etc, and hope that those
  // values are never actually larger than can be represented exactly
  // in a double.

#if defined (HAVE_LONG_LONG_INT)
  octave_value (long long int i);
#endif
#if defined (HAVE_UNSIGNED_LONG_LONG_INT)
  octave_value (unsigned long long int i);
#endif

  octave_value (octave_time t);
  octave_value (double d);
  octave_value (const ArrayN<octave_value>& a, bool is_cs_list = false);
  octave_value (const Cell& c, bool is_cs_list = false);
  octave_value (const Matrix& m, const MatrixType& t = MatrixType());
  octave_value (const NDArray& nda);
  octave_value (const ArrayN<double>& m);
  octave_value (const DiagMatrix& d);
  octave_value (const RowVector& v);
  octave_value (const ColumnVector& v);
  octave_value (const Complex& C);
  octave_value (const ComplexMatrix& m, const MatrixType& t = MatrixType());
  octave_value (const ComplexNDArray& cnda);
  octave_value (const ArrayN<Complex>& m);
  octave_value (const ComplexDiagMatrix& d);
  octave_value (const ComplexRowVector& v);
  octave_value (const ComplexColumnVector& v);
  octave_value (bool b);
  octave_value (const boolMatrix& bm, const MatrixType& t = MatrixType());
  octave_value (const boolNDArray& bnda);
  octave_value (char c, char type = '"');
  octave_value (const char *s, char type = '"');
  octave_value (const std::string& s, char type = '"');
  octave_value (const string_vector& s, char type = '"');
  octave_value (const charMatrix& chm, bool is_string = false,
		char type = '"');
  octave_value (const charNDArray& chnda, bool is_string = false,
		char type = '"');
  octave_value (const ArrayN<char>& chnda, bool is_string = false,
		char type = '"');
  octave_value (const SparseMatrix& m, const MatrixType& t = MatrixType ());
  octave_value (const SparseComplexMatrix& m, 
		const MatrixType& t = MatrixType ());
  octave_value (const SparseBoolMatrix& bm, 
		const MatrixType& t = MatrixType ());
  octave_value (const octave_int8& i);
  octave_value (const octave_int16& i);
  octave_value (const octave_int32& i);
  octave_value (const octave_int64& i);
  octave_value (const octave_uint8& i);
  octave_value (const octave_uint16& i);
  octave_value (const octave_uint32& i);
  octave_value (const octave_uint64& i);
  octave_value (const int8NDArray& inda);
  octave_value (const int16NDArray& inda);
  octave_value (const int32NDArray& inda);
  octave_value (const int64NDArray& inda);
  octave_value (const uint8NDArray& inda);
  octave_value (const uint16NDArray& inda);
  octave_value (const uint32NDArray& inda);
  octave_value (const uint64NDArray& inda);
  octave_value (double base, double limit, double inc);
  octave_value (const Range& r);
  octave_value (const Octave_map& m);
  octave_value (const streamoff_array& off);
  octave_value (const octave_value_list& m, bool is_cs_list = false);
  octave_value (octave_value::magic_colon);

  octave_value (octave_base_value *new_rep);

  // Copy constructor.

  octave_value (const octave_value& a)
    {
      rep = a.rep;
      rep->count++;
    }

  // This should only be called for derived types.

  octave_base_value *clone (void) const;

  octave_base_value *empty_clone (void) const
    { return rep->empty_clone (); }

  // Delete the representation of this constant if the count drops to
  // zero.

  ~octave_value (void);

  void make_unique (void)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = rep->clone ();
	  rep->count = 1;
	}
    }

  // Simple assignment.

  octave_value& operator = (const octave_value& a)
    {
      if (rep != a.rep)
	{
	  if (--rep->count == 0)
	    delete rep;

	  rep = a.rep;
	  rep->count++;
	}

      return *this;
    }

  int get_count (void) const { return rep->count; }

  octave_base_value::type_conv_fcn numeric_conversion_function (void) const
    { return rep->numeric_conversion_function (); }

  void maybe_mutate (void);

  octave_value squeeze (void) const
    { return rep->squeeze (); }

  octave_base_value *try_narrowing_conversion (void)
    { return rep->try_narrowing_conversion (); }

  octave_value single_subsref (const std::string& type,
			       const octave_value_list& idx);

  octave_value subsref (const std::string& type,
				const std::list<octave_value_list>& idx)
    { return rep->subsref (type, idx); }

  octave_value_list subsref (const std::string& type,
				     const std::list<octave_value_list>& idx,
    				     int nargout);

  octave_value next_subsref (const std::string& type, const
			     std::list<octave_value_list>& idx,
			     size_t skip = 1);

  octave_value_list next_subsref (int nargout,
				  const std::string& type, const
				  std::list<octave_value_list>& idx,
				  size_t skip = 1);

  octave_value do_index_op (const octave_value_list& idx,
				    int resize_ok)
    { return rep->do_index_op (idx, resize_ok); }

  octave_value do_index_op (const octave_value_list& idx)
    { return do_index_op (idx, 0); }

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx);

  octave_value subsasgn (const std::string& type,
				 const std::list<octave_value_list>& idx,
				 const octave_value& rhs);

  octave_value assign (assign_op op, const std::string& type,
		       const std::list<octave_value_list>& idx,
		       const octave_value& rhs);

  const octave_value& assign (assign_op, const octave_value& rhs);

  idx_vector index_vector (void) const
    { return rep->index_vector (); }

  // Size.

  dim_vector dims (void) const
    { return rep->dims (); }

  octave_idx_type rows (void) const { return rep->rows (); }

  octave_idx_type columns (void) const { return rep->columns (); }

  octave_idx_type length (void) const;

  int ndims (void) const { return rep->ndims (); }

  bool all_zero_dims (void) const { return dims().all_zero (); }

  octave_idx_type numel (void) const
    { return rep->numel (); }

  octave_idx_type capacity (void) const
    { return rep->capacity (); }

  Matrix size (void) const;

  size_t byte_size (void) const
    { return rep->byte_size (); }

  octave_idx_type nnz (void) const { return rep->nnz (); }

  octave_idx_type nzmax (void) const { return rep->nzmax (); }

  octave_value reshape (const dim_vector& dv) const
    { return rep->reshape (dv); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
    { return rep->permute (vec, inv); }

  octave_value ipermute (const Array<int>& vec) const
    { return rep->permute (vec, true); }

  octave_value resize (const dim_vector& dv, bool fill = false) const
    { return rep->resize (dv, fill);}

  MatrixType matrix_type (void) const
  { return rep->matrix_type (); }

  MatrixType matrix_type (const MatrixType& typ) const
  { return rep->matrix_type (typ); }

  // Does this constant have a type?  Both of these are provided since
  // it is sometimes more natural to write is_undefined() instead of
  // ! is_defined().

  bool is_defined (void) const
    { return rep->is_defined (); }

  bool is_undefined (void) const
    { return ! is_defined (); }

  bool is_empty (void) const
    { return rep->is_empty (); }

  bool is_cell (void) const
    { return rep->is_cell (); }

  bool is_real_scalar (void) const
    { return rep->is_real_scalar (); }

  bool is_real_matrix (void) const
    { return rep->is_real_matrix (); }

  bool is_real_nd_array (void) const
    { return rep->is_real_nd_array (); }

  bool is_complex_scalar (void) const
    { return rep->is_complex_scalar (); }

  bool is_complex_matrix (void) const
    { return rep->is_complex_matrix (); }

  bool is_bool_matrix (void) const
    { return rep->is_bool_matrix (); }

  bool is_char_matrix (void) const
    { return rep->is_char_matrix (); }

  bool is_string (void) const
    { return rep->is_string (); }

  bool is_sq_string (void) const
    { return rep->is_sq_string (); }

  bool is_dq_string (void) const
    { return rep->is_string () && ! rep->is_sq_string (); }

  bool is_range (void) const
    { return rep->is_range (); }

  bool is_map (void) const
    { return rep->is_map (); }

  bool is_streamoff (void) const
    { return rep->is_streamoff (); }

  bool is_cs_list (void) const
    { return rep->is_cs_list (); }

  bool is_list (void) const
    { return rep->is_list (); }

  bool is_magic_colon (void) const
    { return rep->is_magic_colon (); }

  // Are any or all of the elements in this constant nonzero?

  octave_value all (int dim = 0) const
    { return rep->all (dim); }

  octave_value any (int dim = 0) const
    { return rep->any (dim); }

  // Other type stuff.

  bool is_bool_type (void) const
    { return rep->is_bool_type (); }

  bool is_real_type (void) const
    { return rep->is_real_type (); }

  bool is_complex_type (void) const
    { return rep->is_complex_type (); }

  bool is_scalar_type (void) const
    { return rep->is_scalar_type (); }

  bool is_matrix_type (void) const
    { return rep->is_matrix_type (); }

  bool is_numeric_type (void) const
    { return rep->is_numeric_type (); }

  bool is_sparse_type (void) const
    { return rep->is_sparse_type (); }

  bool valid_as_scalar_index (void) const
    { return rep->valid_as_scalar_index (); }

  bool valid_as_zero_index (void) const
    { return rep->valid_as_zero_index (); }

  // Does this constant correspond to a truth value?

  bool is_true (void) const
    { return rep->is_true (); }

  // Are the dimensions of this constant zero by zero?

  bool is_zero_by_zero (void) const
    { return (rows () == 0 && columns () == 0); }

  bool is_constant (void) const
    { return rep->is_constant (); }

  bool is_function_handle (void) const
    { return rep->is_function_handle (); }

  bool is_inline_function (void) const
    { return rep->is_inline_function (); }

  bool is_function (void) const
    { return rep->is_function (); }

  bool is_builtin_function (void) const
    { return rep->is_builtin_function (); }

  bool is_dld_function (void) const
    { return rep->is_dld_function (); }

  // Values.

  octave_value eval (void) { return *this; }

  short int
  short_value (bool req_int = false, bool frc_str_conv = false) const
    { return rep->short_value (req_int, frc_str_conv); }

  unsigned short int
  ushort_value (bool req_int = false, bool frc_str_conv = false) const
    { return rep->ushort_value (req_int, frc_str_conv); }

  int int_value (bool req_int = false, bool frc_str_conv = false) const
    { return rep->int_value (req_int, frc_str_conv); }

  unsigned int
  uint_value (bool req_int = false, bool frc_str_conv = false) const
    { return rep->uint_value (req_int, frc_str_conv); }

  int nint_value (bool frc_str_conv = false) const
    { return rep->nint_value (frc_str_conv); }

  long int
  long_value (bool req_int = false, bool frc_str_conv = false) const
    { return rep->long_value (req_int, frc_str_conv); }

  unsigned long int
  ulong_value (bool req_int = false, bool frc_str_conv = false) const
    { return rep->ulong_value (req_int, frc_str_conv); }

  double double_value (bool frc_str_conv = false) const
    { return rep->double_value (frc_str_conv); }

  double scalar_value (bool frc_str_conv = false) const
    { return rep->scalar_value (frc_str_conv); }

  Cell cell_value (void) const;

  Matrix matrix_value (bool frc_str_conv = false) const
    { return rep->matrix_value (frc_str_conv); }

  NDArray array_value (bool frc_str_conv = false) const
    { return rep->array_value (frc_str_conv); }

  Complex complex_value (bool frc_str_conv = false) const
    { return rep->complex_value (frc_str_conv); }

  ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const
    { return rep->complex_matrix_value (frc_str_conv); }

  ComplexNDArray complex_array_value (bool frc_str_conv = false) const
    { return rep->complex_array_value (frc_str_conv); }

  bool bool_value (void) const
    { return rep->bool_value (); }

  boolMatrix bool_matrix_value (void) const
    { return rep->bool_matrix_value (); }

  boolNDArray bool_array_value (void) const
    { return rep->bool_array_value (); }

  charMatrix char_matrix_value (bool frc_str_conv = false) const
    { return rep->char_matrix_value (frc_str_conv); }

  charNDArray char_array_value (bool frc_str_conv = false) const
    { return rep->char_array_value (frc_str_conv); }

  SparseMatrix sparse_matrix_value (bool frc_str_conv = false) const
    { return rep->sparse_matrix_value (frc_str_conv); }

  SparseComplexMatrix sparse_complex_matrix_value (bool frc_str_conv = false) const
    { return rep->sparse_complex_matrix_value (frc_str_conv); }

  SparseBoolMatrix sparse_bool_matrix_value (bool frc_str_conv = false) const
    { return rep->sparse_bool_matrix_value (frc_str_conv); }

  octave_int8 int8_scalar_value (void) const
    { return rep->int8_scalar_value (); }

  octave_int16 int16_scalar_value (void) const
    { return rep->int16_scalar_value (); }

  octave_int32 int32_scalar_value (void) const
    { return rep->int32_scalar_value (); }

  octave_int64 int64_scalar_value (void) const
    { return rep->int64_scalar_value (); }

  octave_uint8 uint8_scalar_value (void) const
    { return rep->uint8_scalar_value (); }

  octave_uint16 uint16_scalar_value (void) const
    { return rep->uint16_scalar_value (); }

  octave_uint32 uint32_scalar_value (void) const
    { return rep->uint32_scalar_value (); }

  octave_uint64 uint64_scalar_value (void) const
    { return rep->uint64_scalar_value (); }

  int8NDArray int8_array_value (void) const
    { return rep->int8_array_value (); }

  int16NDArray int16_array_value (void) const
    { return rep->int16_array_value (); }

  int32NDArray int32_array_value (void) const
    { return rep->int32_array_value (); }

  int64NDArray int64_array_value (void) const
    { return rep->int64_array_value (); }

  uint8NDArray uint8_array_value (void) const
    { return rep->uint8_array_value (); }

  uint16NDArray uint16_array_value (void) const
    { return rep->uint16_array_value (); }

  uint32NDArray uint32_array_value (void) const
    { return rep->uint32_array_value (); }

  uint64NDArray uint64_array_value (void) const
    { return rep->uint64_array_value (); }

  string_vector all_strings (bool pad = false) const
    { return rep->all_strings (pad); }

  std::string string_value (bool force = false) const
    { return rep->string_value (force); }

  Range range_value (void) const
    { return rep->range_value (); }

  Octave_map map_value (void) const;

  string_vector map_keys (void) const
    { return rep->map_keys (); }

  std::streamoff streamoff_value (void) const;

  streamoff_array streamoff_array_value (void) const;

  octave_function *function_value (bool silent = false);

  octave_user_function *user_function_value (bool silent = false);

  octave_fcn_handle *fcn_handle_value (bool silent = false);

  octave_fcn_inline *fcn_inline_value (bool silent = false);

  octave_value_list list_value (void) const;

  ColumnVector column_vector_value (bool frc_str_conv = false,
			     bool frc_vec_conv = false) const;

  ComplexColumnVector
  complex_column_vector_value (bool frc_str_conv = false,
			bool frc_vec_conv = false) const;

  RowVector row_vector_value (bool frc_str_conv = false,
			      bool frc_vec_conv = false) const;

  ComplexRowVector
  complex_row_vector_value (bool frc_str_conv = false,
			    bool frc_vec_conv = false) const;

  Array<int> int_vector_value (bool req_int = false,
			       bool frc_str_conv = false,
			       bool frc_vec_conv = false) const;

  Array<double> vector_value (bool frc_str_conv = false,
			      bool frc_vec_conv = false) const;

  Array<Complex> complex_vector_value (bool frc_str_conv = false,
				       bool frc_vec_conv = false) const;

  // Conversions.  These should probably be private.  If a user of this
  // class wants a certain kind of constant, he should simply ask for
  // it, and we should convert it if possible.

  octave_value convert_to_str (bool pad = false, bool force = false,
			       char type = '"') const
  { return rep->convert_to_str (pad, force, type); }

  octave_value
  convert_to_str_internal (bool pad, bool force, char type) const
    { return rep->convert_to_str_internal (pad, force, type); }

  void convert_to_row_or_column_vector (void)
    { rep->convert_to_row_or_column_vector (); }

  bool print_as_scalar (void) const
    { return rep->print_as_scalar (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const
    { rep->print (os, pr_as_read_syntax); }

  void print_raw (std::ostream& os,
			  bool pr_as_read_syntax = false) const
    { rep->print_raw (os, pr_as_read_syntax); }

  bool print_name_tag (std::ostream& os, const std::string& name) const
    { return rep->print_name_tag (os, name); }

  void print_with_name (std::ostream& os, const std::string& name,
			bool print_padding = true) const
    { rep->print_with_name (os, name, print_padding); }

  int type_id (void) const { return rep->type_id (); }

  std::string type_name (void) const { return rep->type_name (); }

  std::string class_name (void) const { return rep->class_name (); }

  // Unary and binary operations.

  friend octave_value do_unary_op (unary_op op,
				   const octave_value& a);

  const octave_value& do_non_const_unary_op (unary_op op);

  void do_non_const_unary_op (unary_op op, const octave_value_list& idx);

  octave_value do_non_const_unary_op (unary_op op, const std::string& type,
				      const std::list<octave_value_list>& idx);

  friend octave_value do_binary_op (binary_op op,
				    const octave_value& a,
				    const octave_value& b);

  friend octave_value do_cat_op (const octave_value& a,
				 const octave_value& b,
				 const Array<int>& ra_idx);

  const octave_base_value& get_rep (void) const { return *rep; }

  void print_info (std::ostream& os,
			   const std::string& prefix = std::string ()) const;

  bool save_ascii (std::ostream& os, bool& infnan_warned,
			   bool strip_nan_and_inf) 
    { return rep->save_ascii (os, infnan_warned, strip_nan_and_inf); }

  bool load_ascii (std::istream& is)
    { return rep->load_ascii (is); }

  bool save_binary (std::ostream& os, bool& save_as_floats)
    { return rep->save_binary (os, save_as_floats); }

  bool load_binary (std::istream& is, bool swap,
			    oct_mach_info::float_format fmt)
    { return rep->load_binary (is, swap, fmt); }

#if defined (HAVE_HDF5)
  bool save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats)
    { return rep->save_hdf5 (loc_id, name, save_as_floats); }

  bool load_hdf5 (hid_t loc_id, const char *name,
			  bool have_h5giterate_bug)
    { return rep->load_hdf5 (loc_id, name, have_h5giterate_bug); }
#endif

  int write (octave_stream& os, int block_size,
		     oct_data_conv::data_type output_type, int skip,
		     oct_mach_info::float_format flt_fmt) const;

  octave_base_value *internal_rep (void) const { return rep; }

protected:

  // The real representation.
  octave_base_value *rep;

private:

  assign_op unary_op_to_assign_op (unary_op op);

  binary_op op_eq_to_binary_op (assign_op op);

  DECLARE_OCTAVE_ALLOCATOR
};

// Publish externally used friend functions.

extern octave_value
do_unary_op (octave_value::unary_op op, const octave_value& a);

extern octave_value
do_binary_op (octave_value::binary_op op,
	      const octave_value& a, const octave_value& b);

#define OV_UNOP_FN(name) \
  inline octave_value \
  name (const octave_value& a) \
  { \
    return do_unary_op (octave_value::name, a); \
  }

#define OV_UNOP_OP(name, op) \
  inline octave_value \
  operator op (const octave_value& a) \
  { \
    return name (a); \
  }

#define OV_UNOP_FN_OP(name, op) \
  OV_UNOP_FN (name) \
  OV_UNOP_OP (name, op)

OV_UNOP_FN_OP (op_not, !)
OV_UNOP_FN_OP (op_uminus, -)

OV_UNOP_FN (op_transpose)
OV_UNOP_FN (op_hermitian)

// No simple way to define these for prefix and suffix ops?
//
//   incr
//   decr

#define OV_BINOP_FN(name) \
  inline octave_value \
  name (const octave_value& a1, const octave_value& a2) \
  { \
    return do_binary_op (octave_value::name, a1, a2); \
  }

#define OV_BINOP_OP(name, op) \
  inline octave_value \
  operator op (const octave_value& a1, const octave_value& a2) \
  { \
    return name (a1, a2); \
  }

#define OV_BINOP_FN_OP(name, op) \
  OV_BINOP_FN (name) \
  OV_BINOP_OP (name, op)

OV_BINOP_FN_OP (op_add, +)
OV_BINOP_FN_OP (op_sub, -)
OV_BINOP_FN_OP (op_mul, *)
OV_BINOP_FN_OP (op_div, /)

OV_BINOP_FN (op_pow)
OV_BINOP_FN (op_ldiv)
OV_BINOP_FN (op_lshift)
OV_BINOP_FN (op_rshift)

OV_BINOP_FN_OP (op_lt, <)
OV_BINOP_FN_OP (op_le, <=)
OV_BINOP_FN_OP (op_eq, ==)
OV_BINOP_FN_OP (op_ge, >=)
OV_BINOP_FN_OP (op_gt, >)
OV_BINOP_FN_OP (op_ne, !=)

OV_BINOP_FN (op_el_mul)
OV_BINOP_FN (op_el_div)
OV_BINOP_FN (op_el_pow)
OV_BINOP_FN (op_el_ldiv)
OV_BINOP_FN (op_el_and)
OV_BINOP_FN (op_el_or)

OV_BINOP_FN (op_struct_ref)

extern void install_types (void);

// FIXME -- these trait classes probably belong somehwere else...

template <typename T>
class
octave_type_traits
{
public:
  typedef T val_type;
};

#define OCTAVE_TYPE_TRAIT(T, VAL_T) \
  template <> \
  class \
  octave_type_traits<T> \
  { \
  public: \
    typedef VAL_T val_type; \
  }

OCTAVE_TYPE_TRAIT (octave_int8, octave_int8::val_type);
OCTAVE_TYPE_TRAIT (octave_uint8, octave_uint8::val_type);
OCTAVE_TYPE_TRAIT (octave_int16, octave_int16::val_type);
OCTAVE_TYPE_TRAIT (octave_uint16, octave_uint16::val_type);
OCTAVE_TYPE_TRAIT (octave_int32, octave_int32::val_type);
OCTAVE_TYPE_TRAIT (octave_uint32, octave_uint32::val_type);
OCTAVE_TYPE_TRAIT (octave_int64, octave_int64::val_type);
OCTAVE_TYPE_TRAIT (octave_uint64, octave_uint64::val_type);

template <typename T>
class octave_array_type_traits
{
public:
  typedef T element_type;
};

#define OCTAVE_ARRAY_TYPE_TRAIT(T, ELT_T) \
  template <> \
  class \
  octave_array_type_traits<T> \
  { \
  public: \
    typedef ELT_T element_type; \
  }

OCTAVE_ARRAY_TYPE_TRAIT (charNDArray, char);
OCTAVE_ARRAY_TYPE_TRAIT (boolNDArray, bool);
OCTAVE_ARRAY_TYPE_TRAIT (int8NDArray, octave_int8);
OCTAVE_ARRAY_TYPE_TRAIT (uint8NDArray, octave_uint8);
OCTAVE_ARRAY_TYPE_TRAIT (int16NDArray, octave_int16);
OCTAVE_ARRAY_TYPE_TRAIT (uint16NDArray, octave_uint16);
OCTAVE_ARRAY_TYPE_TRAIT (int32NDArray, octave_int32);
OCTAVE_ARRAY_TYPE_TRAIT (uint32NDArray, octave_uint32);
OCTAVE_ARRAY_TYPE_TRAIT (int64NDArray, octave_int64);
OCTAVE_ARRAY_TYPE_TRAIT (uint64NDArray, octave_uint64);
OCTAVE_ARRAY_TYPE_TRAIT (NDArray, double);

// This will eventually go away, but for now it can be used to
// simplify the transition to the new octave_value class hierarchy,
// which uses octave_base_value instead of octave_value for the type
// of octave_value::rep.
#define OV_REP_TYPE octave_base_value

#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
