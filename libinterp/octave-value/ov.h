/*

Copyright (C) 1996-2017 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_ov_h)
#define octave_ov_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>
#include <list>

#include "Range.h"
#include "data-conv.h"
#include "idx-vector.h"
#include "mach-info.h"
#include "mx-base.h"
#include "oct-sort.h"
#include "oct-time.h"
#include "str-vec.h"

class Cell;
class mxArray;
class octave_map;
class octave_scalar_map;
class octave_function;
class octave_user_function;
class octave_fcn_handle;
class octave_fcn_inline;
class octave_value_list;
class octave_lvalue;

#include "oct-stream.h"
#include "ov-base.h"

// Forward declarations of friend functions that have default arguments.

OCTINTERP_API octave_value do_colon_op (const octave_value& base,
                                        const octave_value& limit,
                                        bool is_for_cmd_expr = false);

OCTINTERP_API octave_value do_colon_op (const octave_value& base,
                                        const octave_value& increment,
                                        const octave_value& limit,
                                        bool is_for_cmd_expr = false);

class
OCTINTERP_API
octave_value
{
public:

  enum unary_op
  {
    op_not,            // not
    op_uplus,          // uplus
    op_uminus,         // uminus
    op_transpose,      // transpose
    op_hermitian,      // ctranspose
    op_incr,
    op_decr,
    num_unary_ops,
    unknown_unary_op
  };

  enum binary_op
  {
    op_add,            // plus
    op_sub,            // minus
    op_mul,            // mtimes
    op_div,            // mrdivide
    op_pow,            // mpower
    op_ldiv,           // mldivide
    op_lt,             // lt
    op_le,             // le
    op_eq,             // eq
    op_ge,             // ge
    op_gt,             // gt
    op_ne,             // ne
    op_el_mul,         // times
    op_el_div,         // rdivide
    op_el_pow,         // power
    op_el_ldiv,        // ldivide
    op_el_and,         // and
    op_el_or,          // or
    op_struct_ref,
    num_binary_ops,
    unknown_binary_op
  };

  enum compound_binary_op
  {
    // ** compound operations **
    op_trans_mul,
    op_mul_trans,
    op_herm_mul,
    op_mul_herm,
    op_trans_ldiv,
    op_herm_ldiv,
    op_el_not_and,
    op_el_not_or,
    op_el_and_not,
    op_el_or_not,
    num_compound_binary_ops,
    unknown_compound_binary_op
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
    op_el_mul_eq,
    op_el_div_eq,
    op_el_ldiv_eq,
    op_el_pow_eq,
    op_el_and_eq,
    op_el_or_eq,
    num_assign_ops,
    unknown_assign_op
  };

  static binary_op assign_op_to_binary_op (assign_op);

  static assign_op binary_op_to_assign_op (binary_op);

  static std::string unary_op_as_string (unary_op);
  static std::string unary_op_fcn_name (unary_op);

  static std::string binary_op_as_string (binary_op);
  static std::string binary_op_fcn_name (binary_op);

  static std::string binary_op_fcn_name (compound_binary_op);

  static std::string assign_op_as_string (assign_op);

  static octave_value empty_conv (const std::string& type,
                                  const octave_value& rhs = octave_value ());

  enum magic_colon { magic_colon_t };

  octave_value (void)
    : rep (nil_rep ())
  {
    rep->count++;
  }

  octave_value (short int i);
  octave_value (unsigned short int i);
  octave_value (int i);
  octave_value (unsigned int i);
  octave_value (long int i);
  octave_value (unsigned long int i);

  // FIXME: These are kluges.  They turn into doubles internally, which will
  // break for very large values.  We just use them to store things like
  // 64-bit ino_t, etc, and hope that those values are never actually larger
  // than can be represented exactly in a double.

#if defined (OCTAVE_HAVE_LONG_LONG_INT)
  octave_value (long long int i);
#endif
#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
  octave_value (unsigned long long int i);
#endif

  octave_value (octave::sys::time t);
  octave_value (double d);
  octave_value (float d);
  octave_value (const Array<octave_value>& a, bool is_cs_list = false);
  octave_value (const Cell& c, bool is_cs_list = false);
  octave_value (const Matrix& m, const MatrixType& t = MatrixType ());
  octave_value (const FloatMatrix& m, const MatrixType& t = MatrixType ());
  octave_value (const NDArray& nda);
  octave_value (const FloatNDArray& nda);
  octave_value (const Array<double>& m);
  octave_value (const Array<float>& m);
  octave_value (const DiagMatrix& d);
  octave_value (const DiagArray2<double>& d);
  octave_value (const DiagArray2<float>& d);
  octave_value (const DiagArray2<Complex>& d);
  octave_value (const DiagArray2<FloatComplex>& d);
  octave_value (const FloatDiagMatrix& d);
  octave_value (const RowVector& v);
  octave_value (const FloatRowVector& v);
  octave_value (const ColumnVector& v);
  octave_value (const FloatColumnVector& v);
  octave_value (const Complex& C);
  octave_value (const FloatComplex& C);
  octave_value (const ComplexMatrix& m, const MatrixType& t = MatrixType ());
  octave_value (const FloatComplexMatrix& m,
                const MatrixType& t = MatrixType ());
  octave_value (const ComplexNDArray& cnda);
  octave_value (const FloatComplexNDArray& cnda);
  octave_value (const Array<Complex>& m);
  octave_value (const Array<FloatComplex>& m);
  octave_value (const ComplexDiagMatrix& d);
  octave_value (const FloatComplexDiagMatrix& d);
  octave_value (const ComplexRowVector& v);
  octave_value (const FloatComplexRowVector& v);
  octave_value (const ComplexColumnVector& v);
  octave_value (const FloatComplexColumnVector& v);
  octave_value (const PermMatrix& p);
  octave_value (bool b);
  octave_value (const boolMatrix& bm, const MatrixType& t = MatrixType ());
  octave_value (const boolNDArray& bnda);
  octave_value (const Array<bool>& bnda);
  octave_value (char c, char type = '\'');
  octave_value (const char *s, char type = '\'');
  octave_value (const std::string& s, char type = '\'');
  octave_value (const string_vector& s, char type = '\'');
  octave_value (const charMatrix& chm,  char type = '\'');
  octave_value (const charNDArray& chnda, char type = '\'');
  octave_value (const Array<char>& chnda, char type = '\'');

  OCTAVE_DEPRECATED ("note: IS_STRING argument is ignored")
  octave_value (const charMatrix& chm, bool is_string, char type = '\'');

  OCTAVE_DEPRECATED ("note: IS_STRING argument is ignored")
  octave_value (const charNDArray& chnda, bool is_string, char type = '\'');

  OCTAVE_DEPRECATED ("note: IS_STRING argument is ignored")
  octave_value (const Array<char>& chnda, bool is_string, char type = '\'');

  octave_value (const SparseMatrix& m, const MatrixType& t = MatrixType ());
  octave_value (const Sparse<double>& m, const MatrixType& t = MatrixType ());
  octave_value (const SparseComplexMatrix& m,
                const MatrixType& t = MatrixType ());
  octave_value (const Sparse<Complex>& m, const MatrixType& t = MatrixType ());
  octave_value (const SparseBoolMatrix& bm,
                const MatrixType& t = MatrixType ());
  octave_value (const Sparse<bool>& m, const MatrixType& t = MatrixType ());
  octave_value (const octave_int8& i);
  octave_value (const octave_int16& i);
  octave_value (const octave_int32& i);
  octave_value (const octave_int64& i);
  octave_value (const octave_uint8& i);
  octave_value (const octave_uint16& i);
  octave_value (const octave_uint32& i);
  octave_value (const octave_uint64& i);
  octave_value (const int8NDArray& inda);
  octave_value (const Array<octave_int8>& inda);
  octave_value (const int16NDArray& inda);
  octave_value (const Array<octave_int16>& inda);
  octave_value (const int32NDArray& inda);
  octave_value (const Array<octave_int32>& inda);
  octave_value (const int64NDArray& inda);
  octave_value (const Array<octave_int64>& inda);
  octave_value (const uint8NDArray& inda);
  octave_value (const Array<octave_uint8>& inda);
  octave_value (const uint16NDArray& inda);
  octave_value (const Array<octave_uint16>& inda);
  octave_value (const uint32NDArray& inda);
  octave_value (const Array<octave_uint32>& inda);
  octave_value (const uint64NDArray& inda);
  octave_value (const Array<octave_uint64>& inda);
  octave_value (const Array<octave_idx_type>& inda,
                bool zero_based = false, bool cache_index = false);
  octave_value (const Array<std::string>& cellstr);
  octave_value (const idx_vector& idx, bool lazy = true);
  octave_value (double base, double limit, double inc);
  octave_value (const Range& r, bool force_range = false);
  octave_value (const octave_map& m);
  octave_value (const octave_scalar_map& m);
  octave_value (const octave_map& m, const std::string& id,
                const std::list<std::string>& plist);
  octave_value (const octave_scalar_map& m, const std::string& id,
                const std::list<std::string>& plist);

  OCTAVE_DEPRECATED ("note: second argument is always ignored; use octave_value (const octave_value_list&) instead")
  octave_value (const octave_value_list& m, bool);

  octave_value (const octave_value_list& m);

  octave_value (octave_value::magic_colon);

  octave_value (octave_base_value *new_rep, bool borrow = false);

  OCTAVE_DEPRECATED ("note: in the future there will be no way to directly set reference count")
  octave_value (octave_base_value *new_rep, int xcount);

  // Copy constructor.

  octave_value (const octave_value& a)
  {
    rep = a.rep;
    rep->count++;
  }

  // This should only be called for derived types.

  octave_base_value * clone (void) const;

  octave_base_value * empty_clone (void) const
  { return rep->empty_clone (); }

  // Delete the representation of this constant if the count drops to zero.

  ~octave_value (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  void make_unique (void)
  {
    if (rep->count > 1)
      {
        octave_base_value *r = rep->unique_clone ();

        if (--rep->count == 0)
          delete rep;

        rep = r;
      }
  }

  // This uniquifies the value if it is referenced by more than a certain
  // number of shallow copies.  This is useful for optimizations where we
  // know a certain copy, typically within a cell array, to be obsolete.
  void make_unique (int obsolete_copies)
  {
    if (rep->count > obsolete_copies + 1)
      {
        octave_base_value *r = rep->unique_clone ();

        if (--rep->count == 0)
          delete rep;

        rep = r;
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

  octave_idx_type get_count (void) const { return rep->count; }

  octave_base_value::type_conv_info numeric_conversion_function (void) const
  { return rep->numeric_conversion_function (); }

  octave_base_value::type_conv_info numeric_demotion_function (void) const
  { return rep->numeric_demotion_function (); }

  void maybe_mutate (void);

  octave_value squeeze (void) const
  { return rep->squeeze (); }

  // The result of full().
  octave_value full_value (void) const
  { return rep->full_value (); }

  // Type conversions.

  octave_value as_double (void) const { return rep->as_double (); }
  octave_value as_single (void) const { return rep->as_single (); }

  octave_value as_int8 (void) const { return rep->as_int8 (); }
  octave_value as_int16 (void) const { return rep->as_int16 (); }
  octave_value as_int32 (void) const { return rep->as_int32 (); }
  octave_value as_int64 (void) const { return rep->as_int64 (); }

  octave_value as_uint8 (void) const { return rep->as_uint8 (); }
  octave_value as_uint16 (void) const { return rep->as_uint16 (); }
  octave_value as_uint32 (void) const { return rep->as_uint32 (); }
  octave_value as_uint64 (void) const { return rep->as_uint64 (); }

  octave_base_value * try_narrowing_conversion (void)
  { return rep->try_narrowing_conversion (); }

  // Close to dims (), but can be overloaded for classes.
  Matrix size (void)
  { return rep->size (); }

  octave_idx_type numel (const octave_value_list& idx)
  { return rep->numel (idx); }

  octave_value single_subsref (const std::string& type,
                               const octave_value_list& idx);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  { return rep->subsref (type, idx); }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        bool auto_add)
  { return rep->subsref (type, idx, auto_add); }

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

  octave_value next_subsref (bool auto_add, const std::string& type, const
                             std::list<octave_value_list>& idx,
                             size_t skip = 1);

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false)
  { return rep->do_index_op (idx, resize_ok); }

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  octave_value undef_subsasgn (const std::string& type,
                               const std::list<octave_value_list>& idx,
                               const octave_value& rhs);

  octave_value& assign (assign_op op, const std::string& type,
                        const std::list<octave_value_list>& idx,
                        const octave_value& rhs);

  octave_value& assign (assign_op, const octave_value& rhs);

  idx_vector index_vector (bool require_integers = false) const
  {
    return rep->index_vector (require_integers);
  }

  // Size.

  dim_vector dims (void) const
  { return rep->dims (); }

  octave_idx_type rows (void) const { return rep->rows (); }

  octave_idx_type columns (void) const { return rep->columns (); }

  octave_idx_type length (void) const;

  int ndims (void) const { return rep->ndims (); }

  bool all_zero_dims (void) const { return dims ().all_zero (); }

  octave_idx_type numel (void) const
  { return rep->numel (); }

  OCTAVE_DEPRECATED ("use 'numel' instead")
  octave_idx_type capacity (void) const
  { return rep->numel (); }

  size_t byte_size (void) const
  { return rep->byte_size (); }

  octave_idx_type nnz (void) const { return rep->nnz (); }

  octave_idx_type nzmax (void) const { return rep->nzmax (); }

  octave_idx_type nfields (void) const { return rep->nfields (); }

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

  // Does this constant have a type?  Both of these are provided since it is
  // sometimes more natural to write is_undefined() instead of ! is_defined().

  bool is_defined (void) const
  { return rep->is_defined (); }

  bool is_undefined (void) const
  { return ! is_defined (); }

  bool isempty (void) const
  { return rep->isempty (); }

  OCTAVE_DEPRECATED ("use 'isempty' instead")
  bool is_empty (void) const
  { return rep->isempty (); }

  bool iscell (void) const
  { return rep->iscell (); }

  OCTAVE_DEPRECATED ("use 'iscell' instead")
  bool is_cell (void) const
  { return rep->iscell (); }

  bool iscellstr (void) const
  { return rep->iscellstr (); }

  OCTAVE_DEPRECATED ("use 'iscellstr' instead")
  bool is_cellstr (void) const
  { return iscellstr (); }

  bool is_real_scalar (void) const
  { return rep->is_real_scalar (); }

  bool is_real_matrix (void) const
  { return rep->is_real_matrix (); }

  bool is_complex_scalar (void) const
  { return rep->is_complex_scalar (); }

  bool is_complex_matrix (void) const
  { return rep->is_complex_matrix (); }

  bool is_bool_scalar (void) const
  { return rep->is_bool_scalar (); }

  bool is_bool_matrix (void) const
  { return rep->is_bool_matrix (); }

  bool is_char_matrix (void) const
  { return rep->is_char_matrix (); }

  bool is_diag_matrix (void) const
  { return rep->is_diag_matrix (); }

  bool is_perm_matrix (void) const
  { return rep->is_perm_matrix (); }

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

  bool is_object (void) const
  { return rep->is_object (); }

  bool is_classdef_object (void) const
  { return rep->is_classdef_object (); }

  bool isjava (void) const
  { return rep->isjava (); }

  OCTAVE_DEPRECATED ("use 'isjava' instead")
  bool is_java (void) const
  { return isjava (); }

  bool is_cs_list (void) const
  { return rep->is_cs_list (); }

  bool is_magic_colon (void) const
  { return rep->is_magic_colon (); }

  bool is_null_value (void) const
  { return rep->is_null_value (); }

  // Are any or all of the elements in this constant nonzero?

  octave_value all (int dim = 0) const
  { return rep->all (dim); }

  octave_value any (int dim = 0) const
  { return rep->any (dim); }

  builtin_type_t builtin_type (void) const
  { return rep->builtin_type (); }

  // Floating point types.

  bool is_double_type (void) const
  { return rep->is_double_type (); }

  bool is_single_type (void) const
  { return rep->is_single_type (); }

  bool is_float_type (void) const
  { return rep->is_float_type (); }

  // Integer types.

  bool is_int8_type (void) const
  { return rep->is_int8_type (); }

  bool is_int16_type (void) const
  { return rep->is_int16_type (); }

  bool is_int32_type (void) const
  { return rep->is_int32_type (); }

  bool is_int64_type (void) const
  { return rep->is_int64_type (); }

  bool is_uint8_type (void) const
  { return rep->is_uint8_type (); }

  bool is_uint16_type (void) const
  { return rep->is_uint16_type (); }

  bool is_uint32_type (void) const
  { return rep->is_uint32_type (); }

  bool is_uint64_type (void) const
  { return rep->is_uint64_type (); }

  // Other type stuff.

  bool islogical (void) const
  { return rep->islogical (); }

  OCTAVE_DEPRECATED ("use 'islogical' instead")
  bool is_bool_type (void) const
  { return rep->islogical (); }

  bool isinteger (void) const
  { return rep->isinteger (); }

  OCTAVE_DEPRECATED ("use 'isinteger' instead")
  bool is_integer_type (void) const
  { return rep->isinteger (); }

  bool is_real_type (void) const
  { return rep->is_real_type (); }

  bool iscomplex (void) const
  { return rep->iscomplex (); }

  OCTAVE_DEPRECATED ("use 'iscomplex' instead")
  bool is_complex_type (void) const
  { return rep->iscomplex (); }

  bool is_scalar_type (void) const
  { return rep->is_scalar_type (); }

  bool is_matrix_type (void) const
  { return rep->is_matrix_type (); }

  bool is_numeric_type (void) const
  { return rep->is_numeric_type (); }

  bool is_sparse_type (void) const
  { return rep->is_sparse_type (); }

  // Does this constant correspond to a truth value?

  bool is_true (void) const
  { return rep->is_true (); }

  // Do two constants match (in a switch statement)?

  bool is_equal (const octave_value&) const;

  // Are the dimensions of this constant zero by zero?

  bool is_zero_by_zero (void) const
  { return (ndims () == 2 && rows () == 0 && columns () == 0); }

  bool is_constant (void) const
  { return rep->is_constant (); }

  bool is_function_handle (void) const
  { return rep->is_function_handle (); }

  bool is_anonymous_function (void) const
  { return rep->is_anonymous_function (); }

  bool is_inline_function (void) const
  { return rep->is_inline_function (); }

  bool is_function (void) const
  { return rep->is_function (); }

  bool is_user_script (void) const
  { return rep->is_user_script (); }

  bool is_user_function (void) const
  { return rep->is_user_function (); }

  bool is_user_code (void) const
  { return rep->is_user_code (); }

  bool is_builtin_function (void) const
  { return rep->is_builtin_function (); }

  bool is_dld_function (void) const
  { return rep->is_dld_function (); }

  bool is_mex_function (void) const
  { return rep->is_mex_function (); }

  void erase_subfunctions (void) { rep->erase_subfunctions (); }

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

  int64_t
  int64_value (bool req_int = false, bool frc_str_conv = false) const
  { return rep->int64_value (req_int, frc_str_conv); }

  uint64_t
  uint64_value (bool req_int = false, bool frc_str_conv = false) const
  { return rep->uint64_value (req_int, frc_str_conv); }

  octave_idx_type
  idx_type_value (bool req_int = false, bool frc_str_conv = false) const;

  double double_value (bool frc_str_conv = false) const
  { return rep->double_value (frc_str_conv); }

  float float_value (bool frc_str_conv = false) const
  { return rep->float_value (frc_str_conv); }

  double scalar_value (bool frc_str_conv = false) const
  { return rep->scalar_value (frc_str_conv); }

  float float_scalar_value (bool frc_str_conv = false) const
  { return rep->float_scalar_value (frc_str_conv); }

  Matrix matrix_value (bool frc_str_conv = false) const
  { return rep->matrix_value (frc_str_conv); }

  FloatMatrix float_matrix_value (bool frc_str_conv = false) const
  { return rep->float_matrix_value (frc_str_conv); }

  NDArray array_value (bool frc_str_conv = false) const
  { return rep->array_value (frc_str_conv); }

  FloatNDArray float_array_value (bool frc_str_conv = false) const
  { return rep->float_array_value (frc_str_conv); }

  Complex complex_value (bool frc_str_conv = false) const
  { return rep->complex_value (frc_str_conv); }

  FloatComplex float_complex_value (bool frc_str_conv = false) const
  { return rep->float_complex_value (frc_str_conv); }

  ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const
  { return rep->complex_matrix_value (frc_str_conv); }

  FloatComplexMatrix
  float_complex_matrix_value (bool frc_str_conv = false) const
  { return rep->float_complex_matrix_value (frc_str_conv); }

  ComplexNDArray complex_array_value (bool frc_str_conv = false) const
  { return rep->complex_array_value (frc_str_conv); }

  FloatComplexNDArray
  float_complex_array_value (bool frc_str_conv = false) const
  { return rep->float_complex_array_value (frc_str_conv); }

  bool bool_value (bool warn = false) const
  { return rep->bool_value (warn); }

  boolMatrix bool_matrix_value (bool warn = false) const
  { return rep->bool_matrix_value (warn); }

  boolNDArray bool_array_value (bool warn = false) const
  { return rep->bool_array_value (warn); }

  charMatrix char_matrix_value (bool frc_str_conv = false) const
  { return rep->char_matrix_value (frc_str_conv); }

  charNDArray char_array_value (bool frc_str_conv = false) const
  { return rep->char_array_value (frc_str_conv); }

  SparseMatrix sparse_matrix_value (bool frc_str_conv = false) const
  { return rep->sparse_matrix_value (frc_str_conv); }

  SparseComplexMatrix
  sparse_complex_matrix_value (bool frc_str_conv = false) const
  { return rep->sparse_complex_matrix_value (frc_str_conv); }

  SparseBoolMatrix sparse_bool_matrix_value (bool warn = false) const
  { return rep->sparse_bool_matrix_value (warn); }

  DiagMatrix diag_matrix_value (bool force = false) const
  { return rep->diag_matrix_value (force); }

  FloatDiagMatrix float_diag_matrix_value (bool force = false) const
  { return rep->float_diag_matrix_value (force); }

  ComplexDiagMatrix complex_diag_matrix_value (bool force = false) const
  { return rep->complex_diag_matrix_value (force); }

  FloatComplexDiagMatrix
  float_complex_diag_matrix_value (bool force = false) const
  { return rep->float_complex_diag_matrix_value (force); }

  PermMatrix perm_matrix_value (void) const
  { return rep->perm_matrix_value (); }

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

  std::string string_value (bool force = false) const
  { return rep->string_value (force); }

  string_vector string_vector_value (bool pad = false) const
  { return rep->string_vector_value (pad); }

  OCTAVE_DEPRECATED ("use 'string_vector_value' instead")
  string_vector all_strings (bool pad = false) const
  { return string_vector_value (pad); }

  Cell cell_value (void) const;

  Array<std::string> cellstr_value (void) const
  { return rep->cellstr_value (); }

  Range range_value (void) const
  { return rep->range_value (); }

  octave_map map_value (void) const;

  octave_scalar_map scalar_map_value (void) const;

  string_vector map_keys (void) const
  { return rep->map_keys (); }

  size_t nparents (void) const
  { return rep->nparents (); }

  std::list<std::string> parent_class_name_list (void) const
  { return rep->parent_class_name_list (); }

  string_vector parent_class_names (void) const
  { return rep->parent_class_names (); }

  octave_base_value *
  find_parent_class (const std::string& parent_class_name)
  { return rep->find_parent_class (parent_class_name); }

  bool is_instance_of (const std::string& cls_name) const
  { return rep->is_instance_of (cls_name); }

  octave_function * function_value (bool silent = false) const;

  octave_user_function * user_function_value (bool silent = false) const;

  octave_user_script * user_script_value (bool silent = false) const;

  octave_user_code * user_code_value (bool silent = false) const;

  octave_fcn_handle * fcn_handle_value (bool silent = false) const;

  octave_fcn_inline * fcn_inline_value (bool silent = false) const;

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

  FloatColumnVector float_column_vector_value (bool frc_str_conv = false,
                                               bool frc_vec_conv = false) const;

  FloatComplexColumnVector
  float_complex_column_vector_value (bool frc_str_conv = false,
                                     bool frc_vec_conv = false) const;

  FloatRowVector float_row_vector_value (bool frc_str_conv = false,
                                         bool frc_vec_conv = false) const;

  FloatComplexRowVector
  float_complex_row_vector_value (bool frc_str_conv = false,
                                  bool frc_vec_conv = false) const;

  Array<int> int_vector_value (bool req_int = false,
                               bool frc_str_conv = false,
                               bool frc_vec_conv = false) const;

  Array<octave_idx_type>
  octave_idx_type_vector_value (bool req_int = false,
                                bool frc_str_conv = false,
                                bool frc_vec_conv = false) const;

  Array<double> vector_value (bool frc_str_conv = false,
                              bool frc_vec_conv = false) const;

  Array<Complex> complex_vector_value (bool frc_str_conv = false,
                                       bool frc_vec_conv = false) const;

  Array<float> float_vector_value (bool frc_str_conv = false,
                                   bool frc_vec_conv = false) const;

  Array<FloatComplex>
  float_complex_vector_value (bool frc_str_conv = false,
                              bool frc_vec_conv = false) const;

  // Extract values of specific types without any implicit type conversions.
  // Throw an error if an object is the wrong type for the requested value
  // extraction.
  //
  // These functions are intended to provide a simple way to extract values of
  // specific types and display error messages that are more meaningful than
  // the generic "error: wrong type argument 'cell'" message.

  short int xshort_value (const char *fmt, ...) const;

  unsigned short int xushort_value (const char *fmt, ...) const;

  int xint_value (const char *fmt, ...) const;

  unsigned int xuint_value (const char *fmt, ...) const;

  int xnint_value (const char *fmt, ...) const;

  long int xlong_value (const char *fmt, ...) const;

  unsigned long int xulong_value (const char *fmt, ...) const;

  int64_t xint64_value (const char *fmt, ...) const;

  uint64_t xuint64_value (const char *fmt, ...) const;

  octave_idx_type xidx_type_value (const char *fmt, ...) const;

  double xdouble_value (const char *fmt, ...) const;

  float xfloat_value (const char *fmt, ...) const;

  double xscalar_value (const char *fmt, ...) const;

  float xfloat_scalar_value (const char *fmt, ...) const;

  Matrix xmatrix_value (const char *fmt, ...) const;

  FloatMatrix xfloat_matrix_value (const char *fmt, ...) const;

  NDArray xarray_value (const char *fmt, ...) const;

  FloatNDArray xfloat_array_value (const char *fmt, ...) const;

  Complex xcomplex_value (const char *fmt, ...) const;

  FloatComplex xfloat_complex_value (const char *fmt, ...) const;

  ComplexMatrix xcomplex_matrix_value (const char *fmt, ...) const;

  FloatComplexMatrix xfloat_complex_matrix_value (const char *fmt, ...) const;

  ComplexNDArray xcomplex_array_value (const char *fmt, ...) const;

  FloatComplexNDArray xfloat_complex_array_value (const char *fmt, ...) const;

  bool xbool_value (const char *fmt, ...) const;

  boolMatrix xbool_matrix_value (const char *fmt, ...) const;

  boolNDArray xbool_array_value (const char *fmt, ...) const;

  charMatrix xchar_matrix_value (const char *fmt, ...) const;

  charNDArray xchar_array_value (const char *fmt, ...) const;

  SparseMatrix xsparse_matrix_value (const char *fmt, ...) const;

  SparseComplexMatrix xsparse_complex_matrix_value (const char *fmt, ...) const;

  SparseBoolMatrix xsparse_bool_matrix_value (const char *fmt, ...) const;

  DiagMatrix xdiag_matrix_value (const char *fmt, ...) const;

  FloatDiagMatrix xfloat_diag_matrix_value (const char *fmt, ...) const;

  ComplexDiagMatrix xcomplex_diag_matrix_value (const char *fmt, ...) const;

  FloatComplexDiagMatrix xfloat_complex_diag_matrix_value (const char *fmt, ...) const;

  PermMatrix xperm_matrix_value (const char *fmt, ...) const;

  octave_int8 xint8_scalar_value (const char *fmt, ...) const;

  octave_int16 xint16_scalar_value (const char *fmt, ...) const;

  octave_int32 xint32_scalar_value (const char *fmt, ...) const;

  octave_int64 xint64_scalar_value (const char *fmt, ...) const;

  octave_uint8 xuint8_scalar_value (const char *fmt, ...) const;

  octave_uint16 xuint16_scalar_value (const char *fmt, ...) const;

  octave_uint32 xuint32_scalar_value (const char *fmt, ...) const;

  octave_uint64 xuint64_scalar_value (const char *fmt, ...) const;

  int8NDArray xint8_array_value (const char *fmt, ...) const;

  int16NDArray xint16_array_value (const char *fmt, ...) const;

  int32NDArray xint32_array_value (const char *fmt, ...) const;

  int64NDArray xint64_array_value (const char *fmt, ...) const;

  uint8NDArray xuint8_array_value (const char *fmt, ...) const;

  uint16NDArray xuint16_array_value (const char *fmt, ...) const;

  uint32NDArray xuint32_array_value (const char *fmt, ...) const;

  uint64NDArray xuint64_array_value (const char *fmt, ...) const;

  std::string xstring_value (const char *fmt, ...) const;

  string_vector xstring_vector_value (const char *fmt, ...) const;

  Cell xcell_value (const char *fmt, ...) const;

  Array<std::string> xcellstr_value (const char *fmt, ...) const;

  Range xrange_value (const char *fmt, ...) const;

  octave_map xmap_value (const char *fmt, ...) const;

  octave_scalar_map xscalar_map_value (const char *fmt, ...) const;

  ColumnVector xcolumn_vector_value (const char *fmt, ...) const;

  ComplexColumnVector
  xcomplex_column_vector_value (const char *fmt, ...) const;

  RowVector xrow_vector_value (const char *fmt, ...) const;

  ComplexRowVector xcomplex_row_vector_value (const char *fmt, ...) const;

  FloatColumnVector xfloat_column_vector_value (const char *fmt, ...) const;

  FloatComplexColumnVector
  xfloat_complex_column_vector_value (const char *fmt, ...) const;

  FloatRowVector xfloat_row_vector_value (const char *fmt, ...) const;

  FloatComplexRowVector
  xfloat_complex_row_vector_value (const char *fmt, ...) const;

  Array<int> xint_vector_value (const char *fmt, ...) const;

  Array<octave_idx_type>
  xoctave_idx_type_vector_value (const char *fmt, ...) const;

  Array<double> xvector_value (const char *fmt, ...) const;

  Array<Complex> xcomplex_vector_value (const char *fmt, ...) const;

  Array<float> xfloat_vector_value (const char *fmt, ...) const;

  Array<FloatComplex> xfloat_complex_vector_value (const char *fmt, ...) const;

  octave_function * xfunction_value (const char *fmt, ...) const;
  octave_user_function * xuser_function_value (const char *fmt, ...) const;
  octave_user_script * xuser_script_value (const char *fmt, ...) const;
  octave_user_code * xuser_code_value (const char *fmt, ...) const;
  octave_fcn_handle * xfcn_handle_value (const char *fmt, ...) const;
  octave_fcn_inline * xfcn_inline_value (const char *fmt, ...) const;

  octave_value_list xlist_value (const char *fmt, ...) const;

  // Possibly economize a lazy-indexed value.

  void maybe_economize (void)
  { rep->maybe_economize (); }

  // The following two hook conversions are called on any octave_value prior to
  // storing it to a "permanent" location, like a named variable, a cell or a
  // struct component, or a return value of a function.

  octave_value storable_value (void) const;

  // Ditto, but in place, i.e., equivalent to *this = this->storable_value (),
  // but possibly more efficient.

  void make_storable_value (void);

  // FIXME: These should probably be private.
  // Conversions.  If a user of this class wants a certain kind of constant,
  // he should simply ask for it, and we should convert it if possible.

  octave_value convert_to_str (bool pad = false, bool force = false,
                               char type = '\'') const
  { return rep->convert_to_str (pad, force, type); }

  octave_value
  convert_to_str_internal (bool pad, bool force, char type) const
  { return rep->convert_to_str_internal (pad, force, type); }

  void convert_to_row_or_column_vector (void)
  { rep->convert_to_row_or_column_vector (); }

  bool print_as_scalar (void) const
  { return rep->print_as_scalar (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false)
  { rep->print (os, pr_as_read_syntax); }

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const
  { rep->print_raw (os, pr_as_read_syntax); }

  bool print_name_tag (std::ostream& os, const std::string& name) const
  { return rep->print_name_tag (os, name); }

  void print_with_name (std::ostream& os, const std::string& name) const
  { rep->print_with_name (os, name, true); }

  void short_disp (std::ostream& os) const { rep->short_disp (os); }

  int type_id (void) const { return rep->type_id (); }

  std::string type_name (void) const { return rep->type_name (); }

  std::string class_name (void) const { return rep->class_name (); }

  // Unary and binary operations.

  friend OCTINTERP_API octave_value do_unary_op (unary_op op,
                                                 const octave_value& a);

  octave_value& do_non_const_unary_op (unary_op op);

  octave_value& do_non_const_unary_op (unary_op op, const std::string& type,
                                       const std::list<octave_value_list>& idx);

  friend OCTINTERP_API octave_value do_binary_op (binary_op op,
                                                  const octave_value& a,
                                                  const octave_value& b);

  friend OCTINTERP_API octave_value do_binary_op (compound_binary_op op,
                                                  const octave_value& a,
                                                  const octave_value& b);

  friend OCTINTERP_API octave_value do_cat_op (const octave_value& a,
                                               const octave_value& b,
                                               const Array<octave_idx_type>& ra_idx);

  friend OCTINTERP_API octave_value do_colon_op (const octave_value& base,
                                                 const octave_value& limit,
                                                 bool is_for_cmd_expr)
  {
    return do_colon_op (base, octave_value (), limit, is_for_cmd_expr);
  }

  friend OCTINTERP_API octave_value do_colon_op (const octave_value& base,
                                                 const octave_value& increment,
                                                 const octave_value& limit,
                                                 bool is_for_cmd_expr);

  const octave_base_value& get_rep (void) const { return *rep; }

  bool is_copy_of (const octave_value& val) const { return rep == val.rep; }

  void print_info (std::ostream& os,
                   const std::string& prefix = "") const;

  bool save_ascii (std::ostream& os) { return rep->save_ascii (os); }

  bool load_ascii (std::istream& is) { return rep->load_ascii (is); }

  bool save_binary (std::ostream& os, bool& save_as_floats)
  { return rep->save_binary (os, save_as_floats); }

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt)
  { return rep->load_binary (is, swap, fmt); }

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                  bool save_as_floats)
  { return rep->save_hdf5 (loc_id, name, save_as_floats); }

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name)
  { return rep->load_hdf5 (loc_id, name); }

  int write (octave::stream& os, int block_size,
             oct_data_conv::data_type output_type, int skip,
             octave::mach_info::float_format flt_fmt) const;

  octave_base_value * internal_rep (void) const { return rep; }

  // Unsafe.  These functions exist to support the MEX interface.
  // You should not use them anywhere else.
  void * mex_get_data (void) const { return rep->mex_get_data (); }

  octave_idx_type * mex_get_ir (void) const { return rep->mex_get_ir (); }

  octave_idx_type * mex_get_jc (void) const { return rep->mex_get_jc (); }

  mxArray * as_mxArray (void) const { return rep->as_mxArray (); }

  octave_value diag (octave_idx_type k = 0) const
  { return rep->diag (k); }

  octave_value diag (octave_idx_type m, octave_idx_type n) const
  { return rep->diag (m, n); }

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
  { return rep->sort (dim, mode); }
  octave_value sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
                     sortmode mode = ASCENDING) const
  { return rep->sort (sidx, dim, mode); }

  sortmode is_sorted (sortmode mode = UNSORTED) const
  { return rep->is_sorted (mode); }

  Array<octave_idx_type> sort_rows_idx (sortmode mode = ASCENDING) const
  { return rep->sort_rows_idx (mode); }

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const
  { return rep->is_sorted_rows (mode); }

  void lock (void) { rep->lock (); }

  void unlock (void) { rep->unlock (); }

  bool islocked (void) const { return rep->islocked (); }

  void dump (std::ostream& os) const { rep->dump (os); }

#define MAPPER_FORWARD(F) \
  octave_value F (void) const                           \
  {                                                     \
    return rep->map (octave_base_value::umap_ ## F);    \
  }

  MAPPER_FORWARD (abs)
  MAPPER_FORWARD (acos)
  MAPPER_FORWARD (acosh)
  MAPPER_FORWARD (angle)
  MAPPER_FORWARD (arg)
  MAPPER_FORWARD (asin)
  MAPPER_FORWARD (asinh)
  MAPPER_FORWARD (atan)
  MAPPER_FORWARD (atanh)
  MAPPER_FORWARD (cbrt)
  MAPPER_FORWARD (ceil)
  MAPPER_FORWARD (conj)
  MAPPER_FORWARD (cos)
  MAPPER_FORWARD (cosh)
  MAPPER_FORWARD (erf)
  MAPPER_FORWARD (erfinv)
  MAPPER_FORWARD (erfcinv)
  MAPPER_FORWARD (erfc)
  MAPPER_FORWARD (erfcx)
  MAPPER_FORWARD (erfi)
  MAPPER_FORWARD (dawson)
  MAPPER_FORWARD (exp)
  MAPPER_FORWARD (expm1)
  MAPPER_FORWARD (isfinite)
  MAPPER_FORWARD (fix)
  MAPPER_FORWARD (floor)
  MAPPER_FORWARD (gamma)
  MAPPER_FORWARD (imag)
  MAPPER_FORWARD (isinf)
  MAPPER_FORWARD (isna)
  MAPPER_FORWARD (isnan)
  MAPPER_FORWARD (lgamma)
  MAPPER_FORWARD (log)
  MAPPER_FORWARD (log2)
  MAPPER_FORWARD (log10)
  MAPPER_FORWARD (log1p)
  MAPPER_FORWARD (real)
  MAPPER_FORWARD (round)
  MAPPER_FORWARD (roundb)
  MAPPER_FORWARD (signum)
  MAPPER_FORWARD (sin)
  MAPPER_FORWARD (sinh)
  MAPPER_FORWARD (sqrt)
  MAPPER_FORWARD (tan)
  MAPPER_FORWARD (tanh)

  // These functions are prefixed with X to avoid potential macro conflicts.

  MAPPER_FORWARD (xisalnum)
  MAPPER_FORWARD (xisalpha)
  MAPPER_FORWARD (xisascii)
  MAPPER_FORWARD (xiscntrl)
  MAPPER_FORWARD (xisdigit)
  MAPPER_FORWARD (xisgraph)
  MAPPER_FORWARD (xislower)
  MAPPER_FORWARD (xisprint)
  MAPPER_FORWARD (xispunct)
  MAPPER_FORWARD (xisspace)
  MAPPER_FORWARD (xisupper)
  MAPPER_FORWARD (xisxdigit)
  MAPPER_FORWARD (xsignbit)
  MAPPER_FORWARD (xtoascii)
  MAPPER_FORWARD (xtolower)
  MAPPER_FORWARD (xtoupper)

#undef MAPPER_FORWARD

  octave_value map (octave_base_value::unary_mapper_t umap) const
  { return rep->map (umap); }

  // Extract the n-th element, aka val(n).
  // Result is undefined if val is not an array type or n is out of range.
  // Never error.
  octave_value
  fast_elem_extract (octave_idx_type n) const
  { return rep->fast_elem_extract (n); }

  // Assign the n-th element, aka val(n) = x.
  // Return false if val is not an array type, x is not a matching scalar type,
  // or n is out of range.
  // Never error.
  virtual bool
  fast_elem_insert (octave_idx_type n, const octave_value& x)
  {
    make_unique ();
    return rep->fast_elem_insert (n, x);
  }

protected:

  // The real representation.
  octave_base_value *rep;

private:

  static octave_base_value *nil_rep (void);

  assign_op unary_op_to_assign_op (unary_op op);

  binary_op op_eq_to_binary_op (assign_op op);

  // This declaration protects against constructing octave_value from
  // const octave_base_value* which actually silently calls octave_value (bool).
  octave_value (const octave_base_value *);

};

// Publish externally used friend functions.

extern OCTINTERP_API octave_value
do_unary_op (octave_value::unary_op op, const octave_value& a);

extern OCTINTERP_API octave_value
do_binary_op (octave_value::binary_op op,
              const octave_value& a, const octave_value& b);

extern OCTINTERP_API octave_value
do_binary_op (octave_value::compound_binary_op op,
              const octave_value& a, const octave_value& b);

#define OV_UNOP_FN(name)                        \
  inline octave_value                           \
  name (const octave_value& a)                  \
  {                                             \
    return do_unary_op (octave_value::name, a); \
  }

#define OV_UNOP_OP(name, op)                    \
  inline octave_value                           \
  operator op (const octave_value& a)           \
  {                                             \
    return name (a);                            \
  }

#define OV_UNOP_FN_OP(name, op)                 \
  OV_UNOP_FN (name)                             \
  OV_UNOP_OP (name, op)

OV_UNOP_FN_OP (op_not, !)
OV_UNOP_FN_OP (op_uplus, +)
OV_UNOP_FN_OP (op_uminus, -)

OV_UNOP_FN (op_transpose)
OV_UNOP_FN (op_hermitian)

// No simple way to define these for prefix and suffix ops?
//
//   incr
//   decr

#define OV_BINOP_FN(name)                               \
  inline octave_value                                   \
  name (const octave_value& a1, const octave_value& a2) \
  {                                                     \
    return do_binary_op (octave_value::name, a1, a2);   \
  }

#define OV_BINOP_OP(name, op)                                   \
  inline octave_value                                           \
  operator op (const octave_value& a1, const octave_value& a2)  \
  {                                                             \
    return name (a1, a2);                                       \
  }

#define OV_BINOP_FN_OP(name, op)                \
  OV_BINOP_FN (name)                            \
  OV_BINOP_OP (name, op)

OV_BINOP_FN_OP (op_add, +)
OV_BINOP_FN_OP (op_sub, -)
OV_BINOP_FN_OP (op_mul, *)
OV_BINOP_FN_OP (op_div, /)

OV_BINOP_FN (op_pow)
OV_BINOP_FN (op_ldiv)

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

#define OV_COMP_BINOP_FN(name)                          \
  inline octave_value                                   \
  name (const octave_value& a1, const octave_value& a2) \
  {                                                     \
    return do_binary_op (octave_value::name, a1, a2);   \
  }

OV_COMP_BINOP_FN (op_trans_mul)
OV_COMP_BINOP_FN (op_mul_trans)
OV_COMP_BINOP_FN (op_herm_mul)
OV_COMP_BINOP_FN (op_mul_herm)

extern OCTINTERP_API void install_types (void);

// Templated value extractors.
template <typename Value>
inline Value octave_value_extract (const octave_value&)
{ assert (false); }

#define DEF_VALUE_EXTRACTOR(VALUE,MPREFIX)                              \
  template <>                                                           \
  inline VALUE octave_value_extract<VALUE> (const octave_value& v)      \
  {                                                                     \
    return v.MPREFIX ## _value ();                                      \
  }

DEF_VALUE_EXTRACTOR (double, scalar)
DEF_VALUE_EXTRACTOR (float, float_scalar)
DEF_VALUE_EXTRACTOR (Complex, complex)
DEF_VALUE_EXTRACTOR (FloatComplex, float_complex)
DEF_VALUE_EXTRACTOR (bool, bool)

DEF_VALUE_EXTRACTOR (octave_int8, int8_scalar)
DEF_VALUE_EXTRACTOR (octave_int16, int16_scalar)
DEF_VALUE_EXTRACTOR (octave_int32, int32_scalar)
DEF_VALUE_EXTRACTOR (octave_int64, int64_scalar)
DEF_VALUE_EXTRACTOR (octave_uint8, uint8_scalar)
DEF_VALUE_EXTRACTOR (octave_uint16, uint16_scalar)
DEF_VALUE_EXTRACTOR (octave_uint32, uint32_scalar)
DEF_VALUE_EXTRACTOR (octave_uint64, uint64_scalar)

DEF_VALUE_EXTRACTOR (NDArray, array)
DEF_VALUE_EXTRACTOR (FloatNDArray, float_array)
DEF_VALUE_EXTRACTOR (ComplexNDArray, complex_array)
DEF_VALUE_EXTRACTOR (FloatComplexNDArray, float_complex_array)
DEF_VALUE_EXTRACTOR (boolNDArray, bool_array)

DEF_VALUE_EXTRACTOR (charNDArray, char_array)
DEF_VALUE_EXTRACTOR (int8NDArray, int8_array)
DEF_VALUE_EXTRACTOR (int16NDArray, int16_array)
DEF_VALUE_EXTRACTOR (int32NDArray, int32_array)
DEF_VALUE_EXTRACTOR (int64NDArray, int64_array)
DEF_VALUE_EXTRACTOR (uint8NDArray, uint8_array)
DEF_VALUE_EXTRACTOR (uint16NDArray, uint16_array)
DEF_VALUE_EXTRACTOR (uint32NDArray, uint32_array)
DEF_VALUE_EXTRACTOR (uint64NDArray, uint64_array)

DEF_VALUE_EXTRACTOR (Matrix, matrix)
DEF_VALUE_EXTRACTOR (FloatMatrix, float_matrix)
DEF_VALUE_EXTRACTOR (ComplexMatrix, complex_matrix)
DEF_VALUE_EXTRACTOR (FloatComplexMatrix, float_complex_matrix)
DEF_VALUE_EXTRACTOR (boolMatrix, bool_matrix)

DEF_VALUE_EXTRACTOR (ColumnVector, column_vector)
DEF_VALUE_EXTRACTOR (FloatColumnVector, float_column_vector)
DEF_VALUE_EXTRACTOR (ComplexColumnVector, complex_column_vector)
DEF_VALUE_EXTRACTOR (FloatComplexColumnVector, float_complex_column_vector)

DEF_VALUE_EXTRACTOR (RowVector, row_vector)
DEF_VALUE_EXTRACTOR (FloatRowVector, float_row_vector)
DEF_VALUE_EXTRACTOR (ComplexRowVector, complex_row_vector)
DEF_VALUE_EXTRACTOR (FloatComplexRowVector, float_complex_row_vector)

DEF_VALUE_EXTRACTOR (DiagMatrix, diag_matrix)
DEF_VALUE_EXTRACTOR (FloatDiagMatrix, float_diag_matrix)
DEF_VALUE_EXTRACTOR (ComplexDiagMatrix, complex_diag_matrix)
DEF_VALUE_EXTRACTOR (FloatComplexDiagMatrix, float_complex_diag_matrix)
DEF_VALUE_EXTRACTOR (PermMatrix, perm_matrix)

DEF_VALUE_EXTRACTOR (SparseMatrix, sparse_matrix)
DEF_VALUE_EXTRACTOR (SparseComplexMatrix, sparse_complex_matrix)
DEF_VALUE_EXTRACTOR (SparseBoolMatrix, sparse_bool_matrix)
#undef DEF_VALUE_EXTRACTOR

#define DEF_DUMMY_VALUE_EXTRACTOR(VALUE,DEFVAL)                         \
  template <>                                                           \
  inline VALUE octave_value_extract<VALUE> (const octave_value&)        \
  {                                                                     \
    assert (false);                                                     \
    return DEFVAL;                                                      \
  }

DEF_DUMMY_VALUE_EXTRACTOR (char, 0)
DEF_DUMMY_VALUE_EXTRACTOR (octave_value, octave_value ())
#undef DEF_DUMMY_VALUE_EXTRACTOR

#endif
