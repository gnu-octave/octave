////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_ov_h)
#define octave_ov_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>
#include <list>
#include <memory>
#include <map>

#include "Range.h"
#include "data-conv.h"
#include "idx-vector.h"
#include "mach-info.h"
#include "mx-base.h"
#include "oct-sort.h"
#include "oct-time.h"
#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class stack_frame;
class type_info;

OCTAVE_END_NAMESPACE(octave)

class Cell;
class float_format;
class mxArray;
class octave_map;
class octave_scalar_map;
class octave_function;
class octave_user_function;
class octave_fcn_handle;
class octave_value_list;

#include "mxtypes.h"

#include "oct-stream.h"
#include "ov-base.h"

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

  static OCTINTERP_API binary_op assign_op_to_binary_op (assign_op);

  static OCTINTERP_API assign_op binary_op_to_assign_op (binary_op);

  static OCTINTERP_API std::string unary_op_as_string (unary_op);
  static OCTINTERP_API std::string unary_op_fcn_name (unary_op);

  static OCTINTERP_API std::string binary_op_as_string (binary_op);
  static OCTINTERP_API std::string binary_op_fcn_name (binary_op);

  static OCTINTERP_API std::string binary_op_fcn_name (compound_binary_op);

  static OCTINTERP_API std::string assign_op_as_string (assign_op);

  static OCTINTERP_API octave_value
  empty_conv (const std::string& type,
              const octave_value& rhs = octave_value ());

  enum magic_colon { magic_colon_t };

  octave_value (void)
    : m_rep (nil_rep ())
  {
    m_rep->count++;
  }

  OCTINTERP_API octave_value (short int i);
  OCTINTERP_API octave_value (unsigned short int i);
  OCTINTERP_API octave_value (int i);
  OCTINTERP_API octave_value (unsigned int i);
  OCTINTERP_API octave_value (long int i);
  OCTINTERP_API octave_value (unsigned long int i);

  // FIXME: These are kluges.  They turn into doubles internally, which will
  // break for very large values.  We just use them to store things like
  // 64-bit ino_t, etc, and hope that those values are never actually larger
  // than can be represented exactly in a double.

#if defined (OCTAVE_HAVE_LONG_LONG_INT)
  OCTINTERP_API octave_value (long long int i);
#endif
#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
  OCTINTERP_API octave_value (unsigned long long int i);
#endif

  OCTINTERP_API octave_value (octave::sys::time t);
  OCTINTERP_API octave_value (double d);
  OCTINTERP_API octave_value (float d);
  OCTINTERP_API octave_value (const Array<octave_value>& a,
                              bool is_cs_list = false);
  OCTINTERP_API octave_value (const Cell& c, bool is_cs_list = false);
  OCTINTERP_API octave_value (const Matrix& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const FloatMatrix& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const NDArray& nda);
  OCTINTERP_API octave_value (const FloatNDArray& nda);
  OCTINTERP_API octave_value (const Array<double>& m);
  OCTINTERP_API octave_value (const Array<float>& m);
  OCTINTERP_API octave_value (const DiagMatrix& d);
  OCTINTERP_API octave_value (const DiagArray2<double>& d);
  OCTINTERP_API octave_value (const DiagArray2<float>& d);
  OCTINTERP_API octave_value (const DiagArray2<Complex>& d);
  OCTINTERP_API octave_value (const DiagArray2<FloatComplex>& d);
  OCTINTERP_API octave_value (const FloatDiagMatrix& d);
  OCTINTERP_API octave_value (const RowVector& v);
  OCTINTERP_API octave_value (const FloatRowVector& v);
  OCTINTERP_API octave_value (const ColumnVector& v);
  OCTINTERP_API octave_value (const FloatColumnVector& v);
  OCTINTERP_API octave_value (const Complex& C);
  OCTINTERP_API octave_value (const FloatComplex& C);
  OCTINTERP_API octave_value (const ComplexMatrix& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const FloatComplexMatrix& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const ComplexNDArray& cnda);
  OCTINTERP_API octave_value (const FloatComplexNDArray& cnda);
  OCTINTERP_API octave_value (const Array<Complex>& m);
  OCTINTERP_API octave_value (const Array<FloatComplex>& m);
  OCTINTERP_API octave_value (const ComplexDiagMatrix& d);
  OCTINTERP_API octave_value (const FloatComplexDiagMatrix& d);
  OCTINTERP_API octave_value (const ComplexRowVector& v);
  OCTINTERP_API octave_value (const FloatComplexRowVector& v);
  OCTINTERP_API octave_value (const ComplexColumnVector& v);
  OCTINTERP_API octave_value (const FloatComplexColumnVector& v);
  OCTINTERP_API octave_value (const PermMatrix& p);
  OCTINTERP_API octave_value (bool b);
  OCTINTERP_API octave_value (const boolMatrix& bm,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const boolNDArray& bnda);
  OCTINTERP_API octave_value (const Array<bool>& bnda);
  OCTINTERP_API octave_value (char c, char type = '\'');
  OCTINTERP_API octave_value (const char *s, char type = '\'');
  OCTINTERP_API octave_value (const std::string& s, char type = '\'');
  OCTINTERP_API octave_value (const string_vector& s, char type = '\'');
  OCTINTERP_API octave_value (const charMatrix& chm,  char type = '\'');
  OCTINTERP_API octave_value (const charNDArray& chnda, char type = '\'');
  OCTINTERP_API octave_value (const Array<char>& chnda, char type = '\'');

  OCTINTERP_API octave_value (const SparseMatrix& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const Sparse<double>& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const SparseComplexMatrix& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const Sparse<Complex>& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const SparseBoolMatrix& bm,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const Sparse<bool>& m,
                              const MatrixType& t = MatrixType ());
  OCTINTERP_API octave_value (const octave_int8& i);
  OCTINTERP_API octave_value (const octave_int16& i);
  OCTINTERP_API octave_value (const octave_int32& i);
  OCTINTERP_API octave_value (const octave_int64& i);
  OCTINTERP_API octave_value (const octave_uint8& i);
  OCTINTERP_API octave_value (const octave_uint16& i);
  OCTINTERP_API octave_value (const octave_uint32& i);
  OCTINTERP_API octave_value (const octave_uint64& i);
  OCTINTERP_API octave_value (const int8NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_int8>& inda);
  OCTINTERP_API octave_value (const int16NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_int16>& inda);
  OCTINTERP_API octave_value (const int32NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_int32>& inda);
  OCTINTERP_API octave_value (const int64NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_int64>& inda);
  OCTINTERP_API octave_value (const uint8NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_uint8>& inda);
  OCTINTERP_API octave_value (const uint16NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_uint16>& inda);
  OCTINTERP_API octave_value (const uint32NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_uint32>& inda);
  OCTINTERP_API octave_value (const uint64NDArray& inda);
  OCTINTERP_API octave_value (const Array<octave_uint64>& inda);
  OCTINTERP_API octave_value (const Array<octave_idx_type>& inda,
                              bool zero_based = false,
                              bool cache_index = false);
  OCTINTERP_API octave_value (const Array<std::string>& cellstr);
  OCTINTERP_API octave_value (const octave::idx_vector& idx, bool lazy = true);

private:

  // Remove when public constructors that use this function are removed.
  static OCTINTERP_API octave_base_value *
  make_range_rep_deprecated (double base, double inc, double limit);

  // Remove when public constructors that use this function are removed.
  static OCTINTERP_API octave_base_value *
  make_range_rep_deprecated (const Range& r, bool force_range);

public:

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "use 'octave_value (range<double>&)' instead")
  octave_value (double base, double limit, double inc)
    : m_rep (make_range_rep_deprecated (base, inc, limit))
  {
    maybe_mutate ();
  }

  OCTAVE_DEPRECATED (7, "use 'octave_value (range<double>&)' instead")
  octave_value (const Range& r, bool force_range = false)
    : m_rep (make_range_rep_deprecated (r, force_range))
  {
    maybe_mutate ();
  }
#endif

  OCTINTERP_API octave_value (const octave::range<double>& r,
                              bool force_range = false);

  // For now, disable all but range<double>.

#if 0

  OCTINTERP_API octave_value (const octave::range<float>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_int8>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_int16>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_int32>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_int64>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_uint8>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_uint16>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_uint32>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<octave_uint64>& r,
                              bool force_range = false);

  OCTINTERP_API octave_value (const octave::range<char>& r, char type,
                              bool force_range = false);
#endif

  OCTINTERP_API octave_value (const octave_map& m);
  OCTINTERP_API octave_value (const octave_scalar_map& m);
  OCTINTERP_API octave_value (const std::map<std::string, octave_value>&);
  OCTINTERP_API octave_value (const octave_map& m, const std::string& id,
                              const std::list<std::string>& plist);
  OCTINTERP_API octave_value (const octave_scalar_map& m, const std::string& id,
                              const std::list<std::string>& plist);

  // This one is explicit because it can cause some trouble to
  // accidentally create a cs-list when one was not intended.
  explicit OCTINTERP_API octave_value (const octave_value_list& m);

  OCTINTERP_API octave_value (octave_value::magic_colon);

  OCTINTERP_API octave_value (octave_base_value *new_rep, bool borrow = false);

  // Copy constructor.

  octave_value (const octave_value& a)
    : m_rep (a.m_rep)
  {
    m_rep->count++;
  }

  octave_value (octave_value&& a)
    : m_rep (a.m_rep)
  {
    a.m_rep = nullptr;
  }

  // This should only be called for derived types.

  OCTINTERP_API octave_base_value * clone (void) const;

  octave_base_value * empty_clone (void) const
  { return m_rep->empty_clone (); }

  // Delete the representation of this constant if the count drops to zero.

  ~octave_value (void)
  {
    // Because we define a move constructor and a move assignment
    // operator, rep may be a nullptr here.  We should only need to
    // protect the move assignment operator in a similar way.

    if (m_rep && --m_rep->count == 0 && m_rep != nil_rep ())
      delete m_rep;
  }

  void make_unique (void)
  {
    if (m_rep->count > 1)
      {
        octave_base_value *r = m_rep->unique_clone ();

        if (--m_rep->count == 0 && m_rep != nil_rep ())
          delete m_rep;

        m_rep = r;
      }
  }

  // This uniquifies the value if it is referenced by more than a certain
  // number of shallow copies.  This is useful for optimizations where we
  // know a certain copy, typically within a cell array, to be obsolete.
  void make_unique (int obsolete_copies)
  {
    if (m_rep->count > obsolete_copies + 1)
      {
        octave_base_value *r = m_rep->unique_clone ();

        if (--m_rep->count == 0 && m_rep != nil_rep ())
          delete m_rep;

        m_rep = r;
      }
  }

  // Convert any nested function handles in this object to use weak
  // references to their enclosing stack frame context.  Used to break
  // shared_ptr reference cycles for handles to nested functions
  // (closures).
  void break_closure_cycles (const std::shared_ptr<octave::stack_frame>&);

  // Simple assignment.

  octave_value& operator = (const octave_value& a)
  {
    if (m_rep != a.m_rep)
      {
        if (--m_rep->count == 0 && m_rep != nil_rep ())
          delete m_rep;

        m_rep = a.m_rep;
        m_rep->count++;
      }

    return *this;
  }

  octave_value& operator = (octave_value&& a)
  {
    // Because we define a move constructor and a move assignment
    // operator, rep may be a nullptr here.  We should only need to
    // protect the destructor in a similar way.

    if (this != &a)
      {
        if (m_rep && --m_rep->count == 0 && m_rep != nil_rep ())
          delete m_rep;

        m_rep = a.m_rep;
        a.m_rep = nullptr;
      }

    return *this;
  }

  octave_idx_type get_count (void) const { return m_rep->count; }

  octave_base_value::type_conv_info numeric_conversion_function (void) const
  { return m_rep->numeric_conversion_function (); }

  octave_base_value::type_conv_info numeric_demotion_function (void) const
  { return m_rep->numeric_demotion_function (); }

  OCTINTERP_API void maybe_mutate (void);

  octave_value squeeze (void) const
  { return m_rep->squeeze (); }

  // The result of full().
  octave_value full_value (void) const
  { return m_rep->full_value (); }

  // Type conversions.

  octave_value as_double (void) const { return m_rep->as_double (); }
  octave_value as_single (void) const { return m_rep->as_single (); }

  octave_value as_int8 (void) const { return m_rep->as_int8 (); }
  octave_value as_int16 (void) const { return m_rep->as_int16 (); }
  octave_value as_int32 (void) const { return m_rep->as_int32 (); }
  octave_value as_int64 (void) const { return m_rep->as_int64 (); }

  octave_value as_uint8 (void) const { return m_rep->as_uint8 (); }
  octave_value as_uint16 (void) const { return m_rep->as_uint16 (); }
  octave_value as_uint32 (void) const { return m_rep->as_uint32 (); }
  octave_value as_uint64 (void) const { return m_rep->as_uint64 (); }

  octave_base_value * try_narrowing_conversion (void)
  { return m_rep->try_narrowing_conversion (); }

  // Close to dims (), but can be overloaded for classes.
  Matrix size (void)
  { return m_rep->size (); }

  // FIXME: should this function be deprecated and removed?  It supports
  // an undocumented feature of Matlab.
  octave_idx_type xnumel (const octave_value_list& idx)
  { return m_rep->xnumel (idx); }

  // FIXME: Do we really need all these different versions of subsref
  // and related functions?

  OCTINTERP_API octave_value
  single_subsref (const std::string& type, const octave_value_list& idx);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  { return m_rep->subsref (type, idx); }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        bool auto_add)
  { return m_rep->subsref (type, idx, auto_add); }

  OCTINTERP_API octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int nargout);

  OCTINTERP_API octave_value
  next_subsref (const std::string& type,
                const std::list<octave_value_list>& idx, std::size_t skip = 1);

  OCTINTERP_API octave_value_list
  next_subsref (int nargout, const std::string& type,
                const std::list<octave_value_list>& idx, std::size_t skip = 1);

  OCTINTERP_API octave_value
  next_subsref (bool auto_add, const std::string& type,
                const std::list<octave_value_list>& idx, std::size_t skip = 1);

  octave_value index_op (const octave_value_list& idx, bool resize_ok = false)
  {
    return m_rep->do_index_op (idx, resize_ok);
  }

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "use 'octave_value::index_op' instead")
  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false)
  {
    return index_op (idx, resize_ok);
  }
#endif

  OCTINTERP_API octave_value
  subsasgn (const std::string& type, const std::list<octave_value_list>& idx,
            const octave_value& rhs);

  OCTINTERP_API octave_value
  undef_subsasgn (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  OCTINTERP_API octave_value&
  assign (assign_op op, const std::string& type,
          const std::list<octave_value_list>& idx, const octave_value& rhs);

  OCTINTERP_API octave_value& assign (assign_op, const octave_value& rhs);

  octave::idx_vector index_vector (bool require_integers = false) const
  {
    return m_rep->index_vector (require_integers);
  }

  // Size.

  dim_vector dims (void) const { return m_rep->dims (); }

  OCTINTERP_API std::string get_dims_str (void) const;

  octave_idx_type rows (void) const { return m_rep->rows (); }

  octave_idx_type columns (void) const { return m_rep->columns (); }

  OCTINTERP_API octave_idx_type length (void) const;

  int ndims (void) const { return m_rep->ndims (); }

  bool all_zero_dims (void) const { return dims ().all_zero (); }

  // Are the dimensions of this constant zero by zero?
  bool is_zero_by_zero (void) const
  { return (ndims () == 2 && rows () == 0 && columns () == 0); }

  octave_idx_type numel (void) const
  { return m_rep->numel (); }

  std::size_t byte_size (void) const
  { return m_rep->byte_size (); }

  octave_idx_type nnz (void) const { return m_rep->nnz (); }

  octave_idx_type nzmax (void) const { return m_rep->nzmax (); }

  octave_idx_type nfields (void) const { return m_rep->nfields (); }

  octave_value reshape (const dim_vector& dv) const
  { return m_rep->reshape (dv); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
  { return m_rep->permute (vec, inv); }

  octave_value ipermute (const Array<int>& vec) const
  { return m_rep->permute (vec, true); }

  octave_value resize (const dim_vector& dv, bool fill = false) const
  { return m_rep->resize (dv, fill);}

  MatrixType matrix_type (void) const
  { return m_rep->matrix_type (); }

  MatrixType matrix_type (const MatrixType& typ) const
  { return m_rep->matrix_type (typ); }

  // Does this constant have a type?  Both of these are provided since it is
  // sometimes more natural to write is_undefined() instead of ! is_defined().

  bool is_defined (void) const
  { return m_rep->is_defined (); }

  bool is_undefined (void) const
  { return ! is_defined (); }

  bool is_legacy_object (void) const
  { return m_rep->is_legacy_object (); }

  bool isempty (void) const
  { return m_rep->isempty (); }

  bool iscell (void) const
  { return m_rep->iscell (); }

  bool iscellstr (void) const
  { return m_rep->iscellstr (); }

  bool is_real_scalar (void) const
  { return m_rep->is_real_scalar (); }

  bool is_real_matrix (void) const
  { return m_rep->is_real_matrix (); }

  bool is_complex_scalar (void) const
  { return m_rep->is_complex_scalar (); }

  bool is_complex_matrix (void) const
  { return m_rep->is_complex_matrix (); }

  bool is_bool_scalar (void) const
  { return m_rep->is_bool_scalar (); }

  bool is_bool_matrix (void) const
  { return m_rep->is_bool_matrix (); }

  bool is_char_matrix (void) const
  { return m_rep->is_char_matrix (); }

  bool is_diag_matrix (void) const
  { return m_rep->is_diag_matrix (); }

  bool is_perm_matrix (void) const
  { return m_rep->is_perm_matrix (); }

  bool is_string (void) const
  { return m_rep->is_string (); }

  bool is_sq_string (void) const
  { return m_rep->is_sq_string (); }

  bool is_dq_string (void) const
  { return m_rep->is_string () && ! m_rep->is_sq_string (); }

  bool is_range (void) const
  { return m_rep->is_range (); }

  bool isstruct (void) const
  { return m_rep->isstruct (); }

  bool is_classdef_meta (void) const
  { return m_rep->is_classdef_meta (); }

  bool is_classdef_object (void) const
  { return m_rep->is_classdef_object (); }

  bool is_classdef_superclass_ref (void) const
  { return m_rep->is_classdef_superclass_ref (); }

  bool is_package (void) const
  { return m_rep->is_package (); }

  bool isobject (void) const
  { return m_rep->isobject (); }

  bool isjava (void) const
  { return m_rep->isjava (); }

  bool is_cs_list (void) const
  { return m_rep->is_cs_list (); }

  bool is_magic_colon (void) const
  { return m_rep->is_magic_colon (); }

  bool is_magic_int (void) const
  { return m_rep->is_magic_int (); }

  bool isnull (void) const
  { return m_rep->isnull (); }

  // Are any or all of the elements in this constant nonzero?

  octave_value all (int dim = 0) const
  { return m_rep->all (dim); }

  octave_value any (int dim = 0) const
  { return m_rep->any (dim); }

  builtin_type_t builtin_type (void) const
  { return m_rep->builtin_type (); }

  // Floating point types.

  bool is_double_type (void) const
  { return m_rep->is_double_type (); }

  bool is_single_type (void) const
  { return m_rep->is_single_type (); }

  bool isfloat (void) const
  { return m_rep->isfloat (); }

  // Integer types.

  bool is_int8_type (void) const
  { return m_rep->is_int8_type (); }

  bool is_int16_type (void) const
  { return m_rep->is_int16_type (); }

  bool is_int32_type (void) const
  { return m_rep->is_int32_type (); }

  bool is_int64_type (void) const
  { return m_rep->is_int64_type (); }

  bool is_uint8_type (void) const
  { return m_rep->is_uint8_type (); }

  bool is_uint16_type (void) const
  { return m_rep->is_uint16_type (); }

  bool is_uint32_type (void) const
  { return m_rep->is_uint32_type (); }

  bool is_uint64_type (void) const
  { return m_rep->is_uint64_type (); }

  bool isinteger (void) const
  { return m_rep->isinteger (); }

  // Other type stuff.

  bool islogical (void) const
  { return m_rep->islogical (); }

  bool isreal (void) const
  { return m_rep->isreal (); }

  bool iscomplex (void) const
  { return m_rep->iscomplex (); }

  bool is_scalar_type (void) const
  { return m_rep->is_scalar_type (); }

  bool is_matrix_type (void) const
  { return m_rep->is_matrix_type (); }

  bool isnumeric (void) const
  { return m_rep->isnumeric (); }

  bool issparse (void) const
  { return m_rep->issparse (); }

  // Does this constant correspond to a truth value?

  bool is_true (void) const
  { return m_rep->is_true (); }

  // Do two constants match (in a switch statement)?

  bool is_equal (const octave_value&) const;

  bool is_constant (void) const
  { return m_rep->is_constant (); }

  bool is_function_handle (void) const
  { return m_rep->is_function_handle (); }

  bool is_anonymous_function (void) const
  { return m_rep->is_anonymous_function (); }

  bool is_inline_function (void) const
  { return m_rep->is_inline_function (); }

  bool is_function (void) const
  { return m_rep->is_function (); }

  bool is_user_script (void) const
  { return m_rep->is_user_script (); }

  bool is_user_function (void) const
  { return m_rep->is_user_function (); }

  bool is_user_code (void) const
  { return m_rep->is_user_code (); }

  bool is_builtin_function (void) const
  { return m_rep->is_builtin_function (); }

  bool is_dld_function (void) const
  { return m_rep->is_dld_function (); }

  bool is_mex_function (void) const
  { return m_rep->is_mex_function (); }

  void erase_subfunctions (void) { m_rep->erase_subfunctions (); }

  // Values.

  octave_value eval (void) { return *this; }

  short int
  short_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->short_value (req_int, frc_str_conv); }

  unsigned short int
  ushort_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->ushort_value (req_int, frc_str_conv); }

  int int_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->int_value (req_int, frc_str_conv); }

  unsigned int
  uint_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->uint_value (req_int, frc_str_conv); }

  int nint_value (bool frc_str_conv = false) const
  { return m_rep->nint_value (frc_str_conv); }

  long int
  long_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->long_value (req_int, frc_str_conv); }

  unsigned long int
  ulong_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->ulong_value (req_int, frc_str_conv); }

  int64_t
  int64_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->int64_value (req_int, frc_str_conv); }

  uint64_t
  uint64_value (bool req_int = false, bool frc_str_conv = false) const
  { return m_rep->uint64_value (req_int, frc_str_conv); }

  octave_idx_type
  idx_type_value (bool req_int = false, bool frc_str_conv = false) const;

  double double_value (bool frc_str_conv = false) const
  { return m_rep->double_value (frc_str_conv); }

  float float_value (bool frc_str_conv = false) const
  { return m_rep->float_value (frc_str_conv); }

  double scalar_value (bool frc_str_conv = false) const
  { return m_rep->scalar_value (frc_str_conv); }

  float float_scalar_value (bool frc_str_conv = false) const
  { return m_rep->float_scalar_value (frc_str_conv); }

  Matrix matrix_value (bool frc_str_conv = false) const
  { return m_rep->matrix_value (frc_str_conv); }

  FloatMatrix float_matrix_value (bool frc_str_conv = false) const
  { return m_rep->float_matrix_value (frc_str_conv); }

  NDArray array_value (bool frc_str_conv = false) const
  { return m_rep->array_value (frc_str_conv); }

  FloatNDArray float_array_value (bool frc_str_conv = false) const
  { return m_rep->float_array_value (frc_str_conv); }

  Complex complex_value (bool frc_str_conv = false) const
  { return m_rep->complex_value (frc_str_conv); }

  FloatComplex float_complex_value (bool frc_str_conv = false) const
  { return m_rep->float_complex_value (frc_str_conv); }

  ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const
  { return m_rep->complex_matrix_value (frc_str_conv); }

  FloatComplexMatrix
  float_complex_matrix_value (bool frc_str_conv = false) const
  { return m_rep->float_complex_matrix_value (frc_str_conv); }

  ComplexNDArray complex_array_value (bool frc_str_conv = false) const
  { return m_rep->complex_array_value (frc_str_conv); }

  FloatComplexNDArray
  float_complex_array_value (bool frc_str_conv = false) const
  { return m_rep->float_complex_array_value (frc_str_conv); }

  bool bool_value (bool warn = false) const
  { return m_rep->bool_value (warn); }

  boolMatrix bool_matrix_value (bool warn = false) const
  { return m_rep->bool_matrix_value (warn); }

  boolNDArray bool_array_value (bool warn = false) const
  { return m_rep->bool_array_value (warn); }

  charMatrix char_matrix_value (bool frc_str_conv = false) const
  { return m_rep->char_matrix_value (frc_str_conv); }

  charNDArray char_array_value (bool frc_str_conv = false) const
  { return m_rep->char_array_value (frc_str_conv); }

  SparseMatrix sparse_matrix_value (bool frc_str_conv = false) const
  { return m_rep->sparse_matrix_value (frc_str_conv); }

  SparseComplexMatrix
  sparse_complex_matrix_value (bool frc_str_conv = false) const
  { return m_rep->sparse_complex_matrix_value (frc_str_conv); }

  SparseBoolMatrix sparse_bool_matrix_value (bool warn = false) const
  { return m_rep->sparse_bool_matrix_value (warn); }

  DiagMatrix diag_matrix_value (bool force = false) const
  { return m_rep->diag_matrix_value (force); }

  FloatDiagMatrix float_diag_matrix_value (bool force = false) const
  { return m_rep->float_diag_matrix_value (force); }

  ComplexDiagMatrix complex_diag_matrix_value (bool force = false) const
  { return m_rep->complex_diag_matrix_value (force); }

  FloatComplexDiagMatrix
  float_complex_diag_matrix_value (bool force = false) const
  { return m_rep->float_complex_diag_matrix_value (force); }

  PermMatrix perm_matrix_value (void) const
  { return m_rep->perm_matrix_value (); }

  octave_int8 int8_scalar_value (void) const
  { return m_rep->int8_scalar_value (); }

  octave_int16 int16_scalar_value (void) const
  { return m_rep->int16_scalar_value (); }

  octave_int32 int32_scalar_value (void) const
  { return m_rep->int32_scalar_value (); }

  octave_int64 int64_scalar_value (void) const
  { return m_rep->int64_scalar_value (); }

  octave_uint8 uint8_scalar_value (void) const
  { return m_rep->uint8_scalar_value (); }

  octave_uint16 uint16_scalar_value (void) const
  { return m_rep->uint16_scalar_value (); }

  octave_uint32 uint32_scalar_value (void) const
  { return m_rep->uint32_scalar_value (); }

  octave_uint64 uint64_scalar_value (void) const
  { return m_rep->uint64_scalar_value (); }

  int8NDArray int8_array_value (void) const
  { return m_rep->int8_array_value (); }

  int16NDArray int16_array_value (void) const
  { return m_rep->int16_array_value (); }

  int32NDArray int32_array_value (void) const
  { return m_rep->int32_array_value (); }

  int64NDArray int64_array_value (void) const
  { return m_rep->int64_array_value (); }

  uint8NDArray uint8_array_value (void) const
  { return m_rep->uint8_array_value (); }

  uint16NDArray uint16_array_value (void) const
  { return m_rep->uint16_array_value (); }

  uint32NDArray uint32_array_value (void) const
  { return m_rep->uint32_array_value (); }

  uint64NDArray uint64_array_value (void) const
  { return m_rep->uint64_array_value (); }

  std::string string_value (bool force = false) const
  { return m_rep->string_value (force); }

  string_vector string_vector_value (bool pad = false) const
  { return m_rep->string_vector_value (pad); }

  Cell cell_value (void) const;

  Array<std::string> cellstr_value (void) const
  { return m_rep->cellstr_value (); }

  octave::range<double> range_value (void) const
  { return m_rep->range_value (); }

  // For now, disable all but range<double>.

#if 0

  octave::range<float> float_range_value (void) const
  { return m_rep->float_range_value (); }

  octave::range<octave_int8> int8_range_value (void) const
  { return m_rep->int8_range_value (); }

  octave::range<octave_int16> int16_range_value (void) const
  { return m_rep->int16_range_value (); }

  octave::range<octave_int32> int32_range_value (void) const
  { return m_rep->int32_range_value (); }

  octave::range<octave_int64> int64_range_value (void) const
  { return m_rep->int64_range_value (); }

  octave::range<octave_uint8> uint8_range_value (void) const
  { return m_rep->uint8_range_value (); }

  octave::range<octave_uint16> uint16_range_value (void) const
  { return m_rep->uint16_range_value (); }

  octave::range<octave_uint32> uint32_range_value (void) const
  { return m_rep->uint32_range_value (); }

  octave::range<octave_uint64> uint64_range_value (void) const
  { return m_rep->uint64_range_value (); }

#endif

  OCTINTERP_API octave_map map_value (void) const;

  OCTINTERP_API octave_scalar_map scalar_map_value (void) const;

  string_vector map_keys (void) const
  { return m_rep->map_keys (); }

  bool isfield (const std::string& field_name) const
  { return m_rep->isfield (field_name); }

  std::size_t nparents (void) const
  { return m_rep->nparents (); }

  std::list<std::string> parent_class_name_list (void) const
  { return m_rep->parent_class_name_list (); }

  string_vector parent_class_names (void) const
  { return m_rep->parent_class_names (); }

  octave_base_value *
  find_parent_class (const std::string& parent_class_name)
  { return m_rep->find_parent_class (parent_class_name); }

  bool is_instance_of (const std::string& cls_name) const
  { return m_rep->is_instance_of (cls_name); }

  OCTINTERP_API octave_classdef *
  classdef_object_value (bool silent = false) const;

  OCTINTERP_API octave_function *
  function_value (bool silent = false) const;

  OCTINTERP_API octave_user_function *
  user_function_value (bool silent = false) const;

  OCTINTERP_API octave_user_script *
  user_script_value (bool silent = false) const;

  OCTINTERP_API octave_user_code * user_code_value (bool silent = false) const;

  OCTINTERP_API octave_fcn_handle *
  fcn_handle_value (bool silent = false) const;

  OCTINTERP_API octave_value_list list_value (void) const;

  OCTINTERP_API ColumnVector
  column_vector_value (bool frc_str_conv = false,
                       bool frc_vec_conv = false) const;

  OCTINTERP_API ComplexColumnVector
  complex_column_vector_value (bool frc_str_conv = false,
                               bool frc_vec_conv = false) const;

  OCTINTERP_API RowVector
  row_vector_value (bool frc_str_conv = false,
                    bool frc_vec_conv = false) const;

  OCTINTERP_API ComplexRowVector
  complex_row_vector_value (bool frc_str_conv = false,
                            bool frc_vec_conv = false) const;

  OCTINTERP_API FloatColumnVector
  float_column_vector_value (bool frc_str_conv = false,
                             bool frc_vec_conv = false) const;

  OCTINTERP_API FloatComplexColumnVector
  float_complex_column_vector_value (bool frc_str_conv = false,
                                     bool frc_vec_conv = false) const;

  OCTINTERP_API FloatRowVector
  float_row_vector_value (bool frc_str_conv = false,
                          bool frc_vec_conv = false) const;

  OCTINTERP_API FloatComplexRowVector
  float_complex_row_vector_value (bool frc_str_conv = false,
                                  bool frc_vec_conv = false) const;

  OCTINTERP_API Array<int>
  int_vector_value (bool req_int = false,
                    bool frc_str_conv = false,
                    bool frc_vec_conv = false) const;

  OCTINTERP_API Array<octave_idx_type>
  octave_idx_type_vector_value (bool req_int = false,
                                bool frc_str_conv = false,
                                bool frc_vec_conv = false) const;

  OCTINTERP_API Array<double>
  vector_value (bool frc_str_conv = false,
                bool frc_vec_conv = false) const;

  OCTINTERP_API Array<Complex>
  complex_vector_value (bool frc_str_conv = false,
                        bool frc_vec_conv = false) const;

  OCTINTERP_API Array<float>
  float_vector_value (bool frc_str_conv = false,
                      bool frc_vec_conv = false) const;

  OCTINTERP_API Array<FloatComplex>
  float_complex_vector_value (bool frc_str_conv = false,
                              bool frc_vec_conv = false) const;

  // Extract values of specific types without any implicit type conversions.
  // Throw an error if an object is the wrong type for the requested value
  // extraction.
  //
  // These functions are intended to provide a simple way to extract values of
  // specific types and display error messages that are more meaningful than
  // the generic "error: wrong type argument 'cell'" message.

  OCTINTERP_API short int xshort_value (const char *fmt, ...) const;

  OCTINTERP_API unsigned short int xushort_value (const char *fmt, ...) const;

  OCTINTERP_API int xint_value (const char *fmt, ...) const;

  OCTINTERP_API unsigned int xuint_value (const char *fmt, ...) const;

  OCTINTERP_API int xnint_value (const char *fmt, ...) const;

  OCTINTERP_API long int xlong_value (const char *fmt, ...) const;

  OCTINTERP_API unsigned long int xulong_value (const char *fmt, ...) const;

  OCTINTERP_API int64_t xint64_value (const char *fmt, ...) const;

  OCTINTERP_API uint64_t xuint64_value (const char *fmt, ...) const;

  OCTINTERP_API octave_idx_type xidx_type_value (const char *fmt, ...) const;

  OCTINTERP_API double xdouble_value (const char *fmt, ...) const;

  OCTINTERP_API float xfloat_value (const char *fmt, ...) const;

  OCTINTERP_API double xscalar_value (const char *fmt, ...) const;

  OCTINTERP_API float xfloat_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API Matrix xmatrix_value (const char *fmt, ...) const;

  OCTINTERP_API FloatMatrix xfloat_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API NDArray xarray_value (const char *fmt, ...) const;

  OCTINTERP_API FloatNDArray xfloat_array_value (const char *fmt, ...) const;

  OCTINTERP_API Complex xcomplex_value (const char *fmt, ...) const;

  OCTINTERP_API FloatComplex xfloat_complex_value (const char *fmt, ...) const;

  OCTINTERP_API ComplexMatrix
  xcomplex_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API FloatComplexMatrix
  xfloat_complex_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API ComplexNDArray
  xcomplex_array_value (const char *fmt, ...) const;

  OCTINTERP_API FloatComplexNDArray
  xfloat_complex_array_value (const char *fmt, ...) const;

  OCTINTERP_API bool xbool_value (const char *fmt, ...) const;

  OCTINTERP_API boolMatrix xbool_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API boolNDArray xbool_array_value (const char *fmt, ...) const;

  OCTINTERP_API charMatrix xchar_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API charNDArray xchar_array_value (const char *fmt, ...) const;

  OCTINTERP_API SparseMatrix xsparse_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API SparseComplexMatrix
  xsparse_complex_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API SparseBoolMatrix
  xsparse_bool_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API DiagMatrix xdiag_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API FloatDiagMatrix
  xfloat_diag_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API ComplexDiagMatrix
  xcomplex_diag_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API FloatComplexDiagMatrix
  xfloat_complex_diag_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API PermMatrix xperm_matrix_value (const char *fmt, ...) const;

  OCTINTERP_API octave_int8 xint8_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API octave_int16 xint16_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API octave_int32 xint32_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API octave_int64 xint64_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API octave_uint8 xuint8_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API octave_uint16 xuint16_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API octave_uint32 xuint32_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API octave_uint64 xuint64_scalar_value (const char *fmt, ...) const;

  OCTINTERP_API int8NDArray xint8_array_value (const char *fmt, ...) const;

  OCTINTERP_API int16NDArray xint16_array_value (const char *fmt, ...) const;

  OCTINTERP_API int32NDArray xint32_array_value (const char *fmt, ...) const;

  OCTINTERP_API int64NDArray xint64_array_value (const char *fmt, ...) const;

  OCTINTERP_API uint8NDArray xuint8_array_value (const char *fmt, ...) const;

  OCTINTERP_API uint16NDArray xuint16_array_value (const char *fmt, ...) const;

  OCTINTERP_API uint32NDArray xuint32_array_value (const char *fmt, ...) const;

  OCTINTERP_API uint64NDArray xuint64_array_value (const char *fmt, ...) const;

  OCTINTERP_API std::string xstring_value (const char *fmt, ...) const;

  OCTINTERP_API string_vector xstring_vector_value (const char *fmt, ...) const;

  OCTINTERP_API Cell xcell_value (const char *fmt, ...) const;

  OCTINTERP_API Array<std::string> xcellstr_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<double>
  xrange_value (const char *fmt, ...) const;

  // For now, disable all but range<double>.

#if 0

  OCTINTERP_API octave::range<float>
  xfloat_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_int8>
  xint8_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_int16>
  xint16_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_int32>
  xint32_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_int64>
  xint64_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_uint8>
  xuint8_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_uint16>
  xuint16_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_uint32>
  xuint32_range_value (const char *fmt, ...) const;

  OCTINTERP_API octave::range<octave_uint64>
  xuint64_range_value (const char *fmt, ...) const;

#endif

  OCTINTERP_API octave_map xmap_value (const char *fmt, ...) const;

  OCTINTERP_API octave_scalar_map
  xscalar_map_value (const char *fmt, ...) const;

  OCTINTERP_API ColumnVector xcolumn_vector_value (const char *fmt, ...) const;

  OCTINTERP_API ComplexColumnVector
  xcomplex_column_vector_value (const char *fmt, ...) const;

  OCTINTERP_API RowVector xrow_vector_value (const char *fmt, ...) const;

  OCTINTERP_API ComplexRowVector
  xcomplex_row_vector_value (const char *fmt, ...) const;

  OCTINTERP_API FloatColumnVector
  xfloat_column_vector_value (const char *fmt, ...) const;

  OCTINTERP_API FloatComplexColumnVector
  xfloat_complex_column_vector_value (const char *fmt, ...) const;

  OCTINTERP_API FloatRowVector
  xfloat_row_vector_value (const char *fmt, ...) const;

  OCTINTERP_API FloatComplexRowVector
  xfloat_complex_row_vector_value (const char *fmt, ...) const;

  OCTINTERP_API Array<int> xint_vector_value (const char *fmt, ...) const;

  OCTINTERP_API Array<octave_idx_type>
  xoctave_idx_type_vector_value (const char *fmt, ...) const;

  OCTINTERP_API Array<double> xvector_value (const char *fmt, ...) const;

  OCTINTERP_API Array<Complex>
  xcomplex_vector_value (const char *fmt, ...) const;

  OCTINTERP_API Array<float> xfloat_vector_value (const char *fmt, ...) const;

  OCTINTERP_API Array<FloatComplex>
  xfloat_complex_vector_value (const char *fmt, ...) const;

  OCTINTERP_API octave_function * xfunction_value (const char *fmt, ...) const;

  OCTINTERP_API octave_user_function *
  xuser_function_value (const char *fmt, ...) const;

  OCTINTERP_API octave_user_script *
  xuser_script_value (const char *fmt, ...) const;

  OCTINTERP_API octave_user_code *
  xuser_code_value (const char *fmt, ...) const;

  OCTINTERP_API octave_fcn_handle *
  xfcn_handle_value (const char *fmt, ...) const;

  OCTINTERP_API octave_value_list xlist_value (const char *fmt, ...) const;

  // Possibly economize a lazy-indexed value.

  void maybe_economize (void)
  { m_rep->maybe_economize (); }

  // The following two hook conversions are called on any octave_value prior to
  // storing it to a "permanent" location, like a named variable, a cell or a
  // struct component, or a return value of a function.

  OCTINTERP_API octave_value storable_value (void) const;

  // Ditto, but in place, i.e., equivalent to *this = this->storable_value (),
  // but possibly more efficient.

  OCTINTERP_API void make_storable_value (void);

  // FIXME: These should probably be private.
  // Conversions.  If a user of this class wants a certain kind of constant,
  // he should simply ask for it, and we should convert it if possible.

  octave_value convert_to_str (bool pad = false, bool force = false,
                               char type = '\'') const
  { return m_rep->convert_to_str (pad, force, type); }

  octave_value
  convert_to_str_internal (bool pad, bool force, char type) const
  { return m_rep->convert_to_str_internal (pad, force, type); }

  void convert_to_row_or_column_vector (void)
  { m_rep->convert_to_row_or_column_vector (); }

  bool print_as_scalar (void) const
  { return m_rep->print_as_scalar (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false)
  { m_rep->print (os, pr_as_read_syntax); }

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const
  { m_rep->print_raw (os, pr_as_read_syntax); }

  bool print_name_tag (std::ostream& os, const std::string& name) const
  { return m_rep->print_name_tag (os, name); }

  void print_with_name (std::ostream& os, const std::string& name) const
  { m_rep->print_with_name (os, name, true); }

  void short_disp (std::ostream& os) const { m_rep->short_disp (os); }

  OCTINTERP_API float_display_format get_edit_display_format (void) const;

  std::string edit_display (const float_display_format& fmt,
                            octave_idx_type i, octave_idx_type j) const
  {
    return m_rep->edit_display (fmt, i, j);
  }

  int type_id (void) const { return m_rep->type_id (); }

  std::string type_name (void) const { return m_rep->type_name (); }

  std::string class_name (void) const { return m_rep->class_name (); }

  // Unary operations that are member functions.  There are also some
  // non-member functions for unary and binary operations declared
  // below, outside of the octave_value class declaration.

  OCTINTERP_API octave_value& non_const_unary_op (unary_op op);

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "use 'octave_value::non_const_unary_op' instead")
  octave_value& do_non_const_unary_op (unary_op op)
  {
    return non_const_unary_op (op);
  }
#endif

  OCTINTERP_API octave_value&
  non_const_unary_op (unary_op op, const std::string& type,
                      const std::list<octave_value_list>& idx);

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "use 'octave_value::non_const_unary_op' instead")
  octave_value& do_non_const_unary_op (unary_op op, const std::string& type,
                                       const std::list<octave_value_list>& idx)
  {
    return non_const_unary_op (op, type, idx);
  }
#endif

  const octave_base_value& get_rep (void) const { return *m_rep; }

  bool is_copy_of (const octave_value& val) const { return m_rep == val.m_rep; }

  OCTINTERP_API void
  print_info (std::ostream& os, const std::string& prefix = "") const;

  bool save_ascii (std::ostream& os) { return m_rep->save_ascii (os); }

  OCTINTERP_API bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats)
  { return m_rep->save_binary (os, save_as_floats); }

  OCTINTERP_API bool load_binary (std::istream& is, bool swap,
                                  octave::mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                  bool save_as_floats)
  { return m_rep->save_hdf5 (loc_id, name, save_as_floats); }

  OCTINTERP_API bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  OCTINTERP_API int
  write (octave::stream& os, int block_size,
         oct_data_conv::data_type output_type, int skip,
         octave::mach_info::float_format flt_fmt) const;

  octave_base_value * internal_rep (void) const { return m_rep; }

  // These functions exist to support the MEX interface.
  // You should not use them anywhere else.

  OCTINTERP_API const void *
  mex_get_data (mxClassID class_id = mxUNKNOWN_CLASS,
                mxComplexity complexity = mxREAL) const;

  const octave_idx_type * mex_get_ir (void) const
  {
    return m_rep->mex_get_ir ();
  }

  const octave_idx_type *
  mex_get_jc (void) const
  {
    return m_rep->mex_get_jc ();
  }

  mxArray * as_mxArray (bool interleaved = false) const
  { return m_rep->as_mxArray (interleaved); }

  octave_value diag (octave_idx_type k = 0) const
  { return m_rep->diag (k); }

  octave_value diag (octave_idx_type m, octave_idx_type n) const
  { return m_rep->diag (m, n); }

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
  { return m_rep->sort (dim, mode); }
  octave_value sort (Array<octave_idx_type>& sidx, octave_idx_type dim = 0,
                     sortmode mode = ASCENDING) const
  { return m_rep->sort (sidx, dim, mode); }

  sortmode issorted (sortmode mode = UNSORTED) const
  { return m_rep->issorted (mode); }

  Array<octave_idx_type> sort_rows_idx (sortmode mode = ASCENDING) const
  { return m_rep->sort_rows_idx (mode); }

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const
  { return m_rep->is_sorted_rows (mode); }

  void lock (void) { m_rep->lock (); }

  void unlock (void) { m_rep->unlock (); }

  bool islocked (void) const { return m_rep->islocked (); }

  void call_object_destructor (void) { return m_rep->call_object_destructor (); }

  octave_value dump (void) const { return m_rep->dump (); }

#define MAPPER_FORWARD(F) \
  octave_value F (void) const                           \
  {                                                     \
    return m_rep->map (octave_base_value::umap_ ## F);    \
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
  MAPPER_FORWARD (xtolower)
  MAPPER_FORWARD (xtoupper)

#undef MAPPER_FORWARD

  octave_value map (octave_base_value::unary_mapper_t umap) const
  { return m_rep->map (umap); }

  //! Extract the n-th element, aka 'val(n)'.
  //!
  //! @return Result is undefined if 'val' is not an array type
  //!         or @p n is out of range.
  //!
  //! @warning Function calls should never error.

  octave_value
  fast_elem_extract (octave_idx_type n) const
  { return m_rep->fast_elem_extract (n); }

  //! Assign the n-th element, aka 'val(n) = x'.
  //!
  //! @returns false if 'val' is not an array type,
  //!          @p x is not a matching scalar type,
  //!          or @p n is out of range.
  //!
  //! @warning Function calls should never error.

  bool
  fast_elem_insert (octave_idx_type n, const octave_value& x)
  {
    make_unique ();
    return m_rep->fast_elem_insert (n, x);
  }

protected:

  //! The real representation.
  octave_base_value *m_rep;

private:

  static OCTINTERP_API octave_base_value * nil_rep (void);

  OCTINTERP_API assign_op unary_op_to_assign_op (unary_op op);

  OCTINTERP_API binary_op op_eq_to_binary_op (assign_op op);

  // This declaration protects against constructing octave_value from
  // const octave_base_value* which actually silently calls octave_value (bool).
  OCTINTERP_API octave_value (const octave_base_value *);

};

// Non-member unary and binary operations on octave_value objects.

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTINTERP_API octave_value
unary_op (type_info& ti, octave_value::unary_op op,
          const octave_value& a);

extern OCTINTERP_API octave_value
unary_op (octave_value::unary_op op, const octave_value& a);

extern OCTINTERP_API octave_value
binary_op (type_info& ti, octave_value::binary_op op,
           const octave_value& a, const octave_value& b);

extern OCTINTERP_API octave_value
binary_op (type_info& ti, octave_value::compound_binary_op op,
           const octave_value& a, const octave_value& b);

extern OCTINTERP_API octave_value
binary_op (octave_value::binary_op op, const octave_value& a,
           const octave_value& b);

extern OCTINTERP_API octave_value
binary_op (octave_value::compound_binary_op op, const octave_value& a,
           const octave_value& b);

extern OCTINTERP_API octave_value
cat_op (type_info& ti, const octave_value& a,
        const octave_value& b, const Array<octave_idx_type>& ra_idx);

extern OCTINTERP_API octave_value
cat_op (const octave_value& a, const octave_value& b,
        const Array<octave_idx_type>& ra_idx);

extern OCTINTERP_API octave_value
colon_op (const octave_value& base, const octave_value& increment,
          const octave_value& limit, bool is_for_cmd_expr = false);

inline octave_value
colon_op (const octave_value& base, const octave_value& limit,
          bool is_for_cmd_expr = false)
{
  // Note, we need to pass an undefined octave_value object instead of
  // octave_value (1.0) so that we can properly detect the
  // two-argument case and correctly pass just two arguments to any
  // user-defined function that is provided if either base or limit is
  // an object.

  return colon_op (base, octave_value (), limit, is_for_cmd_expr);
}

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
OCTAVE_DEPRECATED (7, "use 'octave::unary_op' instead")
inline octave_value
do_unary_op (octave::type_info& ti, octave_value::unary_op op,
             const octave_value& a)
{
  return octave::unary_op (ti, op, a);
}

OCTAVE_DEPRECATED (7, "use 'octave::unary_op' instead")
inline octave_value
do_unary_op (octave_value::unary_op op, const octave_value& a)
{
  return octave::unary_op (op, a);
}

OCTAVE_DEPRECATED (7, "use 'octave::binary_op' instead")
inline octave_value
do_binary_op (octave::type_info& ti, octave_value::binary_op op,
              const octave_value& a, const octave_value& b)
{
  return octave::binary_op (ti, op, a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::binary_op' instead")
inline octave_value
do_binary_op (octave::type_info& ti, octave_value::compound_binary_op op,
              const octave_value& a, const octave_value& b)
{
  return octave::binary_op (ti, op, a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::binary_op' instead")
inline octave_value
do_binary_op (octave_value::binary_op op, const octave_value& a,
              const octave_value& b)
{
  return octave::binary_op (op, a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::binary_op' instead")
inline octave_value
do_binary_op (octave_value::compound_binary_op op, const octave_value& a,
              const octave_value& b)
{
  return octave::binary_op (op, a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::cat_op' instead")
inline octave_value
do_cat_op (octave::type_info& ti, const octave_value& a,
           const octave_value& b, const Array<octave_idx_type>& ra_idx)
{
  return octave::cat_op (ti, a, b, ra_idx);
}

OCTAVE_DEPRECATED (7, "use 'octave::cat_op' instead")
inline octave_value
do_cat_op (const octave_value& a, const octave_value& b,
           const Array<octave_idx_type>& ra_idx)
{
  return octave::cat_op (a, b, ra_idx);
}

OCTAVE_DEPRECATED (7, "use 'octave::colon_op' instead")
inline octave_value
do_colon_op (const octave_value& base, const octave_value& increment,
             const octave_value& limit, bool is_for_cmd_expr = false)
{
  return octave::colon_op (base, increment, limit, is_for_cmd_expr);
}

OCTAVE_DEPRECATED (7, "use 'octave::colon_op' instead")
inline octave_value
do_colon_op (const octave_value& base, const octave_value& limit,
             bool is_for_cmd_expr = false)
{
  return octave::colon_op (base, limit, is_for_cmd_expr);
}
#endif

#define OV_UNOP_FN(name)                                \
  inline octave_value                                   \
  name (const octave_value& a)                          \
  {                                                     \
    return octave::unary_op (octave_value::name, a);    \
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

#define OV_BINOP_FN(name)                                       \
  inline octave_value                                           \
  name (const octave_value& a1, const octave_value& a2)         \
  {                                                             \
    return octave::binary_op (octave_value::name, a1, a2);      \
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

#define OV_COMP_BINOP_FN(name)                                  \
  inline octave_value                                           \
  name (const octave_value& a1, const octave_value& a2)         \
  {                                                             \
    return octave::binary_op (octave_value::name, a1, a2);      \
  }

OV_COMP_BINOP_FN (op_trans_mul)
OV_COMP_BINOP_FN (op_mul_trans)
OV_COMP_BINOP_FN (op_herm_mul)
OV_COMP_BINOP_FN (op_mul_herm)

extern OCTINTERP_API void install_types (octave::type_info&);

// Templated value extractors.
// FIXME: would be more consistent to use panic_impossible(), rather than
//        assert(), but including "error.h" leads to compilation errors.
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
