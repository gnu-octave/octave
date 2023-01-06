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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>

#include <type_traits>

#include "data-conv.h"
#include "quit.h"
#include "str-vec.h"

#include "ovl.h"
#include "oct-stream.h"
#include "ov.h"
#include "ov-base.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-cell.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-re-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-legacy-range.h"
#include "ov-perm.h"
#include "ov-bool-sparse.h"
#include "ov-cx-sparse.h"
#include "ov-re-sparse.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-cx-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-ch-mat.h"
#include "ov-str-mat.h"
#include "ov-range.h"
#include "ov-struct.h"
#include "ov-class.h"
#include "ov-classdef.h"
#include "ov-oncleanup.h"
#include "ov-cs-list.h"
#include "ov-colon.h"
#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "ov-usr-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-typeinfo.h"
#include "ov-magic-int.h"
#include "ov-null-mat.h"
#include "ov-lazy-idx.h"
#include "ov-java.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "pager.h"
#include "parse.h"
#include "pr-flt-fmt.h"
#include "pr-output.h"
#include "symtab.h"
#include "utils.h"
#include "variables.h"

// We are likely to have a lot of octave_value objects to allocate, so
// make the grow_size large.

// If TRUE, create special space-optimized diagonal matrix objects.

static bool Voptimize_diagonal_matrix = true;

// If TRUE, create special space-optimized permutation matrix objects.

static bool Voptimize_permutation_matrix = true;

// If TRUE, create special space-optimized range objects.

static bool Voptimize_range = true;

// FIXME

// Octave's value type.

octave_base_value *
octave_value::nil_rep (void)
{
  static octave_base_value nr;
  return &nr;
}

std::string
octave_value::unary_op_as_string (unary_op op)
{
  switch (op)
    {
    case op_not:
      return "!";

    case op_uplus:
      return "+";

    case op_uminus:
      return "-";

    case op_transpose:
      return ".'";

    case op_hermitian:
      return "'";

    case op_incr:
      return "++";

    case op_decr:
      return "--";

    default:
      return "<unknown>";
    }
}

std::string
octave_value::unary_op_fcn_name (unary_op op)
{
  switch (op)
    {
    case op_not:
      return "not";

    case op_uplus:
      return "uplus";

    case op_uminus:
      return "uminus";

    case op_transpose:
      return "transpose";

    case op_hermitian:
      return "ctranspose";

    default:
      return "<unknown>";
    }
}

std::string
octave_value::binary_op_as_string (binary_op op)
{
  switch (op)
    {
    case op_add:
      return "+";

    case op_sub:
      return "-";

    case op_mul:
      return "*";

    case op_div:
      return "/";

    case op_pow:
      return "^";

    case op_ldiv:
      return R"(\)";

    case op_lt:
      return "<";

    case op_le:
      return "<=";

    case op_eq:
      return "==";

    case op_ge:
      return ">=";

    case op_gt:
      return ">";

    case op_ne:
      return "!=";

    case op_el_mul:
      return ".*";

    case op_el_div:
      return "./";

    case op_el_pow:
      return ".^";

    case op_el_ldiv:
      return R"(.\)";

    case op_el_and:
      return "&";

    case op_el_or:
      return "|";

    case op_struct_ref:
      return ".";

    default:
      return "<unknown>";
    }
}

std::string
octave_value::binary_op_fcn_name (binary_op op)
{
  switch (op)
    {
    case op_add:
      return "plus";

    case op_sub:
      return "minus";

    case op_mul:
      return "mtimes";

    case op_div:
      return "mrdivide";

    case op_pow:
      return "mpower";

    case op_ldiv:
      return "mldivide";

    case op_lt:
      return "lt";

    case op_le:
      return "le";

    case op_eq:
      return "eq";

    case op_ge:
      return "ge";

    case op_gt:
      return "gt";

    case op_ne:
      return "ne";

    case op_el_mul:
      return "times";

    case op_el_div:
      return "rdivide";

    case op_el_pow:
      return "power";

    case op_el_ldiv:
      return "ldivide";

    case op_el_and:
      return "and";

    case op_el_or:
      return "or";

    default:
      return "<unknown>";
    }
}

std::string
octave_value::binary_op_fcn_name (compound_binary_op op)
{
  switch (op)
    {
    case op_trans_mul:
      return "transtimes";

    case op_mul_trans:
      return "timestrans";

    case op_herm_mul:
      return "hermtimes";

    case op_mul_herm:
      return "timesherm";

    case op_trans_ldiv:
      return "transldiv";

    case op_herm_ldiv:
      return "hermldiv";

    case op_el_and_not:
      return "andnot";

    case op_el_or_not:
      return "ornot";

    case op_el_not_and:
      return "notand";

    case op_el_not_or:
      return "notor";

    default:
      return "<unknown>";
    }
}

std::string
octave_value::assign_op_as_string (assign_op op)
{
  switch (op)
    {
    case op_asn_eq:
      return "=";

    case op_add_eq:
      return "+=";

    case op_sub_eq:
      return "-=";

    case op_mul_eq:
      return "*=";

    case op_div_eq:
      return "/=";

    case op_ldiv_eq:
      return R"(\=)";

    case op_pow_eq:
      return "^=";

    case op_el_mul_eq:
      return ".*=";

    case op_el_div_eq:
      return "./=";

    case op_el_ldiv_eq:
      return R"(.\=)";

    case op_el_pow_eq:
      return ".^=";

    case op_el_and_eq:
      return "&=";

    case op_el_or_eq:
      return "|=";

    default:
      return "<unknown>";
    }
}

octave_value::binary_op
octave_value::assign_op_to_binary_op (assign_op op)
{
  switch (op)
    {
    case op_add_eq:
      return op_add;

    case op_sub_eq:
      return op_sub;

    case op_mul_eq:
      return op_mul;

    case op_div_eq:
      return op_div;

    case op_ldiv_eq:
      return op_ldiv;

    case op_pow_eq:
      return op_pow;

    case op_el_mul_eq:
      return op_el_mul;

    case op_el_div_eq:
      return op_el_div;

    case op_el_ldiv_eq:
      return op_el_ldiv;

    case op_el_pow_eq:
      return op_el_pow;

    case op_el_and_eq:
      return op_el_and;

    case op_el_or_eq:
      return op_el_or;

    default:
      return unknown_binary_op;
    }
}

octave_value::assign_op
octave_value::binary_op_to_assign_op (binary_op op)
{
  switch (op)
    {
    case op_add:
      return op_add_eq;

    case op_sub:
      return op_sub_eq;

    case op_mul:
      return op_mul_eq;

    case op_div:
      return op_div_eq;

    case op_el_mul:
      return op_el_mul_eq;

    case op_el_div:
      return op_el_div_eq;

    case op_el_and:
      return op_el_and_eq;

    case op_el_or:
      return op_el_or_eq;

    default:
      return unknown_assign_op;
    }
}

octave_value::octave_value (short int i)
  : m_rep (new octave_scalar (i))
{ }

octave_value::octave_value (unsigned short int i)
  : m_rep (new octave_scalar (i))
{ }

octave_value::octave_value (int i)
  : m_rep (new octave_scalar (i))
{ }

octave_value::octave_value (unsigned int i)
  : m_rep (new octave_scalar (i))
{ }

octave_value::octave_value (long int i)
  : m_rep (new octave_scalar (i))
{ }

octave_value::octave_value (unsigned long int i)
  : m_rep (new octave_scalar (i))
{ }

#if defined (OCTAVE_HAVE_LONG_LONG_INT)
octave_value::octave_value (long long int i)
  : m_rep (new octave_scalar (i))
{ }
#endif

#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
octave_value::octave_value (unsigned long long int i)
  : m_rep (new octave_scalar (i))
{ }
#endif

octave_value::octave_value (octave::sys::time t)
  : m_rep (new octave_scalar (t.double_value ()))
{ }

octave_value::octave_value (double d)
  : m_rep (new octave_scalar (d))
{ }

octave_value::octave_value (float d)
  : m_rep (new octave_float_scalar (d))
{ }

octave_value::octave_value (const Cell& c, bool is_csl)
  : m_rep (is_csl
           ? dynamic_cast<octave_base_value *> (new octave_cs_list (c))
           : dynamic_cast<octave_base_value *> (new octave_cell (c)))
{ }

octave_value::octave_value (const Array<octave_value>& a, bool is_csl)
  : m_rep (is_csl
           ? dynamic_cast<octave_base_value *> (new octave_cs_list (Cell (a)))
           : dynamic_cast<octave_base_value *> (new octave_cell (Cell (a))))
{ }

octave_value::octave_value (const Matrix& m, const MatrixType& t)
  : m_rep (new octave_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatMatrix& m, const MatrixType& t)
  : m_rep (new octave_float_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const NDArray& a)
  : m_rep (new octave_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatNDArray& a)
  : m_rep (new octave_float_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<double>& a)
  : m_rep (new octave_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<float>& a)
  : m_rep (new octave_float_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<double>& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_matrix (Matrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<float>& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_float_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_float_matrix (FloatMatrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<Complex>& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_complex_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_complex_matrix (ComplexMatrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<FloatComplex>& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_float_complex_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_float_complex_matrix (FloatComplexMatrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagMatrix& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_matrix (Matrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatDiagMatrix& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_float_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_float_matrix (FloatMatrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const RowVector& v)
  : m_rep (new octave_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatRowVector& v)
  : m_rep (new octave_float_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const ColumnVector& v)
  : m_rep (new octave_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatColumnVector& v)
  : m_rep (new octave_float_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const Complex& C)
  : m_rep (new octave_complex (C))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplex& C)
  : m_rep (new octave_float_complex (C))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexMatrix& m, const MatrixType& t)
  : m_rep (new octave_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexMatrix& m, const MatrixType& t)
  : m_rep (new octave_float_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexNDArray& a)
  : m_rep (new octave_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexNDArray& a)
  : m_rep (new octave_float_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<Complex>& a)
  : m_rep (new octave_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<FloatComplex>& a)
  : m_rep (new octave_float_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexDiagMatrix& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_complex_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_complex_matrix (ComplexMatrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexDiagMatrix& d)
  : m_rep (Voptimize_diagonal_matrix
           ? dynamic_cast<octave_base_value *> (new octave_float_complex_diag_matrix (d))
           : dynamic_cast<octave_base_value *> (new octave_float_complex_matrix (FloatComplexMatrix (d))))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexRowVector& v)
  : m_rep (new octave_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexRowVector& v)
  : m_rep (new octave_float_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexColumnVector& v)
  : m_rep (new octave_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexColumnVector& v)
  : m_rep (new octave_float_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const PermMatrix& p)
  : m_rep (Voptimize_permutation_matrix
           ? dynamic_cast<octave_base_value *> (new octave_perm_matrix (p))
           : dynamic_cast<octave_base_value *> (new octave_matrix (Matrix (p))))
{
  maybe_mutate ();
}

octave_value::octave_value (bool b)
  : m_rep (new octave_bool (b))
{ }

octave_value::octave_value (const boolMatrix& bm, const MatrixType& t)
  : m_rep (new octave_bool_matrix (bm, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const boolNDArray& bnda)
  : m_rep (new octave_bool_matrix (bnda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<bool>& bnda)
  : m_rep (new octave_bool_matrix (bnda))
{
  maybe_mutate ();
}

octave_value::octave_value (char c, char type)
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (c)
           : new octave_char_matrix_sq_str (c))
{
  maybe_mutate ();
}

octave_value::octave_value (const char *s, char type)
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (s)
           : new octave_char_matrix_sq_str (s))
{
  maybe_mutate ();
}

octave_value::octave_value (const std::string& s, char type)
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (s)
           : new octave_char_matrix_sq_str (s))
{
  maybe_mutate ();
}

octave_value::octave_value (const string_vector& s, char type)
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (s)
           : new octave_char_matrix_sq_str (s))
{
  maybe_mutate ();
}

octave_value::octave_value (const charMatrix& chm, char type)
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (chm)
           : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const charNDArray& chm, char type)
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (chm)
           : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<char>& chm, char type)
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (chm)
           : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const SparseMatrix& m, const MatrixType& t)
  : m_rep (new octave_sparse_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const Sparse<double>& m, const MatrixType& t)
  : m_rep (new octave_sparse_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const SparseComplexMatrix& m, const MatrixType& t)
  : m_rep (new octave_sparse_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const Sparse<Complex>& m, const MatrixType& t)
  : m_rep (new octave_sparse_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const SparseBoolMatrix& bm, const MatrixType& t)
  : m_rep (new octave_sparse_bool_matrix (bm, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const Sparse<bool>& bm, const MatrixType& t)
  : m_rep (new octave_sparse_bool_matrix (bm, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int8& i)
  : m_rep (new octave_int8_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint8& i)
  : m_rep (new octave_uint8_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int16& i)
  : m_rep (new octave_int16_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint16& i)
  : m_rep (new octave_uint16_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int32& i)
  : m_rep (new octave_int32_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint32& i)
  : m_rep (new octave_uint32_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int64& i)
  : m_rep (new octave_int64_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint64& i)
  : m_rep (new octave_uint64_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const int8NDArray& inda)
  : m_rep (new octave_int8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int8>& inda)
  : m_rep (new octave_int8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint8NDArray& inda)
  : m_rep (new octave_uint8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint8>& inda)
  : m_rep (new octave_uint8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const int16NDArray& inda)
  : m_rep (new octave_int16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int16>& inda)
  : m_rep (new octave_int16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint16NDArray& inda)
  : m_rep (new octave_uint16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint16>& inda)
  : m_rep (new octave_uint16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const int32NDArray& inda)
  : m_rep (new octave_int32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int32>& inda)
  : m_rep (new octave_int32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint32NDArray& inda)
  : m_rep (new octave_uint32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint32>& inda)
  : m_rep (new octave_uint32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const int64NDArray& inda)
  : m_rep (new octave_int64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int64>& inda)
  : m_rep (new octave_int64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint64NDArray& inda)
  : m_rep (new octave_uint64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint64>& inda)
  : m_rep (new octave_uint64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_idx_type>& inda, bool zero_based,
                            bool cache_index)
  : m_rep (new octave_matrix (inda, zero_based, cache_index))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::idx_vector& idx, bool lazy)
  : m_rep ()
{
  double scalar;
  octave::range<double> range;
  NDArray array;
  boolNDArray mask;
  octave::idx_vector::idx_class_type idx_class;

  if (lazy)
    {
      // Only make lazy indices out of ranges and index vectors.
      switch (idx.idx_class ())
        {
        case octave::idx_vector::class_range:
        case octave::idx_vector::class_vector:
          m_rep = new octave_lazy_index (idx);
          maybe_mutate ();
          return;

        default:
          break;
        }
    }

  idx.unconvert (idx_class, scalar, range, array, mask);

  switch (idx_class)
    {
    case octave::idx_vector::class_colon:
      m_rep = new octave_magic_colon ();
      break;

    case octave::idx_vector::class_range:
      m_rep = new octave_range (range, idx);
      break;

    case octave::idx_vector::class_scalar:
      m_rep = new octave_scalar (scalar);
      break;

    case octave::idx_vector::class_vector:
      m_rep = new octave_matrix (array, idx);
      break;

    case octave::idx_vector::class_mask:
      m_rep = new octave_bool_matrix (mask, idx);
      break;

    default:
      panic_impossible ();
      break;
    }

  // FIXME: needed?
  maybe_mutate ();
}

octave_value::octave_value (const Array<std::string>& cellstr)
  : m_rep (new octave_cell (cellstr))
{
  maybe_mutate ();
}

// Remove when public constructor that uses this function is removed.
octave_base_value *
octave_value::make_range_rep_deprecated (double base, double inc, double limit)
{
#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

  return dynamic_cast<octave_base_value *>
         (new octave_legacy_range (Range (base, inc, limit)));

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif
}

// Remove when public constructor that uses this function is removed.
octave_base_value *
octave_value::make_range_rep_deprecated (const Range& r, bool force_range)
{
  if (! force_range && ! r.ok ())
    error ("invalid range");

  if ((force_range || Voptimize_range))
    return dynamic_cast<octave_base_value *> (new octave_legacy_range (r));
  else
    return dynamic_cast<octave_base_value *> (new octave_matrix (r.matrix_value ()));
}

octave_value::octave_value (const octave::range<double>& r, bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<double> (r))
           : dynamic_cast<octave_base_value *> (new octave_matrix (r.array_value ())))
{
  maybe_mutate ();
}

// For now, disable all but range<double>.

#if 0

octave_value::octave_value (const octave::range<float>& r, bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<float> (r))
           : dynamic_cast<octave_base_value *> (new octave_float_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_int8>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_int8> (r))
           : dynamic_cast<octave_base_value *> (new octave_int8_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_int16>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_int16> (r))
           : dynamic_cast<octave_base_value *> (new octave_int16_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_int32>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_int32> (r))
           : dynamic_cast<octave_base_value *> (new octave_int32_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_int64>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_int64> (r))
           : dynamic_cast<octave_base_value *> (new octave_int64_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_uint8>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_uint8> (r))
           : dynamic_cast<octave_base_value *> (new octave_uint8_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_uint16>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_uint16> (r))
           : dynamic_cast<octave_base_value *> (new octave_uint16_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_uint32>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_uint32> (r))
           : dynamic_cast<octave_base_value *> (new octave_uint32_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<octave_uint64>& r,
                            bool force_range)
  : m_rep (force_range || Voptimize_range
           ? dynamic_cast<octave_base_value *> (new ov_range<octave_uint64> (r))
           : dynamic_cast<octave_base_value *> (new octave_uint64_matrix (r.array_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave::range<char>& r, char type,
                            bool /*force_range*/)
#if 0
  : m_rep (force_range || optimize_range
           ? dynamic_cast<octave_base_value *> (new octave_char_range (r, type))
           : dynamic_cast<octave_base_value *> (type == '"'
               ? new octave_char_matrix_dq_str (r.array_value ())
               : new octave_char_matrix_sq_str (r.array_value ())))
#else
  : m_rep (type == '"'
           ? new octave_char_matrix_dq_str (r.array_value ())
           : new octave_char_matrix_sq_str (r.array_value ()))
#endif
{
  maybe_mutate ();
}

#endif

octave_value::octave_value (const octave_map& m)
  : m_rep (new octave_struct (m))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_scalar_map& m)
  : m_rep (new octave_scalar_struct (m))
{ }

octave_value::octave_value (const std::map<std::string, octave_value>& m)
  : m_rep (new octave_scalar_struct (m))
{ }

octave_value::octave_value (const octave_map& m, const std::string& id,
                            const std::list<std::string>& plist)
  : m_rep (new octave_class (m, id, plist))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_scalar_map& m, const std::string& id,
                            const std::list<std::string>& plist)
  : m_rep (new octave_class (m, id, plist))
{ }

octave_value::octave_value (const octave_value_list& l)
  : m_rep (new octave_cs_list (l))
{ }

octave_value::octave_value (octave_value::magic_colon)
  : m_rep (new octave_magic_colon ())
{ }

octave_value::octave_value (octave_base_value *new_rep, bool borrow)
  : m_rep (new_rep)
{
  if (borrow)
    m_rep->count++;
}

octave_base_value *
octave_value::clone (void) const
{
  return m_rep->clone ();
}

void
octave_value::break_closure_cycles (const std::shared_ptr<octave::stack_frame>& frame)
{
  if (is_function_handle ())
    {
      octave_fcn_handle *fhdl = m_rep->fcn_handle_value ();

      if (fhdl->is_nested (frame) && ! fhdl->is_weak_nested ())
        *this = fhdl->make_weak_nested_handle ();
      else if (fhdl->is_anonymous () && ! fhdl->is_weak_anonymous ())
        *this = fhdl->make_weak_anonymous_handle ();
    }
  else
    {
      // FIXME: Is there a efficient way to avoid calling make_unique
      // if REP doesn't contain any nested function handles?
      //
      // Probably we should be asking REP to make a modified copy IFF it
      // is needed, then replace our REP with that if a copy is made,
      // otherwise we leave it alone.

      make_unique ();

      m_rep->break_closure_cycles (frame);
    }
}

void
octave_value::maybe_mutate (void)
{
  octave_base_value *tmp = m_rep->try_narrowing_conversion ();

  if (tmp && tmp != m_rep)
    {
      if (--m_rep->count == 0)
        delete m_rep;

      m_rep = tmp;
    }
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (double, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} double (@var{x})
Convert @var{x} to double precision type.
@seealso{single}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).as_double ());
}

/*
%!assert (class (double (single (1))), "double")
%!assert (class (double (single (1 + i))), "double")
%!assert (class (double (int8 (1))), "double")
%!assert (class (double (uint8 (1))), "double")
%!assert (class (double (int16 (1))), "double")
%!assert (class (double (uint16 (1))), "double")
%!assert (class (double (int32 (1))), "double")
%!assert (class (double (uint32 (1))), "double")
%!assert (class (double (int64 (1))), "double")
%!assert (class (double (uint64 (1))), "double")
%!assert (class (double (true)), "double")
%!assert (class (double ("A")), "double")
%!test
%! x = sparse (logical ([1 0; 0 1]));
%! y = double (x);
%! assert (class (x), "logical");
%! assert (class (y), "double");
%! assert (issparse (y));
%!test
%! x = diag (single ([1 3 2]));
%! y = double (x);
%! assert (class (x), "single");
%! assert (class (y), "double");
%!test
%! x = diag (single ([i 3 2]));
%! y = double (x);
%! assert (class (x), "single");
%! assert (class (y), "double");
*/

DEFUN (single, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} single (@var{x})
Convert @var{x} to single precision type.
@seealso{double}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_single ();

  return ovl ();
}

/*
%!assert (class (single (1)), "single")
%!assert (class (single (1 + i)), "single")
%!assert (class (single (int8 (1))), "single")
%!assert (class (single (uint8 (1))), "single")
%!assert (class (single (int16 (1))), "single")
%!assert (class (single (uint16 (1))), "single")
%!assert (class (single (int32 (1))), "single")
%!assert (class (single (uint32 (1))), "single")
%!assert (class (single (int64 (1))), "single")
%!assert (class (single (uint64 (1))), "single")
%!assert (class (single (true)), "single")
%!assert (class (single ("A")), "single")
%!error single (sparse (1))
%!test
%! x = diag ([1 3 2]);
%! y = single (x);
%! assert (class (x), "double");
%! assert (class (y), "single");
%!test
%! x = diag ([i 3 2]);
%! y = single (x);
%! assert (class (x), "double");
%! assert (class (y), "single");
*/

DEFUN (int8, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} int8 (@var{x})
Convert @var{x} to 8-bit integer type.
@seealso{uint8, int16, uint16, int32, uint32, int64, uint64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_int8 ();
}

/*
%!assert (class (int8 (1)), "int8")
%!assert (int8 (1.25), int8 (1))
%!assert (int8 (1.5), int8 (2))
%!assert (int8 (-1.5), int8 (-2))
%!assert (int8 (2^9), int8 (2^8-1))
%!assert (int8 (-2^9), int8 (-2^8))
*/

DEFUN (int16, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} int16 (@var{x})
Convert @var{x} to 16-bit integer type.
@seealso{int8, uint8, uint16, int32, uint32, int64, uint64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_int16 ();
}

/*
%!assert (class (int16 (1)), "int16")
%!assert (int16 (1.25), int16 (1))
%!assert (int16 (1.5), int16 (2))
%!assert (int16 (-1.5), int16 (-2))
%!assert (int16 (2^17), int16 (2^16-1))
%!assert (int16 (-2^17), int16 (-2^16))
*/

DEFUN (int32, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} int32 (@var{x})
Convert @var{x} to 32-bit integer type.
@seealso{int8, uint8, int16, uint16, uint32, int64, uint64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_int32 ();
}

/*
%!assert (class (int32 (1)), "int32")
%!assert (int32 (1.25), int32 (1))
%!assert (int32 (1.5), int32 (2))
%!assert (int32 (-1.5), int32 (-2))
%!assert (int32 (2^33), int32 (2^32-1))
%!assert (int32 (-2^33), int32 (-2^32))
*/

DEFUN (int64, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} int64 (@var{x})
Convert @var{x} to 64-bit integer type.
@seealso{int8, uint8, int16, uint16, int32, uint32, uint64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_int64 ();
}

/*
%!assert (class (int64 (1)), "int64")
%!assert (int64 (1.25), int64 (1))
%!assert (int64 (1.5), int64 (2))
%!assert (int64 (-1.5), int64 (-2))
%!assert (int64 (2^65), int64 (2^64-1))
%!assert (int64 (-2^65), int64 (-2^64))
*/

DEFUN (uint8, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} uint8 (@var{x})
Convert @var{x} to unsigned 8-bit integer type.
@seealso{int8, int16, uint16, int32, uint32, int64, uint64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_uint8 ();
}

/*
%!assert (class (uint8 (1)), "uint8")
%!assert (uint8 (1.25), uint8 (1))
%!assert (uint8 (1.5), uint8 (2))
%!assert (uint8 (-1.5), uint8 (0))
%!assert (uint8 (2^9), uint8 (2^8-1))
%!assert (uint8 (-2^9), uint8 (0))
*/

DEFUN (uint16, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} uint16 (@var{x})
Convert @var{x} to unsigned 16-bit integer type.
@seealso{int8, uint8, int16, int32, uint32, int64, uint64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_uint16 ();
}

/*
%!assert (class (uint16 (1)), "uint16")
%!assert (uint16 (1.25), uint16 (1))
%!assert (uint16 (1.5), uint16 (2))
%!assert (uint16 (-1.5), uint16 (0))
%!assert (uint16 (2^17), uint16 (2^16-1))
%!assert (uint16 (-2^17), uint16 (0))
*/

DEFUN (uint32, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} uint32 (@var{x})
Convert @var{x} to unsigned 32-bit integer type.
@seealso{int8, uint8, int16, uint16, int32, int64, uint64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_uint32 ();
}

/*
%!assert (class (uint32 (1)), "uint32")
%!assert (uint32 (1.25), uint32 (1))
%!assert (uint32 (1.5), uint32 (2))
%!assert (uint32 (-1.5), uint32 (0))
%!assert (uint32 (2^33), uint32 (2^32-1))
%!assert (uint32 (-2^33), uint32 (0))
*/

DEFUN (uint64, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} uint64 (@var{x})
Convert @var{x} to unsigned 64-bit integer type.
@seealso{int8, uint8, int16, uint16, int32, uint32, int64}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return args(0).as_uint64 ();
}

/*
%!assert (class (uint64 (1)), "uint64")
%!assert (uint64 (1.25), uint64 (1))
%!assert (uint64 (1.5), uint64 (2))
%!assert (uint64 (-1.5), uint64 (0))
%!assert (uint64 (2^65), uint64 (2^64-1))
%!assert (uint64 (-2^65), uint64 (0))
*/

OCTAVE_END_NAMESPACE(octave)

octave_value
octave_value::single_subsref (const std::string& type,
                              const octave_value_list& idx)
{
  std::list<octave_value_list> i;

  i.push_back (idx);

  return m_rep->subsref (type, i);
}

octave_value_list
octave_value::subsref (const std::string& type,
                       const std::list<octave_value_list>& idx, int nargout)
{
  return m_rep->subsref (type, idx, nargout);
}

octave_value
octave_value::next_subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            std::size_t skip)
{
  if (idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (std::size_t i = 0; i < skip; i++)
        new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx);
    }
  else
    return *this;
}

octave_value_list
octave_value::next_subsref (int nargout, const std::string& type,
                            const std::list<octave_value_list>& idx,
                            std::size_t skip)
{
  if (idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (std::size_t i = 0; i < skip; i++)
        new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx, nargout);
    }
  else
    return *this;
}

octave_value
octave_value::next_subsref (bool auto_add, const std::string& type,
                            const std::list<octave_value_list>& idx,
                            std::size_t skip)
{
  if (idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (std::size_t i = 0; i < skip; i++)
        new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx, auto_add);
    }
  else
    return *this;
}

octave_value
octave_value::subsasgn (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        const octave_value& rhs)
{
  return m_rep->subsasgn (type, idx, rhs);
}

octave_value
octave_value::undef_subsasgn (const std::string& type,
                              const std::list<octave_value_list>& idx,
                              const octave_value& rhs)
{
  return m_rep->undef_subsasgn (type, idx, rhs);
}

octave_value&
octave_value::assign (assign_op op, const std::string& type,
                      const std::list<octave_value_list>& idx,
                      const octave_value& rhs)
{
  make_unique ();

  octave_value t_rhs = rhs;

  if (op != op_asn_eq)
    {
      if (! is_defined ())
        error ("in computed assignment A(index) OP= X, A must be defined first");

      octave_value t = subsref (type, idx);

      binary_op binop = op_eq_to_binary_op (op);

      t_rhs = octave::binary_op (binop, t, rhs);
    }

  *this = subsasgn (type, idx, t_rhs);

  return *this;
}

octave_value&
octave_value::assign (assign_op op, const octave_value& rhs)
{
  if (op == op_asn_eq)
    // Regularize a null matrix if stored into a variable.
    operator = (rhs.storable_value ());
  else if (is_defined ())
    {
      octave::type_info::assign_op_fcn f = nullptr;

      // Only attempt to operate in-place if this variable is unshared.
      if (m_rep->count == 1)
        {
          int tthis = this->type_id ();
          int trhs = rhs.type_id ();

          octave::type_info& ti = octave::__get_type_info__ ();

          f = ti.lookup_assign_op (op, tthis, trhs);
        }

      if (f)
        {
          f (*m_rep, octave_value_list (), rhs.get_rep ());
          // Usually unnecessary, but may be needed (complex arrays).
          maybe_mutate ();
        }
      else
        {

          binary_op binop = op_eq_to_binary_op (op);

          octave_value t = octave::binary_op (binop, *this, rhs);

          operator = (t);
        }
    }
  else
    error ("in computed assignment A OP= X, A must be defined first");

  return *this;
}

// FIXME: This is a bit of a kluge.  We'd like to just use val.dims()
// and if val is an object, expect that dims will call size if it is
// overloaded by a user-defined method.  But there are currently some
// unresolved const issues that prevent that solution from working.

std::string
octave_value::get_dims_str (void) const
{
  octave_value tmp = *this;

  Matrix sz = tmp.size ();

  dim_vector dv = dim_vector::alloc (sz.numel ());

  for (octave_idx_type i = 0; i < dv.ndims (); i++)
    dv(i) = sz(i);

  return dv.str ();
}

octave_idx_type
octave_value::length (void) const
{
  octave_idx_type retval = 0;

  const dim_vector dv = dims ();

  for (int i = 0; i < dv.ndims (); i++)
    {
      if (dv(i) == 0)
        {
          retval = 0;
          break;
        }

      if (dv(i) > retval)
        retval = dv(i);
    }

  return retval;
}

bool
octave_value::is_equal (const octave_value& test) const
{
  bool retval = false;

  // If there is no op_eq for these types, we can't compare values.

  if (rows () == test.rows () && columns () == test.columns ())
    {
      octave_value tmp = octave::binary_op (octave_value::op_eq, *this, test);

      // Empty array also means a match.
      if (tmp.is_defined ())
        {
          if (tmp.isempty ())
            retval = true;
          else
            {
              // Reshape into a vector and call all() explicitly,
              // to avoid Octave:array-as-logical warning.
              tmp = tmp.reshape (dim_vector (tmp.numel (), 1));
              retval = tmp.all ().is_true ();
            }
        }
    }

  return retval;
}

// Define the idx_type_value function here instead of in ov.h to avoid
// needing definitions for the SIZEOF_X macros in ov.h.

octave_idx_type
octave_value::idx_type_value (bool req_int, bool frc_str_conv) const
{
#if defined (OCTAVE_ENABLE_64)
  return int64_value (req_int, frc_str_conv);
#else
  return int_value (req_int, frc_str_conv);
#endif
}

Cell
octave_value::cell_value (void) const
{
  return m_rep->cell_value ();
}

octave_map
octave_value::map_value (void) const
{
  return m_rep->map_value ();
}

octave_scalar_map
octave_value::scalar_map_value (void) const
{
  return m_rep->scalar_map_value ();
}

octave_function *
octave_value::function_value (bool silent) const
{
  return m_rep->function_value (silent);
}

octave_classdef *
octave_value::classdef_object_value (bool silent) const
{
  return m_rep->classdef_object_value (silent);
}

octave_user_function *
octave_value::user_function_value (bool silent) const
{
  return m_rep->user_function_value (silent);
}

octave_user_script *
octave_value::user_script_value (bool silent) const
{
  return m_rep->user_script_value (silent);
}

octave_user_code *
octave_value::user_code_value (bool silent) const
{
  return m_rep->user_code_value (silent);
}

octave_fcn_handle *
octave_value::fcn_handle_value (bool silent) const
{
  return m_rep->fcn_handle_value (silent);
}

octave_value_list
octave_value::list_value (void) const
{
  return m_rep->list_value ();
}

static dim_vector
make_vector_dims (const dim_vector& dv, bool force_vector_conversion,
                  const std::string& my_type, const std::string& wanted_type)
{
  dim_vector retval (dv);
  retval.chop_trailing_singletons ();
  octave_idx_type nel = dv.numel ();

  if (retval.ndims () > 2 || (retval(0) != 1 && retval(1) != 1))
    {
      if (! force_vector_conversion)
        warn_implicit_conversion ("Octave:array-to-vector",
                                  my_type.c_str (), wanted_type.c_str ());
      retval = dim_vector (nel, 1);
    }

  return retval;
}

ColumnVector
octave_value::column_vector_value (bool force_string_conv,
                                   bool frc_vec_conv) const
{
  return ColumnVector (vector_value (force_string_conv,
                                     frc_vec_conv));
}

ComplexColumnVector
octave_value::complex_column_vector_value (bool force_string_conv,
    bool frc_vec_conv) const
{
  return ComplexColumnVector (complex_vector_value (force_string_conv,
                              frc_vec_conv));
}

RowVector
octave_value::row_vector_value (bool force_string_conv,
                                bool frc_vec_conv) const
{
  return RowVector (vector_value (force_string_conv,
                                  frc_vec_conv));
}

ComplexRowVector
octave_value::complex_row_vector_value (bool force_string_conv,
                                        bool frc_vec_conv) const
{
  return ComplexRowVector (complex_vector_value (force_string_conv,
                           frc_vec_conv));
}

Array<double>
octave_value::vector_value (bool force_string_conv,
                            bool force_vector_conversion) const
{
  Array<double> retval = array_value (force_string_conv);

  return retval.reshape (make_vector_dims (retval.dims (),
                         force_vector_conversion,
                         type_name (), "real vector"));
}

template <typename T>
static Array<int>
convert_to_int_array (const Array<octave_int<T>>& A)
{
  Array<int> retval (A.dims ());
  octave_idx_type n = A.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    retval.xelem (i) = octave_int<int> (A.xelem (i));

  return retval;
}

Array<int>
octave_value::int_vector_value (bool require_int, bool force_string_conv,
                                bool force_vector_conversion) const
{
  Array<int> retval;

  if (isinteger ())
    {
      if (is_int32_type ())
        retval = convert_to_int_array (int32_array_value ());
      else if (is_int64_type ())
        retval = convert_to_int_array (int64_array_value ());
      else if (is_int16_type ())
        retval = convert_to_int_array (int16_array_value ());
      else if (is_int8_type ())
        retval = convert_to_int_array (int8_array_value ());
      else if (is_uint32_type ())
        retval = convert_to_int_array (uint32_array_value ());
      else if (is_uint64_type ())
        retval = convert_to_int_array (uint64_array_value ());
      else if (is_uint16_type ())
        retval = convert_to_int_array (uint16_array_value ());
      else if (is_uint8_type ())
        retval = convert_to_int_array (uint8_array_value ());
      else
        retval = array_value (force_string_conv);
    }
  else
    {
      const NDArray a = array_value (force_string_conv);

      if (require_int)
        {
          retval.resize (a.dims ());
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              double ai = a.elem (i);
              int v = static_cast<int> (ai);
              if (ai == v)
                retval.xelem (i) = v;
              else
                {
                  error_with_cfn ("conversion of %g to int value failed", ai);
                  break;
                }
            }
        }
      else
        retval = Array<int> (a);
    }

  return retval.reshape (make_vector_dims (retval.dims (),
                         force_vector_conversion,
                         type_name (), "integer vector"));
}

template <typename T>
static Array<octave_idx_type>
convert_to_octave_idx_type_array (const Array<octave_int<T>>& A)
{
  Array<octave_idx_type> retval (A.dims ());
  octave_idx_type n = A.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    retval.xelem (i) = octave_int<octave_idx_type> (A.xelem (i));

  return retval;
}

Array<octave_idx_type>
octave_value::octave_idx_type_vector_value (bool require_int,
    bool force_string_conv,
    bool force_vector_conversion) const
{
  Array<octave_idx_type> retval;

  if (isinteger ())
    {
      if (is_int32_type ())
        retval = convert_to_octave_idx_type_array (int32_array_value ());
      else if (is_int64_type ())
        retval = convert_to_octave_idx_type_array (int64_array_value ());
      else if (is_int16_type ())
        retval = convert_to_octave_idx_type_array (int16_array_value ());
      else if (is_int8_type ())
        retval = convert_to_octave_idx_type_array (int8_array_value ());
      else if (is_uint32_type ())
        retval = convert_to_octave_idx_type_array (uint32_array_value ());
      else if (is_uint64_type ())
        retval = convert_to_octave_idx_type_array (uint64_array_value ());
      else if (is_uint16_type ())
        retval = convert_to_octave_idx_type_array (uint16_array_value ());
      else if (is_uint8_type ())
        retval = convert_to_octave_idx_type_array (uint8_array_value ());
      else
        retval = array_value (force_string_conv);
    }
  else
    {
      const NDArray a = array_value (force_string_conv);

      if (require_int)
        {
          retval.resize (a.dims ());
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              double ai = a.elem (i);
              octave_idx_type v = static_cast<octave_idx_type> (ai);
              if (ai == v)
                retval.xelem (i) = v;
              else
                {
                  error_with_cfn ("conversion of %g to octave_idx_type value failed", ai);
                  break;
                }
            }
        }
      else
        retval = Array<octave_idx_type> (a);
    }

  return retval.reshape (make_vector_dims (retval.dims (),
                         force_vector_conversion,
                         type_name (), "integer vector"));
}

Array<Complex>
octave_value::complex_vector_value (bool force_string_conv,
                                    bool force_vector_conversion) const
{
  Array<Complex> retval = complex_array_value (force_string_conv);

  return retval.reshape (make_vector_dims (retval.dims (),
                         force_vector_conversion,
                         type_name (), "complex vector"));
}

FloatColumnVector
octave_value::float_column_vector_value (bool force_string_conv,
    bool frc_vec_conv) const
{
  return FloatColumnVector (float_vector_value (force_string_conv,
                            frc_vec_conv));
}

FloatComplexColumnVector
octave_value::float_complex_column_vector_value (bool force_string_conv,
    bool frc_vec_conv) const
{
  return
    FloatComplexColumnVector (float_complex_vector_value (force_string_conv,
                              frc_vec_conv));
}

FloatRowVector
octave_value::float_row_vector_value (bool force_string_conv,
                                      bool frc_vec_conv) const
{
  return FloatRowVector (float_vector_value (force_string_conv,
                         frc_vec_conv));
}

FloatComplexRowVector
octave_value::float_complex_row_vector_value (bool force_string_conv,
    bool frc_vec_conv) const
{
  return FloatComplexRowVector (float_complex_vector_value (force_string_conv,
                                frc_vec_conv));
}

Array<float>
octave_value::float_vector_value (bool force_string_conv,
                                  bool force_vector_conversion) const
{
  Array<float> retval = float_array_value (force_string_conv);

  return retval.reshape (make_vector_dims (retval.dims (),
                         force_vector_conversion,
                         type_name (), "real vector"));
}

Array<FloatComplex>
octave_value::float_complex_vector_value (bool force_string_conv,
    bool force_vector_conversion) const
{
  Array<FloatComplex> retval = float_complex_array_value (force_string_conv);

  return retval.reshape (make_vector_dims (retval.dims (),
                         force_vector_conversion,
                         type_name (), "complex vector"));
}

// NAME can't always be "x ## FCN" because some of the original
// value extraction functions perform implicit type conversions that we
// wish to avoid for these functions.

#define XVALUE_EXTRACTOR(TYPE, NAME, FCN)               \
  TYPE                                                  \
  octave_value::NAME (const char *fmt, ...) const       \
  {                                                     \
    TYPE retval;                                        \
                                                        \
    try                                                 \
      {                                                 \
        retval = FCN ();                                \
      }                                                 \
    catch (octave::execution_exception& ee)             \
      {                                                 \
        if (fmt)                                        \
          {                                             \
            va_list args;                               \
            va_start (args, fmt);                       \
            verror (ee, fmt, args);                     \
            va_end (args);                              \
          }                                             \
                                                        \
        throw ee;                                       \
      }                                                 \
                                                        \
    return retval;                                      \
  }

XVALUE_EXTRACTOR (short int, xshort_value, short_value)

XVALUE_EXTRACTOR (unsigned short int, xushort_value, ushort_value)

XVALUE_EXTRACTOR (int, xint_value, int_value)

XVALUE_EXTRACTOR (unsigned int, xuint_value, uint_value)

XVALUE_EXTRACTOR (int, xnint_value, nint_value)

XVALUE_EXTRACTOR (long int, xlong_value, long_value)

XVALUE_EXTRACTOR (unsigned long int, xulong_value, ulong_value)

XVALUE_EXTRACTOR (int64_t, xint64_value, int64_value)

XVALUE_EXTRACTOR (uint64_t, xuint64_value, uint64_value)

XVALUE_EXTRACTOR (octave_idx_type, xidx_type_value, idx_type_value)

XVALUE_EXTRACTOR (double, xdouble_value, double_value)
XVALUE_EXTRACTOR (float, xfloat_value, float_value)

XVALUE_EXTRACTOR (double, xscalar_value, scalar_value)
XVALUE_EXTRACTOR (float, xfloat_scalar_value, float_scalar_value)

XVALUE_EXTRACTOR (Matrix, xmatrix_value, matrix_value)
XVALUE_EXTRACTOR (FloatMatrix, xfloat_matrix_value, float_matrix_value)

XVALUE_EXTRACTOR (NDArray, xarray_value, array_value)
XVALUE_EXTRACTOR (FloatNDArray, xfloat_array_value, float_array_value)

XVALUE_EXTRACTOR (Complex, xcomplex_value, complex_value)
XVALUE_EXTRACTOR (FloatComplex, xfloat_complex_value, float_complex_value)

XVALUE_EXTRACTOR (ComplexMatrix, xcomplex_matrix_value, complex_matrix_value)
XVALUE_EXTRACTOR (FloatComplexMatrix, xfloat_complex_matrix_value, float_complex_matrix_value)

XVALUE_EXTRACTOR (ComplexNDArray, xcomplex_array_value, complex_array_value)
XVALUE_EXTRACTOR (FloatComplexNDArray, xfloat_complex_array_value, float_complex_array_value)

XVALUE_EXTRACTOR (bool, xbool_value, bool_value)
XVALUE_EXTRACTOR (boolMatrix, xbool_matrix_value, bool_matrix_value)
XVALUE_EXTRACTOR (boolNDArray, xbool_array_value, bool_array_value)

XVALUE_EXTRACTOR (charMatrix, xchar_matrix_value, char_matrix_value)
XVALUE_EXTRACTOR (charNDArray, xchar_array_value, char_array_value)

XVALUE_EXTRACTOR (SparseMatrix, xsparse_matrix_value, sparse_matrix_value)
XVALUE_EXTRACTOR (SparseComplexMatrix, xsparse_complex_matrix_value, sparse_complex_matrix_value)
XVALUE_EXTRACTOR (SparseBoolMatrix, xsparse_bool_matrix_value, sparse_bool_matrix_value)

XVALUE_EXTRACTOR (DiagMatrix, xdiag_matrix_value, diag_matrix_value)
XVALUE_EXTRACTOR (FloatDiagMatrix, xfloat_diag_matrix_value, float_diag_matrix_value)
XVALUE_EXTRACTOR (ComplexDiagMatrix, xcomplex_diag_matrix_value, complex_diag_matrix_value)
XVALUE_EXTRACTOR (FloatComplexDiagMatrix, xfloat_complex_diag_matrix_value,
                  float_complex_diag_matrix_value)

XVALUE_EXTRACTOR (PermMatrix, xperm_matrix_value, perm_matrix_value)

XVALUE_EXTRACTOR (octave_int8, xint8_scalar_value, int8_scalar_value)
XVALUE_EXTRACTOR (octave_int16, xint16_scalar_value, int16_scalar_value)
XVALUE_EXTRACTOR (octave_int32, xint32_scalar_value, int32_scalar_value)
XVALUE_EXTRACTOR (octave_int64, xint64_scalar_value, int64_scalar_value)

XVALUE_EXTRACTOR (octave_uint8, xuint8_scalar_value, uint8_scalar_value)
XVALUE_EXTRACTOR (octave_uint16, xuint16_scalar_value, uint16_scalar_value)
XVALUE_EXTRACTOR (octave_uint32, xuint32_scalar_value, uint32_scalar_value)
XVALUE_EXTRACTOR (octave_uint64, xuint64_scalar_value, uint64_scalar_value)

XVALUE_EXTRACTOR (int8NDArray, xint8_array_value, int8_array_value)
XVALUE_EXTRACTOR (int16NDArray, xint16_array_value, int16_array_value)
XVALUE_EXTRACTOR (int32NDArray, xint32_array_value, int32_array_value)
XVALUE_EXTRACTOR (int64NDArray, xint64_array_value, int64_array_value)

XVALUE_EXTRACTOR (uint8NDArray, xuint8_array_value, uint8_array_value)
XVALUE_EXTRACTOR (uint16NDArray, xuint16_array_value, uint16_array_value)
XVALUE_EXTRACTOR (uint32NDArray, xuint32_array_value, uint32_array_value)
XVALUE_EXTRACTOR (uint64NDArray, xuint64_array_value, uint64_array_value)

XVALUE_EXTRACTOR (std::string, xstring_value, m_rep->xstring_value)
XVALUE_EXTRACTOR (string_vector, xstring_vector_value, string_vector_value)

XVALUE_EXTRACTOR (Cell, xcell_value, cell_value)
XVALUE_EXTRACTOR (Array<std::string>, xcellstr_value, cellstr_value)

XVALUE_EXTRACTOR (octave::range<double>, xrange_value, range_value)

// For now, disable all but ov_range<double>.

#if 0

XVALUE_EXTRACTOR (octave::range<float>, xfloat_range_value, float_range_value)
XVALUE_EXTRACTOR (octave::range<octave_int8>, xint8_range_value, int8_range_value)
XVALUE_EXTRACTOR (octave::range<octave_int16>, xint16_range_value, int16_range_value)
XVALUE_EXTRACTOR (octave::range<octave_int32>, xint32_range_value, int32_range_value)
XVALUE_EXTRACTOR (octave::range<octave_int64>, xint64_range_value, int64_range_value)
XVALUE_EXTRACTOR (octave::range<octave_uint8>, xuint8_range_value, uint8_range_value)
XVALUE_EXTRACTOR (octave::range<octave_uint16>, xuint16_range_value, uint16_range_value)
XVALUE_EXTRACTOR (octave::range<octave_uint32>, xuint32_range_value, uint32_range_value)
XVALUE_EXTRACTOR (octave::range<octave_uint64>, xuint64_range_value, uint64_range_value)

#endif

XVALUE_EXTRACTOR (octave_map, xmap_value, map_value)
XVALUE_EXTRACTOR (octave_scalar_map, xscalar_map_value, scalar_map_value)

XVALUE_EXTRACTOR (ColumnVector, xcolumn_vector_value, column_vector_value)
XVALUE_EXTRACTOR (ComplexColumnVector, xcomplex_column_vector_value, complex_column_vector_value)

XVALUE_EXTRACTOR (RowVector, xrow_vector_value, row_vector_value)
XVALUE_EXTRACTOR (ComplexRowVector, xcomplex_row_vector_value, complex_row_vector_value)

XVALUE_EXTRACTOR (FloatColumnVector, xfloat_column_vector_value, float_column_vector_value)
XVALUE_EXTRACTOR (FloatComplexColumnVector, xfloat_complex_column_vector_value,
                  float_complex_column_vector_value)

XVALUE_EXTRACTOR (FloatRowVector, xfloat_row_vector_value, float_row_vector_value)
XVALUE_EXTRACTOR (FloatComplexRowVector, xfloat_complex_row_vector_value,
                  float_complex_row_vector_value)

XVALUE_EXTRACTOR (Array<int>, xint_vector_value, int_vector_value)
XVALUE_EXTRACTOR (Array<octave_idx_type>, xoctave_idx_type_vector_value,
                  octave_idx_type_vector_value)

XVALUE_EXTRACTOR (Array<double>, xvector_value, vector_value)
XVALUE_EXTRACTOR (Array<Complex>, xcomplex_vector_value, complex_vector_value)

XVALUE_EXTRACTOR (Array<float>, xfloat_vector_value, float_vector_value)
XVALUE_EXTRACTOR (Array<FloatComplex>, xfloat_complex_vector_value, float_complex_vector_value)

XVALUE_EXTRACTOR (octave_function *, xfunction_value, function_value)
XVALUE_EXTRACTOR (octave_user_function *, xuser_function_value, user_function_value)
XVALUE_EXTRACTOR (octave_user_script *, xuser_script_value, user_script_value)
XVALUE_EXTRACTOR (octave_user_code *, xuser_code_value, user_code_value)
XVALUE_EXTRACTOR (octave_fcn_handle *, xfcn_handle_value, fcn_handle_value)

XVALUE_EXTRACTOR (octave_value_list, xlist_value, list_value)

#undef XVALUE_EXTRACTOR

octave_value
octave_value::storable_value (void) const
{
  octave_value retval = *this;
  if (isnull ())
    retval = octave_value (m_rep->empty_clone ());
  else if (is_magic_int ())
    retval = octave_value (m_rep->double_value ());
  else if (is_range () && ! m_rep->is_storable ())
    error ("range with infinite number of elements cannot be stored");
  else
    retval.maybe_economize ();

  return retval;
}

void
octave_value::make_storable_value (void)
{
  if (isnull ())
    {
      octave_base_value *rc = m_rep->empty_clone ();
      if (--m_rep->count == 0)
        delete m_rep;
      m_rep = rc;
    }
  else if (is_magic_int ())
    {
      octave_base_value *rc = new octave_scalar (m_rep->double_value ());
      if (--m_rep->count == 0)
        delete m_rep;
      m_rep = rc;
    }
  else if (is_range () && ! m_rep->is_storable ())
    error ("range with infinite number of elements cannot be stored");
  else
    maybe_economize ();
}

float_display_format
octave_value::get_edit_display_format (void) const
{
  return m_rep->get_edit_display_format ();
}

int
octave_value::write (octave::stream& os, int block_size,
                     oct_data_conv::data_type output_type, int skip,
                     octave::mach_info::float_format flt_fmt) const
{
  return m_rep->write (os, block_size, output_type, skip, flt_fmt);
}

void
octave_value::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "type_name: " << type_name () << "\n"
     << prefix << "count:     " << get_count () << "\n"
     << prefix << "m_rep info:  ";

  m_rep->print_info (os, prefix + ' ');
}

bool octave_value::load_ascii (std::istream& is)
{
  bool status = m_rep->load_ascii (is);

  // Force conversion of legacy objects.
  if (is_legacy_object ())
    maybe_mutate ();

  return status;
}
bool octave_value::load_binary (std::istream& is, bool swap,
                                octave::mach_info::float_format fmt)
{
  bool status = m_rep->load_binary (is, swap, fmt);

  // Force conversion of legacy objects.
  if (is_legacy_object ())
    maybe_mutate ();

  return status;
}

bool octave_value::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool status = m_rep->load_hdf5 (loc_id, name);

  // Force conversion of legacy objects.
  if (is_legacy_object ())
    maybe_mutate ();

  return status;
}

const void *
octave_value::mex_get_data (mxClassID class_id, mxComplexity complexity) const
{
  // If class_id is set to mxUNKNOWN_CLASS, return data for any type.
  // Otherwise, require that REP matches the requested type and
  // complexity.

  if (class_id != mxUNKNOWN_CLASS)
    {
      bool type_ok = false;

      switch (class_id)
        {
        case mxDOUBLE_CLASS:
          type_ok = is_double_type ();
          break;

        case mxSINGLE_CLASS:
          type_ok = is_single_type ();
          break;

        case mxINT8_CLASS:
          type_ok = is_int8_type ();
          break;

        case mxINT16_CLASS:
          type_ok = is_int16_type ();
          break;

        case mxINT32_CLASS:
          type_ok = is_int32_type ();
          break;

        case mxINT64_CLASS:
          type_ok = is_int64_type ();
          break;

        case mxUINT8_CLASS:
          type_ok = is_uint8_type ();
          break;

        case mxUINT16_CLASS:
          type_ok = is_uint16_type ();
          break;

        case mxUINT32_CLASS:
          type_ok = is_uint32_type ();
          break;

        case mxUINT64_CLASS:
          type_ok = is_uint64_type ();
          break;

        default:
          // We only expect to see numeric types explicitly requested.
          error ("mex_get_data: unexpected type requested");
        }

      if (! type_ok)
        error ("mex_get_data: type mismatch");

      if (complexity == mxCOMPLEX && ! iscomplex ())
        error ("mex_get_data: objectis not complex as requested");
    }

  return m_rep->mex_get_data ();
}

OCTAVE_NORETURN static void
err_unary_op_conversion_failed (const std::string& op,
                                const std::string& tn)
{
  error ("operator %s: type conversion for '%s' failed",
         op.c_str (), tn.c_str ());
}

OCTAVE_NORETURN static void
err_unary_op (const std::string& on, const std::string& tn)
{
  error ("unary operator '%s' not implemented for '%s' operands",
         on.c_str (), tn.c_str ());
}

octave_value&
octave_value::non_const_unary_op (unary_op op)
{
  if (op == op_incr || op == op_decr)
    {
      // We want the error just here, because in the other branch this should
      // not happen, and if it did anyway (internal error), the message would
      // be confusing.
      if (is_undefined ())
        {
          std::string op_str = unary_op_as_string (op);
          error ("in x%s or %sx, x must be defined first",
                 op_str.c_str (), op_str.c_str ());
          return *this;
        }

      // Genuine.
      int t = type_id ();

      octave::type_info& ti = octave::__get_type_info__ ();

      octave::type_info::non_const_unary_op_fcn f
        = ti.lookup_non_const_unary_op (op, t);

      if (f)
        {
          make_unique ();

          f (*m_rep);
        }
      else
        {
          octave_base_value::type_conv_fcn cf = numeric_conversion_function ();

          if (! cf)
            err_unary_op (octave_value::unary_op_as_string (op), type_name ());

          octave_base_value *tmp = cf (*m_rep);

          if (! tmp)
            err_unary_op_conversion_failed
            (octave_value::unary_op_as_string (op), type_name ());

          octave_base_value *old_rep = m_rep;
          m_rep = tmp;

          t = type_id ();

          f = ti.lookup_non_const_unary_op (op, t);

          if (f)
            {
              f (*m_rep);

              if (old_rep && --old_rep->count == 0)
                delete old_rep;
            }
          else
            {
              if (old_rep)
                {
                  if (--m_rep->count == 0)
                    delete m_rep;

                  m_rep = old_rep;
                }

              err_unary_op (octave_value::unary_op_as_string (op),
                            type_name ());
            }
        }
    }
  else
    {
      // Non-genuine.
      int t = type_id ();

      octave::type_info::non_const_unary_op_fcn f = nullptr;

      // Only attempt to operate in-place if this variable is unshared.
      if (m_rep->count == 1)
        {
          octave::type_info& ti = octave::__get_type_info__ ();

          f = ti.lookup_non_const_unary_op (op, t);
        }

      if (f)
        f (*m_rep);
      else
        *this = octave::unary_op (op, *this);
    }

  return *this;
}

octave_value&
octave_value::non_const_unary_op (unary_op op, const std::string& type,
                                  const std::list<octave_value_list>& idx)
{
  if (idx.empty ())
    non_const_unary_op (op);
  else
    {
      // FIXME: only do the following stuff if we can't find a
      // specific function to call to handle the op= operation for the
      // types we have.

      assign_op assop = unary_op_to_assign_op (op);

      assign (assop, type, idx, 1.0);
    }

  return *this;
}

octave_value::assign_op
octave_value::unary_op_to_assign_op (unary_op op)
{
  switch (op)
    {
    case op_incr:
      return op_add_eq;

    case op_decr:
      return op_sub_eq;

    default:
      {
        std::string on = unary_op_as_string (op);
        error ("operator %s: no assign operator found", on.c_str ());
      }
    }
}

octave_value::binary_op
octave_value::op_eq_to_binary_op (assign_op op)
{
  switch (op)
    {
    case op_add_eq:
      return op_add;

    case op_sub_eq:
      return op_sub;

    case op_mul_eq:
      return op_mul;

    case op_div_eq:
      return op_div;

    case op_ldiv_eq:
      return op_ldiv;

    case op_pow_eq:
      return op_pow;

    case op_el_mul_eq:
      return op_el_mul;

    case op_el_div_eq:
      return op_el_div;

    case op_el_ldiv_eq:
      return op_el_ldiv;

    case op_el_pow_eq:
      return op_el_pow;

    case op_el_and_eq:
      return op_el_and;

    case op_el_or_eq:
      return op_el_or;

    default:
      {
        std::string on = assign_op_as_string (op);
        error ("operator %s: no binary operator found", on.c_str ());
      }
    }
}

octave_value
octave_value::empty_conv (const std::string& type, const octave_value& rhs)
{
  if (type.length () > 0)
    {
      switch (type[0])
        {
        case '(':
          if (type.length () > 1 && type[1] == '.')
            return octave_map ();
          else
            return octave_value (rhs.empty_clone ());

        case '{':
          return Cell ();

        case '.':
          return octave_scalar_map ();

        default:
          panic_impossible ();
        }
    }
  else
    return octave_value (rhs.empty_clone ());
}

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_NORETURN static void
err_binary_op (const std::string& on, const std::string& tn1,
               const std::string& tn2)
{
  error ("binary operator '%s' not implemented for '%s' by '%s' operations",
         on.c_str (), tn1.c_str (), tn2.c_str ());
}

OCTAVE_NORETURN static void
err_binary_op_conv (const std::string& on)
{
  error ("type conversion failed for binary operator '%s'", on.c_str ());
}

octave_value
binary_op (type_info& ti, octave_value::binary_op op,
           const octave_value& v1, const octave_value& v2)
{
  octave_value retval;

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  if (t1 == octave_class::static_type_id ()
      || t2 == octave_class::static_type_id ()
      || t1 == octave_classdef::static_type_id ()
      || t2 == octave_classdef::static_type_id ())
    {
      type_info::binary_class_op_fcn f = ti.lookup_binary_class_op (op);

      if (! f)
        err_binary_op (octave_value::binary_op_as_string (op),
                       v1.class_name (), v2.class_name ());

      retval = f (v1, v2);
    }
  else
    {
      // FIXME: we need to handle overloading operators for built-in
      // classes (double, char, int8, etc.)

      type_info::binary_op_fcn f
        = ti.lookup_binary_op (op, t1, t2);

      if (f)
        retval = f (v1.get_rep (), v2.get_rep ());
      else
        {
          octave_value tv1;
          octave_base_value::type_conv_info cf1
            = v1.numeric_conversion_function ();

          octave_value tv2;
          octave_base_value::type_conv_info cf2
            = v2.numeric_conversion_function ();

          // Try biased (one-sided) conversions first.
          if (cf2.type_id () >= 0
              && ti.lookup_binary_op (op, t1, cf2.type_id ()))
            cf1 = nullptr;
          else if (cf1.type_id () >= 0
                   && ti.lookup_binary_op (op, cf1.type_id (), t2))
            cf2 = nullptr;

          if (cf1)
            {
              octave_base_value *tmp = cf1 (v1.get_rep ());

              if (! tmp)
                err_binary_op_conv (octave_value::binary_op_as_string (op));

              tv1 = octave_value (tmp);
              t1 = tv1.type_id ();
            }
          else
            tv1 = v1;

          if (cf2)
            {
              octave_base_value *tmp = cf2 (v2.get_rep ());

              if (! tmp)
                err_binary_op_conv (octave_value::binary_op_as_string (op));

              tv2 = octave_value (tmp);
              t2 = tv2.type_id ();
            }
          else
            tv2 = v2;

          if (cf1 || cf2)
            {
              retval = binary_op (op, tv1, tv2);
            }
          else
            {
              //demote double -> single and try again
              cf1 = tv1.numeric_demotion_function ();

              cf2 = tv2.numeric_demotion_function ();

              // Try biased (one-sided) conversions first.
              if (cf2.type_id () >= 0
                  && ti.lookup_binary_op (op, t1, cf2.type_id ()))
                cf1 = nullptr;
              else if (cf1.type_id () >= 0
                       && ti.lookup_binary_op (op, cf1.type_id (), t2))
                cf2 = nullptr;

              if (cf1)
                {
                  octave_base_value *tmp = cf1 (tv1.get_rep ());

                  if (! tmp)
                    err_binary_op_conv (octave_value::binary_op_as_string (op));

                  tv1 = octave_value (tmp);
                  t1 = tv1.type_id ();
                }

              if (cf2)
                {
                  octave_base_value *tmp = cf2 (tv2.get_rep ());

                  if (! tmp)
                    err_binary_op_conv (octave_value::binary_op_as_string (op));

                  tv2 = octave_value (tmp);
                  t2 = tv2.type_id ();
                }

              if (! cf1 && ! cf2)
                err_binary_op (octave_value::binary_op_as_string (op),
                               v1.type_name (), v2.type_name ());

              f = ti.lookup_binary_op (op, t1, t2);

              if (! f)
                err_binary_op (octave_value::binary_op_as_string (op),
                               v1.type_name (), v2.type_name ());

              retval = f (tv1.get_rep (), tv2.get_rep ());
            }
        }
    }

  return retval;
}

octave_value
binary_op (octave_value::binary_op op, const octave_value& v1,
           const octave_value& v2)
{
  type_info& ti = __get_type_info__ ();

  return binary_op (ti, op, v1, v2);
}

static octave_value
decompose_binary_op (type_info& ti, octave_value::compound_binary_op op,
                     const octave_value& v1, const octave_value& v2)
{
  switch (op)
    {
    case octave_value::op_trans_mul:
      return binary_op (octave_value::op_mul,
                        unary_op (octave_value::op_transpose, v1), v2);

    case octave_value::op_mul_trans:
      return binary_op (ti, octave_value::op_mul,
                        v1, unary_op (octave_value::op_transpose, v2));

    case octave_value::op_herm_mul:
      return binary_op (ti, octave_value::op_mul,
                        unary_op (octave_value::op_hermitian, v1), v2);

    case octave_value::op_mul_herm:
      return binary_op (ti, octave_value::op_mul,
                        v1, unary_op (octave_value::op_hermitian, v2));

    case octave_value::op_trans_ldiv:
      return binary_op (ti, octave_value::op_ldiv,
                        unary_op (octave_value::op_transpose, v1), v2);

    case octave_value::op_herm_ldiv:
      return binary_op (ti, octave_value::op_ldiv,
                        unary_op (octave_value::op_hermitian, v1), v2);

    case octave_value::op_el_not_and:
      return binary_op (ti, octave_value::op_el_and,
                        unary_op (octave_value::op_not, v1), v2);

    case octave_value::op_el_not_or:
      return binary_op (ti, octave_value::op_el_or,
                        unary_op (octave_value::op_not, v1), v2);

    case octave_value::op_el_and_not:
      return binary_op (ti, octave_value::op_el_and,
                        v1, unary_op (octave_value::op_not, v2));

    case octave_value::op_el_or_not:
      return binary_op (ti, octave_value::op_el_or,
                        v1, unary_op (octave_value::op_not, v2));

    default:
      error ("invalid compound operator");
    }
}

octave_value
binary_op (type_info& ti, octave_value::compound_binary_op op,
           const octave_value& v1, const octave_value& v2)
{
  octave_value retval;

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  if (t1 == octave_class::static_type_id ()
      || t2 == octave_class::static_type_id ()
      || t1 == octave_classdef::static_type_id ()
      || t2 == octave_classdef::static_type_id ())
    {
      type_info::binary_class_op_fcn f = ti.lookup_binary_class_op (op);

      if (f)
        retval = f (v1, v2);
      else
        retval = decompose_binary_op (ti, op, v1, v2);
    }
  else
    {
      type_info::binary_op_fcn f = ti.lookup_binary_op (op, t1, t2);

      if (f)
        retval = f (v1.get_rep (), v2.get_rep ());
      else
        retval = decompose_binary_op (ti, op, v1, v2);
    }

  return retval;
}

octave_value
binary_op (octave_value::compound_binary_op op,
           const octave_value& v1, const octave_value& v2)
{
  type_info& ti = __get_type_info__ ();

  return binary_op (ti, op, v1, v2);
}

OCTAVE_NORETURN static void
err_cat_op (const std::string& tn1, const std::string& tn2)
{
  error ("concatenation operator not implemented for '%s' by '%s' operations",
         tn1.c_str (), tn2.c_str ());
}

OCTAVE_NORETURN static void
err_cat_op_conv (void)
{
  error ("type conversion failed for concatenation operator");
}

octave_value
cat_op (type_info& ti, const octave_value& v1,
        const octave_value& v2, const Array<octave_idx_type>& ra_idx)
{
  octave_value retval;

  // Can't rapid return for concatenation with an empty object here as
  // something like cat(1,[],single([]) must return the correct type.

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  type_info::cat_op_fcn f = ti.lookup_cat_op (t1, t2);

  if (f)
    retval = f (v1.get_rep (), v2.get_rep (), ra_idx);
  else
    {
      octave_value tv1;
      octave_base_value::type_conv_info cf1 = v1.numeric_conversion_function ();

      octave_value tv2;
      octave_base_value::type_conv_info cf2 = v2.numeric_conversion_function ();

      // Try biased (one-sided) conversions first.
      if (cf2.type_id () >= 0 && ti.lookup_cat_op (t1, cf2.type_id ()))
        cf1 = nullptr;
      else if (cf1.type_id () >= 0 && ti.lookup_cat_op (cf1.type_id (), t2))
        cf2 = nullptr;

      if (cf1)
        {
          octave_base_value *tmp = cf1 (v1.get_rep ());

          if (! tmp)
            err_cat_op_conv ();

          tv1 = octave_value (tmp);
          t1 = tv1.type_id ();
        }
      else
        tv1 = v1;

      if (cf2)
        {
          octave_base_value *tmp = cf2 (v2.get_rep ());

          if (! tmp)
            err_cat_op_conv ();

          tv2 = octave_value (tmp);
          t2 = tv2.type_id ();
        }
      else
        tv2 = v2;

      if (! cf1 && ! cf2)
        err_cat_op (v1.type_name (), v2.type_name ());

      retval = cat_op (ti, tv1, tv2, ra_idx);
    }

  return retval;
}

octave_value
cat_op (const octave_value& v1, const octave_value& v2,
        const Array<octave_idx_type>& ra_idx)
{
  type_info& ti = __get_type_info__ ();

  return cat_op (ti, v1, v2, ra_idx);
}

// Unless the colon operator is used with a class or classdef object,
// then all arguments must be the same type or mixed with double
// values.

static builtin_type_t
get_colon_op_type (builtin_type_t op1_type, builtin_type_t op2_type)
{
  if (op1_type == op2_type)
    return op1_type;

  if (op1_type == btyp_double)
    return op2_type;

  if (op2_type == btyp_double)
    return op1_type;

  return btyp_unknown;
}

static builtin_type_t
get_colon_op_type (const octave_value& base, const octave_value& increment,
                   const octave_value& limit)
{
  builtin_type_t typ
    = get_colon_op_type (base.builtin_type (), increment.builtin_type ());

  if (typ == btyp_unknown)
    return typ;

  return get_colon_op_type (typ, limit.builtin_type ());
}

// This check depends on the type of VAL either being the expected
// integer type or a double value.

template <typename T>
static void
check_colon_operand (const octave_value& val, const char *op_str)
{
  if (! val.is_double_type ())
    return;

  double dval = val.double_value ();
  double intpart;
  static const double out_of_range_top
    = static_cast<double> (std::numeric_limits<typename T::val_type>::max ())
      + 1.;

  if (dval >= out_of_range_top
      || dval < std::numeric_limits<typename T::val_type>::min ()
      || std::modf (dval, &intpart) != 0.0)
    error ("colon operator %s invalid (not an integer or out of range for given integer type)", op_str);
}

// Return the difference between two unsigned integers as an unsigned
// integer of the same type.

template <typename UT,
          typename std::enable_if<(std::is_integral<UT>::value
                                   && std::is_unsigned<UT>::value),
                                  bool>::type = true>
UT
integer_difference (UT a, UT b)
{
  return a > b ? a - b : b - a;
}

// Return the difference between two signed integers as an unsigned
// integer corresponding to the signed type.

template <typename ST,
          typename UT = typename std::make_unsigned<ST>::type,
          typename std::enable_if<(std::is_integral<ST>::value
                                   && std::is_signed<ST>::value),
                                  bool>::type = true>
UT
integer_difference (ST a, ST b)
{
  // Map to unsigned.
  // Idea from https://stackoverflow.com/questions/10589559

  static const UT offset
    = UT (0) - static_cast<UT> (std::numeric_limits<ST>::min ());

  UT au = static_cast<UT> (a) + offset;
  UT bu = static_cast<UT> (b) + offset;

  return integer_difference (au, bu);
}

// Number of elements in an integer range taking care to avoid
// overflow.  Base and limit are of the same type.  If they are
// unsigned, then increment is also of the same type.  If they are
// signed, then the type of increment is the unsigned type
// corresponding to T.  Assumes that the base and limit values are
// consistent with the sign of the original increment (not an empty
// range) so we can calculate numel with the absolute value of the
// increment and the absolute difference between the base and limit
// values.

template <typename T,
          typename UT = typename std::make_unsigned<T>::type,
          typename std::enable_if<std::is_integral<T>::value,
                                  bool>::type = true>
octave_idx_type
range_numel_aux (T base, UT unsigned_increment, T limit)
{
  // Adding one to DIFF/INCREMENT may overflow, so check whether it is
  // out of range before adding.

  UT nel_m1 = integer_difference (limit, base) / unsigned_increment;

  // FIXME: fix error message.
  if (nel_m1 > std::numeric_limits<octave_idx_type>::max () - 1)
    error ("too many elements for range!");

  return static_cast<octave_idx_type> (nel_m1) + 1;
}

// Convert signed range increment to unsigned.

template <typename ST,
          typename UT = typename std::make_unsigned<ST>::type,
          typename std::enable_if<(std::is_integral<ST>::value
                                   && std::is_signed<ST>::value),
                                  bool>::type = true>
UT
range_increment (ST increment)
{
  return (increment < 0
          ? UT (0) - static_cast<UT> (increment)
          : static_cast<UT> (increment));
}

// "Convert" unsigned range increment to unsigned.  A no-op, but
// needed to provide a consistent interface for other template
// functions.

template <typename T,
          typename UT = typename std::make_unsigned<T>::type,
          typename std::enable_if<(std::is_integral<UT>::value
                                   && std::is_unsigned<UT>::value),
                                  bool>::type = true>
UT
range_increment (UT increment)
{
  return increment;
}

// Convert double range increment to unsigned.  Enable by return type.

template <typename T,
          typename UT = typename std::make_unsigned<T>::type>
typename std::enable_if<(std::is_integral<UT>::value
                         && std::is_unsigned<UT>::value), UT>::type
range_increment (double increment)
{
  double abs_increment = std::abs (increment);

  return static_cast<UT> (abs_increment);
}

// Number of elements in an integer range base:increment:limit.  Base,
// increment, and limit are of the same signed type.

template <typename ST,
          typename std::enable_if<(std::is_integral<ST>::value
                                   && std::is_signed<ST>::value),
                                  bool>::type = true>
octave_idx_type
range_numel (ST base, ST increment, ST limit)
{
  typedef typename std::make_unsigned<ST>::type UT;

  if (increment == 0
      || (increment > 0 && base > limit)
      || (increment < 0 && base < limit))
    return 0;

  UT unsigned_increment = range_increment<ST> (increment);

  return range_numel_aux (base, unsigned_increment, limit);
}

// Number of elements in an integer range base:increment:limit.  Base,
// increment, and limit are unsigned and of the same type.

template <typename UT,
          typename std::enable_if<(std::is_integral<UT>::value
                                   && std::is_unsigned<UT>::value),
                                  bool>::type = true>
octave_idx_type
range_numel (UT base, UT increment, UT limit)
{
  // Unsigned, INCREMENT is always >= 0.
  if (increment == 0 || base > limit)
    return 0;

  return range_numel_aux (base, increment, limit);
}

// Number of elements in an integer range base:increment:limit.  Base
// and limit are of the same type and increment is a double value.

template <typename T,
          typename UT = typename std::make_unsigned<T>::type,
          typename std::enable_if<std::is_integral<T>::value,
                                  bool>::type = true>
octave_idx_type
range_numel (T base, double increment, T limit)
{
  double intpart;
  if (math::isnan (increment) || std::modf (increment, &intpart) != 0.0)
    error ("colon operator increment invalid (not an integer)");

  if (increment == 0
      || (increment > 0 && base > limit)
      || (increment < 0 && base < limit))
    return 0;

  static const double out_of_range_top
    = static_cast<double> (std::numeric_limits<UT>::max ()) + 1.;

  double abs_increment = std::abs (increment);

  // Technically, this condition should be
  // `abs_increment > std::numeric_limits<UT>::max ()`.
  // But intmax('uint64') is not representable exactly as floating point
  // number.  Instead, it "rounds" up by 1 to 2^64.  To account for
  // this, use the following expression which works for all unsigned
  // integer types.
  if (abs_increment >= out_of_range_top)
    return 1;

  UT unsigned_increment = range_increment<T> (increment);

  return range_numel_aux (base, unsigned_increment, limit);
}

// Make a range from integer values.  Increment may be integer or double.

template <typename T,
          typename IT,
          typename std::enable_if<(std::is_integral<T>::value
                                   && std::is_arithmetic<IT>::value),
                                  bool>::type = true>
octave_value
make_int_range (T base, IT increment, T limit)
{
  octave_idx_type nel = range_numel (base, increment, limit);

  // For now, we create arrays instead of range<T> for all types
  // except double.

  Array<octave_int<T>> result (dim_vector (1, nel));

  if (nel > 0)
    {
      typedef typename std::make_unsigned<T>::type UT;

      UT unsigned_increment = range_increment<T> (increment);

      T val = base;
      result.xelem (0) = val;

      if (limit > base)
        {
          for (octave_idx_type i = 1; i < nel; i++)
            {
              val += unsigned_increment;
              result.xelem (i) = val;
            }
        }
      else
        {
          for (octave_idx_type i = 1; i < nel; i++)
            {
              val -= unsigned_increment;
              result.xelem (i) = val;
            }
        }
    }

  return octave_value (result);
}

// Make a range from floating point values.

// FIXME: Try again to define memory efficient range classes for
// integer and floating point values?  Maybe with the templates
// defined in this file we could do that in a reasonable way?
// Regardless of that, it might be good to provide special treatment
// of colon expressions in FOR loops so that we can eliminate the
// "is_for_cmd_expr / force_range" flag from the parser and the
// octave_value constructors for range objects.

// NOTE: We define this function separately for float and double so
// that we can avoid having to instantiate ov_range<float>.  We DO
// instantiate range<float> but only so we can take advantage of the
// range<T> class to generate the corresponding array of float values
// and not have to duplicate that code here.

template <typename T,
          typename std::enable_if<std::is_same<T, double>::value,
                                  bool>::type = true>
octave_value
make_float_range (T base, T increment, T limit, bool is_for_cmd_expr)
{
  if (math::isnan (base)
      || math::isnan (increment)
      || math::isnan (limit))
    return octave_value (numeric_limits<T>::NaN ());

  if (increment == 0
      || (increment > 0 && base > limit)
      || (increment < 0 && base < limit))
    return octave_value (Array<T> (dim_vector (1, 0)));

  // At this point, we know that the base and limit values are
  // consistent with the sign of the increment (not an empty range).

  range<T> r (base, increment, limit);

  if (! is_for_cmd_expr && ! r.is_storable ())
    error ("range with infinite number of elements cannot be stored");

  return octave_value (r, is_for_cmd_expr);
}

template <typename T,
          typename std::enable_if<std::is_same<T, float>::value,
                                  bool>::type = true>
octave_value
make_float_range (T base, T increment, T limit, bool is_for_cmd_expr)
{
  if (math::isnan (base)
      || math::isnan (increment)
      || math::isnan (limit))
    return octave_value (numeric_limits<T>::NaN ());

  if (increment == 0
      || (increment > 0 && base > limit)
      || (increment < 0 && base < limit))
    return octave_value (Array<T> (dim_vector (1, 0)));

  // At this point, we know that the base and limit values are
  // consistent with the sign of the increment (not an empty range).

  range<T> r (base, increment, limit);

  if (! is_for_cmd_expr && ! r.is_storable ())
    error ("range with infinite number of elements cannot be stored");

  return octave_value (r.array_value ());
}

template <typename T,
          typename std::enable_if<(std::is_same<T, octave_int8>::value
                                   || std::is_same<T, octave_uint8>::value
                                   || std::is_same<T, octave_int16>::value
                                   || std::is_same<T, octave_uint16>::value
                                   || std::is_same<T, octave_int32>::value
                                   || std::is_same<T, octave_uint32>::value
                                   || std::is_same<T, octave_int64>::value
                                   || std::is_same<T, octave_uint64>::value),
                                  bool>::type = true>
octave_value
make_int_range (const octave_value& base, const octave_value& increment,
                const octave_value& limit)
{
  if (base.isempty () || increment.isempty () || limit.isempty ())
    return octave_value (Array<T> (dim_vector (1, 0)));

  check_colon_operand<T> (base, "lower bound");
  check_colon_operand<T> (limit, "upper bound");

  typename T::val_type base_val = octave_value_extract<T> (base).value ();
  typename T::val_type limit_val = octave_value_extract<T> (limit).value ();

  if (increment.is_double_type ())
    {
      double increment_val = increment.double_value ();

      return make_int_range (base_val, increment_val, limit_val);
    }

  check_colon_operand<T> (increment, "increment");

  typename T::val_type increment_val
    = octave_value_extract<T> (increment).value ();

  return make_int_range (base_val, increment_val, limit_val);
}

template <typename T,
          typename std::enable_if<std::is_floating_point<T>::value,
                                  bool>::type = true>
octave_value
make_float_range (const octave_value& base, const octave_value& increment,
                  const octave_value& limit, bool is_for_cmd_expr)
{
  if (base.isempty () || increment.isempty () || limit.isempty ())
    return octave_value (Array<T> (dim_vector (1, 0)));

  T base_val = octave_value_extract<T> (base);
  T increment_val = octave_value_extract<T> (increment);
  T limit_val = octave_value_extract<T> (limit);

  return make_float_range (base_val, increment_val, limit_val,
                           is_for_cmd_expr);
}


octave_value
make_char_range (const octave_value& base, const octave_value& increment,
                 const octave_value& limit)
{
  octave_value retval;

  bool dq_str = (base.is_dq_string () || increment.is_dq_string ()
                 || limit.is_dq_string ());

  char type = dq_str ? '"' : '\'';

  if (base.isempty () || increment.isempty () || limit.isempty ())
    retval = octave_value ("", type);
  else
    {
      Matrix mtx_base = base.matrix_value (true);
      Matrix mtx_increment = increment.matrix_value (true);
      Matrix mtx_limit = limit.matrix_value (true);

      range<double> tmp (mtx_base(0), mtx_increment(0), mtx_limit(0));

      retval = octave_value (tmp);
    }

  return retval.convert_to_str (false, true, type);
}

octave_value
colon_op (const octave_value& base, const octave_value& increment_arg,
          const octave_value& limit, bool is_for_cmd_expr)
{
  if (base.isobject () || increment_arg.isobject () || limit.isobject ())
    {
      octave_value_list tmp1;

      if (increment_arg.is_defined ())
        {
          tmp1(2) = limit;
          tmp1(1) = increment_arg;
          tmp1(0) = base;
        }
      else
        {
          tmp1(1) = limit;
          tmp1(0) = base;
        }

      interpreter& interp = __get_interpreter__ ();

      symbol_table& symtab = interp.get_symbol_table ();

      octave_value fcn = symtab.find_function ("colon", tmp1);

      if (fcn.is_defined ())
        {
          octave_value_list tmp2 = interp.feval (fcn, tmp1, 1);

          return tmp2(0);
        }
    }

  octave_value increment
    = increment_arg.is_defined () ? increment_arg : octave_value (1.0);

  if (base.numel () > 1 || limit.numel () > 1 || increment.numel () > 1)
    warning_with_id ("Octave:colon-nonscalar-argument",
                     "colon arguments should be scalars");

  if (base.iscomplex () || limit.iscomplex () || increment.iscomplex ())
    warning_with_id ("Octave:colon-complex-argument",
                     "imaginary part of complex colon arguments is ignored");

  // FIXME: is there a better way to do this job, maybe using type traits?

  builtin_type_t type_id = get_colon_op_type (base, increment, limit);

  // For compatibility with Matlab, don't allow the range used in
  // a FOR loop expression to be converted to a Matrix.

  // For now, these functions create arrays instead of range<T> for
  // all types except double.

  switch (type_id)
    {
    case btyp_double:
    case btyp_complex:
      return make_float_range<double> (base, increment, limit, is_for_cmd_expr);

    case btyp_float:
    case btyp_float_complex:
      return make_float_range<float> (base, increment, limit, is_for_cmd_expr);

    case btyp_int8:
      return make_int_range<octave_int8> (base, increment, limit);

    case btyp_int16:
      return make_int_range<octave_int16> (base, increment, limit);

    case btyp_int32:
      return make_int_range<octave_int32> (base, increment, limit);

    case btyp_int64:
      return make_int_range<octave_int64> (base, increment, limit);

    case btyp_uint8:
      return make_int_range<octave_uint8> (base, increment, limit);

    case btyp_uint16:
      return make_int_range<octave_uint16> (base, increment, limit);

    case btyp_uint32:
      return make_int_range<octave_uint32> (base, increment, limit);

    case btyp_uint64:
      return make_int_range<octave_uint64> (base, increment, limit);

    case btyp_char:
      return make_char_range (base, increment, limit);

    case btyp_unknown:
      error ("incompatible types found in range expression");

    default:
      error ("invalid types found in range expression");
    }

  return octave_value ();
}

OCTAVE_NORETURN static void
err_unary_op_conv (const std::string& on)
{
  error ("type conversion failed for unary operator '%s'", on.c_str ());
}

octave_value
unary_op (type_info& ti, octave_value::unary_op op,
          const octave_value& v)
{
  octave_value retval;

  int t = v.type_id ();

  if (t == octave_class::static_type_id ()
      || t == octave_classdef::static_type_id ())
    {
      type_info::unary_class_op_fcn f = ti.lookup_unary_class_op (op);

      if (! f)
        err_unary_op (octave_value::unary_op_as_string (op), v.class_name ());

      retval = f (v);
    }
  else
    {
      // FIXME: we need to handle overloading operators for built-in
      // classes (double, char, int8, etc.)

      type_info::unary_op_fcn f = ti.lookup_unary_op (op, t);

      if (f)
        retval = f (v.get_rep ());
      else
        {
          octave_value tv;
          octave_base_value::type_conv_fcn cf
            = v.numeric_conversion_function ();

          if (! cf)
            err_unary_op (octave_value::unary_op_as_string (op),
                          v.type_name ());

          octave_base_value *tmp = cf (v.get_rep ());

          if (! tmp)
            err_unary_op_conv (octave_value::unary_op_as_string (op));

          tv = octave_value (tmp);
          retval = unary_op (op, tv);
        }
    }

  return retval;
}

octave_value
unary_op (octave_value::unary_op op, const octave_value& v)
{
  type_info& ti = __get_type_info__ ();

  return unary_op (ti, op, v);
}

OCTAVE_END_NAMESPACE(octave)

void
install_types (octave::type_info& ti)
{
  octave_base_value::register_type (ti);
  octave_cell::register_type (ti);
  octave_scalar::register_type (ti);
  octave_complex::register_type (ti);
  octave_matrix::register_type (ti);
  octave_diag_matrix::register_type (ti);
  octave_complex_matrix::register_type (ti);
  octave_complex_diag_matrix::register_type (ti);

  // Legacy range type, preserved to allow loading "constant" ranges
  // from data files.
  octave_legacy_range::register_type (ti);

  ov_range<double>::register_type (ti);

  // For now, disable all but ov_range<double>.

#if 0

  ov_range<float>::register_type (ti);
  ov_range<octave_int8>::register_type (ti);
  ov_range<octave_int16>::register_type (ti);
  ov_range<octave_int32>::register_type (ti);
  ov_range<octave_int64>::register_type (ti);
  ov_range<octave_uint8>::register_type (ti);
  ov_range<octave_uint16>::register_type (ti);
  ov_range<octave_uint32>::register_type (ti);
  ov_range<octave_uint64>::register_type (ti);

#endif

  octave_bool::register_type (ti);
  octave_bool_matrix::register_type (ti);
  octave_char_matrix_str::register_type (ti);
  octave_char_matrix_sq_str::register_type (ti);
  octave_int8_scalar::register_type (ti);
  octave_int16_scalar::register_type (ti);
  octave_int32_scalar::register_type (ti);
  octave_int64_scalar::register_type (ti);
  octave_uint8_scalar::register_type (ti);
  octave_uint16_scalar::register_type (ti);
  octave_uint32_scalar::register_type (ti);
  octave_uint64_scalar::register_type (ti);
  octave_int8_matrix::register_type (ti);
  octave_int16_matrix::register_type (ti);
  octave_int32_matrix::register_type (ti);
  octave_int64_matrix::register_type (ti);
  octave_uint8_matrix::register_type (ti);
  octave_uint16_matrix::register_type (ti);
  octave_uint32_matrix::register_type (ti);
  octave_uint64_matrix::register_type (ti);
  octave_sparse_bool_matrix::register_type (ti);
  octave_sparse_matrix::register_type (ti);
  octave_sparse_complex_matrix::register_type (ti);
  octave_struct::register_type (ti);
  octave_scalar_struct::register_type (ti);
  octave_class::register_type (ti);
  octave_cs_list::register_type (ti);
  octave_magic_colon::register_type (ti);
  octave_builtin::register_type (ti);
  octave_user_function::register_type (ti);
  octave_dld_function::register_type (ti);
  octave_fcn_handle::register_type (ti);
  octave_float_scalar::register_type (ti);
  octave_float_complex::register_type (ti);
  octave_float_matrix::register_type (ti);
  octave_float_diag_matrix::register_type (ti);
  octave_float_complex_matrix::register_type (ti);
  octave_float_complex_diag_matrix::register_type (ti);
  octave_perm_matrix::register_type (ti);
  octave_magic_int::register_type (ti);
  octave_magic_uint::register_type (ti);
  octave_null_matrix::register_type (ti);
  octave_null_str::register_type (ti);
  octave_null_sq_str::register_type (ti);
  octave_lazy_index::register_type (ti);
  octave_oncleanup::register_type (ti);
  octave_java::register_type (ti);
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (sizeof, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{sz} =} sizeof (@var{val})
Return the size of @var{val} in bytes.
@seealso{whos}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).byte_size ());
}

/*
%!assert (sizeof (uint64 (ones (3))), 72)
%!assert (sizeof (double (zeros (2,4))), 64)
%!assert (sizeof ({"foo", "bar", "baaz"}), 10)
*/

static void
decode_subscripts (const char *name, const octave_value& arg,
                   std::string& type_string,
                   std::list<octave_value_list>& idx)
{
  const octave_map m =
    arg.xmap_value ("%s: second argument must be a structure with fields 'type' and 'subs'", name);

  if (m.nfields () != 2 || ! m.contains ("type") || ! m.contains ("subs"))
    error ("%s: second argument must be a structure with fields 'type' and 'subs'",
           name);

  octave_idx_type nel = m.numel ();

  type_string = std::string (nel, '\0');
  idx = std::list<octave_value_list> ();

  if (nel == 0)
    return;

  const Cell type = m.contents ("type");
  const Cell subs = m.contents ("subs");

  for (int k = 0; k < nel; k++)
    {
      std::string item = type(k).xstring_value ("%s: type(%d) must be a string", name, k+1);

      if (item == "{}")
        type_string[k] = '{';
      else if (item == "()")
        type_string[k] = '(';
      else if (item == ".")
        type_string[k] = '.';
      else
        error ("%s: invalid indexing type '%s'", name, item.c_str ());

      octave_value_list idx_item;

      if (subs(k).is_string ())
        idx_item(0) = subs(k);
      else if (subs(k).iscell ())
        {
          Cell subs_cell = subs(k).cell_value ();

          for (int n = 0; n < subs_cell.numel (); n++)
            {
              if (subs_cell(n).is_string ()
                  && subs_cell(n).string_value () == ":")
                idx_item(n) = octave_value(octave_value::magic_colon_t);
              else
                idx_item(n) = subs_cell(n);
            }
        }
      else
        error ("%s: subs(%d) must be a string or cell array", name, k+1);

      idx.push_back (idx_item);
    }
}

DEFUN (subsref, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{newval} =} subsref (@var{val}, @var{idx})
Perform the subscripted element selection operation on @var{val} according
to the subscript specified by @var{idx}.

The subscript @var{idx} must be a structure array with fields @samp{type}
and @samp{subs}.  Valid values for @samp{type} are @qcode{"()"},
@qcode{"@{@}"}, and @qcode{"."}.  The @samp{subs} field may be either
@qcode{":"} or a cell array of index values.

The following example shows how to extract the first two columns of a matrix

@example
@group
val = magic (3)
    @result{} val = [ 8   1   6
               3   5   7
               4   9   2 ]
idx.type = "()";
idx.subs = @{":", 1:2@};
subsref (val, idx)
     @result{} [ 8   1
          3   5
          4   9 ]
@end group
@end example

@noindent
Note that this is the same as writing @code{val(:, 1:2)}.

If @var{idx} is an empty structure array with fields @samp{type} and
@samp{subs}, return @var{val}.
@seealso{subsasgn, substruct}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string type;
  std::list<octave_value_list> idx;

  decode_subscripts ("subsref", args(1), type, idx);

  octave_value arg0 = args(0);

  if (type.empty ())
    return ovl (arg0);
  else
    return arg0.subsref (type, idx, nargout);
}

DEFUN (subsasgn, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{newval} =} subsasgn (@var{val}, @var{idx}, @var{rhs})
Perform the subscripted assignment operation according to the subscript
specified by @var{idx}.

The subscript @var{idx} must be a structure array with fields @samp{type}
and @samp{subs}.  Valid values for @samp{type} are @qcode{"()"},
@qcode{"@{@}"}, and @qcode{"."}.  The @samp{subs} field may be either
@qcode{":"} or a cell array of index values.

The following example shows how to set the two first columns of a 3-by-3
matrix to zero.

@example
@group
val = magic (3);
idx.type = "()";
idx.subs = @{":", 1:2@};
val = subsasgn (val, idx, 0)
     @result{}  [ 0   0   6
           0   0   7
           0   0   2 ]
@end group
@end example

Note that this is the same as writing @code{val(:, 1:2) = 0}.

If @var{idx} is an empty structure array with fields @samp{type} and
@samp{subs}, return @var{rhs}.
@seealso{subsref, substruct, optimize_subsasgn_calls}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  std::string type;
  std::list<octave_value_list> idx;

  decode_subscripts ("subsasgn", args(1), type, idx);

  if (type.empty ())
    {
      // Regularize a null matrix if stored into a variable.
      return ovl (args(2).storable_value ());
    }
  else
    {
      octave_value arg0 = args(0);
      octave_value arg2 = args(2);

      arg0.make_unique ();

      bool arg2_null = arg2.is_zero_by_zero () && arg2.is_double_type ();

      return ovl (arg0.subsasgn (type, idx, (arg2_null
                                             ? octave_null_matrix::instance
                                             : arg2)));
    }
}

/*
%!test
%! a = reshape ([1:25], 5,5);
%! idx1 = substruct ("()", {3, 3});
%! idx2 = substruct ("()", {2:2:5, 2:2:5});
%! idx3 = substruct ("()", {":", [1,5]});
%! idx4 = struct ("type", {}, "subs", {});
%! assert (subsref (a, idx1), 13);
%! assert (subsref (a, idx2), [7 17; 9 19]);
%! assert (subsref (a, idx3), [1:5; 21:25]');
%! assert (subsref (a, idx4), a);
%! a = subsasgn (a, idx1, 0);
%! a = subsasgn (a, idx2, 0);
%! a = subsasgn (a, idx3, 0);
%!# a = subsasgn (a, idx4, 0);
%! b = [0    6   11   16    0
%!      0    0   12    0    0
%!      0    8    0   18    0
%!      0    0   14    0    0
%!      0   10   15   20    0];
%! assert (a, b);

%!test
%! x = 1:10;
%! assert (subsasgn (x, substruct ("()", {1}), zeros (0, 0)), 2:10);

%!test
%! c = num2cell (reshape ([1:25],5,5));
%! idx1 = substruct  ("{}", {3, 3});
%! idx2 = substruct  ("()", {2:2:5, 2:2:5});
%! idx3 = substruct  ("()", {":", [1,5]});
%! idx2p = substruct ("{}", {2:2:5, 2:2:5});
%! idx3p = substruct ("{}", {":", [1,5]});
%! idx4 = struct ("type", {}, "subs", {});
%! assert ({ subsref(c, idx1) }, {13});
%! assert ({ subsref(c, idx2p) }, {7 9 17 19});
%! assert ({ subsref(c, idx3p) }, num2cell ([1:5, 21:25]));
%! assert (subsref (c, idx4), c);
%! c = subsasgn (c, idx1, 0);
%! c = subsasgn (c, idx2, 0);
%! c = subsasgn (c, idx3, 0);
%!# c = subsasgn (c, idx4, 0);
%! d = {0    6   11   16    0
%!      0    0   12    0    0
%!      0    8    0   18    0
%!      0    0   14    0    0
%!      0   10   15   20    0};
%! assert (c, d);

%!test
%! s.a = "ohai";
%! s.b = "dere";
%! s.c = 42;
%! idx1 = substruct (".", "a");
%! idx2 = substruct (".", "b");
%! idx3 = substruct (".", "c");
%! idx4 = struct ("type", {}, "subs", {});
%! assert (subsref (s, idx1), "ohai");
%! assert (subsref (s, idx2), "dere");
%! assert (subsref (s, idx3), 42);
%! assert (subsref (s, idx4), s);
%! s = subsasgn (s, idx1, "Hello");
%! s = subsasgn (s, idx2, "There");
%! s = subsasgn (s, idx3, 163);
%!# s = subsasgn (s, idx4, 163);
%! t.a = "Hello";
%! t.b = "There";
%! t.c = 163;
%! assert (s, t);
*/

DEFUN (is_sq_string, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} is_sq_string (@var{x})
Return true if @var{x} is a single-quoted character string.
@seealso{is_dq_string, ischar}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_sq_string ());
}

/*
%!assert (is_sq_string ('foo'), true)
%!assert (is_sq_string ("foo"), false)
%!assert (is_sq_string (1.0), false)
%!assert (is_sq_string ({2.0}), false)

%!error is_sq_string ()
%!error is_sq_string ('foo', 2)
*/

DEFUN (is_dq_string, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} is_dq_string (@var{x})
Return true if @var{x} is a double-quoted character string.
@seealso{is_sq_string, ischar}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_dq_string ());
}

/*
%!assert (is_dq_string ("foo"), true)
%!assert (is_dq_string ('foo'), false)
%!assert (is_dq_string (1.0), false)
%!assert (is_dq_string ({2.0}), false)

%!error is_dq_string ()
%!error is_dq_string ("foo", 2)
*/

DEFUN (optimize_permutation_matrix, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} optimize_permutation_matrix ()
@deftypefnx {} {@var{old_val} =} optimize_permutation_matrix (@var{new_val})
@deftypefnx {} {@var{old_val} =} optimize_permutation_matrix (@var{new_val}, "local")
Query or set whether a special space-efficient format is used for storing
permutation matrices.

The default value is true.  If this option is set to false, Octave will store
permutation matrices as full matrices.

When called from inside a function with the @qcode{"local"} option, the setting
is changed locally for the function and any subroutines it calls.  The original
setting is restored when exiting the function.
@seealso{optimize_range, optimize_diagonal_matrix}
@end deftypefn */)
{
  return set_internal_variable (Voptimize_permutation_matrix, args, nargout,
                                "optimize_permutation_matrix");
}

/*
%!function p = __test_dpm__ (dpm)
%!  optimize_permutation_matrix (dpm, "local");
%!  [~, ~, p] = lu ([1,2;3,4]);
%!endfunction

%!assert (typeinfo (__test_dpm__ (true)), "permutation matrix")
%!assert (typeinfo (__test_dpm__ (false)), "matrix")
*/

DEFUN (optimize_diagonal_matrix, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} optimize_diagonal_matrix ()
@deftypefnx {} {@var{old_val} =} optimize_diagonal_matrix (@var{new_val})
@deftypefnx {} {@var{old_val} =} optimize_diagonal_matrix (@var{new_val}, "local")
Query or set whether a special space-efficient format is used for storing
diagonal matrices.

The default value is true.  If this option is set to false, Octave will store
diagonal matrices as full matrices.

When called from inside a function with the @qcode{"local"} option, the setting
is changed locally for the function and any subroutines it calls.  The original
setting is restored when exiting the function.
@seealso{optimize_range, optimize_permutation_matrix}
@end deftypefn */)
{
  return set_internal_variable (Voptimize_diagonal_matrix, args, nargout,
                                "optimize_diagonal_matrix");
}

/*
%!function [x, xi, fx, fxi] = __test_ddm__ (ddm)
%!  optimize_diagonal_matrix (ddm, "local");
%!  x = eye (2);
%!  xi = x*i;
%!  fx = single (x);
%!  fxi = single (xi);
%!endfunction

%!shared x, xi, fx, fxi
%!  [x, xi, fx, fxi] = __test_ddm__ (true);
%!assert (typeinfo (x), "diagonal matrix")
%!assert (typeinfo (xi), "complex diagonal matrix")
%!assert (typeinfo (fx), "float diagonal matrix")
%!assert (typeinfo (fxi), "float complex diagonal matrix")

%!shared x, xi, fx, fxi
%!  [x, xi, fx, fxi] = __test_ddm__ (false);
%!assert (typeinfo (x), "matrix")
%!assert (typeinfo (xi), "complex matrix")
%!assert (typeinfo (fx), "float matrix")
%!assert (typeinfo (fxi), "float complex matrix")
*/

DEFUN (optimize_range, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} optimize_range ()
@deftypefnx {} {@var{old_val} =} optimize_range (@var{new_val})
@deftypefnx {} {@var{old_val} =} optimize_range (@var{new_val}, "local")
Query or set whether a special space-efficient format is used for storing
ranges.

The default value is true.  If this option is set to false, Octave will store
ranges as full matrices.

When called from inside a function with the @qcode{"local"} option, the setting
is changed locally for the function and any subroutines it calls.  The original
setting is restored when exiting the function.
@seealso{optimize_diagonal_matrix, optimize_permutation_matrix}
@end deftypefn */)
{
  return set_internal_variable (Voptimize_range, args, nargout,
                                "optimize_range");
}

/*
%!function r = __test_dr__ (dr)
%!  optimize_range (dr, "local");
%!  ## Constant folding will produce range for 1:13.
%!  base = 1;
%!  limit = 13;
%!  r = base:limit;
%!endfunction

%!assert (typeinfo (__test_dr__ (true)), "double_range")
%!assert (typeinfo (__test_dr__ (false)), "matrix")
*/

OCTAVE_END_NAMESPACE(octave)
