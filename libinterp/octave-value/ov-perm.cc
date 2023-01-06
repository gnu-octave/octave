////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#include "byte-swap.h"
#include "dim-vector.h"

#include "mxarray.h"
#include "ov-perm.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "error.h"
#include "errwarn.h"
#include "ops.h"
#include "pr-output.h"

#include "ls-oct-text.h"

octave_value
octave_perm_matrix::subsref (const std::string& type,
                             const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval.next_subsref (type, idx);
}

octave_value
octave_perm_matrix::do_index_op (const octave_value_list& idx,
                                 bool resize_ok)
{
  octave_value retval;
  octave_idx_type nidx = idx.length ();
  octave::idx_vector idx0, idx1;
  if (nidx == 2)
    {
      int k = 0;    // index we're processing when index_vector throws
      try
        {
          idx0 = idx(0).index_vector ();
          k = 1;
          idx1 = idx(1).index_vector ();
        }
      catch (octave::index_exception& ie)
        {
          // Rethrow to allow more info to be reported later.
          ie.set_pos_if_unset (2, k+1);
          throw;
        }
    }

  // This hack is to allow constructing permutation matrices using
  // eye(n)(p,:), eye(n)(:,q) && eye(n)(p,q) where p & q are permutation
  // vectors.
  // Note that, for better consistency, eye(n)(:,:) still converts to a full
  // matrix.
  if (nidx == 2)
    {
      bool left = idx0.is_permutation (m_matrix.rows ());
      bool right = idx1.is_permutation (m_matrix.cols ());

      if (left && right)
        {
          if (idx0.is_colon ()) left = false;
          if (idx1.is_colon ()) right = false;
          if (left || right)
            {
              PermMatrix p = m_matrix;
              if (left)
                p = PermMatrix (idx0, false) * p;
              if (right)
                p = p * PermMatrix (idx1, true);
              retval = p;
            }
          else
            {
              retval = this;
              this->count++;
            }
        }
    }

  if (! retval.is_defined ())
    {
      if (nidx == 2 && ! resize_ok && idx0.is_scalar () && idx1.is_scalar ())
        retval = m_matrix.checkelem (idx0(0), idx1(0));
      else
        retval = to_dense ().index_op (idx, resize_ok);
    }

  return retval;
}

// Return true if this matrix has all true elements (non-zero, not NaN/NA).
// A permutation cannot have NaN/NA.
bool
octave_perm_matrix::is_true (void) const
{
  if (dims ().numel () > 1)
    {
      warn_array_as_logical (dims ());
      return false;    // > 1x1 permutation always has zeros, and no NaN.
    }
  else
    return dims ().numel ();    // 1x1 is [1] == true, 0x0 == false.
}

double
octave_perm_matrix::double_value (bool) const
{
  if (isempty ())
    err_invalid_conversion (type_name (), "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "real scalar");

  return m_matrix(0, 0);
}

float
octave_perm_matrix::float_value (bool) const
{
  if (isempty ())
    err_invalid_conversion (type_name (), "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "real scalar");

  return m_matrix(0, 0);
}

Complex
octave_perm_matrix::complex_value (bool) const
{
  if (rows () == 0 || columns () == 0)
    err_invalid_conversion (type_name (), "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "complex scalar");

  return Complex (m_matrix(0, 0), 0);
}

FloatComplex
octave_perm_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion (type_name (), "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "complex scalar");

  retval = m_matrix(0, 0);

  return retval;
}

#define FORWARD_MATRIX_VALUE(TYPE, PREFIX)                              \
  TYPE                                                                  \
  octave_perm_matrix::PREFIX ## _value (bool frc_str_conv) const        \
  {                                                                     \
    return to_dense ().PREFIX ## _value (frc_str_conv);                 \
  }

SparseMatrix
octave_perm_matrix::sparse_matrix_value (bool) const
{
  return SparseMatrix (m_matrix);
}

SparseBoolMatrix
octave_perm_matrix::sparse_bool_matrix_value (bool) const
{
  return SparseBoolMatrix (m_matrix);
}

SparseComplexMatrix
octave_perm_matrix::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (sparse_matrix_value ());
}

FORWARD_MATRIX_VALUE (Matrix, matrix)
FORWARD_MATRIX_VALUE (FloatMatrix, float_matrix)
FORWARD_MATRIX_VALUE (ComplexMatrix, complex_matrix)
FORWARD_MATRIX_VALUE (FloatComplexMatrix, float_complex_matrix)

FORWARD_MATRIX_VALUE (NDArray, array)
FORWARD_MATRIX_VALUE (FloatNDArray, float_array)
FORWARD_MATRIX_VALUE (ComplexNDArray, complex_array)
FORWARD_MATRIX_VALUE (FloatComplexNDArray, float_complex_array)

FORWARD_MATRIX_VALUE (boolNDArray, bool_array)
FORWARD_MATRIX_VALUE (charNDArray, char_array)

octave::idx_vector
octave_perm_matrix::index_vector (bool require_integers) const
{
  return to_dense ().index_vector (require_integers);
}

octave_value
octave_perm_matrix::convert_to_str_internal (bool pad, bool force,
    char type) const
{
  return to_dense ().convert_to_str_internal (pad, force, type);
}

octave_value
octave_perm_matrix::as_double (void) const
{
  return m_matrix;
}

octave_value
octave_perm_matrix::as_single (void) const
{
  return float_array_value ();
}

octave_value
octave_perm_matrix::as_int8 (void) const
{
  return int8_array_value  ();
}

octave_value
octave_perm_matrix::as_int16 (void) const
{
  return int16_array_value ();
}

octave_value
octave_perm_matrix::as_int32 (void) const
{
  return int32_array_value ();
}

octave_value
octave_perm_matrix::as_int64 (void) const
{
  return int64_array_value ();
}

octave_value
octave_perm_matrix::as_uint8 (void) const
{
  return uint8_array_value ();
}

octave_value
octave_perm_matrix::as_uint16 (void) const
{
  return uint16_array_value ();
}

octave_value
octave_perm_matrix::as_uint32 (void) const
{
  return uint32_array_value ();
}

octave_value
octave_perm_matrix::as_uint64 (void) const
{
  return uint64_array_value ();
}

float_display_format
octave_perm_matrix::get_edit_display_format (void) const
{
  return float_display_format (float_format (1, 0, 0));
}

std::string
octave_perm_matrix::edit_display (const float_display_format& fmt,
                                  octave_idx_type i,
                                  octave_idx_type j) const
{
  std::ostringstream buf;
  octave_print_internal (buf, fmt, octave_int<octave_idx_type> (m_matrix(i, j)));
  return buf.str ();
}

bool
octave_perm_matrix::save_ascii (std::ostream& os)
{
  os << "# size: " << m_matrix.rows () << "\n";
  os << "# orient: c\n";

  Array<octave_idx_type> pvec = m_matrix.col_perm_vec ();
  octave_idx_type n = pvec.numel ();
  ColumnVector tmp (n);
  for (octave_idx_type i = 0; i < n; i++) tmp(i) = pvec(i) + 1;
  os << tmp;

  return true;
}

bool
octave_perm_matrix::load_ascii (std::istream& is)
{
  octave_idx_type n;
  char orient;

  if (! extract_keyword (is, "size", n, true)
      || ! extract_keyword (is, "orient", orient, true))
    error ("load: failed to extract size & orientation");

  bool colp = orient == 'c';
  ColumnVector tmp (n);
  is >> tmp;
  if (! is)
    error ("load: failed to load permutation matrix constant");

  Array<octave_idx_type> pvec (dim_vector (n, 1));
  for (octave_idx_type i = 0; i < n; i++) pvec(i) = tmp(i) - 1;
  m_matrix = PermMatrix (pvec, colp);

  // Invalidate cache.  Probably not necessary, but safe.
  m_dense_cache = octave_value ();

  return true;
}

bool
octave_perm_matrix::save_binary (std::ostream& os, bool)
{

  int32_t sz = m_matrix.rows ();
  bool colp = true;
  os.write (reinterpret_cast<char *> (&sz), 4);
  os.write (reinterpret_cast<char *> (&colp), 1);
  const Array<octave_idx_type>& col_perm = m_matrix.col_perm_vec ();
  os.write (reinterpret_cast<const char *> (col_perm.data ()),
            col_perm.byte_size ());

  return true;
}

bool
octave_perm_matrix::load_binary (std::istream& is, bool swap,
                                 octave::mach_info::float_format)
{
  int32_t sz;
  bool colp;
  if (! (is.read (reinterpret_cast<char *> (&sz), 4)
         && is.read (reinterpret_cast<char *> (&colp), 1)))
    return false;

  MArray<octave_idx_type> m (dim_vector (sz, 1));

  if (! is.read (reinterpret_cast<char *> (m.fortran_vec ()), m.byte_size ()))
    return false;

  if (swap)
    {
      int nel = m.numel ();
      for (int i = 0; i < nel; i++)
        switch (sizeof (octave_idx_type))
          {
          case 8:
            swap_bytes<8> (&m(i));
            break;
          case 4:
            swap_bytes<4> (&m(i));
            break;
          case 2:
            swap_bytes<2> (&m(i));
            break;
          case 1:
          default:
            break;
          }
    }

  m_matrix = PermMatrix (m, colp);
  return true;
}

void
octave_perm_matrix::print_raw (std::ostream& os,
                               bool pr_as_read_syntax) const
{
  return octave_print_internal (os, m_matrix, pr_as_read_syntax,
                                current_print_indent_level ());
}

mxArray *
octave_perm_matrix::as_mxArray (bool interleaved) const
{
  return to_dense ().as_mxArray (interleaved);
}

bool
octave_perm_matrix::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

void
octave_perm_matrix::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

int
octave_perm_matrix::write (octave::stream& os, int block_size,
                           oct_data_conv::data_type output_type, int skip,
                           octave::mach_info::float_format flt_fmt) const
{
  return to_dense ().write (os, block_size, output_type, skip, flt_fmt);
}

void
octave_perm_matrix::print_info (std::ostream& os,
                                const std::string& prefix) const
{
  m_matrix.print_info (os, prefix);
}

octave_value
octave_perm_matrix::to_dense (void) const
{
  if (! m_dense_cache.is_defined ())
    m_dense_cache = Matrix (m_matrix);

  return m_dense_cache;
}

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_perm_matrix,
                                     "permutation matrix", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  const octave_perm_matrix& v = dynamic_cast<const octave_perm_matrix&> (a);

  return new octave_matrix (v.matrix_value ());
}

octave_base_value::type_conv_info
octave_perm_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
         octave_matrix::static_type_id ());
}

// FIXME: This is duplicated from octave_base_matrix<T>.  Could
// octave_perm_matrix be derived from octave_base_matrix<T>?

void
octave_perm_matrix::short_disp (std::ostream& os) const
{
  if (m_matrix.isempty ())
    os << "[]";
  else if (m_matrix.ndims () == 2)
    {
      // FIXME: should this be configurable?
      octave_idx_type max_elts = 10;
      octave_idx_type elts = 0;

      octave_idx_type nel = m_matrix.numel ();

      octave_idx_type nr = m_matrix.rows ();
      octave_idx_type nc = m_matrix.columns ();

      os << '[';

      for (octave_idx_type i = 0; i < nr; i++)
        {
          for (octave_idx_type j = 0; j < nc; j++)
            {
              std::ostringstream buf;
              octave_int<octave_idx_type> tval (m_matrix(i, j));
              octave_print_internal (buf, tval);
              std::string tmp = buf.str ();
              std::size_t pos = tmp.find_first_not_of (' ');
              if (pos != std::string::npos)
                os << tmp.substr (pos);
              else if (! tmp.empty ())
                os << tmp[0];

              if (++elts >= max_elts)
                goto done;

              if (j < nc - 1)
                os << ", ";
            }

          if (i < nr - 1 && elts < max_elts)
            os << "; ";
        }

    done:

      if (nel <= max_elts)
        os << ']';
    }
  else
    os << "...";
}

octave_base_value *
octave_perm_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = nullptr;

  if (m_matrix.numel () == 1)
    retval = new octave_scalar (m_matrix (0, 0));

  return retval;
}

octave_value
octave_perm_matrix::fast_elem_extract (octave_idx_type n) const
{
  if (n < m_matrix.numel ())
    {
      octave_idx_type nr = m_matrix.rows ();

      octave_idx_type r = n % nr;
      octave_idx_type c = n / nr;

      return octave_value (m_matrix.elem (r, c));
    }
  else
    return octave_value ();
}
