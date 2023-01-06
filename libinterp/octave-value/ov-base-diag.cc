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

// This file should not include config.h.  It is only included in other
// C++ source files that should have included config.h before including
// this file.

#include <istream>
#include <ostream>
#include <sstream>

#include "mach-info.h"
#include "lo-ieee.h"

#include "ov-base-diag.h"
#include "mxarray.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "pr-output.h"
#include "error.h"
#include "errwarn.h"
#include "oct-stream.h"
#include "ops.h"

#include "ls-oct-text.h"

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::subsref (const std::string& type,
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

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::diag (octave_idx_type k) const
{
  octave_value retval;
  if (m_matrix.rows () == 1 || m_matrix.cols () == 1)
    {
      // Rather odd special case.  This is a row or column vector
      // represented as a diagonal matrix with a single nonzero entry, but
      // Fdiag semantics are to product a diagonal matrix for vector
      // inputs.
      if (k == 0)
        // Returns Diag2Array<T> with nnz <= 1.
        retval = m_matrix.build_diag_matrix ();
      else
        // Returns Array<T> matrix
        retval = m_matrix.array_value ().diag (k);
    }
  else
    // Returns Array<T> vector
    retval = m_matrix.extract_diag (k);
  return retval;
}

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::do_index_op (const octave_value_list& idx,
                                        bool resize_ok)
{
  octave_value retval;

  if (idx.length () == 2 && ! resize_ok)
    {
      int k = 0;        // index we're accessing when index_vector throws
      try
        {
          octave::idx_vector idx0 = idx(0).index_vector ();
          k = 1;
          octave::idx_vector idx1 = idx(1).index_vector ();

          if (idx0.is_scalar () && idx1.is_scalar ())
            {
              retval = m_matrix.checkelem (idx0(0), idx1(0));
            }
          else
            {
              octave_idx_type m = idx0.length (m_matrix.rows ());
              octave_idx_type n = idx1.length (m_matrix.columns ());
              if (idx0.is_colon_equiv (m) && idx1.is_colon_equiv (n)
                  && m <= m_matrix.rows () && n <= m_matrix.rows ())
                {
                  DMT rm (m_matrix);
                  rm.resize (m, n);
                  retval = rm;
                }
              else
                retval = to_dense ().index_op (idx, resize_ok);
            }
        }
      catch (octave::index_exception& ie)
        {
          // Rethrow to allow more info to be reported later.
          ie.set_pos_if_unset (2, k+1);
          throw;
        }
    }
  else
    retval = to_dense ().index_op (idx, resize_ok);

  return retval;
}

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::subsasgn (const std::string& type,
                                     const std::list<octave_value_list>& idx,
                                     const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      {
        if (type.length () != 1)
          {
            std::string nm = type_name ();
            error ("in indexed assignment of %s, last lhs index must be ()",
                   nm.c_str ());
          }

        octave_value_list jdx = idx.front ();

        // FIXME: Mostly repeated code for cases 1 and 2 could be
        //        consolidated for DRY (Don't Repeat Yourself).
        // Check for assignments to diagonal elements which should not
        // destroy the diagonal property of the matrix.
        // If D is a diagonal matrix then the assignment can be
        // 1) linear, D(i) = x, where ind2sub results in case #2 below
        // 2) subscript D(i,i) = x, where both indices are equal.
        if (jdx.length () == 1 && jdx(0).is_scalar_type ())
          {
            typename DMT::element_type val;
            int k = 0;
            try
              {
                octave::idx_vector ind = jdx(0).index_vector ();
                k = 1;
                dim_vector dv (m_matrix.rows (), m_matrix.cols ());
                Array<octave::idx_vector> ivec = ind2sub (dv, ind);
                octave::idx_vector i0 = ivec(0);
                octave::idx_vector i1 = ivec(1);

                if (i0(0) == i1(0)
                    && chk_valid_scalar (rhs, val))
                  {
                    m_matrix.dgelem (i0(0)) = val;
                    retval = this;
                    this->count++;
                    // invalidate cache
                    m_dense_cache = octave_value ();
                  }
              }
            catch (octave::index_exception& ie)
              {
                // Rethrow to allow more info to be reported later.
                ie.set_pos_if_unset (2, k+1);
                throw;
              }
          }
        else if (jdx.length () == 2
                 && jdx(0).is_scalar_type () && jdx(1).is_scalar_type ())
          {
            typename DMT::element_type val;
            int k = 0;
            try
              {
                octave::idx_vector i0 = jdx(0).index_vector ();
                k = 1;
                octave::idx_vector i1 = jdx(1).index_vector ();
                if (i0(0) == i1(0)
                    && i0(0) < m_matrix.rows () && i1(0) < m_matrix.cols ()
                    && chk_valid_scalar (rhs, val))
                  {
                    m_matrix.dgelem (i0(0)) = val;
                    retval = this;
                    this->count++;
                    // invalidate cache
                    m_dense_cache = octave_value ();
                  }
              }
            catch (octave::index_exception& ie)
              {
                // Rethrow to allow more info to be reported later.
                ie.set_pos_if_unset (2, k+1);
                throw;
              }
          }

        if (! retval.is_defined ())
          retval = numeric_assign (type, idx, rhs);
      }
      break;

    case '{':
    case '.':
      {
        if (! isempty ())
          {
            std::string nm = type_name ();
            error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
          }

        octave_value tmp = octave_value::empty_conv (type, rhs);

        retval = tmp.subsasgn (type, idx, rhs);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::resize (const dim_vector& dv, bool fill) const
{
  octave_value retval;
  if (dv.ndims () == 2)
    {
      DMT rm (m_matrix);
      rm.resize (dv(0), dv(1));
      retval = rm;
    }
  else
    retval = to_dense ().resize (dv, fill);
  return retval;
}

// Return true if this matrix has all true elements (non-zero, not NA/NaN).
template <typename DMT, typename MT>
bool
octave_base_diag<DMT, MT>::is_true (void) const
{
  if (dims ().numel () > 1)
    {
      warn_array_as_logical (dims ());
      // Throw error if any NaN or NA by calling is_true().
      octave_value (m_matrix.extract_diag ()).is_true ();
      return false;                 // > 1x1 diagonal always has zeros
    }
  else
    return to_dense ().is_true ();  // 0x0 or 1x1, handle NaN etc.
}

// FIXME: This should be achieveable using ::real
template <typename T> inline T helper_getreal (T x) { return x; }
template <typename T> inline T helper_getreal (std::complex<T> x)
{ return x.real (); }
// FIXME: We really need some traits so that ad hoc hooks like this
//        are not necessary.
template <typename T> inline T helper_iscomplex (T) { return false; }
template <typename T> inline T helper_iscomplex (std::complex<T>)
{ return true; }

template <typename DMT, typename MT>
double
octave_base_diag<DMT, MT>::double_value (bool force_conversion) const
{
  typedef typename DMT::element_type el_type;

  if (helper_iscomplex (el_type ()) && ! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real scalar");

  if (isempty ())
    err_invalid_conversion (type_name (), "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "real scalar");

  return helper_getreal (el_type (m_matrix (0, 0)));
}

template <typename DMT, typename MT>
float
octave_base_diag<DMT, MT>::float_value (bool force_conversion) const
{
  typedef typename DMT::element_type el_type;

  if (helper_iscomplex (el_type ()) && ! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real scalar");

  if (! (numel () > 0))
    err_invalid_conversion (type_name (), "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "real scalar");

  return helper_getreal (el_type (m_matrix (0, 0)));
}

template <typename DMT, typename MT>
Complex
octave_base_diag<DMT, MT>::complex_value (bool) const
{
  if (rows () == 0 || columns () == 0)
    err_invalid_conversion (type_name (), "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "complex scalar");

  return m_matrix(0, 0);
}

template <typename DMT, typename MT>
FloatComplex
octave_base_diag<DMT, MT>::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion (type_name (), "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            type_name (), "complex scalar");

  retval = m_matrix (0, 0);

  return retval;
}

template <typename DMT, typename MT>
Matrix
octave_base_diag<DMT, MT>::matrix_value (bool) const
{
  return Matrix (diag_matrix_value ());
}

template <typename DMT, typename MT>
FloatMatrix
octave_base_diag<DMT, MT>::float_matrix_value (bool) const
{
  return FloatMatrix (float_diag_matrix_value ());
}

template <typename DMT, typename MT>
ComplexMatrix
octave_base_diag<DMT, MT>::complex_matrix_value (bool) const
{
  return ComplexMatrix (complex_diag_matrix_value ());
}

template <typename DMT, typename MT>
FloatComplexMatrix
octave_base_diag<DMT, MT>::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (float_complex_diag_matrix_value ());
}

template <typename DMT, typename MT>
NDArray
octave_base_diag<DMT, MT>::array_value (bool) const
{
  return NDArray (matrix_value ());
}

template <typename DMT, typename MT>
FloatNDArray
octave_base_diag<DMT, MT>::float_array_value (bool) const
{
  return FloatNDArray (float_matrix_value ());
}

template <typename DMT, typename MT>
ComplexNDArray
octave_base_diag<DMT, MT>::complex_array_value (bool) const
{
  return ComplexNDArray (complex_matrix_value ());
}

template <typename DMT, typename MT>
FloatComplexNDArray
octave_base_diag<DMT, MT>::float_complex_array_value (bool) const
{
  return FloatComplexNDArray (float_complex_matrix_value ());
}

template <typename DMT, typename MT>
boolNDArray
octave_base_diag<DMT, MT>::bool_array_value (bool warn) const
{
  return to_dense ().bool_array_value (warn);
}

template <typename DMT, typename MT>
charNDArray
octave_base_diag<DMT, MT>::char_array_value (bool warn) const
{
  return to_dense ().char_array_value (warn);
}

template <typename DMT, typename MT>
SparseMatrix
octave_base_diag<DMT, MT>::sparse_matrix_value (bool) const
{
  return SparseMatrix (diag_matrix_value ());
}

template <typename DMT, typename MT>
SparseComplexMatrix
octave_base_diag<DMT, MT>::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (complex_diag_matrix_value ());
}

template <typename DMT, typename MT>
octave::idx_vector
octave_base_diag<DMT, MT>::index_vector (bool require_integers) const
{
  return to_dense ().index_vector (require_integers);
}

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::convert_to_str_internal (bool pad, bool force,
    char type) const
{
  return to_dense ().convert_to_str_internal (pad, force, type);
}

template <typename DMT, typename MT>
float_display_format
octave_base_diag<DMT, MT>::get_edit_display_format (void) const
{
  // FIXME
  return float_display_format ();
}

template <typename DMT, typename MT>
std::string
octave_base_diag<DMT, MT>::edit_display (const float_display_format& fmt,
    octave_idx_type i,
    octave_idx_type j) const
{
  std::ostringstream buf;
  octave_print_internal (buf, fmt, m_matrix(i, j));
  return buf.str ();
}

template <typename DMT, typename MT>
bool
octave_base_diag<DMT, MT>::save_ascii (std::ostream& os)
{
  os << "# rows: " << m_matrix.rows () << "\n"
     << "# columns: " << m_matrix.columns () << "\n";

  os << m_matrix.extract_diag ();

  return true;
}

template <typename DMT, typename MT>
bool
octave_base_diag<DMT, MT>::load_ascii (std::istream& is)
{
  octave_idx_type r = 0;
  octave_idx_type c = 0;

  if (! extract_keyword (is, "rows", r, true)
      || ! extract_keyword (is, "columns", c, true))
    error ("load: failed to extract number of rows and columns");

  octave_idx_type l = (r < c ? r : c);
  MT tmp (l, 1);
  is >> tmp;

  if (! is)
    error ("load: failed to load diagonal matrix constant");

  // This is a little tricky, as we have the Matrix type, but
  // not ColumnVector type.  We need to help the compiler get
  // through the inheritance tree.
  typedef typename DMT::element_type el_type;
  m_matrix = DMT (MDiagArray2<el_type> (MArray<el_type> (tmp)));
  m_matrix.resize (r, c);

  // Invalidate cache.  Probably not necessary, but safe.
  m_dense_cache = octave_value ();

  return true;
}

template <typename DMT, typename MT>
void
octave_base_diag<DMT, MT>::print_raw (std::ostream& os,
                                      bool pr_as_read_syntax) const
{
  return octave_print_internal (os, m_matrix, pr_as_read_syntax,
                                current_print_indent_level ());
}

template <typename DMT, typename MT>
mxArray *
octave_base_diag<DMT, MT>::as_mxArray (bool interleaved) const
{
  return to_dense ().as_mxArray (interleaved);
}

template <typename DMT, typename MT>
bool
octave_base_diag<DMT, MT>::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

template <typename DMT, typename MT>
void
octave_base_diag<DMT, MT>::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}
template <typename DMT, typename MT>
int
octave_base_diag<DMT, MT>::write (octave::stream& os, int block_size,
                                  oct_data_conv::data_type output_type,
                                  int skip,
                                  octave::mach_info::float_format flt_fmt) const
{
  return to_dense ().write (os, block_size, output_type, skip, flt_fmt);
}

template <typename DMT, typename MT>
void
octave_base_diag<DMT, MT>::print_info (std::ostream& os,
                                       const std::string& prefix) const
{
  m_matrix.print_info (os, prefix);
}

// FIXME: this function is duplicated in octave_base_matrix<T>.  Could
// it somehow be shared instead?

template <typename DMT, typename MT>
void
octave_base_diag<DMT, MT>::short_disp (std::ostream& os) const
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
              octave_print_internal (buf, m_matrix(i, j));
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

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::fast_elem_extract (octave_idx_type n) const
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

template <typename DMT, typename MT>
octave_value
octave_base_diag<DMT, MT>::to_dense (void) const
{
  if (! m_dense_cache.is_defined ())
    m_dense_cache = MT (m_matrix);

  return m_dense_cache;
}
