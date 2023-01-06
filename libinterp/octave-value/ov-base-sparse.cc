////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

#include <iomanip>
#include <istream>
#include <ostream>
#include <sstream>

#include "ovl.h"
#include "ov-base.h"
#include "quit.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-oct-text.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

#include "boolSparse.h"
#include "ov-base-sparse.h"
#include "octave-preserve-stream-state.h"
#include "pager.h"
#include "utils.h"

#include "lo-array-errwarn.h"

template <typename T>
octave_value
octave_base_sparse<T>::do_index_op (const octave_value_list& idx,
                                    bool resize_ok)
{
  octave_value retval;

  octave_idx_type n_idx = idx.length ();

  // If we catch an indexing error in index_vector, we flag an error in
  // index k.  Ensure it is the right value before each idx_vector call.
  // Same variable as used in the for loop in the default case.

  octave_idx_type k = 0;

  try
    {
      switch (n_idx)
        {
        case 0:
          retval = matrix;
          break;

        case 1:
          {
            octave::idx_vector i = idx (0).index_vector ();

            retval = octave_value (matrix.index (i, resize_ok));
          }
          break;

        case 2:
          {
            octave::idx_vector i = idx (0).index_vector ();

            k = 1;
            octave::idx_vector j = idx (1).index_vector ();

            retval = octave_value (matrix.index (i, j, resize_ok));
          }
          break;

        default:
          error ("sparse indexing needs 1 or 2 indices");
        }
    }
  catch (octave::index_exception& ie)
    {
      // Rethrow to allow more info to be reported later.
      ie.set_pos_if_unset (n_idx, k+1);
      throw;
    }

  return retval;
}

template <typename T>
octave_value
octave_base_sparse<T>::subsref (const std::string& type,
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

template <typename T>
octave_value
octave_base_sparse<T>::subsasgn (const std::string& type,
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

template <typename MT>
void
octave_base_sparse<MT>::delete_elements (const octave_value_list& idx)
{
  octave_idx_type len = idx.length ();

  // If we catch an indexing error in index_vector, we flag an error in
  // index k.  Ensure it is the right value before each idx_vector call.
  // Same variable as used in the for loop in the default case.

  octave_idx_type k = 0;

  try
    {
      switch (len)
        {
        case 1:
          {
            octave::idx_vector i = idx (0).index_vector ();

            matrix.delete_elements (i);

            break;
          }

        case 2:
          {
            octave::idx_vector i = idx (0).index_vector ();

            k = 1;
            octave::idx_vector j = idx (1).index_vector ();

            matrix.delete_elements (i, j);

            break;
          }

        default:
          error ("sparse indexing needs 1 or 2 indices");
        }
    }
  catch (octave::index_exception& ie)
    {
      // Rethrow to allow more info to be reported later.
      ie.set_pos_if_unset (len, k+1);
      throw;
    }

  // Invalidate the matrix type
  typ.invalidate_type ();
}

template <typename T>
octave_value
octave_base_sparse<T>::resize (const dim_vector& dv, bool) const
{
  T retval (matrix);
  retval.resize (dv);
  return retval;
}

template <typename T>
bool
octave_base_sparse<T>::is_true (void) const
{
  bool retval = false;
  dim_vector dv = matrix.dims ();
  octave_idx_type nel = dv.numel ();
  octave_idx_type nz = nnz ();

  if (nel > 0)
    {
      T t1 (matrix.reshape (dim_vector (nel, 1)));

      if (t1.any_element_is_nan ())
        octave::err_nan_to_logical_conversion ();

      if (nel > 1)
        warn_array_as_logical (dv);

      if (nz == nel)
        {
          SparseBoolMatrix t2 = t1.all ();

          retval = t2(0);
        }
    }

  return retval;
}

template <typename T>
bool
octave_base_sparse<T>::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

template <typename T>
void
octave_base_sparse<T>::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <typename T>
void
octave_base_sparse<T>::print_info (std::ostream& os,
                                   const std::string& prefix) const
{
  matrix.print_info (os, prefix);
}

template <typename T>
void
octave_base_sparse<T>::print_raw (std::ostream& os,
                                  bool pr_as_read_syntax) const
{
  octave::preserve_stream_state stream_state (os);

  octave_idx_type nr = matrix.rows ();
  octave_idx_type nc = matrix.cols ();
  octave_idx_type nz = nnz ();

  // FIXME: this should probably all be handled by a
  // separate octave_print_internal function that can handle format
  // compact, loose, etc.

  os << "Compressed Column Sparse (rows = " << nr
     << ", cols = " << nc
     << ", nnz = " << nz;

  // Avoid calling numel here since it can easily overflow
  // octave_idx_type even when there is no real problem storing the
  // sparse array.

  double dnr = nr;
  double dnc = nc;
  double dnel = dnr * dnc;

  if (dnel > 0)
    {
      double pct = (nz / dnel * 100);

      int prec = 2;

      // Display at least 2 significant figures and up to 4 as we
      // approach 100%.  Avoid having limited precision of the display
      // result in reporting 100% for matrices that are not actually
      // 100% full.

      if (pct == 100)
        prec = 3;
      else
        {
          if (pct > 99.9)
            prec = 4;
          else if (pct > 99)
            prec = 3;

          if (pct > 99.99)
            pct = 99.99;
        }

      os << " [" << std::setprecision (prec) << pct << "%]";
    }

  os << ")\n";

  // add one to the printed indices to go from
  //  zero-based to one-based arrays

  if (nz != 0)
    {
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          // FIXME: is there an easy way to get the max row
          // and column indices so we can set the width appropriately
          // and line up the columns here?  Similarly, we should look
          // at all the nonzero values and display them with the same
          // formatting rules that apply to columns of a matrix.

          for (octave_idx_type i = matrix.cidx (j); i < matrix.cidx (j+1); i++)
            {
              os << "\n";
              os << "  (" << matrix.ridx (i)+1 << ", "  << j+1 << ") -> ";

              octave_print_internal (os, matrix.data (i), pr_as_read_syntax);
            }
        }
    }
}

template <typename MT>
float_display_format
octave_base_sparse<MT>::get_edit_display_format (void) const
{
  return float_display_format ();
  //  return make_format (this->matrix);
}

template <typename MT>
std::string
octave_base_sparse<MT>::edit_display (const float_display_format& fmt,
                                      octave_idx_type i,
                                      octave_idx_type j) const
{
  std::ostringstream buf;
  octave_print_internal (buf, fmt, this->matrix(i, j));
  return buf.str ();
}

template <typename T>
bool
octave_base_sparse<T>::save_ascii (std::ostream& os)
{
  dim_vector dv = this->dims ();

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();

  os << "# nnz: "      << nnz () << "\n";
  os << "# rows: "     << dv(0) << "\n";
  os << "# columns: "  << dv(1) << "\n";

  os << this->matrix;

  return true;
}

template <typename T>
bool
octave_base_sparse<T>::load_ascii (std::istream& is)
{
  octave_idx_type nz = 0;
  octave_idx_type nr = 0;
  octave_idx_type nc = 0;

  if (! extract_keyword (is, "nnz", nz, true)
      || ! extract_keyword (is, "rows", nr, true)
      || ! extract_keyword (is, "columns", nc, true))
    error ("load: failed to extract number of rows and columns");

  T tmp (nr, nc, nz);

  is >> tmp;

  if (! is)
    error ("load: failed to load matrix constant");

  matrix = tmp;

  return true;
}

template <typename T>
octave_value
octave_base_sparse<T>::fast_elem_extract (octave_idx_type n) const
{
  octave_idx_type nr = matrix.rows ();
  octave_idx_type nc = matrix.cols ();

  octave_idx_type i = n % nr;
  octave_idx_type j = n / nr;

  return (i < nr && j < nc) ? octave_value (matrix(i, j)) : octave_value ();
}

template <typename T>
octave_value
octave_base_sparse<T>::map (octave_base_value::unary_mapper_t umap) const
{
  if (umap == umap_xtolower || umap == umap_xtoupper)
    return matrix;

  // Try the map on the dense value.
  // FIXME: We should probably be smarter about this, especially for the
  // cases that are expected to return sparse matrices.
  octave_value retval = this->full_value ().map (umap);

  // Sparsify the result if possible.

  switch (umap)
    {
    case umap_xisalnum:
    case umap_xisalpha:
    case umap_xisascii:
    case umap_xiscntrl:
    case umap_xisdigit:
    case umap_xisgraph:
    case umap_xislower:
    case umap_xisprint:
    case umap_xispunct:
    case umap_xisspace:
    case umap_xisupper:
    case umap_xisxdigit:
      // FIXME: intentionally skip this step for string mappers.
      // Is this wanted?
      break;

    default:
      {
        switch (retval.builtin_type ())
          {
          case btyp_double:
            retval = retval.sparse_matrix_value ();
            break;

          case btyp_complex:
            retval = retval.sparse_complex_matrix_value ();
            break;

          case btyp_bool:
            retval = retval.sparse_bool_matrix_value ();
            break;

          default:
            break;
          }
      }
    }

  return retval;
}
