/*

Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Andy Adler

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iomanip>
#include <iostream>

#include "oct-obj.h"
#include "ov-base.h"
#include "quit.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

#include "boolSparse.h"
#include "ov-base-sparse.h"
#include "pager.h"

template <class T>
octave_value
octave_base_sparse<T>::do_index_op (const octave_value_list& idx, 
				    bool resize_ok)
{
  octave_value retval;

  octave_idx_type n_idx = idx.length ();

  int nd = matrix.ndims ();

  switch (n_idx)
    {
    case 0:
      retval = matrix;
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	if (! error_state)
	  retval = octave_value (matrix.index (i, resize_ok));
      }
      break;

    default:
      {
	if (n_idx == 2 && nd == 2)
	  {
	    idx_vector i = idx (0).index_vector ();

	    if (! error_state)
	      {
		idx_vector j = idx (1).index_vector ();

		if (! error_state)
		  retval = octave_value (matrix.index (i, j, resize_ok));
	      }
	  }
	else
	  {
	    Array<idx_vector> idx_vec (n_idx);

	    for (octave_idx_type i = 0; i < n_idx; i++)
	      {
		idx_vec(i) = idx(i).index_vector ();

		if (error_state)
		  break;
	      }

	    if (! error_state)
	      retval = octave_value (matrix.index (idx_vec, resize_ok));
	  }
      }
      break;
    }

  return retval;
}

template <class T>
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

template <class T>
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
	if (type.length () == 1)
	  retval = numeric_assign (type, idx, rhs);
	else
	  {
	    std::string nm = type_name ();
	    error ("in indexed assignment of %s, last lhs index must be ()",
		   nm.c_str ());
	  }
      }
      break;

    case '{':
    case '.':
      {
	if (is_empty ())
	  {
	    octave_value tmp = octave_value::empty_conv (type, rhs);

	    retval = tmp.subsasgn (type, idx, rhs);
	  }
	else
	  {
	    std::string nm = type_name ();
	    error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
	  }
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <class T>
void 
octave_base_sparse<T>::assign (const octave_value_list& idx, const T& rhs)
{
  octave_idx_type len = idx.length ();

  for (octave_idx_type i = 0; i < len; i++)
    matrix.set_index (idx(i).index_vector ());

  ::assign (matrix, rhs);

  // Invalidate matrix type.
  typ.invalidate_type ();
}

template <class MT>
void
octave_base_sparse<MT>::delete_elements (const octave_value_list& idx)
{
  octave_idx_type len = idx.length ();

  Array<idx_vector> ra_idx (len);

  for (octave_idx_type i = 0; i < len; i++)
    ra_idx(i) = idx(i).index_vector ();

  matrix.maybe_delete_elements (ra_idx);

  // Invalidate the matrix type
  typ.invalidate_type ();
}

template <class T>
octave_value 
octave_base_sparse<T>::resize (const dim_vector& dv, bool) const
{ 
  T retval (matrix); 
  retval.resize (dv); 
  return retval; 
}

template <class T>
bool 
octave_base_sparse<T>::is_true (void) const
{
  bool retval = false;
  dim_vector dv = matrix.dims ();
  octave_idx_type nel = dv.numel ();
  octave_idx_type nz = nzmax ();

  if (nz == nel && nel > 0)
    {
      T t1 (matrix.reshape (dim_vector (nel, 1)));

      SparseBoolMatrix t2 = t1.all ();

      retval = t2(0);
    }

  return retval;
}

template <class T>
bool 
octave_base_sparse<T>::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

template <class T>
void 
octave_base_sparse<T>::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class T>
void 
octave_base_sparse<T>::print_info (std::ostream& os, 
				   const std::string& prefix) const
{
  matrix.print_info (os, prefix);
}

template <class T>
void
octave_base_sparse<T>::print_raw (std::ostream& os,
				  bool pr_as_read_syntax) const
{
  octave_idx_type nr = matrix.rows ();
  octave_idx_type nc = matrix.cols ();
  octave_idx_type nz = nnz ();

  // FIXME -- this should probably all be handled by a
  // separate octave_print_internal function that can handle format
  // compact, loose, etc.

  os << "Compressed Column Sparse (rows = " << nr
     << ", cols = " << nc
     << ", nnz = " << nz;

  double dnel = matrix.numel ();

  if (dnel > 0)
    os << " [" << std::setprecision (2) << (nz / dnel * 100) << "%]";

  os << ")\n";

  // add one to the printed indices to go from
  //  zero-based to one-based arrays

  if (nz != 0)
    {
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  octave_quit ();

	  // FIXME -- is there an easy way to get the max row
	  // and column indices so we can set the width appropriately
	  // and line up the columns here?  Similarly, we should look
	  // at all the nonzero values and display them with the same
	  // formatting rules that apply to columns of a matrix.

	  for (octave_idx_type i = matrix.cidx(j); i < matrix.cidx(j+1); i++)
	    {
	      os << "\n";
	      os << "  (" << matrix.ridx(i)+1 <<
		", "  << j+1 << ") -> ";

	      octave_print_internal (os, matrix.data(i), pr_as_read_syntax);
	    }
	}
    }
}

template <class T>
bool
octave_base_sparse<T>::save_ascii (std::ostream& os)
{
  dim_vector dv = this->dims ();

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();

  os << "# nnz: "      << nzmax () << "\n";
  os << "# rows: "     << dv (0) << "\n";
  os << "# columns: "  << dv (1) << "\n";

  os << this->matrix;

  return true;
}

template <class T>
bool 
octave_base_sparse<T>::load_ascii (std::istream& is)
{
  octave_idx_type nz = 0;
  octave_idx_type nr = 0;
  octave_idx_type nc = 0;
  bool success = true;

  if (extract_keyword (is, "nnz", nz, true) &&
      extract_keyword (is, "rows", nr, true) &&
      extract_keyword (is, "columns", nc, true))
    {
      T tmp (nr, nc, nz);

      is >> tmp;

      if (!is) 
	{
	  error ("load: failed to load matrix constant");
	  success = false;
	}

      matrix = tmp;
    }
  else
    {
      error ("load: failed to extract number of rows and columns");
      success = false;
    }

  return success;
}

template <class T>
octave_value
octave_base_sparse<T>::map (octave_base_value::unary_mapper_t umap) const
{
  // Try the map on the dense value.
  octave_value retval = this->full_value ().map (umap);

  // Sparsify the result if possible.
  // FIXME: intentionally skip this step for string mappers. Is this wanted?
  if (umap >= umap_xisalnum && umap <= umap_xtoupper)
    return retval;

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

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
