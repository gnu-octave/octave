/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <climits>

#include <iostream>
#include <vector>

#include "ov-base.h"
#include "ov-scalar.h"
#include "ov-complex.h"
#include "gripes.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

#include "ov-base-sparse.h"
#include "ov-base-sparse.cc"

#include "ov-bool-sparse.h"

template class octave_base_sparse<SparseComplexMatrix>;

DEFINE_OCTAVE_ALLOCATOR (octave_sparse_complex_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_sparse_complex_matrix, "sparse complex matrix", "sparse");

octave_base_value *
octave_sparse_complex_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  int nr = matrix.rows ();
  int nc = matrix.cols ();

  // Don't use numel, since it can overflow for very large matrices
  // Note that for the tests on matrix size, they become approximative
  // since they involves a cast to double to avoid issues of overflow
  if (matrix.rows () == 1 && matrix.cols () == 1)
    {
      // Const copy of the matrix, so the right version of () operator used
      const SparseComplexMatrix tmp (matrix);

      Complex c = tmp (0, 0);

      if (imag (c) == 0.0)
	retval = new octave_scalar (std::real (c));
      else
	retval = new octave_complex (c);
    }
  else if (nr == 0 || nc == 0)
    retval = new octave_matrix (Matrix (nr, nc));
  else if (matrix.all_elements_are_real ())
    if (matrix.cols () > 0 && matrix.rows () > 0 && 
	double (matrix.byte_size ()) > double (matrix.rows ()) *
	double (matrix.cols ()) * sizeof (double))
      retval = new octave_matrix (::real (matrix.matrix_value ()));
    else
      retval = new octave_sparse_matrix (::real (matrix));
  else if (matrix.cols () > 0 && matrix.rows () > 0 && 
	   double (matrix.byte_size ()) > double (matrix.rows ()) *
	   double (matrix.cols ()) * sizeof (Complex))
    retval = new octave_complex_matrix (matrix.matrix_value ());
    
  return retval;
}

void
octave_sparse_complex_matrix::assign (const octave_value_list& idx,
				      const SparseComplexMatrix& rhs)
{
  octave_base_sparse<SparseComplexMatrix>::assign (idx, rhs);
}

void
octave_sparse_complex_matrix::assign (const octave_value_list& idx,
				      const SparseMatrix& rhs)
{
  int len = idx.length ();

  for (int i = 0; i < len; i++)
    matrix.set_index (idx(i).index_vector ());

  ::assign (matrix, rhs);
}

bool
octave_sparse_complex_matrix::valid_as_scalar_index (void) const
{
  // FIXME
  return false;
}

double
octave_sparse_complex_matrix::double_value (bool force_conversion) const
{
  double retval = lo_ieee_nan_value ();

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex sparse matrix", "real scalar");

  // FIXME -- maybe this should be a function, valid_as_scalar()
  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "complex sparse matrix", "real scalar");

      retval = std::real (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("complex sparse matrix", "real scalar");

  return retval;
}

Matrix
octave_sparse_complex_matrix::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex sparse matrix", "real matrix");

  retval = ::real (matrix.matrix_value ());

  return retval;
}

Complex
octave_sparse_complex_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  // FIXME -- maybe this should be a function, valid_as_scalar()
  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "complex sparse matrix", "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("complex sparse matrix", "real scalar");

  return retval;
}

ComplexMatrix
octave_sparse_complex_matrix::complex_matrix_value (bool) const
{
  return matrix.matrix_value ();
}

ComplexNDArray 
octave_sparse_complex_matrix::complex_array_value (bool) const
{
  return ComplexNDArray (matrix.matrix_value ());
}

SparseMatrix
octave_sparse_complex_matrix::sparse_matrix_value (bool force_conversion) const
{
  SparseMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex sparse matrix", 
			       "real sparse matrix");

  retval = ::real (matrix);

  return retval;
}

bool 
octave_sparse_complex_matrix::save_binary (std::ostream& os, 
					   bool&save_as_floats)
{
  dim_vector d = this->dims ();
  if (d.length() < 1)
    return false;

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();

  int nr = d(0);
  int nc = d(1);
  int nz = nzmax ();

  int32_t itmp;
  // Use negative value for ndims to be consistent with other formats
  itmp= -2;        
  os.write (reinterpret_cast<char *> (&itmp), 4);
  
  itmp= nr;    
  os.write (reinterpret_cast<char *> (&itmp), 4);
  
  itmp= nc;
  os.write (reinterpret_cast<char *> (&itmp), 4);
  
  itmp= nz;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  save_type st = LS_DOUBLE;
  if (save_as_floats)
    {
      if (matrix.too_large_for_float ())
	{
	  warning ("save: some values too large to save as floats --");
	  warning ("save: saving as doubles instead");
	}
      else
	st = LS_FLOAT;
    }
  else if (matrix.nzmax () > 8192) // FIXME -- make this configurable.
    {
      double max_val, min_val;
      if (matrix.all_integers (max_val, min_val))
	st = get_save_type (max_val, min_val);
    }

  // add one to the printed indices to go from
  // zero-based to one-based arrays
   for (int i = 0; i < nc+1; i++)  
     {
       OCTAVE_QUIT;
       itmp = matrix.cidx(i);
       os.write (reinterpret_cast<char *> (&itmp), 4);
     }

   for (int i = 0; i < nz; i++) 
     {
       OCTAVE_QUIT;
       itmp = matrix.ridx(i); 
       os.write (reinterpret_cast<char *> (&itmp), 4);
     }

   write_doubles (os, reinterpret_cast<const double *> (matrix.data()), st, 2 * nz);

  return true;
}

bool
octave_sparse_complex_matrix::load_binary (std::istream& is, bool swap,
				   oct_mach_info::float_format fmt)
{
  int32_t nz, nc, nr, tmp;
  char ctmp;

  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;

  if (swap)
    swap_bytes<4> (&tmp);

  if (tmp != -2) {
    error("load: only 2D sparse matrices are supported");
    return false;
  }

  if (! is.read (reinterpret_cast<char *> (&nr), 4))
    return false;
  if (! is.read (reinterpret_cast<char *> (&nc), 4))
    return false;
  if (! is.read (reinterpret_cast<char *> (&nz), 4))
    return false;

  if (swap)
    {
      swap_bytes<4> (&nr);
      swap_bytes<4> (&nc);
      swap_bytes<4> (&nz);
    }

  SparseComplexMatrix m (static_cast<octave_idx_type> (nr),
			 static_cast<octave_idx_type> (nc),
			 static_cast<octave_idx_type> (nz));

  for (int i = 0; i < nc+1; i++) 
    {
      OCTAVE_QUIT;
      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
	return false;
      if (swap)
	swap_bytes<4> (&tmp);
      m.cidx(i) = tmp;
    }

  for (int i = 0; i < nz; i++) 
    {
      OCTAVE_QUIT;
      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
	return false;
      if (swap)
	swap_bytes<4> (&tmp);
      m.ridx(i) = tmp;
    }

  if (! is.read (reinterpret_cast<char *> (&ctmp), 1))
    return false;
  
  read_doubles (is, reinterpret_cast<double *> (m.data ()),
		static_cast<save_type> (ctmp), 2 * nz, swap, fmt);

  if (error_state || ! is)
    return false;
  matrix = m;

  return true;
}

#if defined (HAVE_HDF5)
bool
octave_sparse_complex_matrix::save_hdf5 (hid_t loc_id, const char *name, 
					 bool save_as_floats)
{
  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();

  hid_t group_hid = H5Gcreate (loc_id, name, 0);
  if (group_hid < 0)
    return false;

  hid_t space_hid = -1, data_hid = -1;
  bool retval = true;
  SparseComplexMatrix m = sparse_complex_matrix_value ();
  octave_idx_type tmp;
  hsize_t hdims[2];

  space_hid = H5Screate_simple (0, hdims, 0);
  if (space_hid < 0) 
    {
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "nr", H5T_NATIVE_IDX, space_hid, 
			H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }
  
  tmp = m.rows ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }    

  data_hid = H5Dcreate (group_hid, "nc", H5T_NATIVE_IDX, space_hid, 
			H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }
  
  tmp = m.cols ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }    

  data_hid = H5Dcreate (group_hid, "nz", H5T_NATIVE_IDX, space_hid, 
			H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }
  
  tmp = m.nzmax ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);

  hdims[0] = m.cols() + 1;
  hdims[1] = 1;

  space_hid = H5Screate_simple (2, hdims, 0);

  if (space_hid < 0) 
    {
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "cidx", H5T_NATIVE_IDX, space_hid, 
			H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }
  
  octave_idx_type * itmp = m.xcidx ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, itmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }    

  H5Sclose (space_hid);

  hdims[0] = m.nzmax ();
  hdims[1] = 1;

  space_hid = H5Screate_simple (2, hdims, 0);

  if (space_hid < 0) 
    {
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "ridx", H5T_NATIVE_IDX, space_hid, 
			H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }
  
  itmp = m.xridx ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, itmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  hid_t save_type_hid = H5T_NATIVE_DOUBLE;

  if (save_as_floats)
    {
      if (m.too_large_for_float ())
	{
	  warning ("save: some values too large to save as floats --");
	  warning ("save: saving as doubles instead");
	}
      else
	save_type_hid = H5T_NATIVE_FLOAT;
    }
#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
  // hdf5 currently doesn't support float/integer conversions
  else
    {
      double max_val, min_val;

      if (m.all_integers (max_val, min_val))
	save_type_hid
	  = save_type_to_hdf5 (get_save_type (max_val, min_val));
    }
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

  hid_t type_hid = hdf5_make_complex_type (save_type_hid);
  if (type_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "data", type_hid, space_hid, H5P_DEFAULT);
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  hid_t complex_type_hid = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
  retval = false;
  if (complex_type_hid >= 0) 
    {
      Complex * ctmp = m.xdata ();

      retval = H5Dwrite (data_hid, complex_type_hid, H5S_ALL, H5S_ALL,
			 H5P_DEFAULT, ctmp) >= 0;
    }

  H5Dclose (data_hid);
  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;
}

bool
octave_sparse_complex_matrix::load_hdf5 (hid_t loc_id, const char *name,
					 bool /* have_h5giterate_bug */)
{
  octave_idx_type nr, nc, nz;
  hid_t group_hid, data_hid, space_hid;
  hsize_t rank;
  
  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize(dv);
  if (empty)
    return (empty > 0);
  
  group_hid = H5Gopen (loc_id, name);
  if (group_hid < 0 ) return false;

  data_hid = H5Dopen (group_hid, "nr");
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    { 
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, &nr) < 0)
    { 
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

  data_hid = H5Dopen (group_hid, "nc");
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    { 
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, &nc) < 0)
    { 
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);
  
  data_hid = H5Dopen (group_hid, "nz");
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    { 
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, &nz) < 0)
    { 
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

  SparseComplexMatrix m (static_cast<octave_idx_type> (nr),
			 static_cast<octave_idx_type> (nc),
			 static_cast<octave_idx_type> (nz));

  data_hid = H5Dopen (group_hid, "cidx");
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  if (static_cast<int> (hdims[0]) != nc + 1 || 
      static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  octave_idx_type *itmp = m.xcidx ();
  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, itmp) < 0) 
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  H5Dclose (data_hid);

  data_hid = H5Dopen (group_hid, "ridx");
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  if (static_cast<int> (hdims[0]) != nz || 
      static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  itmp = m.xridx ();
  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, itmp) < 0) 
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  H5Dclose (data_hid);

  data_hid = H5Dopen (group_hid, "data");
  hid_t type_hid = H5Dget_type (data_hid);

  hid_t complex_type = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);

  if (! hdf5_types_compatible (type_hid, complex_type))
    {
      H5Tclose (complex_type);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  if (static_cast<int> (hdims[0]) != nz || 
      static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  Complex *ctmp = m.xdata ();
  bool retval = false;
  if (H5Dread (data_hid, complex_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, ctmp) >= 0) 
    {
      retval = true;
      matrix = m;
    }

  H5Tclose (complex_type);
  H5Sclose (space_hid);
  H5Dclose (data_hid);
  H5Gclose (group_hid);

  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
