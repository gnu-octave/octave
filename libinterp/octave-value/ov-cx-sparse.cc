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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <istream>
#include <ostream>
#include <vector>

#include "lo-specfun.h"
#include "lo-mappers.h"
#include "oct-locbuf.h"

#include "mxarray.h"
#include "ov-base.h"
#include "ov-scalar.h"
#include "ov-complex.h"
#include "errwarn.h"

#include "oct-hdf5.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

#include "ov-base-sparse.h"
#include "ov-base-sparse.cc"

#include "ov-bool-sparse.h"


template class octave_base_sparse<SparseComplexMatrix>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_sparse_complex_matrix,
                                     "sparse complex matrix", "double");

octave_base_value *
octave_sparse_complex_matrix::try_narrowing_conversion (void)
{
  return (matrix.all_elements_are_real ()
          ? new octave_sparse_matrix (::real (matrix)) : nullptr);
}

double
octave_sparse_complex_matrix::double_value (bool force_conversion) const
{
  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex sparse matrix", "real scalar");

  // FIXME: maybe this should be a function, valid_as_scalar()
  if (isempty ())
    err_invalid_conversion ("complex sparse matrix", "real scalar");

  if (numel () > 1)
    warn_implicit_conversion ("Octave:array-to-scalar",
                              "complex sparse matrix", "real scalar");

  return std::real (matrix(0, 0));
}

Matrix
octave_sparse_complex_matrix::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex sparse matrix", "real matrix");

  retval = ::real (matrix.matrix_value ());

  return retval;
}

Complex
octave_sparse_complex_matrix::complex_value (bool) const
{
  // FIXME: maybe this should be a function, valid_as_scalar()
  if (isempty ())
    err_invalid_conversion ("complex sparse matrix", "real scalar");

  if (numel () > 1)
    warn_implicit_conversion ("Octave:array-to-scalar",
                              "complex sparse matrix", "real scalar");

  return matrix(0, 0);
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

charNDArray
octave_sparse_complex_matrix::char_array_value (bool frc_str_conv) const
{
  charNDArray retval;

  if (! frc_str_conv)
    warn_implicit_conversion ("Octave:num-to-str",
                              "sparse complex matrix", "string");
  else
    {
      retval = charNDArray (dims (), 0);
      octave_idx_type nc = matrix.cols ();
      octave_idx_type nr = matrix.rows ();

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = matrix.cidx (j); i < matrix.cidx (j+1); i++)
          retval(matrix.ridx (i) + nr * j)
            = static_cast<char> (std::real (matrix.data (i)));
    }

  return retval;
}

SparseMatrix
octave_sparse_complex_matrix::sparse_matrix_value (bool force_conversion) const
{
  SparseMatrix retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex sparse matrix",
                              "real sparse matrix");

  retval = ::real (matrix);

  return retval;
}

SparseBoolMatrix
octave_sparse_complex_matrix::sparse_bool_matrix_value (bool warn) const
{
  if (matrix.any_element_is_nan ())
    octave::err_nan_to_logical_conversion ();
  if (warn && (! matrix.all_elements_are_real ()
               || real (matrix).any_element_not_one_or_zero ()))
    warn_logical_conversion ();

  return mx_el_ne (matrix, Complex (0.0));
}

octave_value
octave_sparse_complex_matrix::as_double (void) const
{
  return this->matrix;
}

bool
octave_sparse_complex_matrix::save_binary (std::ostream& os,
    bool save_as_floats)
{
  dim_vector dv = this->dims ();
  if (dv.ndims () < 1)
    return false;

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();

  int nr = dv(0);
  int nc = dv(1);
  int nz = nnz ();

  int32_t itmp;
  // Use negative value for ndims to be consistent with other formats
  itmp = -2;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  itmp = nr;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  itmp = nc;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  itmp = nz;
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
  else if (matrix.nnz () > 8192) // FIXME: make this configurable.
    {
      double max_val, min_val;
      if (matrix.all_integers (max_val, min_val))
        st = octave::get_save_type (max_val, min_val);
    }

  // add one to the printed indices to go from
  // zero-based to one-based arrays
  for (int i = 0; i < nc+1; i++)
    {
      octave_quit ();
      itmp = matrix.cidx (i);
      os.write (reinterpret_cast<char *> (&itmp), 4);
    }

  for (int i = 0; i < nz; i++)
    {
      octave_quit ();
      itmp = matrix.ridx (i);
      os.write (reinterpret_cast<char *> (&itmp), 4);
    }

  write_doubles (os, reinterpret_cast<const double *> (matrix.data ()), st,
                 2 * nz);

  return true;
}

bool
octave_sparse_complex_matrix::load_binary (std::istream& is, bool swap,
    octave::mach_info::float_format fmt)
{
  int32_t nz, nc, nr, tmp;
  char ctmp;

  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;

  if (swap)
    swap_bytes<4> (&tmp);

  if (tmp != -2)
    error ("load: only 2-D sparse matrices are supported");

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
      octave_quit ();
      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);
      m.cidx (i) = tmp;
    }

  for (int i = 0; i < nz; i++)
    {
      octave_quit ();
      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);
      m.ridx (i) = tmp;
    }

  if (! is.read (reinterpret_cast<char *> (&ctmp), 1))
    return false;

  read_doubles (is, reinterpret_cast<double *> (m.data ()),
                static_cast<save_type> (ctmp), 2 * nz, swap, fmt);

  if (! is)
    return false;

  if (! m.indices_ok ())
    return false;

  matrix = m;

  return true;
}

bool
octave_sparse_complex_matrix::save_hdf5 (octave_hdf5_id loc_id,
    const char *name,
    bool save_as_floats)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();

#if defined (HAVE_HDF5_18)
  hid_t group_hid = H5Gcreate (loc_id, name, octave_H5P_DEFAULT,
                               octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
  hid_t group_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (group_hid < 0)
    return false;

  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;
  SparseComplexMatrix m = sparse_complex_matrix_value ();
  octave_idx_type tmp;
  hsize_t hdims[2];

  space_hid = H5Screate_simple (0, hdims, nullptr);
  if (space_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "nr", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nr", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  tmp = m.rows ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (! retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "nc", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nc", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  tmp = m.cols ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (! retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "nz", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nz", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  tmp = m.nnz ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (! retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);

  hdims[0] = m.cols () + 1;
  hdims[1] = 1;

  space_hid = H5Screate_simple (2, hdims, nullptr);

  if (space_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "cidx", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "cidx", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  octave_idx_type *itmp = m.xcidx ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, itmp) >= 0;
  H5Dclose (data_hid);
  if (! retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);

  hdims[0] = m.nnz ();
  hdims[1] = 1;

  space_hid = H5Screate_simple (2, hdims, nullptr);

  if (space_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "ridx", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "ridx", H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  itmp = m.xridx ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, itmp) >= 0;
  H5Dclose (data_hid);
  if (! retval)
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
#if defined (HAVE_HDF5_INT2FLOAT_CONVERSIONS)
  // hdf5 currently doesn't support float/integer conversions
  else
    {
      double max_val, min_val;

      if (m.all_integers (max_val, min_val))
        save_type_hid
          = save_type_to_hdf5 (octave::get_save_type (max_val, min_val));
    }
#endif

  hid_t type_hid = hdf5_make_complex_type (save_type_hid);
  if (type_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "data", type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "data", type_hid, space_hid,
                        octave_H5P_DEFAULT);
#endif
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
      Complex *ctmp = m.xdata ();

      retval = H5Dwrite (data_hid, complex_type_hid, octave_H5S_ALL,
                         octave_H5S_ALL, octave_H5P_DEFAULT, ctmp) >= 0;
    }

  H5Dclose (data_hid);
  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (save_as_floats);

  warn_save ("hdf5");
#endif

  return retval;
}

bool
octave_sparse_complex_matrix::load_hdf5 (octave_hdf5_id loc_id,
    const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  octave_idx_type nr, nc, nz;
  hid_t group_hid, data_hid, space_hid;
  hsize_t rank;

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize (dv);
  if (empty)
    return (empty > 0);

#if defined (HAVE_HDF5_18)
  group_hid = H5Gopen (loc_id, name, octave_H5P_DEFAULT);
#else
  group_hid = H5Gopen (loc_id, name);
#endif
  if (group_hid < 0) return false;

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "nr", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nr");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL,
               octave_H5S_ALL, octave_H5P_DEFAULT, &nr) < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "nc", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nc");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL,
               octave_H5S_ALL, octave_H5P_DEFAULT, &nc) < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "nz", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nz");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL,
               octave_H5S_ALL, octave_H5P_DEFAULT, &nz) < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

  SparseComplexMatrix m (static_cast<octave_idx_type> (nr),
                         static_cast<octave_idx_type> (nc),
                         static_cast<octave_idx_type> (nz));

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "cidx", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "cidx");
#endif
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

  if (static_cast<int> (hdims[0]) != nc + 1
      || static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  octave_idx_type *itmp = m.xcidx ();
  if (H5Dread (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL,
               octave_H5S_ALL, octave_H5P_DEFAULT, itmp) < 0)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  H5Dclose (data_hid);

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "ridx", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "ridx");
#endif
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

  if (static_cast<int> (hdims[0]) != nz
      || static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  itmp = m.xridx ();
  if (H5Dread (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL,
               octave_H5S_ALL, octave_H5P_DEFAULT, itmp) < 0)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  H5Dclose (data_hid);

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "data", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "data");
#endif
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

  if (static_cast<int> (hdims[0]) != nz
      || static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  Complex *ctmp = m.xdata ();

  if (H5Dread (data_hid, complex_type, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, ctmp) >= 0
      && m.indices_ok ())
    {
      retval = true;
      matrix = m;
    }

  H5Tclose (complex_type);
  H5Sclose (space_hid);
  H5Dclose (data_hid);
  H5Gclose (group_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");
#endif

  return retval;
}

mxArray *
octave_sparse_complex_matrix::as_mxArray (bool interleaved) const
{
  mwSize nz = nzmax ();
  mwSize nr = rows ();
  mwSize nc = columns ();

  mxArray *retval = new mxArray (interleaved, mxDOUBLE_CLASS, nr, nc, nz,
                                 mxCOMPLEX);

  mwIndex *ir = retval->get_ir ();

  const Complex *pdata = matrix.data ();
  const octave_idx_type *pridx = matrix.ridx ();

  if (interleaved)
    {
      mxComplexDouble *pd
        = static_cast<mxComplexDouble *> (retval->get_data ());

      for (mwIndex i = 0; i < nz; i++)
        {
          pd[i].real = pdata[i].real ();
          pd[i].imag = pdata[i].imag ();

          ir[i] = pridx[i];
        }
    }
  else
    {
      mxDouble *pr = static_cast<mxDouble *> (retval->get_data ());
      mxDouble *pi = static_cast<mxDouble *> (retval->get_imag_data ());

      for (mwIndex i = 0; i < nz; i++)
        {
          pr[i] = pdata[i].real ();
          pi[i] = pdata[i].imag ();

          ir[i] = pridx[i];
        }
    }

  mwIndex *jc = retval->get_jc ();

  const octave_idx_type *pcidx = matrix.cidx ();

  for (mwIndex i = 0; i < nc + 1; i++)
    jc[i] = pcidx[i];

  return retval;
}

octave_value
octave_sparse_complex_matrix::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    // Mappers handled specially.
    case umap_real:
      return ::real (matrix);
    case umap_imag:
      return ::imag (matrix);

#define ARRAY_METHOD_MAPPER(UMAP, FCN)        \
    case umap_ ## UMAP:                       \
      return octave_value (matrix.FCN ())

      ARRAY_METHOD_MAPPER (abs, abs);

#define ARRAY_MAPPER(UMAP, TYPE, FCN)                 \
    case umap_ ## UMAP:                               \
      return octave_value (matrix.map<TYPE> (FCN))

      ARRAY_MAPPER (acos, Complex, octave::math::acos);
      ARRAY_MAPPER (acosh, Complex, octave::math::acosh);
      ARRAY_MAPPER (angle, double, std::arg);
      ARRAY_MAPPER (arg, double, std::arg);
      ARRAY_MAPPER (asin, Complex, octave::math::asin);
      ARRAY_MAPPER (asinh, Complex, octave::math::asinh);
      ARRAY_MAPPER (atan, Complex, octave::math::atan);
      ARRAY_MAPPER (atanh, Complex, octave::math::atanh);
      ARRAY_MAPPER (erf, Complex, octave::math::erf);
      ARRAY_MAPPER (erfc, Complex, octave::math::erfc);
      ARRAY_MAPPER (erfcx, Complex, octave::math::erfcx);
      ARRAY_MAPPER (erfi, Complex, octave::math::erfi);
      ARRAY_MAPPER (dawson, Complex, octave::math::dawson);
      ARRAY_MAPPER (ceil, Complex, octave::math::ceil);
      ARRAY_MAPPER (conj, Complex, std::conj<double>);
      ARRAY_MAPPER (cos, Complex, std::cos);
      ARRAY_MAPPER (cosh, Complex, std::cosh);
      ARRAY_MAPPER (exp, Complex, std::exp);
      ARRAY_MAPPER (expm1, Complex, octave::math::expm1);
      ARRAY_MAPPER (fix, Complex, octave::math::fix);
      ARRAY_MAPPER (floor, Complex, octave::math::floor);
      ARRAY_MAPPER (log, Complex, std::log);
      ARRAY_MAPPER (log2, Complex, octave::math::log2);
      ARRAY_MAPPER (log10, Complex, std::log10);
      ARRAY_MAPPER (log1p, Complex, octave::math::log1p);
      ARRAY_MAPPER (round, Complex, octave::math::round);
      ARRAY_MAPPER (roundb, Complex, octave::math::roundb);
      ARRAY_MAPPER (signum, Complex, octave::math::signum);
      ARRAY_MAPPER (sin, Complex, std::sin);
      ARRAY_MAPPER (sinh, Complex, std::sinh);
      ARRAY_MAPPER (sqrt, Complex, std::sqrt);
      ARRAY_MAPPER (tan, Complex, std::tan);
      ARRAY_MAPPER (tanh, Complex, std::tanh);
      ARRAY_MAPPER (isnan, bool, octave::math::isnan);
      ARRAY_MAPPER (isna, bool, octave::math::isna);
      ARRAY_MAPPER (isinf, bool, octave::math::isinf);
      ARRAY_MAPPER (isfinite, bool, octave::math::isfinite);

    default: // Attempt to go via dense matrix.
      return octave_base_sparse<SparseComplexMatrix>::map (umap);
    }
}
