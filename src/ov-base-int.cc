/*

Copyright (C) 2004 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <climits>

#include <iostream>
#include <vector>

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "quit.h"

#include "defun.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-base-int.h"
#include "ov-int-traits.h"
#include "pr-output.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

template <class T>
octave_value *
octave_base_int_matrix<T>::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  if (matrix.nelem () == 1)
    retval = new typename octave_value_int_traits<T>::scalar_type (matrix (0));

  return retval;
}

template <class T>
bool
octave_base_int_matrix<T>::save_ascii (std::ostream& os, bool& infnan_warned,
				       bool /* strip_nan_and_inf */)
{
  infnan_warned = false;

  dim_vector d = dims ();

  os << "# ndims: " << d.length () << "\n";

  for (int i = 0; i < d.length (); i++)
    os << " " << d (i);

  os << "\n" << matrix;

  return true;
}

template <class T>
bool 
octave_base_int_matrix<T>::load_ascii (std::istream& is)
{
  int mdims = 0;
  bool success = true;

  if (extract_keyword (is, "ndims", mdims, true))
    {
      if (mdims >= 0)
	{
	  dim_vector dv;
	  dv.resize (mdims);

	  for (int i = 0; i < mdims; i++)
	    is >> dv(i);

	  T tmp(dv);

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
    }
  else
    error ("load: failed to extract number of dimensions");

  return success;
}

template <class T>
bool 
octave_base_int_matrix<T>::save_binary (std::ostream& os, bool& save_as_floats)
{
#if 0

  dim_vector d = dims ();
  if (d.length() < 1)
    return false;

  // Use negative value for ndims to differentiate with old format!!
  FOUR_BYTE_INT tmp = - d.length();
  os.write (X_CAST (char *, &tmp), 4);
  for (int i=0; i < d.length (); i++)
    {
      tmp = d(i);
      os.write (X_CAST (char *, &tmp), 4);
    }

  NDArray m = array_value ();
  save_type st = LS_DOUBLE;
  if (save_as_floats)
    {
      if (m.too_large_for_float ())
	{
	  warning ("save: some values too large to save as floats --");
	  warning ("save: saving as doubles instead");
	}
      else
	st = LS_FLOAT;
    }
  else if (d.numel () > 8192) // XXX FIXME XXX -- make this configurable.
    {
      double max_val, min_val;
      if (m.all_integers (max_val, min_val))
	st = get_save_type (max_val, min_val);
    }

  const double *mtmp = m.data ();
  write_doubles (os, mtmp, st, d.numel ());

#endif

  return true;
}

template <class T>
bool
octave_base_int_matrix<T>::load_binary (std::istream& is, bool swap,
					oct_mach_info::float_format fmt)
{
#if 0

  char tmp;
  FOUR_BYTE_INT mdims;
  if (! is.read (X_CAST (char *, &mdims), 4))
    return false;
  if (swap)
    swap_4_bytes (X_CAST (char *, &mdims));
  if (mdims < 0)
    {
      mdims = - mdims;
      FOUR_BYTE_INT di;
      dim_vector dv;
      dv.resize (mdims);

      for (int i = 0; i < mdims; i++)
	{
	  if (! is.read (X_CAST (char *, &di), 4))
	    return false;
	  if (swap)
	    swap_4_bytes (X_CAST (char *, &di));
	  dv(i) = di;
	}

      if (! is.read (X_CAST (char *, &tmp), 1))
	return false;

      NDArray m(dv);
      double *re = m.fortran_vec ();
      read_doubles (is, re, X_CAST (save_type, tmp), dv.numel (), swap, fmt);
      if (error_state || ! is)
	return false;
      matrix = m;
    }
  else
    {
      FOUR_BYTE_INT nr, nc;
      nr = mdims;
      if (! is.read (X_CAST (char *, &nc), 4))
	return false;
      if (swap)
	swap_4_bytes (X_CAST (char *, &nc));
      if (! is.read (X_CAST (char *, &tmp), 1))
	return false;
      Matrix m (nr, nc);
      double *re = m.fortran_vec ();
      int len = nr * nc;
      read_doubles (is, re, X_CAST (save_type, tmp), len, swap, fmt);
      if (error_state || ! is)
	return false;
      matrix = m;
    }

#endif

  return true;
}

#if defined (HAVE_HDF5)

template <class T>
bool
octave_base_int_matrix<T>::save_hdf5 (hid_t loc_id, const char *name,
				      bool save_as_floats)
{
  bool retval = true;

#if 0

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.length ();
  hid_t space_hid = -1, data_hid = -1;
  NDArray m = array_value ();

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (int i = 0; i < rank; i++)
    hdims[i] = dv (rank-i-1);
 
  space_hid = H5Screate_simple (rank, hdims, 0);

  if (space_hid < 0) return false;

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
  
  data_hid = H5Dcreate (loc_id, name, save_type_hid, space_hid, 
			H5P_DEFAULT);
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  double *mtmp = m.fortran_vec ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, mtmp) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#endif

  return retval;
}

template <class T>
bool
octave_base_int_matrix<T>::load_hdf5 (hid_t loc_id, const char *name,
				      bool /* have_h5giterate_bug */)
{
  bool retval = false;

#if 0

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize(dv);
  if (empty)
      return (empty > 0);

  hid_t data_hid = H5Dopen (loc_id, name);
  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);
  
  if (rank < 1)
    {
      H5Sclose (space_id);
      H5Dclose (data_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_id, hdims, maxdims);

  // Octave uses column-major, while HDF5 uses row-major ordering
  if (rank == 1)
    {
      dv.resize (2);
      dv(0) = 1;
      dv(1) = hdims[0];
    }
  else
    {
      dv.resize (rank);
      for (hsize_t i = 0, j = rank - 1; i < rank; i++, j--)
	dv(j) = hdims[i];
    }

  NDArray m (dv);
  double *re = m.fortran_vec ();
  if (H5Dread (data_hid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, 
	       H5P_DEFAULT, re) >= 0) 
    {
      retval = true;
      matrix = m;
    }

  H5Sclose (space_id);
  H5Dclose (data_hid);

#endif

  return retval;
}

#endif

template <class T>
void
octave_base_int_matrix<T>::print_raw (std::ostream& os,
				      bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
   			 current_print_indent_level ());
}

template <class T>
bool
octave_base_int_scalar<T>::save_ascii (std::ostream& os, bool& infnan_warned, 
				       bool strip_nan_and_inf)
{
#if 0

  double d = double_value ();

  if (strip_nan_and_inf)
    {
      if (xisnan (d))
	{
	  error ("only value to plot is NaN");
	  return false;
	}
      else
	{
	  d = xisinf (d) ? (d > 0 ? OCT_RBV : -OCT_RBV) : d;
	  octave_write_double (os, d);
	  os << "\n";
	}
    }
  else
    {
      if (! infnan_warned && (xisnan (d) || xisinf (d)))
	{
	  warning ("save: Inf or NaN values may not be reloadable");
	  infnan_warned = true;
	}

      octave_write_double (os, d);
      os << "\n";
    }

#endif

  return true;
}

template <class T>
bool 
octave_base_int_scalar<T>::load_ascii (std::istream& is)
{
#if 0

  scalar = octave_read_double (is);
  if (!is)
    {
      error ("load: failed to load scalar constant");
      return false;
    }

#endif

  return true;
}

template <class T>
bool 
octave_base_int_scalar<T>::save_binary (std::ostream& os,
					bool& /* save_as_floats */)
{
#if 0

  char tmp = (char) LS_DOUBLE;
  os.write (X_CAST (char *, &tmp), 1);
  double dtmp = double_value ();
  os.write (X_CAST (char *, &dtmp), 8);

#endif

  return true;
}

template <class T>
bool 
octave_base_int_scalar<T>::load_binary (std::istream& is, bool swap,
					oct_mach_info::float_format fmt)
{
#if 0

  char tmp;
  if (! is.read (X_CAST (char *, &tmp), 1))
    return false;

  double dtmp;
  read_doubles (is, &dtmp, X_CAST (save_type, tmp), 1, swap, fmt);
  if (error_state || ! is)
    return false;

  scalar = dtmp;

#endif

  return true;
}

#if defined (HAVE_HDF5)
template <class T>
bool
octave_base_int_scalar<T>::save_hdf5 (hid_t loc_id, const char *name,
				      bool /* save_as_floats */)
{
  bool retval = true;

#if 0

  hsize_t dimens[3];
  hid_t space_hid = -1, data_hid = -1;

  space_hid = H5Screate_simple (0, dimens, 0);
  if (space_hid < 0) return false;

  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_DOUBLE, space_hid, 
			H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      return false;
    }

  double tmp = double_value ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, &tmp) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#endif

  return retval;
}

template <class T>
bool
octave_base_int_scalar<T>::load_hdf5 (hid_t loc_id, const char *name,
				      bool /* have_h5giterate_bug */)
{
#if 0

  hid_t data_hid = H5Dopen (loc_id, name);
  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank != 0)
    { 
      H5Dclose (data_hid);
      return false;
    }

  double dtmp;
  if (H5Dread (data_hid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, 
	       H5P_DEFAULT, &dtmp) < 0)
    { 
      H5Dclose (data_hid);
      return false;
    }

  scalar = dtmp;

  H5Dclose (data_hid);
#endif

  return true;
}
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
