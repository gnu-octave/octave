/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-ieee.h"

#include "oct-obj.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-complex.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "gripes.h"
#include "pr-output.h"

#include "ls-oct-ascii.h"
#include "ls-hdf5.h"

template class octave_base_scalar<Complex>;

DEFINE_OCTAVE_ALLOCATOR (octave_complex);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex,
				     "complex scalar", "double");

octave_base_value *
octave_complex::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  double im = std::imag (scalar);

  if (im == 0.0 && ! lo_ieee_signbit (im))
    retval = new octave_scalar (real (scalar));

  return retval;
}

octave_value
octave_complex::do_index_op (const octave_value_list& idx, int resize_ok)
{
  octave_value retval;

  if (idx.valid_scalar_indices ())
    retval = scalar;
  else
    {
      // FIXME -- this doesn't solve the problem of
      //
      //   a = i; a([1,1], [1,1], [1,1])
      //
      // and similar constructions.  Hmm...

      // FIXME -- using this constructor avoids narrowing the
      // 1x1 matrix back to a scalar value.  Need a better solution
      // to this problem.

      octave_value tmp (new octave_complex_matrix (complex_matrix_value ()));

      retval = tmp.do_index_op (idx, resize_ok);
    }

  return retval;
}

double
octave_complex::double_value (bool force_conversion) const
{
  double retval = lo_ieee_nan_value ();

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex scalar", "real scalar");

  retval = std::real (scalar);

  return retval;
}

Matrix
octave_complex::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex scalar", "real matrix");

  retval = Matrix (1, 1, std::real (scalar));

  return retval;
}

NDArray
octave_complex::array_value (bool force_conversion) const
{
  NDArray retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex scalar", "real matrix");

  retval = NDArray (dim_vector (1, 1), std::real (scalar));

  return retval;
}

Complex
octave_complex::complex_value (bool) const
{
  return scalar;
}


ComplexMatrix
octave_complex::complex_matrix_value (bool) const
{
  return ComplexMatrix (1, 1, scalar);
}

ComplexNDArray
octave_complex::complex_array_value (bool /* force_conversion */) const
{
  return ComplexNDArray (dim_vector (1, 1), scalar);
}

octave_value 
octave_complex::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      ComplexNDArray retval (dv, ComplexNDArray::resize_fill_value ());

      if (dv.numel ())
	retval(0) = scalar;

      return retval;
    }
  else
    {
      ComplexNDArray retval (dv);

      if (dv.numel ())
	retval(0) = scalar;

      return retval;
    }
}

bool 
octave_complex::save_ascii (std::ostream& os, bool& infnan_warned, 
			    bool strip_nan_and_inf)
{
  Complex c = complex_value ();

  if (strip_nan_and_inf)
    {
      if (xisnan (c))
	{
	  error ("only value to plot is NaN");
	  return false;
	}
      else
	{
	  double re = real (c);
	  double im = imag (c);

	  re = xisinf (re) ? (re > 0 ? OCT_RBV : -OCT_RBV) : re;
	  im = xisinf (im) ? (im > 0 ? OCT_RBV : -OCT_RBV) : im;

	  c = Complex (re, im);

	  octave_write_complex (os, c);
	  os << "\n";
	}
    }
  else
    {
      if (! infnan_warned && (xisnan (c) || xisinf (c)))
	{
	  warning ("save: Inf or NaN values may not be reloadable");
	  infnan_warned = true;
	}
      
      octave_write_complex (os, c);
      os << "\n";
    }

  return true;
}

bool 
octave_complex::load_ascii (std::istream& is)
{
  scalar = octave_read_complex (is);

  if (!is) 
    {
      error ("load: failed to load complex scalar constant");
      return false;
    }

  return true;
}


bool 
octave_complex::save_binary (std::ostream& os, bool& /* save_as_floats */)
{
  char tmp = static_cast<char> (LS_DOUBLE);
  os.write (reinterpret_cast<char *> (&tmp), 1);
  Complex ctmp = complex_value ();
  os.write (reinterpret_cast<char *> (&ctmp), 16);

  return true;
}

bool 
octave_complex::load_binary (std::istream& is, bool swap,
			     oct_mach_info::float_format fmt)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;

  Complex ctmp;
  read_doubles (is, reinterpret_cast<double *> (&ctmp),
		static_cast<save_type> (tmp), 2, swap, fmt);
  if (error_state || ! is)
    return false;

  scalar = ctmp;
  return true;
}

#if defined (HAVE_HDF5)

bool
octave_complex::save_hdf5 (hid_t loc_id, const char *name,
			   bool /* save_as_floats */)
{
  hsize_t dimens[3];
  hid_t space_hid = -1, type_hid = -1, data_hid = -1;
  bool retval = true;

  space_hid = H5Screate_simple (0, dimens, 0);
  if (space_hid < 0)
    return false;

  type_hid = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
  if (type_hid < 0) 
    {
      H5Sclose (space_hid);
      return false;
    }

  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid, H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      return false;
    }

  Complex tmp = complex_value ();
  retval = H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL, H5P_DEFAULT, 
		     &tmp) >= 0;

  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

  return retval;
}

bool
octave_complex::load_hdf5 (hid_t loc_id, const char *name,
			   bool /* have_h5giterate_bug */)
{
  bool retval = false;
  hid_t data_hid = H5Dopen (loc_id, name);
  hid_t type_hid = H5Dget_type (data_hid);

  hid_t complex_type = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);

  if (! hdf5_types_compatible (type_hid, complex_type))
    {
      H5Tclose (complex_type);
      H5Dclose (data_hid);
      return false;
    }

  hid_t space_id = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank != 0) 
    {
      H5Tclose (complex_type);
      H5Sclose (space_id);
      H5Dclose (data_hid);
      return false;
    }

  // complex scalar:
  Complex ctmp;
  if (H5Dread (data_hid, complex_type, H5S_ALL, H5S_ALL, H5P_DEFAULT,
	       &ctmp) >= 0)
    {
      retval = true;
      scalar = ctmp;
    }

  H5Tclose (complex_type);
  H5Sclose (space_id);
  H5Dclose (data_hid);

  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
