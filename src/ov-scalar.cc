/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2006,
              2007 John W. Eaton

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

#include <iostream>

#include "data-conv.h"
#include "mach-info.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "defun.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-stream.h"
#include "ov-scalar.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "pr-output.h"
#include "xdiv.h"
#include "xpow.h"

#include "ls-oct-ascii.h"
#include "ls-hdf5.h"

template class octave_base_scalar<double>;

DEFINE_OCTAVE_ALLOCATOR (octave_scalar);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_scalar, "scalar", "double");

octave_value
octave_scalar::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  octave_value retval;

  if (idx.valid_scalar_indices ())
    retval = scalar;
  else
    {
      // FIXME -- this doesn't solve the problem of
      //
      //   a = 1; a([1,1], [1,1], [1,1])
      //
      // and similar constructions.  Hmm...

      // FIXME -- using this constructor avoids narrowing the
      // 1x1 matrix back to a scalar value.  Need a better solution
      // to this problem.

      octave_value tmp (new octave_matrix (matrix_value ()));

      retval = tmp.do_index_op (idx, resize_ok);
    }

  return retval;
}

std::streamoff
octave_scalar::streamoff_value (void) const
{
  std::streamoff retval (-1);

  if (D_NINT (scalar) == scalar)
    retval = std::streamoff (static_cast<long> (scalar));
  else
    error ("conversion to streamoff value failed");

  return retval;
}

streamoff_array
octave_scalar::streamoff_array_value (void) const
{
  streamoff_array retval;

  std::streamoff soff = streamoff_value ();

  if (! error_state)
    retval = streamoff_array (dim_vector (1, 1), soff);

  return retval;
}

octave_value 
octave_scalar::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      NDArray retval (dv, NDArray::resize_fill_value());

      if (dv.numel ())
	retval(0) = scalar;

      return retval;
    }
  else
    {
      NDArray retval (dv);

      if (dv.numel ())
	retval(0) = scalar;

      return retval;
    }
}

octave_value
octave_scalar::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;

  if (xisnan (scalar))
    ::error ("invalid conversion from NaN to character");
  else
    {
      int ival = NINT (scalar);

      if (ival < 0 || ival > UCHAR_MAX)
	{
	  // FIXME -- is there something better we could do?

	  ival = 0;

	  ::warning ("range error for conversion to character value");
	}

      retval = octave_value (std::string (1, static_cast<char> (ival)), type);
    }

  return retval;
}

bool 
octave_scalar::save_ascii (std::ostream& os)
{
  double d = double_value ();

  octave_write_double (os, d);

  os << "\n";

  return true;
}

bool 
octave_scalar::load_ascii (std::istream& is)
{
  scalar = octave_read_double (is);
  if (!is)
    {
      error ("load: failed to load scalar constant");
      return false;
    }

  return true;
}

bool 
octave_scalar::save_binary (std::ostream& os, bool& /* save_as_floats */)
{
  char tmp = LS_DOUBLE;
  os.write (reinterpret_cast<char *> (&tmp), 1);
  double dtmp = double_value ();
  os.write (reinterpret_cast<char *> (&dtmp), 8);

  return true;
}

bool 
octave_scalar::load_binary (std::istream& is, bool swap,
			    oct_mach_info::float_format fmt)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;

  double dtmp;
  read_doubles (is, &dtmp, static_cast<save_type> (tmp), 1, swap, fmt);
  if (error_state || ! is)
    return false;

  scalar = dtmp;
  return true;
}

#if defined (HAVE_HDF5)

bool
octave_scalar::save_hdf5 (hid_t loc_id, const char *name,
			  bool /* save_as_floats */)
{
  hsize_t dimens[3];
  hid_t space_hid = -1, data_hid = -1;
  bool retval = true;

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

  return retval;
}

bool
octave_scalar::load_hdf5 (hid_t loc_id, const char *name,
			  bool /* have_h5giterate_bug */)
{
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

  return true;
}

#endif

mxArray *
octave_scalar::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxDOUBLE_CLASS, 1, 1, mxREAL);

  double *pr = static_cast<double *> (retval->get_data ());

  pr[0] = scalar;

  return retval;
}

#define SCALAR_MAPPER(MAP, FCN) \
  octave_value \
  octave_scalar::MAP (void) const \
  { \
    return octave_value (FCN (scalar)); \
  }

#define CD_SCALAR_MAPPER(MAP, RFCN, CFCN, L1, L2) \
  octave_value \
  octave_scalar::MAP (void) const \
  { \
    return (scalar < L1 || scalar > L2 \
            ? octave_value (CFCN (Complex (scalar))) \
	    : octave_value (RFCN (scalar))); \
  }

static double
xconj (double x)
{
  return x;
}

SCALAR_MAPPER (erf, ::erf)
SCALAR_MAPPER (erfc, ::erfc)
SCALAR_MAPPER (gamma, xgamma)
CD_SCALAR_MAPPER (lgamma, xlgamma, xlgamma, 0.0, octave_Inf)
SCALAR_MAPPER (abs, ::fabs)
SCALAR_MAPPER (acos, ::acos)
CD_SCALAR_MAPPER (acosh, ::acosh, ::acosh, 1.0, octave_Inf)
SCALAR_MAPPER (angle, ::arg)
SCALAR_MAPPER (arg, ::arg)
CD_SCALAR_MAPPER (asin, ::asin, ::asin, -1.0, 1.0)
SCALAR_MAPPER (asinh, ::asinh)
SCALAR_MAPPER (atan, ::atan)
CD_SCALAR_MAPPER (atanh, ::atanh, ::atanh, -1.0, 1.0)
SCALAR_MAPPER (ceil, ::ceil)
SCALAR_MAPPER (conj, xconj)
SCALAR_MAPPER (cos, ::cos)
SCALAR_MAPPER (cosh, ::cosh)
SCALAR_MAPPER (exp, ::exp)
SCALAR_MAPPER (fix, ::fix)
SCALAR_MAPPER (floor, ::floor)
SCALAR_MAPPER (imag, ::imag)
CD_SCALAR_MAPPER (log, ::log, std::log, 0.0, octave_Inf)
CD_SCALAR_MAPPER (log10, ::log10, std::log10, 0.0, octave_Inf)
SCALAR_MAPPER (real, ::real)
SCALAR_MAPPER (round, xround)
SCALAR_MAPPER (signum, ::signum)
SCALAR_MAPPER (sin, ::sin)
SCALAR_MAPPER (sinh, ::sinh)
CD_SCALAR_MAPPER (sqrt, ::sqrt, std::sqrt, 0.0, octave_Inf)
SCALAR_MAPPER (tan, ::tan)
SCALAR_MAPPER (tanh, ::tanh)
SCALAR_MAPPER (finite, xfinite)
SCALAR_MAPPER (isinf, xisinf)
SCALAR_MAPPER (isna, octave_is_NA)
SCALAR_MAPPER (isnan, xisnan)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
