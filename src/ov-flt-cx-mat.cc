/*

Copyright (C) 1996, 1997, 1998, 2000, 2001, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

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
#include <vector>

#include "data-conv.h"
#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"
#include "mx-base.h"
#include "mach-info.h"

#include "gripes.h"
#include "oct-obj.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "pr-output.h"
#include "ops.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

template class octave_base_matrix<FloatComplexNDArray>;

DEFINE_OCTAVE_ALLOCATOR (octave_float_complex_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_float_complex_matrix,
				     "float complex matrix", "single");

octave_base_value *
octave_float_complex_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.ndims () == 2)
    {
      FloatComplexMatrix cm = matrix.matrix_value ();

      octave_idx_type nr = cm.rows ();
      octave_idx_type nc = cm.cols ();

      if (nr == 1 && nc == 1)
	{
	  FloatComplex c = matrix (0, 0);

	  float im = std::imag (c);

	  if (im == 0.0 && ! lo_ieee_signbit (im))
	    retval = new octave_float_scalar (std::real (c));
	  else
	    retval = new octave_float_complex (c);
	}
      else if (nr == 0 || nc == 0)
	retval = new octave_float_matrix (FloatMatrix (nr, nc));
      else if (cm.all_elements_are_real ())
	retval = new octave_float_matrix (::real (cm));
    }
  else if (matrix.all_elements_are_real ())
    retval = new octave_float_matrix (::real (matrix));

  return retval;
}

void
octave_float_complex_matrix::assign (const octave_value_list& idx,
			       const FloatComplexNDArray& rhs)
{
  octave_base_matrix<FloatComplexNDArray>::assign (idx, rhs);
}

void
octave_float_complex_matrix::assign (const octave_value_list& idx,
			       const FloatNDArray& rhs)
{
  octave_idx_type len = idx.length ();

  for (octave_idx_type i = 0; i < len; i++)
    matrix.set_index (idx(i).index_vector ());

  ::assign (matrix, rhs);
}

bool
octave_float_complex_matrix::valid_as_scalar_index (void) const
{
  // FIXME
  return false;
}

double
octave_float_complex_matrix::double_value (bool force_conversion) const
{
  double retval = lo_ieee_nan_value ();

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex matrix", "real scalar");

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "complex matrix", "real scalar");

      retval = std::real (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("complex matrix", "real scalar");

  return retval;
}

float
octave_float_complex_matrix::float_value (bool force_conversion) const
{
  float retval = lo_ieee_float_nan_value ();

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex matrix", "real scalar");

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "complex matrix", "real scalar");

      retval = std::real (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("complex matrix", "real scalar");

  return retval;
}

Matrix
octave_float_complex_matrix::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex matrix", "real matrix");

  retval = ::real (matrix.matrix_value ());

  return retval;
}

FloatMatrix
octave_float_complex_matrix::float_matrix_value (bool force_conversion) const
{
  FloatMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex matrix", "real matrix");

  retval = ::real (matrix.matrix_value ());

  return retval;
}

Complex
octave_float_complex_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "complex matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("complex matrix", "complex scalar");

  return retval;
}

FloatComplex
octave_float_complex_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "complex matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("complex matrix", "complex scalar");

  return retval;
}

ComplexMatrix
octave_float_complex_matrix::complex_matrix_value (bool) const
{
  return matrix.matrix_value ();
}

FloatComplexMatrix
octave_float_complex_matrix::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (matrix.matrix_value ());
}

charNDArray
octave_float_complex_matrix::char_array_value (bool frc_str_conv) const
{
  charNDArray retval;

  if (! frc_str_conv)
    gripe_implicit_conversion ("Octave:num-to-str",
			       "complex matrix", "string");
  else
    {
      retval = charNDArray (dims ());
      octave_idx_type nel = numel ();
  
      for (octave_idx_type i = 0; i < nel; i++)
	retval.elem (i) = static_cast<char>(std::real (matrix.elem (i)));
    }

  return retval;
}  

FloatComplexNDArray 
octave_float_complex_matrix::float_complex_array_value (bool) const 
{ 
  return FloatComplexNDArray (matrix);
}

SparseMatrix
octave_float_complex_matrix::sparse_matrix_value (bool force_conversion) const
{
  SparseMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex matrix", "real matrix");

  retval = SparseMatrix (::real (matrix.matrix_value ()));

  return retval;
}

SparseComplexMatrix
octave_float_complex_matrix::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (matrix.matrix_value ());
}

bool 
octave_float_complex_matrix::save_ascii (std::ostream& os)
{
  dim_vector d = dims ();
  if (d.length () > 2)
    {
      FloatComplexNDArray tmp = complex_array_value ();

      os << "# ndims: " << d.length () << "\n";

      for (int i = 0; i < d.length (); i++)
	os << " " << d (i);

      os << "\n" << tmp;
    }
  else
    {
      // Keep this case, rather than use generic code above for backward 
      // compatiability. Makes load_ascii much more complex!!
      os << "# rows: " << rows () << "\n"
	 << "# columns: " << columns () << "\n";

      os << complex_matrix_value ();
    }

  return true;
}

bool 
octave_float_complex_matrix::load_ascii (std::istream& is)
{
  bool success = true;

  string_vector keywords(2);

  keywords[0] = "ndims";
  keywords[1] = "rows";

  std::string kw;
  octave_idx_type val = 0;

  if (extract_keyword (is, keywords, kw, val, true))
    {
      if (kw == "ndims")
	{
	  int mdims = static_cast<int> (val);

	  if (mdims >= 0)
	    {
	      dim_vector dv;
	      dv.resize (mdims);

	      for (int i = 0; i < mdims; i++)
		is >> dv(i);

	      if (is)
		{
		  FloatComplexNDArray tmp(dv);

		  if (tmp.is_empty ())
		    matrix = tmp;
		  else
		    {
		      is >> tmp;

		      if (is)
			matrix = tmp;
		      else
			{
			  error ("load: failed to load matrix constant");
			  success = false;
			}
		    }
		}
	      else
		{
		  error ("load: failed to read dimensions");
		  success = false;
		}
	    }
	  else
	    {
	      error ("load: failed to extract number of dimensions");
	      success = false;
	    }
	}
      else if (kw == "rows")
	{
	  octave_idx_type nr = val;
	  octave_idx_type nc = 0;

	  if (nr >= 0 && extract_keyword (is, "columns", nc) && nc >= 0)
	    {
	      if (nr > 0 && nc > 0)
		{
		  FloatComplexMatrix tmp (nr, nc);
		  is >> tmp;
		  if (is)
		    matrix = tmp;
		  else
		    {
		      error ("load: failed to load matrix constant");
		      success = false;
		    }
		}
	      else if (nr == 0 || nc == 0)
		matrix = FloatComplexMatrix (nr, nc);
	      else
		panic_impossible ();
	    }
	  else
	    {
	      error ("load: failed to extract number of rows and columns");
	      success = false;
	    }
	}
      else
	panic_impossible ();
    }
  else
    {
      error ("load: failed to extract number of rows and columns");
      success = false;
    }

  return success;
}

bool 
octave_float_complex_matrix::save_binary (std::ostream& os, bool&)
{
  dim_vector d = dims ();
  if (d.length() < 1)
    return false;

  // Use negative value for ndims to differentiate with old format!!
  int32_t tmp = - d.length();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  for (int i = 0; i < d.length (); i++)
    {
      tmp = d(i);
      os.write (reinterpret_cast<char *> (&tmp), 4);
    }

  FloatComplexNDArray m = complex_array_value ();
  save_type st = LS_FLOAT;
  if (d.numel () > 4096) // FIXME -- make this configurable.
    {
      float max_val, min_val;
      if (m.all_integers (max_val, min_val))
	st = get_save_type (max_val, min_val);
    }

  const FloatComplex *mtmp = m.data ();
  write_floats (os, reinterpret_cast<const float *> (mtmp), st, 2 * d.numel ());

  return true;
}

bool 
octave_float_complex_matrix::load_binary (std::istream& is, bool swap,
				 oct_mach_info::float_format fmt)
{
  char tmp;
  int32_t mdims;
  if (! is.read (reinterpret_cast<char *> (&mdims), 4))
    return false;
  if (swap)
    swap_bytes<4> (&mdims);
  if (mdims < 0)
    {
      mdims = - mdims;
      int32_t di;
      dim_vector dv;
      dv.resize (mdims);

      for (int i = 0; i < mdims; i++)
	{
	  if (! is.read (reinterpret_cast<char *> (&di), 4))
	    return false;
	  if (swap)
	    swap_bytes<4> (&di);
	  dv(i) = di;
	}

      // Convert an array with a single dimension to be a row vector.
      // Octave should never write files like this, other software
      // might.

      if (mdims == 1)
	{
	  mdims = 2;
	  dv.resize (mdims);
	  dv(1) = dv(0);
	  dv(0) = 1;
	}

      if (! is.read (reinterpret_cast<char *> (&tmp), 1))
	return false;

      FloatComplexNDArray m(dv);
      FloatComplex *im = m.fortran_vec ();
      read_floats (is, reinterpret_cast<float *> (im),
		    static_cast<save_type> (tmp), 2 * dv.numel (), swap, fmt);
      if (error_state || ! is)
	return false;
      matrix = m;
    }
  else
    {
      int32_t nr, nc;
      nr = mdims;
      if (! is.read (reinterpret_cast<char *> (&nc), 4))
	return false;
      if (swap)
	swap_bytes<4> (&nc);
      if (! is.read (reinterpret_cast<char *> (&tmp), 1))
	return false;
      FloatComplexMatrix m (nr, nc);
      FloatComplex *im = m.fortran_vec ();
      octave_idx_type len = nr * nc;
      read_floats (is, reinterpret_cast<float *> (im),
		    static_cast<save_type> (tmp), 2*len, swap, fmt);
      if (error_state || ! is)
	return false;
      matrix = m;
    }
  return true;
}

#if defined (HAVE_HDF5)

bool
octave_float_complex_matrix::save_hdf5 (hid_t loc_id, const char *name, bool)
{
  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.length ();
  hid_t space_hid = -1, data_hid = -1, type_hid = -1;
  bool retval = true;
  FloatComplexNDArray m = complex_array_value ();

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (int i = 0; i < rank; i++)
    hdims[i] = dv (rank-i-1);
 
  space_hid = H5Screate_simple (rank, hdims, 0);
  if (space_hid < 0) return false;

  hid_t save_type_hid = H5T_NATIVE_FLOAT;

#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
  // hdf5 currently doesn't support float/integer conversions
  else
    {
      float max_val, min_val;
      
      if (m.all_integers (max_val, min_val))
	save_type_hid
	  = save_type_to_hdf5 (get_save_type (max_val, min_val));
    }
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

  type_hid = hdf5_make_complex_type (save_type_hid);
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

  hid_t complex_type_hid = hdf5_make_complex_type (H5T_NATIVE_FLOAT);
  if (complex_type_hid < 0) retval = false;

  if (retval)
    {
      FloatComplex *mtmp = m.fortran_vec ();
      if (H5Dwrite (data_hid, complex_type_hid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    mtmp) < 0)
	{
	  H5Tclose (complex_type_hid);
	  retval = false;
	}
    }

  H5Tclose (complex_type_hid);
  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

  return retval;
}

bool 
octave_float_complex_matrix::load_hdf5 (hid_t loc_id, const char *name,
				  bool /* have_h5giterate_bug */)
{
  bool retval = false;

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize(dv);
  if (empty)
      return (empty > 0);

  hid_t data_hid = H5Dopen (loc_id, name);
  hid_t type_hid = H5Dget_type (data_hid);

  hid_t complex_type = hdf5_make_complex_type (H5T_NATIVE_FLOAT);

  if (! hdf5_types_compatible (type_hid, complex_type))
    {
      H5Tclose (complex_type);
      H5Dclose (data_hid);
      return false;
    }

  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);
  
  if (rank < 1)
    {
      H5Tclose (complex_type);
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

  FloatComplexNDArray m (dv);
  FloatComplex *reim = m.fortran_vec ();
  if (H5Dread (data_hid, complex_type, H5S_ALL, H5S_ALL, H5P_DEFAULT,
	       reim) >= 0) 
    {
      retval = true;
      matrix = m;
    }

  H5Tclose (complex_type);
  H5Sclose (space_id);
  H5Dclose (data_hid);

  return retval;
}

#endif

void
octave_float_complex_matrix::print_raw (std::ostream& os,
				  bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level ());
}

mxArray *
octave_float_complex_matrix::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxSINGLE_CLASS, dims (), mxCOMPLEX);

  float *pr = static_cast<float *> (retval->get_data ());
  float *pi = static_cast<float *> (retval->get_imag_data ());

  mwSize nel = numel ();

  const FloatComplex *p = matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    {
      pr[i] = std::real (p[i]);
      pi[i] = std::imag (p[i]);
    }

  return retval;
}

static float
xabs (const FloatComplex& x)
{
  return (xisinf (x.real ()) || xisinf (x.imag ())) ? octave_Inf : abs (x);
}

static float
ximag (const FloatComplex& x)
{
  return x.imag ();
}

static float
xreal (const FloatComplex& x)
{
  return x.real ();
}

static bool
any_element_less_than (const FloatNDArray& a, float val)
{
  octave_idx_type len = a.length ();
  const float *m = a.fortran_vec ();

  for (octave_idx_type i = 0; i < len; i++)
    {
      OCTAVE_QUIT;

      if (m[i] < val)
	return true;
    }

  return false;
}

static bool
any_element_greater_than (const FloatNDArray& a, float val)
{
  octave_idx_type len = a.length ();
  const float *m = a.fortran_vec ();

  for (octave_idx_type i = 0; i < len; i++)
    {
      OCTAVE_QUIT;

      if (m[i] > val)
	return true;
    }

  return false;
}

#define ARRAY_MAPPER(MAP, AMAP, FCN) \
  octave_value \
  octave_float_complex_matrix::MAP (void) const \
  { \
    static AMAP cmap = FCN; \
    return matrix.map (cmap); \
  }

#define DARRAY_MAPPER(MAP, AMAP, FCN) \
  octave_value \
  octave_float_complex_matrix::MAP (void) const \
  { \
    static FloatComplexNDArray::dmapper dmap = ximag; \
    FloatNDArray m = matrix.map (dmap); \
    if (m.all_elements_are_zero ()) \
      { \
	dmap = xreal; \
	m = matrix.map (dmap); \
        static AMAP cmap = FCN; \
        return m.map (cmap); \
      } \
    else \
      { \
        error ("%s: not defined for complex arguments", #MAP); \
        return octave_value (); \
      } \
  }

#define CD_ARRAY_MAPPER(MAP, RFCN, CFCN, L1, L2) \
  octave_value \
  octave_float_complex_matrix::MAP (void) const \
  { \
    static FloatComplexNDArray::dmapper idmap = ximag; \
    NDArray m = matrix.map (idmap); \
    if (m.all_elements_are_zero ()) \
      { \
	static FloatComplexNDArray::dmapper rdmap = xreal; \
	m = matrix.map (rdmap); \
        static NDArray::dmapper dmap = RFCN; \
        static NDArray::cmapper cmap = CFCN; \
        return (any_element_less_than (m, L1) \
                ? octave_value (m.map (cmap)) \
	        : (any_element_greater_than (m, L2) \
	           ? octave_value (m.map (cmap)) \
	           : octave_value (m.map (dmap)))); \
      } \
    else \
      { \
        /*error ("%s: not defined for complex arguments", #MAP); */	\
        return octave_value (m); \
      } \
  }

DARRAY_MAPPER (erf, FloatNDArray::dmapper, ::erff)
DARRAY_MAPPER (erfc, FloatNDArray::dmapper, ::erfcf)
DARRAY_MAPPER (gamma, FloatNDArray::dmapper, xgamma)
CD_ARRAY_MAPPER (lgamma, xlgamma, xlgamma, 0.0, octave_Inf)

ARRAY_MAPPER (abs, FloatComplexNDArray::dmapper, xabs)
ARRAY_MAPPER (acos, FloatComplexNDArray::cmapper, ::acos)
ARRAY_MAPPER (acosh, FloatComplexNDArray::cmapper, ::acosh)
ARRAY_MAPPER (angle, FloatComplexNDArray::dmapper, std::arg)
ARRAY_MAPPER (arg, FloatComplexNDArray::dmapper, std::arg)
ARRAY_MAPPER (asin, FloatComplexNDArray::cmapper, ::asin)
ARRAY_MAPPER (asinh, FloatComplexNDArray::cmapper, ::asinh)
ARRAY_MAPPER (atan, FloatComplexNDArray::cmapper, ::atan)
ARRAY_MAPPER (atanh, FloatComplexNDArray::cmapper, ::atanh)
ARRAY_MAPPER (ceil, FloatComplexNDArray::cmapper, ::ceil)
ARRAY_MAPPER (conj, FloatComplexNDArray::cmapper, std::conj)
ARRAY_MAPPER (cos, FloatComplexNDArray::cmapper, std::cos)
ARRAY_MAPPER (cosh, FloatComplexNDArray::cmapper, std::cosh)
ARRAY_MAPPER (exp, FloatComplexNDArray::cmapper, std::exp)
ARRAY_MAPPER (expm1, FloatComplexNDArray::cmapper, ::expm1f)
ARRAY_MAPPER (fix, FloatComplexNDArray::cmapper, ::fix)
ARRAY_MAPPER (floor, FloatComplexNDArray::cmapper, ::floor)
ARRAY_MAPPER (imag, FloatComplexNDArray::dmapper, ximag)
ARRAY_MAPPER (log, FloatComplexNDArray::cmapper, std::log)
ARRAY_MAPPER (log2, FloatComplexNDArray::cmapper, xlog2)
ARRAY_MAPPER (log10, FloatComplexNDArray::cmapper, std::log10)
ARRAY_MAPPER (log1p, FloatComplexNDArray::cmapper, ::log1pf)
ARRAY_MAPPER (real, FloatComplexNDArray::dmapper, xreal)
ARRAY_MAPPER (round, FloatComplexNDArray::cmapper, xround)
ARRAY_MAPPER (roundb, FloatComplexNDArray::cmapper, xroundb)
ARRAY_MAPPER (signum, FloatComplexNDArray::cmapper, ::signum)
ARRAY_MAPPER (sin, FloatComplexNDArray::cmapper, std::sin)
ARRAY_MAPPER (sinh, FloatComplexNDArray::cmapper, std::sinh)
ARRAY_MAPPER (sqrt, FloatComplexNDArray::cmapper, std::sqrt)
ARRAY_MAPPER (tan, FloatComplexNDArray::cmapper, std::tan)
ARRAY_MAPPER (tanh, FloatComplexNDArray::cmapper, std::tanh)
ARRAY_MAPPER (finite, FloatComplexNDArray::bmapper, xfinite)
ARRAY_MAPPER (isinf, FloatComplexNDArray::bmapper, xisinf)
ARRAY_MAPPER (isna, FloatComplexNDArray::bmapper, octave_is_NA)
ARRAY_MAPPER (isnan, FloatComplexNDArray::bmapper, xisnan)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
