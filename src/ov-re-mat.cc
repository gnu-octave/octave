/*

Copyright (C) 1996, 1997, 1998, 2000, 2001, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton

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

#include <climits>

#include <iostream>
#include <vector>

#include "data-conv.h"
#include "lo-ieee.h"
#include "lo-utils.h"
#include "lo-specfun.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "mx-base.h"
#include "quit.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-perm.h"
#include "ov-type-conv.h"
#include "pr-output.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

#if ! defined (UCHAR_MAX)
#define UCHAR_MAX 255
#endif

template class octave_base_matrix<NDArray>;

DEFINE_OCTAVE_ALLOCATOR (octave_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_matrix, "matrix", "double");

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_matrix&);

  return new octave_float_matrix (v.float_array_value ());
}

octave_base_value::type_conv_info
octave_matrix::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info(default_numeric_demotion_function,
                                           octave_float_matrix::static_type_id ());
}

octave_base_value *
octave_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.nelem () == 1)
    retval = new octave_scalar (matrix (0));

  return retval;
}

double
octave_matrix::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "real matrix", "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("real matrix", "real scalar");

  return retval;
}

float
octave_matrix::float_value (bool) const
{
  float retval = lo_ieee_float_nan_value ();

  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "real matrix", "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("real matrix", "real scalar");

  return retval;
}

// FIXME

Matrix
octave_matrix::matrix_value (bool) const
{
  return matrix.matrix_value ();
}

FloatMatrix
octave_matrix::float_matrix_value (bool) const
{
  return FloatMatrix (matrix.matrix_value ());
}

Complex
octave_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "real matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("real matrix", "complex scalar");

  return retval;
}

FloatComplex
octave_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "real matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("real matrix", "complex scalar");

  return retval;
}

// FIXME

ComplexMatrix
octave_matrix::complex_matrix_value (bool) const
{
  return ComplexMatrix (matrix.matrix_value ());
}

FloatComplexMatrix
octave_matrix::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (matrix.matrix_value ());
}

ComplexNDArray
octave_matrix::complex_array_value (bool) const
{
  return ComplexNDArray (matrix);
}

FloatComplexNDArray
octave_matrix::float_complex_array_value (bool) const
{
  return FloatComplexNDArray (matrix);
}

boolNDArray
octave_matrix::bool_array_value (bool warn) const
{
  if (matrix.any_element_is_nan ())
    error ("invalid conversion from NaN to logical");
  else if (warn && matrix.any_element_not_one_or_zero ())
    gripe_logical_conversion ();

  return boolNDArray (matrix);
}
  
charNDArray
octave_matrix::char_array_value (bool) const
{
  charNDArray retval (dims ());

  octave_idx_type nel = numel ();
  
  for (octave_idx_type i = 0; i < nel; i++)
    retval.elem (i) = static_cast<char>(matrix.elem (i));

  return retval;
}
  
SparseMatrix 
octave_matrix::sparse_matrix_value (bool) const
{
  return SparseMatrix (matrix.matrix_value ());
}

SparseComplexMatrix 
octave_matrix::sparse_complex_matrix_value (bool) const
{
  // FIXME Need a SparseComplexMatrix (Matrix) constructor to make
  // this function more efficient. Then this should become
  // return SparseComplexMatrix (matrix.matrix_value ());
  return SparseComplexMatrix (sparse_matrix_value ());
}

octave_value
octave_matrix::diag (octave_idx_type k) const
{
  octave_value retval;
  if (k == 0 && matrix.ndims () == 2 
      && (matrix.rows () == 1 || matrix.columns () == 1))
    retval = DiagMatrix (DiagArray2<double> (matrix));
  else
    retval = octave_base_matrix<NDArray>::diag (k);

  return retval;
}

octave_value
octave_matrix::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;
  dim_vector dv = dims ();
  octave_idx_type nel = dv.numel ();

  charNDArray chm (dv);

  bool warned = false;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;

      double d = matrix (i);

      if (xisnan (d))
	{
	  ::error ("invalid conversion from NaN to character");
	  return retval;
	}
      else
	{
	  int ival = NINT (d);

	  if (ival < 0 || ival > UCHAR_MAX)
	    {
	      // FIXME -- is there something
	      // better we could do?

	      ival = 0;

	      if (! warned)
		{
		  ::warning ("range error for conversion to character value");
		  warned = true;
		}
	    }

	  chm (i) = static_cast<char> (ival);
	}
    }

  retval = octave_value (chm, true, type);

  return retval;
}

bool 
octave_matrix::save_ascii (std::ostream& os)
{
  dim_vector d = dims ();

  if (d.length () > 2)
    {
      NDArray tmp = array_value ();

      os << "# ndims: " << d.length () << "\n";

      for (int i=0; i < d.length (); i++)
	os << " " << d (i);

      os << "\n" << tmp;
    }
  else
    {
      // Keep this case, rather than use generic code above for backward 
      // compatiability. Makes load_ascii much more complex!!
      os << "# rows: " << rows () << "\n"
	 << "# columns: " << columns () << "\n";

      os << matrix_value ();
    }

  return true;
}

bool 
octave_matrix::load_ascii (std::istream& is)
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
		  NDArray tmp(dv);

                  is >> tmp;

                  if (is)
                    matrix = tmp;
                  else
                    {
                      error ("load: failed to load matrix constant");
                      success = false;
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
		  Matrix tmp (nr, nc);
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
		matrix = Matrix (nr, nc);
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
octave_matrix::save_binary (std::ostream& os, bool& save_as_floats)
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
  else if (d.numel () > 8192) // FIXME -- make this configurable.
    {
      double max_val, min_val;
      if (m.all_integers (max_val, min_val))
	st = get_save_type (max_val, min_val);
    }

  const double *mtmp = m.data ();
  write_doubles (os, mtmp, st, d.numel ());

  return true;
}

bool 
octave_matrix::load_binary (std::istream& is, bool swap,
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

      NDArray m(dv);
      double *re = m.fortran_vec ();
      read_doubles (is, re, static_cast<save_type> (tmp), dv.numel (), swap, fmt);
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
      Matrix m (nr, nc);
      double *re = m.fortran_vec ();
      octave_idx_type len = nr * nc;
      read_doubles (is, re, static_cast<save_type> (tmp), len, swap, fmt);
      if (error_state || ! is)
	return false;
      matrix = m;
    }
  return true;
}

#if defined (HAVE_HDF5)

bool
octave_matrix::save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats)
{
  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.length ();
  hid_t space_hid = -1, data_hid = -1;
  bool retval = true;
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

  return retval;
}

bool
octave_matrix::load_hdf5 (hid_t loc_id, const char *name,
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

  return retval;
}

#endif

void
octave_matrix::print_raw (std::ostream& os,
			  bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level ());
}

mxArray *
octave_matrix::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxDOUBLE_CLASS, dims (), mxREAL);

  double *pr = static_cast<double *> (retval->get_data ());

  mwSize nel = numel ();

  const double *p = matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    pr[i] = p[i];

  return retval;
}

static bool
any_element_less_than (const NDArray& a, double val)
{
  octave_idx_type len = a.length ();
  const double *m = a.fortran_vec ();

  for (octave_idx_type i = 0; i < len; i++)
    {
      OCTAVE_QUIT;

      if (m[i] < val)
	return true;
    }

  return false;
}

static bool
any_element_greater_than (const NDArray& a, double val)
{
  octave_idx_type len = a.length ();
  const double *m = a.fortran_vec ();

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
  octave_matrix::MAP (void) const \
  { \
    static AMAP dmap = FCN; \
    return matrix.map (dmap); \
  }

#define CD_ARRAY_MAPPER(MAP, RFCN, CFCN, L1, L2) \
  octave_value \
  octave_matrix::MAP (void) const \
  { \
    static NDArray::dmapper dmap = RFCN; \
    static NDArray::cmapper cmap = CFCN; \
 \
    return (any_element_less_than (matrix, L1) \
            ? octave_value (matrix.map (cmap)) \
	    : (any_element_greater_than (matrix, L2) \
	       ? octave_value (matrix.map (cmap)) \
	       : octave_value (matrix.map (dmap)))); \
  }

// The fast mappers.
octave_value
octave_matrix::abs (void) const
{
  return matrix.abs ();
}

octave_value
octave_matrix::real (void) const
{
  return matrix;
}

octave_value
octave_matrix::conj (void) const
{
  return matrix;
}

octave_value
octave_matrix::imag (void) const
{
  return NDArray (matrix.dims (), 0.0);
}

octave_value
octave_matrix::isnan (void) const
{
  return matrix.isnan ();
}

octave_value
octave_matrix::isinf (void) const
{
  return matrix.isinf ();
}

octave_value
octave_matrix::finite (void) const
{
  return matrix.isfinite ();
}

ARRAY_MAPPER (erf, NDArray::dmapper, ::erf)
ARRAY_MAPPER (erfc, NDArray::dmapper, ::erfc)
ARRAY_MAPPER (gamma, NDArray::dmapper, xgamma)
CD_ARRAY_MAPPER (lgamma, xlgamma, xlgamma, 0.0, octave_Inf)
ARRAY_MAPPER (acos, NDArray::dmapper, ::acos)
CD_ARRAY_MAPPER (acosh, ::acosh, ::acosh, 1.0, octave_Inf)
ARRAY_MAPPER (angle, NDArray::dmapper, ::arg)
ARRAY_MAPPER (arg, NDArray::dmapper, ::arg)
CD_ARRAY_MAPPER (asin, ::asin, ::asin, -1.0, 1.0)
ARRAY_MAPPER (asinh, NDArray::dmapper,::asinh)
ARRAY_MAPPER (atan, NDArray::dmapper, ::atan)
CD_ARRAY_MAPPER (atanh, ::atanh, ::atanh, -1.0, 1.0)
ARRAY_MAPPER (ceil, NDArray::dmapper, ::ceil)
ARRAY_MAPPER (cos, NDArray::dmapper, ::cos)
ARRAY_MAPPER (cosh, NDArray::dmapper, ::cosh)
ARRAY_MAPPER (exp, NDArray::dmapper, ::exp)
ARRAY_MAPPER (expm1, NDArray::dmapper, ::expm1)
ARRAY_MAPPER (fix, NDArray::dmapper, ::fix)
ARRAY_MAPPER (floor, NDArray::dmapper, ::floor)
CD_ARRAY_MAPPER (log, ::log, std::log, 0.0, octave_Inf)
CD_ARRAY_MAPPER (log2, xlog2, xlog2, 0.0, octave_Inf)
CD_ARRAY_MAPPER (log10, ::log10, std::log10, 0.0, octave_Inf)
CD_ARRAY_MAPPER (log1p, ::log1p, ::log1p, -1.0, octave_Inf)
ARRAY_MAPPER (round, NDArray::dmapper, xround)
ARRAY_MAPPER (roundb, NDArray::dmapper, xroundb)
ARRAY_MAPPER (signum, NDArray::dmapper, ::signum)
ARRAY_MAPPER (sin, NDArray::dmapper, ::sin)
ARRAY_MAPPER (sinh, NDArray::dmapper, ::sinh)
CD_ARRAY_MAPPER (sqrt, ::sqrt, std::sqrt, 0.0, octave_Inf)
ARRAY_MAPPER (tan, NDArray::dmapper, ::tan)
ARRAY_MAPPER (tanh, NDArray::dmapper, ::tanh)
ARRAY_MAPPER (isna, NDArray::bmapper, octave_is_NA)

DEFUN (double, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} double (@var{x})\n\
Convert @var{x} to double precision type.\n\
@end deftypefn")
{
  // The OCTAVE_TYPE_CONV_BODY3 macro declares retval, so they go
  // inside their own scopes, and we don't declare retval here to
  // avoid a shadowed declaration warning.

  if (args.length () == 1)
    {
      if (args(0).is_perm_matrix ())
        {
          OCTAVE_TYPE_CONV_BODY3 (double, octave_perm_matrix, octave_scalar);
        }
      else if (args(0).is_diag_matrix ())
        {
	  if (args(0).is_complex_type ())
	    {
	      OCTAVE_TYPE_CONV_BODY3 (double, octave_complex_diag_matrix, octave_complex);
	    }
	  else
	    {
	      OCTAVE_TYPE_CONV_BODY3 (double, octave_diag_matrix, octave_scalar);
	    }
        }
      else if (args(0).is_sparse_type ())
	{
	  if (args(0).is_complex_type ())
	    {
	      OCTAVE_TYPE_CONV_BODY3 (double, octave_sparse_complex_matrix, octave_complex);
	    }
	  else
	    {
	      OCTAVE_TYPE_CONV_BODY3 (double, octave_sparse_matrix, octave_scalar);
	    }
	}
      else if (args(0).is_complex_type ())
	{
	  OCTAVE_TYPE_CONV_BODY3 (double, octave_complex_matrix, octave_complex);
	}
      else
	{
	  OCTAVE_TYPE_CONV_BODY3 (double, octave_matrix, octave_scalar);
	}
    }
  else
    print_usage ();

  return octave_value ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
