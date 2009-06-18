/*

Copyright (C) 2008, 2009 Jaroslav Hajek

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

#include "mach-info.h"
#include "lo-ieee.h"

#include "ov-base.h"
#include "ov-base-mat.h"
#include "pr-output.h"
#include "error.h"
#include "gripes.h"
#include "oct-stream.h"
#include "ops.h"

#include "ls-oct-ascii.h"

template <class DMT, class MT>
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

template <class DMT, class MT>
octave_value
octave_base_diag<DMT, MT>::do_index_op (const octave_value_list& idx,
                                        bool resize_ok)
{
  octave_value retval;
  typedef typename DMT::element_type el_type;

  if (idx.length () == 2 && ! resize_ok)
    {
      idx_vector idx0 = idx(0).index_vector ();
      idx_vector idx1 = idx(1).index_vector ();

      if (idx0.is_scalar () && idx1.is_scalar ())
        {
          // FIXME: the proxy mechanism of DiagArray2 causes problems here.
          retval = el_type (matrix.checkelem (idx0(0), idx1(0)));
        }
      else
        {
          octave_idx_type m = idx0.length (matrix.rows ());
          octave_idx_type n = idx1.length (matrix.columns ());
          if (idx0.is_colon_equiv (m) && idx1.is_colon_equiv (n)
              && m <= matrix.rows () && n <= matrix.rows ())
            {
              DMT rm (matrix);
              rm.resize (m, n);
              retval = rm;
            }
          else
            retval = to_dense ().do_index_op (idx, resize_ok);
        }
    }
  else
    retval = to_dense ().do_index_op (idx, resize_ok);

  return retval;
}

template <class DMT, class MT>
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
	if (type.length () == 1)
          {
            octave_value_list jdx = idx.front ();
            // Check for a simple element assignment. That means, if D is a diagonal matrix,
            // `D(i,i) = x' will not destroy its diagonality (provided i is a valid index).
            if (jdx.length () == 2 && jdx(0).is_scalar_type () && jdx(1).is_scalar_type ())
              {
                typename DMT::element_type val;
                idx_vector i0 = jdx(0).index_vector (), i1 = jdx(1).index_vector ();
                if (! error_state  && i0(0) == i1(0) 
                    && i0(0) < matrix.rows () && i1(0) < matrix.cols ()
                    && chk_valid_scalar (rhs, val))
                  {
                    matrix (i0(0), i1(0)) = val;                    
                    retval = this;
                    this->count++;
                    // invalidate cache
                    dense_cache = octave_value ();
                  }
              }

            if (! error_state && ! retval.is_defined ())
              retval = numeric_assign (type, idx, rhs);
          }
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

template <class DMT, class MT>
octave_value
octave_base_diag<DMT, MT>::resize (const dim_vector& dv, bool fill) const
{
  octave_value retval;
  if (dv.length () == 2)
    {
      DMT rm (matrix);
      rm.resize (dv(0), dv(1));
      retval = rm;
    }
  else
    retval = to_dense ().resize (dv, fill);
  return retval;
}

template <class DMT, class MT>
bool
octave_base_diag<DMT, MT>::is_true (void) const
{
  return to_dense ().is_true ();
}

// FIXME: this should be achieveable using ::real
template <class T> inline T helper_getreal (T x) { return x; }
template <class T> inline T helper_getreal (std::complex<T> x) { return x.real (); }
// FIXME: we really need some traits so that ad hoc hooks like this are not necessary
template <class T> inline T helper_iscomplex (T) { return false; }
template <class T> inline T helper_iscomplex (std::complex<T>) { return true; }

template <class DMT, class MT>
double
octave_base_diag<DMT, MT>::double_value (bool force_conversion) const
{
  double retval = lo_ieee_nan_value ();
  typedef typename DMT::element_type el_type;

  if (helper_iscomplex (el_type ()) && ! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex matrix", "real scalar");

  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 type_name (), "real scalar");

      retval = helper_getreal (el_type (matrix (0, 0)));
    }
  else
    gripe_invalid_conversion (type_name (), "real scalar");

  return retval;
}

template <class DMT, class MT>
float
octave_base_diag<DMT, MT>::float_value (bool force_conversion) const
{
  float retval = lo_ieee_float_nan_value ();
  typedef typename DMT::element_type el_type;

  if (helper_iscomplex (el_type ()) && ! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       "complex matrix", "real scalar");

  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 type_name (), "real scalar");

      retval = helper_getreal (el_type (matrix (0, 0)));
    }
  else
    gripe_invalid_conversion (type_name (), "real scalar");

  return retval;
}

template <class DMT, class MT>
Complex
octave_base_diag<DMT, MT>::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 type_name (), "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion (type_name (), "complex scalar");

  return retval;
}

template <class DMT, class MT>
FloatComplex
octave_base_diag<DMT, MT>::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 type_name (), "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion (type_name (), "complex scalar");

  return retval;
}

template <class DMT, class MT>
Matrix
octave_base_diag<DMT, MT>::matrix_value (bool) const
{
  return Matrix (diag_matrix_value ());
}

template <class DMT, class MT>
FloatMatrix
octave_base_diag<DMT, MT>::float_matrix_value (bool) const
{
  return FloatMatrix (float_diag_matrix_value ());
}

template <class DMT, class MT>
ComplexMatrix
octave_base_diag<DMT, MT>::complex_matrix_value (bool) const
{
  return ComplexMatrix (complex_diag_matrix_value ());
}

template <class DMT, class MT>
FloatComplexMatrix
octave_base_diag<DMT, MT>::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (float_complex_diag_matrix_value ());
}

template <class DMT, class MT>
NDArray
octave_base_diag<DMT, MT>::array_value (bool) const
{
  return NDArray (matrix_value ());
}

template <class DMT, class MT>
FloatNDArray
octave_base_diag<DMT, MT>::float_array_value (bool) const
{
  return FloatNDArray (float_matrix_value ());
}

template <class DMT, class MT>
ComplexNDArray
octave_base_diag<DMT, MT>::complex_array_value (bool) const
{
  return ComplexNDArray (complex_matrix_value ());
}

template <class DMT, class MT>
FloatComplexNDArray
octave_base_diag<DMT, MT>::float_complex_array_value (bool) const
{
  return FloatComplexNDArray (float_complex_matrix_value ());
}

template <class DMT, class MT>
boolNDArray
octave_base_diag<DMT, MT>::bool_array_value (bool warn) const
{
  return to_dense ().bool_array_value (warn); 
}
  
template <class DMT, class MT>
charNDArray
octave_base_diag<DMT, MT>::char_array_value (bool warn) const
{
  return to_dense ().char_array_value (warn); 
}
  
template <class DMT, class MT>
SparseMatrix 
octave_base_diag<DMT, MT>::sparse_matrix_value (bool) const
{
  return SparseMatrix (diag_matrix_value ());
}

template <class DMT, class MT>
SparseComplexMatrix 
octave_base_diag<DMT, MT>::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (complex_diag_matrix_value ());
}

template <class DMT, class MT>
idx_vector
octave_base_diag<DMT, MT>::index_vector (void) const
{
  return to_dense ().index_vector ();
}

template <class DMT, class MT>
octave_value
octave_base_diag<DMT, MT>::convert_to_str_internal (bool pad, bool force, char type) const
{
  return to_dense ().convert_to_str_internal (pad, force, type);
}

template <class DMT, class MT>
bool 
octave_base_diag<DMT, MT>::save_ascii (std::ostream& os)
{
  os << "# rows: " << matrix.rows () << "\n"
    << "# columns: " << matrix.columns () << "\n";

  os << matrix.diag ();

  return true;
}

template <class DMT, class MT>
bool 
octave_base_diag<DMT, MT>::load_ascii (std::istream& is)
{
  octave_idx_type r = 0, c = 0;
  bool success = true;

  if (extract_keyword (is, "rows", r, true)
      && extract_keyword (is, "columns", c, true))
    {
      octave_idx_type l = r < c ? r : c;
      MT tmp (l, 1);
      is >> tmp;

      if (!is) 
	{
	  error ("load: failed to load diagonal matrix constant");
	  success = false;
	}
      else
        {
          // This is a little tricky, as we have the Matrix type, but
          // not ColumnVector type. We need to help the compiler get
          // through the inheritance tree.
          typedef typename DMT::element_type el_type;
          matrix = DMT (MDiagArray2<el_type> (MArray<el_type> (tmp)));
          matrix.resize (r, c);

          // Invalidate cache. Probably not necessary, but safe.
          dense_cache = octave_value ();
        }
    }
  else
    {
      error ("load: failed to extract number of rows and columns");
      success = false;
    }

  return success;
}

template <class DMT, class MT>
void
octave_base_diag<DMT, MT>::print_raw (std::ostream& os,
                                      bool pr_as_read_syntax) const
{
  return octave_print_internal (os, matrix, pr_as_read_syntax,
                                current_print_indent_level ());
}

template <class DMT, class MT>
mxArray *
octave_base_diag<DMT, MT>::as_mxArray (void) const
{
  return to_dense ().as_mxArray ();
}

template <class DMT, class MT>
bool
octave_base_diag<DMT, MT>::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

template <class DMT, class MT>
void
octave_base_diag<DMT, MT>::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}
template <class DMT, class MT>
int
octave_base_diag<DMT, MT>::write (octave_stream& os, int block_size,
                                  oct_data_conv::data_type output_type, int skip,
                                  oct_mach_info::float_format flt_fmt) const
{ 
  return to_dense ().write (os, block_size, output_type, skip, flt_fmt); 
}

template <class DMT, class MT>
void
octave_base_diag<DMT, MT>::print_info (std::ostream& os,
                                       const std::string& prefix) const
{
  matrix.print_info (os, prefix);
}

template <class DMT, class MT>
octave_value
octave_base_diag<DMT, MT>::to_dense (void) const
{
  if (! dense_cache.is_defined ())
    dense_cache = MT (matrix);

  return dense_cache;
}

#define FORWARD_MAPPER(MAP) \
  template <class DMT, class MT> \
  octave_value \
  octave_base_diag<DMT, MT>::MAP (void) const \
  { \
    return to_dense ().MAP (); \
  }

FORWARD_MAPPER (erf)
FORWARD_MAPPER (erfc)
FORWARD_MAPPER (gamma)
FORWARD_MAPPER (lgamma)
FORWARD_MAPPER (acos)
FORWARD_MAPPER (acosh)
FORWARD_MAPPER (angle)
FORWARD_MAPPER (arg)
FORWARD_MAPPER (asin)
FORWARD_MAPPER (asinh)
FORWARD_MAPPER (atan)
FORWARD_MAPPER (atanh)
FORWARD_MAPPER (ceil)
FORWARD_MAPPER (cos)
FORWARD_MAPPER (cosh)
FORWARD_MAPPER (exp)
FORWARD_MAPPER (expm1)
FORWARD_MAPPER (fix)
FORWARD_MAPPER (floor)
FORWARD_MAPPER (log)
FORWARD_MAPPER (log2)
FORWARD_MAPPER (log10)
FORWARD_MAPPER (log1p)
FORWARD_MAPPER (round)
FORWARD_MAPPER (roundb)
FORWARD_MAPPER (signum)
FORWARD_MAPPER (sin)
FORWARD_MAPPER (sinh)
FORWARD_MAPPER (tan)
FORWARD_MAPPER (tanh)
FORWARD_MAPPER (finite)
FORWARD_MAPPER (isinf)
FORWARD_MAPPER (isna)
FORWARD_MAPPER (isnan)

