/*

Copyright (C) 2008 Jaroslav Hajek

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

#include "ov-cx-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-re-diag.h"
#include "ov-base-diag.cc"
#include "ov-complex.h"
#include "ov-cx-mat.h"

template class octave_base_diag<ComplexDiagMatrix, ComplexMatrix>;

DEFINE_OCTAVE_ALLOCATOR (octave_complex_diag_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex_diag_matrix, 
                                     "complex diagonal matrix", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_complex_diag_matrix&);

  return new octave_complex_matrix (v.complex_matrix_value ());
}

octave_base_value::type_conv_info
octave_complex_diag_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_complex_matrix::static_type_id ());
}

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_complex_diag_matrix&);

  return new octave_float_complex_diag_matrix (v.float_complex_diag_matrix_value ());
}

octave_base_value::type_conv_info
octave_complex_diag_matrix::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_demotion_function,
                                            octave_float_complex_diag_matrix::static_type_id ());
}

octave_base_value *
octave_complex_diag_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.nelem () == 1)
    {
      // FIXME: the proxy mechanism of DiagArray2 causes problems here.
      retval = new octave_complex (Complex (matrix (0, 0)));
      octave_base_value *rv2 = retval->try_narrowing_conversion ();
      if (rv2)
        {
          delete retval;
          retval = rv2;
        }
    }
  else if (matrix.all_elements_are_real ())
    {
      return new octave_diag_matrix (::real (matrix));
    }

  return retval;
}

DiagMatrix
octave_complex_diag_matrix::diag_matrix_value (bool force_conversion) const
{
  DiagMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       type_name (), "real matrix");

  retval = ::real (matrix);

  return retval;
}

FloatDiagMatrix
octave_complex_diag_matrix::float_diag_matrix_value (bool force_conversion) const
{
  DiagMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
			       type_name (), "real matrix");

  retval = ::real (matrix);

  return retval;
}

ComplexDiagMatrix
octave_complex_diag_matrix::complex_diag_matrix_value (bool) const
{
  return matrix;
}

FloatComplexDiagMatrix
octave_complex_diag_matrix::float_complex_diag_matrix_value (bool) const
{
  return FloatComplexDiagMatrix (matrix);
}

octave_value
octave_complex_diag_matrix::abs (void) const
{
  return matrix.abs ();
}

octave_value
octave_complex_diag_matrix::real (void) const
{
  return ::real (matrix);
}

octave_value
octave_complex_diag_matrix::conj (void) const
{
  return ::conj (matrix);
}

octave_value
octave_complex_diag_matrix::imag (void) const
{
  return ::imag (matrix);
}

