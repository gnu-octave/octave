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

#include "ov-flt-re-diag.h"
#include "ov-base-diag.cc"
#include "ov-float.h"
#include "ov-flt-re-mat.h"

template class octave_base_diag<FloatDiagMatrix, FloatMatrix>;

DEFINE_OCTAVE_ALLOCATOR (octave_float_diag_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_float_diag_matrix, 
                                     "float diagonal matrix", "single");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_float_diag_matrix&);

  return new octave_float_matrix (v.float_matrix_value ());
}

octave_base_value::type_conv_info
octave_float_diag_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_float_matrix::static_type_id ());
}

octave_base_value *
octave_float_diag_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  // FIXME: the proxy mechanism of DiagArray2 causes problems here.
  if (matrix.nelem () == 1)
    retval = new octave_float_scalar (float (matrix (0, 0)));

  return retval;
}

DiagMatrix
octave_float_diag_matrix::diag_matrix_value (bool) const
{
  return DiagMatrix (matrix);
}

FloatDiagMatrix
octave_float_diag_matrix::float_diag_matrix_value (bool) const
{
  return matrix;
}

ComplexDiagMatrix
octave_float_diag_matrix::complex_diag_matrix_value (bool) const
{
  return ComplexDiagMatrix (matrix);
}

FloatComplexDiagMatrix
octave_float_diag_matrix::float_complex_diag_matrix_value (bool) const
{
  return FloatComplexDiagMatrix (matrix);
}

octave_value
octave_float_diag_matrix::abs (void) const
{
  return matrix.abs ();
}

octave_value
octave_float_diag_matrix::real (void) const
{
  return matrix;
}

octave_value
octave_float_diag_matrix::conj (void) const
{
  return matrix;
}

octave_value
octave_float_diag_matrix::imag (void) const
{
  return DiagMatrix (matrix.rows (), matrix.cols ());
}
