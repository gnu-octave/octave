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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-ieee.h"

#include "oct-obj.h"
#include "ops.h"
#include "ov-complex.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "gripes.h"
#include "pr-output.h"

template class octave_base_scalar<Complex>;

DEFINE_OCTAVE_ALLOCATOR (octave_complex);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex,
				     "complex scalar", "double");

octave_value *
octave_complex::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  if (imag (scalar) == 0.0)
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
      // XXX FIXME XXX -- this doesn't solve the problem of
      //
      //   a = i; a([1,1], [1,1], [1,1])
      //
      // and similar constructions.  Hmm...

      // XXX FIXME XXX -- using this constructor avoids narrowing the
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

  if (! force_conversion && Vwarn_imag_to_real)
    gripe_implicit_conversion ("complex scalar", "real scalar");

  retval = std::real (scalar);

  return retval;
}

Matrix
octave_complex::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion && Vwarn_imag_to_real)
    gripe_implicit_conversion ("complex scalar", "real matrix");

  retval = Matrix (1, 1, std::real (scalar));

  return retval;
}

NDArray
octave_complex::array_value (bool force_conversion) const
{
  NDArray retval;

  if (! force_conversion && Vwarn_imag_to_real)
    gripe_implicit_conversion ("complex scalar", "real matrix");

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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
