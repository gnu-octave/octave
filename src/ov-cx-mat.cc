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
#include "mx-base.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

template class octave_base_matrix<ComplexNDArray>;

DEFINE_OCTAVE_ALLOCATOR (octave_complex_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex_matrix,
				     "complex matrix", "double");

octave_value *
octave_complex_matrix::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  if (matrix.ndims () == 2)
    {
      ComplexMatrix cm = matrix.matrix_value ();

      int nr = cm.rows ();
      int nc = cm.cols ();

      if (nr == 1 && nc == 1)
	{
	  Complex c = matrix (0, 0);

	  if (imag (c) == 0.0)
	    retval = new octave_scalar (std::real (c));
	  else
	    retval = new octave_complex (c);
	}
      else if (nr == 0 || nc == 0)
	retval = new octave_matrix (Matrix (nr, nc));
      else if (cm.all_elements_are_real ())
	retval = new octave_matrix (::real (cm));
    }

  return retval;
}

void
octave_complex_matrix::assign (const octave_value_list& idx,
			       const ComplexMatrix& rhs)
{
  octave_base_matrix<ComplexNDArray>::assign (idx, rhs);
}

void
octave_complex_matrix::assign (const octave_value_list& idx,
			       const Matrix& rhs)
{
  int len = idx.length ();

  for (int i = 0; i < len; i++)
    matrix.set_index (idx(i).index_vector ());

  ::assign (matrix, rhs);
}

bool
octave_complex_matrix::valid_as_scalar_index (void) const
{
  // XXX FIXME XXX
  return false;
}

double
octave_complex_matrix::double_value (bool force_conversion) const
{
  double retval = lo_ieee_nan_value ();

  if (! force_conversion && Vwarn_imag_to_real)
    gripe_implicit_conversion ("complex matrix", "real scalar");

  // XXX FIXME XXX -- maybe this should be a function, valid_as_scalar()
  if (rows () > 0 && columns () > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (Vwarn_fortran_indexing)
	gripe_implicit_conversion ("complex matrix", "real scalar");

      retval = std::real (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("complex matrix", "real scalar");

  return retval;
}

Matrix
octave_complex_matrix::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion && Vwarn_imag_to_real)
    gripe_implicit_conversion ("complex matrix", "real matrix");

  retval = ::real (matrix.matrix_value ());

  return retval;
}

Complex
octave_complex_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  // XXX FIXME XXX -- maybe this should be a function, valid_as_scalar()
  if (rows () > 0 && columns () > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (Vwarn_fortran_indexing)
	gripe_implicit_conversion ("complex matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("complex matrix", "complex scalar");

  return retval;
}

ComplexMatrix
octave_complex_matrix::complex_matrix_value (bool) const
{
  return matrix.matrix_value ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
