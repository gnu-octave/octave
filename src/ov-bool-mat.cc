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

#if defined (__GNUG__)
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
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-re-mat.h"
#include "pr-output.h"

template class octave_base_matrix<boolMatrix>;

DEFINE_OCTAVE_ALLOCATOR (octave_bool_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_bool_matrix, "bool matrix");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_bool_matrix&);

  return new octave_matrix (Matrix (v.bool_matrix_value ()));
}

type_conv_fcn
octave_bool_matrix::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

octave_value *
octave_bool_matrix::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  int nr = matrix.rows ();
  int nc = matrix.cols ();

  if (nr == 1 && nc == 1)
    retval = new octave_bool (matrix (0, 0));

  return retval;
}

void
octave_bool_matrix::assign (const octave_value_list& idx,
			    const boolMatrix& rhs)
{
  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	matrix.set_index (i);
	matrix.set_index (j);

	::assign (matrix, rhs);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	matrix.set_index (i);

	::assign (matrix, rhs);
      }
      break;

    default:
      error ("invalid number of indices (%d) for indexed matrix assignment",
	     len);
      break;
    }
}

bool
octave_bool_matrix::valid_as_scalar_index (void) const
{
  // XXX FIXME XXX
  return false;
}

double
octave_bool_matrix::double_value (bool) const
{
  double retval = octave_NaN;

  if ((rows () == 1 && columns () == 1)
      || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
    retval = matrix (0, 0);
  else
    gripe_invalid_conversion ("bool matrix", "real scalar");

  return retval;
}

Complex
octave_bool_matrix::complex_value (bool) const
{
  Complex retval (octave_NaN, octave_NaN);

  if ((rows () == 1 && columns () == 1)
      || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
    retval = matrix (0, 0);
  else
    gripe_invalid_conversion ("bool matrix", "complex scalar");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
