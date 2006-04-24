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
#include "mx-base.h"

#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-ch-mat.h"
#include "gripes.h"
#include "pr-output.h"

template class octave_base_matrix<charNDArray>;

DEFINE_OCTAVE_ALLOCATOR (octave_char_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_char_matrix,
				     "char matrix", "int8");

bool
octave_char_matrix::valid_as_scalar_index (void) const
{
  bool retval = false;
  error ("octave_char_matrix::valid_as_scalar_index(): not implemented");
  return retval;
}

double
octave_char_matrix::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  // FIXME -- maybe this should be a function, valid_as_scalar()
  if (rows () > 0 && columns () > 0)
    {
      // FIXME -- is warn_fortran_indexing the right variable here?
      if (Vwarn_fortran_indexing)
	gripe_implicit_conversion ("character matrix", "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("character matrix", "real scalar");

  return retval;
}

Complex
octave_char_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  // FIXME -- maybe this should be a function, valid_as_scalar()
  if (rows () > 0 && columns () > 0)
    {
      // FIXME -- is warn_fortran_indexing the right variable here?
      if (Vwarn_fortran_indexing)
	gripe_implicit_conversion ("character matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("character matrix", "complex scalar");

  return retval;
}

void
octave_char_matrix::print_raw (std::ostream& os,
			       bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
