/*

Copyright (C) 1996 John W. Eaton

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

#include "lo-ieee.h"
#include "lo-utils.h"

#include "gripes.h"
#include "ops.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "pr-output.h"

int octave_range::t_id = -1;

const string octave_range::t_name ("range");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_range&);

  return new octave_matrix (v.matrix_value ());
}

octave_value::numeric_conv_fcn
octave_range::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

double
octave_range::double_value (bool) const
{
  double retval = octave_NaN;

  int nel = range.nelem ();

  if (nel == 1 || (nel > 1 && Vdo_fortran_indexing))
    retval = range.base ();
  else
    gripe_invalid_conversion ("range", "real scalar");

  return retval;
}

octave_value
octave_range::all (void) const
{
  octave_value retval;
  error ("octave_range::all(): not implemented");
  return retval;
}

octave_value
octave_range::any (void) const
{
  octave_value retval;
  error ("octave_range::any(): not implemented");
  return retval;
}

bool
octave_range::is_true (void) const
{
  bool retval = false;
  error ("octave_range::is_true(): not implemented");
  return retval;
}

Complex
octave_range::complex_value (bool) const
{
  Complex retval (octave_NaN, octave_NaN);

  int nel = range.nelem ();

  if (nel == 1 || (nel > 1 && Vdo_fortran_indexing))
    retval = range.base ();
  else
    gripe_invalid_conversion ("range", "complex scalar");

  return retval;
}

octave_value
octave_range::transpose (void) const
{
  Matrix tmp (matrix_value ());
  return tmp.transpose ();
}

octave_value
octave_range::hermitian (void) const
{
  Matrix tmp (matrix_value ());
  return tmp.transpose ();
}

void
octave_range::print (ostream& os)
{
  octave_print_internal (os, range, false, struct_indent);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
