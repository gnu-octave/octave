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

#include "lo-ieee.h"
#include "mx-base.h"

#include "ov-ch-mat.h"
#include "gripes.h"
#include "pr-output.h"

octave_allocator
octave_char_matrix::allocator (sizeof (octave_char_matrix));

int
octave_char_matrix::t_id (-1);

const string
octave_char_matrix::t_name ("char matrix");

bool
octave_char_matrix::valid_as_scalar_index (void) const
{
  // XXX FIXME XXX
  return false;
}

bool
octave_char_matrix::valid_as_zero_index (void) const
{
  // XXX FIXME XXX
  return false;
}

bool
octave_char_matrix::is_true (void) const
{
  // XXX FIXME XXX
  return false;
}

double
octave_char_matrix::double_value (bool) const
{
  double retval = octave_NaN;

  if ((rows () == 1 && columns () == 1)
      || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
    retval = matrix (0, 0);
  else
    gripe_invalid_conversion ("character matrix", "real scalar");

  return retval;
}

Complex
octave_char_matrix::complex_value (bool) const
{
  Complex retval (octave_NaN, octave_NaN);

  if ((rows () == 1 && columns () == 1)
      || (Vdo_fortran_indexing && rows () > 0 && columns () > 0))
    retval = matrix (0, 0);
  else
    gripe_invalid_conversion ("character matrix", "complex scalar");

  return retval;
}

void
octave_char_matrix::print (ostream& os, bool pr_as_read_syntax)
{
  octave_print_internal (os, matrix, pr_as_read_syntax, false, struct_indent);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
