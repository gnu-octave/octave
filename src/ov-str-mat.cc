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
#include "mx-base.h"

#include "ops.h"
#include "ov-re-mat.h"
#include "ov-str-mat.h"
#include "gripes.h"
#include "pr-output.h"

int octave_char_matrix_str::t_id = -1;

const string octave_char_matrix_str::t_name ("string");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_char_matrix_str&);

  return new octave_matrix (v.matrix_value ());
}

octave_value::numeric_conv_fcn
octave_char_matrix_str::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

octave_value
octave_char_matrix_str::all (void) const
{
  octave_value retval;
  error ("octave_char_matrix_str::all(): not implemented");
  return retval;
}

octave_value
octave_char_matrix_str::any (void) const
{
  octave_value retval;
  error ("octave_char_matrix_str::any(): not implemented");
  return retval;
}

bool
octave_char_matrix_str::valid_as_scalar_index (void) const
{
  bool retval = false;
  error ("octave_char_matrix_str::valid_as_scalar_index(): not implemented");
  return retval;
}
bool
octave_char_matrix_str::valid_as_zero_index (void) const
{
  bool retval = false;
  error ("octave_char_matrix_str::valid_as_zero_index(): not implemented");
  return retval;
}

bool
octave_char_matrix_str::is_true (void) const
{
  bool retval = false;
  error ("octave_char_matrix_str::is_true(): not implemented");
  return retval;
}

Matrix
octave_char_matrix_str::matrix_value (bool force_string_conv) const
{
  Matrix retval;

  int flag = force_string_conv;

  if (! flag)
    flag = Vimplicit_str_to_num_ok;

  if (flag < 0)
    gripe_implicit_conversion ("string", "real matrix");

  if (flag)
    retval = Matrix (matrix);
  else
    gripe_invalid_conversion ("string", "real matrix");

  return retval;
}

charMatrix
octave_char_matrix_str::all_strings (void) const
{
  charMatrix retval;
  error ("octave_char_matrix_str::all_strings(): not implemented");
  return retval;
}

string
octave_char_matrix_str::string_value (void) const
{
  return matrix.row_as_string (0);  // XXX FIXME??? XXX
}

void
octave_char_matrix_str::print (ostream& os)
{
  octave_print_internal (os, matrix, false, true, struct_indent);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
