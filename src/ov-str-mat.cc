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

#if defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-ieee.h"
#include "mx-base.h"

#include "oct-obj.h"
#include "ops.h"
#include "ov-re-mat.h"
#include "ov-str-mat.h"
#include "gripes.h"
#include "pr-output.h"
#include "pt-mat.h"

DEFINE_OCTAVE_ALLOCATOR (octave_char_matrix_str);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_char_matrix_str, "string");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_char_matrix_str&);

  Matrix m = v.matrix_value ();

  return error_state ? 0 : new octave_matrix (m);
}

type_conv_fcn
octave_char_matrix_str::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

octave_value
octave_char_matrix_str::do_index_op (const octave_value_list& idx,
				     int resize_ok)
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = octave_value (charMatrix (matrix.index (i, j, resize_ok)),
			       true);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = octave_value (charMatrix (matrix.index (i, resize_ok)), true);
      }
      break;

    default:
      error ("invalid number of indices (%d) for matrix value", len);
      break;
    }

  return retval;
}

void
octave_char_matrix_str::assign (const octave_value_list& idx,
				const charMatrix& rhs)
{
  int len = idx.length ();

  // XXX FIXME XXX
  charMatrix tmp = rhs;
  if (tmp.rows () == 1 && tmp.columns () == 0)
    tmp.resize (0, 0);    

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	matrix.set_index (i);
	matrix.set_index (j);

	::assign (matrix, tmp, Vstring_fill_char);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	matrix.set_index (i);

	::assign (matrix, tmp, Vstring_fill_char);
      }
      break;

    default:
      error ("invalid number of indices (%d) for indexed matrix assignment",
	     len);
      break;
    }
}

bool
octave_char_matrix_str::valid_as_scalar_index (void) const
{
  bool retval = false;
  error ("octave_char_matrix_str::valid_as_scalar_index(): not implemented");
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

string_vector
octave_char_matrix_str::all_strings (void) const
{
  int n = matrix.rows ();

  string_vector retval (n);

  for (int i = 0; i < n; i++)
    retval[i] = matrix.row_as_string (i, true);

  return retval;
}

std::string
octave_char_matrix_str::string_value (void) const
{
  return matrix.row_as_string (0);  // XXX FIXME??? XXX
}

void
octave_char_matrix_str::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level (), true);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
