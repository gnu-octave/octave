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

#include <iostream.h>

#include "lo-ieee.h"
#include "mx-base.h"

#include "oct-obj.h"
#include "ops.h"
#include "ov-re-mat.h"
#include "ov-str-mat.h"
#include "gripes.h"
#include "pr-output.h"

int
octave_char_matrix_str::t_id (-1);

const string
octave_char_matrix_str::t_name ("string");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_char_matrix_str&);

  return new octave_matrix (v.matrix_value ());
}

type_conv_fcn
octave_char_matrix_str::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

octave_value
octave_char_matrix_str::do_index_op (const octave_value_list& idx)
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = octave_value (charMatrix (matrix.index (i, j)), true);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = octave_value (charMatrix (matrix.index (i)), true);
      }
      break;

    default:
      error ("invalid number of indices (%d) for matrix value", len);
      break;
    }

  return retval;
}

extern void assign (Array2<char>&, const Array2<char>&);

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

	::assign (matrix, tmp);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	matrix.set_index (i);

	::assign (matrix, tmp);
      }
      break;

    default:
      error ("invalid number of indices (%d) for indexed matrix assignment",
	     len);
      break;
    }
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

string_vector
octave_char_matrix_str::all_strings (void) const
{
  int n = matrix.rows ();

  string_vector retval (n);

  for (int i = 0; i < n; i++)
    retval[i] = matrix.row_as_string (i, true);

  return retval;
}

string
octave_char_matrix_str::string_value (void) const
{
  return matrix.row_as_string (0);  // XXX FIXME??? XXX
}

void
octave_char_matrix_str::print (ostream& os, bool pr_as_read_syntax) const
{
  // indent (os);
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_char_matrix_str::print_raw (ostream& os, bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax, true,
			 current_print_indent_level ());
}

bool
octave_char_matrix_str::print_name_tag (ostream& os, const string& name) const
{
  bool retval = false;

  indent (os);

  if (rows () <= 1)
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      newline (os);
      retval = true;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
