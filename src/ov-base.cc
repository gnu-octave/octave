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

#include "gripes.h"
#include "oct-map.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-ch-mat.h"
#include "ov-str-mat.h"
#include "ov-range.h"

int octave_base_value::t_id = -1;

const string octave_base_value::t_name ("<unknown type>");

octave_value
octave_base_value::index (const octave_value_list&) const
{
  string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

idx_vector
octave_base_value::index_vector (void) const
{
  string nm = type_name ();
  error ("%s type invalid as index value", nm.c_str ());
  return idx_vector ();
}

octave_value
octave_base_value::struct_elt_val (const string&) const
{
  string nm = type_name ();
  error ("can't perform structure reference operations for %s type",
	 nm.c_str ());
  return octave_value ();
}

octave_value&
octave_base_value::struct_elt_ref (const string&)
{
  static octave_value foo;
  string nm = type_name ();
  error ("can't perform structure reference operations for %s type",
	 nm.c_str ());
  return foo;
}

octave_value
octave_base_value::convert_to_str (void) const
{
  gripe_wrong_type_arg ("octave_base_value::convert_to_str ()",
			type_name ());
  return octave_value ();
}

void
octave_base_value::convert_to_row_or_column_vector (void)
{
  gripe_wrong_type_arg
    ("octave_base_value::convert_to_row_or_column_vector ()",
     type_name ());
}

void
octave_base_value::print (ostream&)
{
  gripe_wrong_type_arg ("octave_base_value::print()", type_name ());
}

double
octave_base_value::double_value (bool) const
{
  double retval = octave_NaN;
  gripe_wrong_type_arg ("octave_base_value::double_value ()", type_name ());
  return retval;
}

Matrix
octave_base_value::matrix_value (bool) const
{
  Matrix retval;
  gripe_wrong_type_arg ("octave_base_value::matrix_value()", type_name ());
  return retval;
}

Complex
octave_base_value::complex_value (bool) const
{
  Complex retval (octave_NaN, octave_NaN);
  gripe_wrong_type_arg ("octave_base_value::complex_value()", type_name ());
  return retval;
}

ComplexMatrix
octave_base_value::complex_matrix_value (bool) const
{
  ComplexMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::complex_matrix_value()",
			type_name ());
  return retval;
}

charMatrix
octave_base_value::char_matrix_value (bool) const
{
  charMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::char_matrix_value()",
			type_name ());
  return retval;
}

charMatrix
octave_base_value::all_strings (void) const
{
  charMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::all_strings()", type_name ());
  return retval;
}

string
octave_base_value::string_value (void) const
{
  string retval;
  gripe_wrong_type_arg ("octave_base_value::string_value()", type_name ());
  return retval;
}

Range
octave_base_value::range_value (void) const
{
  Range retval;
  gripe_wrong_type_arg ("octave_base_value::range_value()", type_name ());
  return retval;
}

Octave_map
octave_base_value::map_value (void) const
{
  Octave_map retval;
  gripe_wrong_type_arg ("octave_base_value::map_value()", type_name ());
  return retval;
}

octave_value
octave_base_value::not (void) const
{
  octave_value retval;
  gripe_wrong_type_arg ("octave_base_value::not()", type_name ());
  return retval;
}

octave_value
octave_base_value::uminus (void) const
{
  octave_value retval;
  gripe_wrong_type_arg ("octave_base_value::uminus()", type_name ());
  return retval;
}

octave_value
octave_base_value::transpose (void) const
{
  octave_value retval;
  gripe_wrong_type_arg ("octave_base_value::transpose()", type_name ());
  return retval;
}

octave_value
octave_base_value::hermitian (void) const
{
  octave_value retval;
  gripe_wrong_type_arg ("octave_base_value::hermitian()", type_name ());
  return retval;
}

void
octave_base_value::increment (void)
{
  gripe_wrong_type_arg ("octave_base_value::increment()", type_name ());
}

void
octave_base_value::decrement (void)
{
  gripe_wrong_type_arg ("octave_base_value::decrement()", type_name ());
}

static octave_value *
matrix_conv (const octave_value&)
{
  return new octave_matrix ();
}

static octave_value *
complex_matrix_conv (const octave_value&)
{
  return new octave_complex_matrix ();
}

static octave_value *
string_conv (const octave_value&)
{
  return new octave_char_matrix_str ();
}

void
install_base_type_conversions (void)
{
  INSTALL_ASSIGNCONV (octave_base_value, octave_scalar, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_matrix, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_complex, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_complex_matrix, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_range, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_char_matrix_str, octave_char_matrix_str);

  INSTALL_WIDENOP (octave_base_value, octave_matrix, matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_complex_matrix, complex_matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_char_matrix_str, string_conv);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
