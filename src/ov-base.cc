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

#if defined (__GNUG__) && ! defined (NO_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-ieee.h"

#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-cell.h"
#include "ov-ch-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-list.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-str-mat.h"
#include "variables.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_base_value, "<unknown type>");

octave_value
octave_base_value::subsref (const std::string,
			    const SLList<octave_value_list>&)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value_list
octave_base_value::subsref (const std::string,
			    const SLList<octave_value_list>&, int)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value
octave_base_value::do_index_op (const octave_value_list&, int)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value_list
octave_base_value::do_multi_index_op (int, const octave_value_list&)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

idx_vector
octave_base_value::index_vector (void) const
{
  std::string nm = type_name ();
  error ("%s type invalid as index value", nm.c_str ());
  return idx_vector ();
}

octave_value
octave_base_value::subsasgn (const std::string type,
			     const SLList<octave_value_list>& idx,
			     const octave_value& rhs)
{
  octave_value retval;

  if (is_defined ())
    {
      std::string nm = type_name ();
      error ("can't perform indexed assignment for %s type", nm.c_str ());
    }
  else
    {
      // Create new object of appropriate type for given index and rhs
      // types and then call subsasgn again for that object.

      octave_value tmp = octave_value::empty_conv (type, rhs);

      retval = tmp.subsasgn (type, idx, rhs);
    }

  return retval;
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
octave_base_value::print (std::ostream&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::print ()", type_name ());
}

void
octave_base_value::print_raw (std::ostream&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::print_raw ()", type_name ());
}

bool
octave_base_value::print_name_tag (std::ostream& os, const std::string& name) const
{
  indent (os);
  os << name << " =";
  newline (os);
  newline (os);
  return true;
}

void
octave_base_value::print_info (std::ostream& os,
			       const std::string& prefix) const
{
  os << "no info for type: " << type_name () << "\n";
}

int
octave_base_value::int_value (bool require_int, bool frc_str_conv) const
{
  int retval = 0;

  double d = double_value (frc_str_conv);

  if (! error_state)
    {
      if (require_int && D_NINT (d) != d)
	{
	  error ("conversion to integer value failed");
	  return retval;
	}

      retval = static_cast<int> (d);
    }
  else
    gripe_wrong_type_arg ("octave_base_value::int_value ()", type_name ());

  return retval;
}

int
octave_base_value::nint_value (bool frc_str_conv) const
{
  int retval = 0;

  double d = double_value (frc_str_conv);

  if (! error_state)
    {
      if (xisnan (d))
	{
	  error ("conversion of NaN to integer value failed");
	  return retval;
	}

      retval = NINT (d);
    }
  else
    gripe_wrong_type_arg ("octave_base_value::nint_value ()", type_name ());

  return retval;
}

double
octave_base_value::double_value (bool) const
{
  double retval = octave_NaN;
  gripe_wrong_type_arg ("octave_base_value::double_value ()", type_name ());
  return retval;
}

Cell
octave_base_value::cell_value () const
{
  Cell retval;
  gripe_wrong_type_arg ("octave_base_value::cell_value()", type_name ());
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

string_vector
octave_base_value::all_strings (void) const
{
  string_vector retval;
  gripe_wrong_type_arg ("octave_base_value::all_strings()", type_name ());
  return retval;
}

std::string
octave_base_value::string_value (void) const
{
  std::string retval;
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

string_vector
octave_base_value::map_keys (void) const
{
  string_vector retval;
  gripe_wrong_type_arg ("octave_base_value::map_keys()", type_name ());
  return retval;
}

octave_stream
octave_base_value::stream_value (void) const
{
  octave_stream retval;
  gripe_wrong_type_arg ("octave_base_value::stream_value()", type_name ());
  return retval;
}

int
octave_base_value::stream_number (void) const
{
  int retval = -1;
  gripe_wrong_type_arg ("octave_base_value::stream_number()", type_name ());
  return retval;
}

octave_function *
octave_base_value::function_value (bool silent)
{
  octave_function *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::function_value()",
			  type_name ());
  return retval;
}

octave_value_list
octave_base_value::list_value (void) const
{
  octave_value_list retval;
  gripe_wrong_type_arg ("octave_base_value::list_value()", type_name ());
  return retval;
}

bool
octave_base_value::bool_value (void) const
{
  bool retval = false;
  gripe_wrong_type_arg ("octave_base_value::bool_value()", type_name ());
  return retval;
}

boolMatrix
octave_base_value::bool_matrix_value (void) const
{
  boolMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::bool_matrix_value()",
			type_name ());
  return retval;
}

CONVDECLX (matrix_conv)
{
  return new octave_matrix ();
}

CONVDECLX (complex_matrix_conv)
{
  return new octave_complex_matrix ();
}

CONVDECLX (string_conv)
{
  return new octave_char_matrix_str ();
}

CONVDECLX (cell_conv)
{
  return new octave_cell ();
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
  INSTALL_ASSIGNCONV (octave_base_value, octave_cell, octave_cell);

  INSTALL_WIDENOP (octave_base_value, octave_matrix, matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_complex_matrix, complex_matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_char_matrix_str, string_conv);
  INSTALL_WIDENOP (octave_base_value, octave_cell, cell_conv);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
