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
#include "lo-utils.h"

#include "gripes.h"
#include "ops.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

octave_allocator
octave_range::allocator (sizeof (octave_range));

int
octave_range::t_id (-1);

const string
octave_range::t_name ("range");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_range&);

  return new octave_matrix (v.matrix_value ());
}

type_conv_fcn
octave_range::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

octave_value *
octave_range::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  switch (range.nelem ())
    {
    case 1:
      retval = new octave_scalar (range.base ());
      break;

    case 0:
      retval = new octave_matrix (Matrix ());
      break;

    default:
      break;
    }

  return retval;
}

octave_value
octave_range::index (const octave_value_list& idx) const
{
  // XXX FIXME XXX -- this doesn't solve the problem of
  //
  //   a = 1:5; a(1, 1, 1)
  //
  // and similar constructions.  Hmm...

  // XXX FIXME XXX -- using this constructor avoids possibly narrowing
  // the range to a scalar value.  Need a better solution to this
  // problem.

  octave_value tmp (new octave_matrix (range.matrix_value ()));

  return tmp.index (idx);
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
  // XXX FIXME XXX -- this is a potential waste of memory.

  Matrix m = range.matrix_value ();

  return m.all ();
}

octave_value
octave_range::any (void) const
{
  return static_cast<double> (range.base () != 0.0 || range.nelem () > 1);
}

bool
octave_range::is_true (void) const
{
  bool retval = false;

  if (range.nelem () == 0)
    {
      int flag = Vpropagate_empty_matrices;

      if (flag < 0)
	warning ("empty range used in conditional expression");
      else if (flag == 0)
	error ("empty range used in conditional expression");
    }
  else
    {
      // XXX FIXME XXX -- this is a potential waste of memory.

      Matrix m ((range.matrix_value () . all ()) . all ());

      retval = (m.rows () == 1 && m.columns () == 1 && m (0, 0) != 0.0);
    }

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
octave_range::not (void) const
{
  Matrix tmp (range.matrix_value ());
  return (! tmp);
}

octave_value
octave_range::transpose (void) const
{
  Matrix tmp (range.matrix_value ());
  return tmp.transpose ();
}

octave_value
octave_range::hermitian (void) const
{
  Matrix tmp (range.matrix_value ());
  return tmp.transpose ();
}

octave_value
octave_range::convert_to_str (void) const
{
  octave_value tmp (range.matrix_value ());
  return tmp.convert_to_str ();
}

void
octave_range::print (ostream& os, bool pr_as_read_syntax)
{
  octave_print_internal (os, range, pr_as_read_syntax, struct_indent);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
