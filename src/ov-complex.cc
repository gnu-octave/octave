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

#include "oct-obj.h"
#include "ops.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "gripes.h"
#include "pr-output.h"

octave_allocator
octave_complex::allocator (sizeof (octave_complex));

int
octave_complex::t_id (-1);

const string
octave_complex::t_name ("complex scalar");

octave_value *
octave_complex::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  if (imag (scalar) == 0.0)
    retval = new octave_scalar (::real (scalar));

  return retval;
}

static inline bool
valid_scalar_indices (const octave_value_list& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (! args(i).valid_as_scalar_index ())
      return false;

  return true;
}

octave_value
octave_complex::do_index_op (const octave_value_list& idx) const
{
  octave_value retval;

  if (valid_scalar_indices (idx))
    retval = scalar;
  else
    {
      // XXX FIXME XXX -- this doesn't solve the problem of
      //
      //   a = i; a([1,1], [1,1], [1,1])
      //
      // and similar constructions.  Hmm...

      // XXX FIXME XXX -- using this constructor avoids narrowing the
      // 1x1 matrix back to a scalar value.  Need a better solution
      // to this problem.

      octave_value tmp (new octave_complex_matrix (complex_matrix_value ()));

      retval = tmp.do_index_op (idx);
    }

  return retval;
}

double
octave_complex::double_value (bool force_conversion) const
{
  double retval = octave_NaN;

  int flag = force_conversion;

  if (! flag)
    flag = Vok_to_lose_imaginary_part;

  if (flag < 0)
    gripe_implicit_conversion ("complex scalar", "real scalar");

  if (flag)
    retval = ::real (scalar);
  else
    gripe_invalid_conversion ("complex scalar", "real scalar");

  return retval;
}

Matrix
octave_complex::matrix_value (bool force_conversion) const
{
  Matrix retval;

  int flag = force_conversion;

  if (! flag)
    flag = Vok_to_lose_imaginary_part;

  if (flag < 0)
    gripe_implicit_conversion ("complex scalar", "real matrix");

  if (flag)
    retval = Matrix (1, 1, ::real (scalar));
  else
    gripe_invalid_conversion ("complex scalar", "real matrix");

  return retval;
}

Complex
octave_complex::complex_value (bool) const
{
  return scalar;
}


ComplexMatrix
octave_complex::complex_matrix_value (bool) const
{
  return ComplexMatrix (1, 1, scalar);
}

void
octave_complex::print (ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_complex::print_raw (ostream& os, bool pr_as_read_syntax) const
{
  indent (os);
  octave_print_internal (os, scalar, pr_as_read_syntax);
}

bool
octave_complex::print_name_tag (ostream& os, const string& name) const
{
  indent (os);
  os << name << " = ";
  return false;    
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
