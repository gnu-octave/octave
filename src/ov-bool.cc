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

#include "mx-base.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ops.h"
#include "ov-bool.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

octave_allocator
octave_bool::allocator (sizeof (octave_bool));

int
octave_bool::t_id (-1);

const string
octave_bool::t_name ("bool");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_bool&);

  return new octave_scalar (v.bool_value ());
}

type_conv_fcn
octave_bool::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
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
octave_bool::index (const octave_value_list& idx) const
{
  octave_value retval;

  if (valid_scalar_indices (idx))
    retval = scalar;
  else
    {
      // XXX FIXME XXX -- this doesn't solve the problem of
      //
      //   a = 1; a([1,1], [1,1], [1,1])
      //
      // and similar constructions.  Hmm...

      // XXX FIXME XXX -- using this constructor avoids narrowing the
      // 1x1 matrix back to a scalar value.  Need a better solution
      // to this problem.

      octave_value tmp (new octave_matrix (matrix_value ()));

      retval = tmp.index (idx);
    }

  return retval;
}

octave_value
octave_bool::convert_to_str (void) const
{
  char s[2];
  s[0] = (char) scalar;
  s[1] = '\0';

  return octave_value (s);
}

void
octave_bool::print (ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_bool::print_raw (ostream& os, bool pr_as_read_syntax) const
{
  indent (os);
  octave_print_internal (os, scalar, pr_as_read_syntax);
}

bool
octave_bool::print_name_tag (ostream& os, const string& name) const
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
