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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "mx-base.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ops.h"
#include "ov-bool.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

template class octave_base_scalar<bool>;

DEFINE_OCTAVE_ALLOCATOR (octave_bool);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_bool, "bool");

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
octave_bool::do_index_op (const octave_value_list& idx, int resize_ok)
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

      retval = tmp.do_index_op (idx, resize_ok);
    }

  return retval;
}

octave_value
octave_bool::convert_to_str (bool) const
{
  char s[2];
  s[0] = static_cast<char> (scalar);
  s[1] = '\0';

  return octave_value (s);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
