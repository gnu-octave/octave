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

#include "defun.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-scalar.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "pr-output.h"
#include "xdiv.h"
#include "xpow.h"

template class octave_base_scalar<double>;

DEFINE_OCTAVE_ALLOCATOR (octave_scalar);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_scalar, "scalar", "double");

octave_value
octave_scalar::do_index_op (const octave_value_list& idx, int resize_ok)
{
  octave_value retval;

  if (idx.valid_scalar_indices ())
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

std::streamoff
octave_scalar::streamoff_value (void) const
{
  std::streamoff retval (-1);

  if (D_NINT (scalar) == scalar)
    retval = std::streamoff (static_cast<long> (scalar));
  else
    error ("conversion to streamoff value failed");

  return retval;
}

octave_value
octave_scalar::convert_to_str_internal (bool, bool) const
{
  octave_value retval;

  if (xisnan (scalar))
    ::error ("invalid conversion from NaN to character");
  else
    {
      int ival = NINT (scalar);

      if (ival < 0 || ival > UCHAR_MAX)
	{
	  // XXX FIXME XXX -- is there something better we could do?

	  ival = 0;

	  ::warning ("range error for conversion to character value");
	}

      retval = octave_value (std::string (1, static_cast<char> (ival)));
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
