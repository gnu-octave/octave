/*

Copyright (C) 2003 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "ArrayN.h"
#include "Array-util.h"
// XXX FIXME XXX -- we are including the MArray{,2,N}.h files just for
// their gripe_nonconformant function decls.
#include "MArray.h"
#include "MArray2.h"
#include "MArrayN.h"
#include "MArray-defs.h"
#include "boolMatrix.h"
#include "boolNDArray.h"
#include "mx-op-defs.h"
#include "so-array.h"

boolNDArray
streamoff_array::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (MX_ND_ALL_EXPR), true);
}

boolNDArray
streamoff_array::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ANY_EVAL (MX_ND_ANY_EXPR), false);
}

streamoff_array&
operator += (streamoff_array& a, const std::streamoff& s)
{
  DO_VS_OP2 (std::streamoff, a, +=, s)
  return a;
}

streamoff_array&
operator -= (streamoff_array& a, const std::streamoff& s)
{
  DO_VS_OP2 (std::streamoff, a, -=, s);
  return a;
}

streamoff_array&
operator += (streamoff_array& a, const streamoff_array& b)
{
  int l = a.length ();
  if (l > 0)
    {
      int bl = b.length ();
      if (l != bl)
	gripe_nonconformant ("operator +=", l, bl);
      else
	DO_VV_OP2 (std::streamoff, a, +=, b);
    }
  return a;
}

streamoff_array&
operator -= (streamoff_array& a, const streamoff_array& b)
{
  int l = a.length ();
  if (l > 0)
    {
      int bl = b.length ();
      if (l != bl)
	gripe_nonconformant ("operator -=", l, bl);
      else
	DO_VV_OP2 (std::streamoff, a, -=, b);
    }
  return a;
}

int
streamoff_array::compute_index (Array<int>& ra_idx,
				const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

SND_CMP_OP (mx_el_eq, ==, std::streamoff, , streamoff_array, )
SND_CMP_OP (mx_el_ne, !=, std::streamoff, , streamoff_array, )

NDS_CMP_OP (mx_el_eq, ==, streamoff_array, , std::streamoff, )
NDS_CMP_OP (mx_el_ne, !=, streamoff_array, , std::streamoff, )

NDND_CMP_OP (mx_el_eq, ==, streamoff_array, , streamoff_array, )
NDND_CMP_OP (mx_el_ne, !=, streamoff_array, , streamoff_array, )

NDND_BIN_OP (streamoff_array, operator +,
	     streamoff_array, streamoff_array, mx_inline_add)

NDND_BIN_OP (streamoff_array, operator -,
	     streamoff_array, streamoff_array, mx_inline_subtract)


NDS_BIN_OP (streamoff_array, operator +,
	    streamoff_array, std::streamoff, mx_inline_add)

NDS_BIN_OP (streamoff_array, operator -,
	    streamoff_array, std::streamoff, mx_inline_subtract)


SND_BIN_OP (streamoff_array, operator +,
	    std::streamoff, streamoff_array, mx_inline_add)

SND_BIN_OP (streamoff_array, operator -,
	    std::streamoff, streamoff_array, mx_inline_subtract)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
