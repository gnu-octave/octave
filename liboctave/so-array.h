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

#if !defined (octave_streamoff_array_h)
#define octave_streamoff_array_h 1

#include <iostream>

#include "ArrayN.h"
#include "mx-op-defs.h"

class boolNDArray;

// Stream offsets.

class streamoff_array : public ArrayN<std::streamoff>
{
public:

  streamoff_array (void) : ArrayN<std::streamoff> () { }

  streamoff_array (const dim_vector& dv,
		   const std::streamoff& val = resize_fill_value ())
    : ArrayN<std::streamoff> (dv, val) { }

  streamoff_array (const ArrayN<std::streamoff>& sa)
    : ArrayN<std::streamoff> (sa) { }

  streamoff_array (const streamoff_array& sa)
    : ArrayN<std::streamoff> (sa) { }

  ~streamoff_array (void) { }

  streamoff_array& operator = (const streamoff_array& sa)
    {
      if (this != &sa)
	ArrayN<std::streamoff>::operator = (sa);

      return *this;
    }

  streamoff_array squeeze (void) const
    { return ArrayN<std::streamoff>::squeeze (); }

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  static int compute_index (Array<int>& ra_idx,
			    const dim_vector& dimensions);

  static std::streamoff resize_fill_value (void) { return 0; }
};

streamoff_array& operator += (streamoff_array& l, const std::streamoff& r);
streamoff_array& operator -= (streamoff_array& l, const std::streamoff& r);

streamoff_array& operator += (streamoff_array& l, const streamoff_array& r);
streamoff_array& operator -= (streamoff_array& l, const streamoff_array& r);

NDCMP_OP_DECL (mx_el_eq, std::streamoff, streamoff_array);
NDCMP_OP_DECL (mx_el_ne, std::streamoff, streamoff_array);

NDCMP_OP_DECL (mx_el_eq, streamoff_array, std::streamoff);
NDCMP_OP_DECL (mx_el_ne, streamoff_array, std::streamoff);

NDCMP_OP_DECL (mx_el_eq, streamoff_array, streamoff_array);
NDCMP_OP_DECL (mx_el_ne, streamoff_array, streamoff_array);

BIN_OP_DECL (streamoff_array, operator +, streamoff_array, streamoff_array);
BIN_OP_DECL (streamoff_array, operator -, streamoff_array, streamoff_array);

BIN_OP_DECL (streamoff_array, operator +, streamoff_array, std::streamoff);
BIN_OP_DECL (streamoff_array, operator -, streamoff_array, std::streamoff);

BIN_OP_DECL (streamoff_array, operator +, std::streamoff, streamoff_array);
BIN_OP_DECL (streamoff_array, operator -, std::streamoff, streamoff_array);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
