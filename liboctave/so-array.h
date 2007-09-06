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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_streamoff_array_h)
#define octave_streamoff_array_h 1

#include <iostream>

#include "ArrayN.h"
#include "mx-op-defs.h"

class boolNDArray;

// Stream offsets.

class OCTAVE_API streamoff_array : public ArrayN<std::streamoff>
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

  octave_idx_type nnz (void) const
    {
      octave_idx_type retval = 0;

      const std::streamoff *d = this->data ();

      octave_idx_type nel = this->numel ();

      for (octave_idx_type i = 0; i < nel; i++)
	{
	  if (d[i])
	    retval++;
	}

      return retval;
    }

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
			    const dim_vector& dimensions);

  static std::streamoff resize_fill_value (void) { return 0; }
};

OCTAVE_API streamoff_array& operator += (streamoff_array& l, const std::streamoff& r);
OCTAVE_API streamoff_array& operator -= (streamoff_array& l, const std::streamoff& r);

OCTAVE_API streamoff_array& operator += (streamoff_array& l, const streamoff_array& r);
OCTAVE_API streamoff_array& operator -= (streamoff_array& l, const streamoff_array& r);

NDCMP_OP_DECL (mx_el_eq, std::streamoff, streamoff_array, OCTAVE_API);
NDCMP_OP_DECL (mx_el_ne, std::streamoff, streamoff_array, OCTAVE_API);

NDCMP_OP_DECL (mx_el_eq, streamoff_array, std::streamoff, OCTAVE_API);
NDCMP_OP_DECL (mx_el_ne, streamoff_array, std::streamoff, OCTAVE_API);

NDCMP_OP_DECL (mx_el_eq, streamoff_array, streamoff_array, OCTAVE_API);
NDCMP_OP_DECL (mx_el_ne, streamoff_array, streamoff_array, OCTAVE_API);

BIN_OP_DECL (streamoff_array, operator +, streamoff_array, streamoff_array, OCTAVE_API);
BIN_OP_DECL (streamoff_array, operator -, streamoff_array, streamoff_array, OCTAVE_API);

BIN_OP_DECL (streamoff_array, operator +, streamoff_array, std::streamoff, OCTAVE_API);
BIN_OP_DECL (streamoff_array, operator -, streamoff_array, std::streamoff, OCTAVE_API);

BIN_OP_DECL (streamoff_array, operator +, std::streamoff, streamoff_array, OCTAVE_API);
BIN_OP_DECL (streamoff_array, operator -, std::streamoff, streamoff_array, OCTAVE_API);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
