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

#if !defined (octave_streamoff_h)
#define octave_streamoff_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <iostream>
#include <string>

#include "mx-base.h"
#include "mx-op-defs.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "error.h"
#include "oct-obj.h"
#include "ov-base-mat.h"
#include "ov-typeinfo.h"

class tree_walker;

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

  streamoff_array& operator = (const streamoff_array& a)
    {
      if (this != &a)
	ArrayN<std::streamoff>::operator = (a);

      return *this;
    }

  streamoff_array squeeze (void) const
    { return ArrayN<std::streamoff>::squeeze (); }

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  // streamoff_array& operator += (const streamoff_array& a);
  // streamoff_array& operator -= (const streamoff_array& a);

  static int compute_index (Array<int>& ra_idx,
			    const dim_vector& dimensions);

  static std::streamoff resize_fill_value (void) { return 0; }
};

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

class
octave_streamoff : public octave_base_matrix<streamoff_array>
{
public:

  octave_streamoff (void)
    : octave_base_matrix<streamoff_array> () { }

  octave_streamoff (const std::streamoff& off)
    : octave_base_matrix<streamoff_array>
        (streamoff_array (dim_vector (1, 1), off)) { }

  octave_streamoff (const streamoff_array& off)
    : octave_base_matrix<streamoff_array> (off) { }

  octave_streamoff (const octave_streamoff& off)
    : octave_base_matrix<streamoff_array> (off) { }

  ~octave_streamoff (void) { }

  octave_value *clone (void) const { return new octave_streamoff (*this); }
  octave_value *empty_clone (void) const { return new octave_streamoff (); }

  bool is_defined (void) const { return true; }

  bool is_streamoff (void) const { return true; }

  std::streamoff streamoff_value (void) const;

  streamoff_array streamoff_array_value (void) const { return matrix; }

  //  void increment (void) { matrix += 1; }

  //  void decrement (void) { matrix -= 1; }

  bool print_as_scalar (void) const { return true; }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
