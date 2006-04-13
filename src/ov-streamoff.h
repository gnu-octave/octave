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

#if !defined (octave_streamoff_h)
#define octave_streamoff_h 1

#include <iostream>

#include "so-array.h"
#include "oct-alloc.h"

#include "ov.h"
#include "ov-base-mat.h"
#include "ov-typeinfo.h"

class tree_walker;

// Stream offsets.

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

  octave_base_value *clone (void) const { return new octave_streamoff (*this); }
  octave_base_value *empty_clone (void) const { return new octave_streamoff (); }

  bool is_defined (void) const { return true; }

  bool is_streamoff (void) const { return true; }

  std::streamoff streamoff_value (void) const;

  streamoff_array streamoff_array_value (void) const { return matrix; }

  void increment (void) { matrix += std::streamoff (1); }

  void decrement (void) { matrix -= std::streamoff (1); }

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
