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

#if !defined (octave_bool_matrix_h)
#define octave_bool_matrix_h 1

#if defined (__GNUG__) && ! defined (NO_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "oct-alloc.h"

#include "error.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Character matrix values.

class
octave_bool_matrix : public octave_base_matrix<boolMatrix>
{
public:

  octave_bool_matrix (void)
    : octave_base_matrix<boolMatrix> () { }

  octave_bool_matrix (const boolMatrix& bm)
    : octave_base_matrix<boolMatrix> (bm) { }

  octave_bool_matrix (const Array2<bool>& a)
    : octave_base_matrix<boolMatrix> (a) { }

  octave_bool_matrix (const octave_bool_matrix& bm)
    : octave_base_matrix<boolMatrix> (bm) { }

  ~octave_bool_matrix (void) { }

  octave_value *clone (void) const { return new octave_bool_matrix (*this); }
  octave_value *empty_clone (void) const { return new octave_bool_matrix (); }

  type_conv_fcn numeric_conversion_function (void) const;

  octave_value *try_narrowing_conversion (void);

  idx_vector index_vector (void) const { return idx_vector (matrix); }

  bool is_bool_matrix (void) const { return true; }

  bool is_bool_type (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool valid_as_scalar_index (void) const;

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const { return Matrix (matrix); }

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const
    { return ComplexMatrix (matrix); }

  boolMatrix bool_matrix_value (void) const
    { return matrix; }

  octave_value convert_to_str (void) const
    { return octave_value (matrix); }

protected:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
