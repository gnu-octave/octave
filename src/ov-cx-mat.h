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

#if !defined (octave_complex_matrix_h)
#define octave_complex_matrix_h 1

#if defined (__GNUG__) && ! defined (NO_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Complex matrix values.

class
octave_complex_matrix : public octave_base_matrix<ComplexMatrix>
{
public:

  octave_complex_matrix (void)
    : octave_base_matrix<ComplexMatrix> () { }

  octave_complex_matrix (const ComplexMatrix& m)
    : octave_base_matrix<ComplexMatrix> (m) { }

  octave_complex_matrix (const ComplexDiagMatrix& d)
    : octave_base_matrix<ComplexMatrix> (ComplexMatrix (d)) { }

  octave_complex_matrix (const ComplexRowVector& v)
    : octave_base_matrix<ComplexMatrix> (ComplexMatrix (v)) { }

  octave_complex_matrix (const ComplexColumnVector& v)
    : octave_base_matrix<ComplexMatrix> (ComplexMatrix (v)) { }

  octave_complex_matrix (const octave_complex_matrix& cm)
    : octave_base_matrix<ComplexMatrix> (cm) { }

  ~octave_complex_matrix (void) { }

  octave_value *clone (void) const { return new octave_complex_matrix (*this); }
  octave_value *empty_clone (void) const { return new octave_complex_matrix (); }

  octave_value *try_narrowing_conversion (void);

  void assign (const octave_value_list& idx, const ComplexMatrix& rhs)
    { octave_base_matrix<ComplexMatrix>::assign (idx, rhs); }

  void assign (const octave_value_list& idx, const Matrix& rhs);

  bool is_complex_matrix (void) const { return true; }

  bool is_complex_type (void) const { return true; }

  bool valid_as_scalar_index (void) const;

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  void increment (void) { matrix += Complex (1.0); }

  void decrement (void) { matrix -= Complex (1.0); }

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
