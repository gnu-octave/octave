/*

Copyright (C) 2000 John W. Eaton

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

#if !defined (octave_re_nd_array_h)
#define octave_re_nd_array_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "ArrayN.h"
#include "oct-alloc.h"

#include "error.h"
#include "ov-base.h"
#include "ov-base-nd-array.h"
#include "ov-typeinfo.h"

class octave_value_list;

// Real N-dimensional array values.

class
octave_double_nd_array : public octave_base_nd_array<ArrayN<double> >
{
public:

  octave_double_nd_array (void)
    : octave_base_nd_array<ArrayN<double> > () { }

  octave_double_nd_array (const ArrayN<double>& a)
    : octave_base_nd_array<ArrayN<double> > (a) { }

  octave_double_nd_array (const octave_double_nd_array& a)
    : octave_base_nd_array<ArrayN<double> > (a) { }

  ~octave_double_nd_array (void) { }

  octave_value *clone (void) const { return new octave_double_nd_array (*this); }
  octave_value *empty_clone (void) const { return new octave_double_nd_array (); }

#if 0
  octave_value *try_narrowing_conversion (void);

  void assign (const octave_value_list& idx, const Matrix& rhs);

  idx_vector index_vector (void) const { return idx_vector (matrix); }

  bool is_real_matrix (void) const { return false; }

  bool is_real_type (void) const { return true; }

  bool valid_as_scalar_index (void) const;

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const
    { return ComplexMatrix (matrix_value ()); }
#endif

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
