/*

Copyright (C) 2008 Jaroslav Hajek

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_float_perm_matrix_h)
#define octave_float_perm_matrix_h 1

#include "ov-perm.h"
#include "ov-perm.h"

class 
OCTINTERP_API
octave_float_perm_matrix : public octave_perm_matrix
{
public:
  octave_float_perm_matrix (void) : octave_perm_matrix () { }

  octave_float_perm_matrix (const PermMatrix& p) : octave_perm_matrix (p) { }

  octave_base_value *clone (void) const { return new octave_float_perm_matrix (*this); }
  octave_base_value *empty_clone (void) const { return new octave_float_perm_matrix (); }

  bool is_double_type (void) const { return false; }

  bool is_single_type (void) const { return true; }

  type_conv_info numeric_conversion_function (void) const;

  type_conv_info numeric_demotion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

protected:

  virtual octave_value to_dense (void) const;

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
