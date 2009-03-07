/*

Copyright (C) 2008, 2009 Jaroslav Hajek

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

#if !defined (octave_complex_diag_matrix_h)
#define octave_complex_diag_matrix_h 1

#include "ov-base.h"
#include "ov-base-diag.h"
#include "ov-cx-mat.h"
#include "ov-typeinfo.h"

// Real diagonal matrix values.

class
OCTINTERP_API
octave_complex_diag_matrix 
  : public octave_base_diag<ComplexDiagMatrix, ComplexMatrix>
{
public:

  octave_complex_diag_matrix (void)
    : octave_base_diag<ComplexDiagMatrix, ComplexMatrix> () { }

  octave_complex_diag_matrix (const ComplexDiagMatrix& m)
    : octave_base_diag<ComplexDiagMatrix, ComplexMatrix> (m) { }

  octave_complex_diag_matrix (const octave_complex_diag_matrix& m)
    : octave_base_diag<ComplexDiagMatrix, ComplexMatrix> (m) { }

  ~octave_complex_diag_matrix (void) { }

  octave_base_value *clone (void) const { return new octave_complex_diag_matrix (*this); }
  octave_base_value *empty_clone (void) const { return new octave_complex_diag_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  type_conv_info numeric_demotion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  bool is_complex_matrix (void) const { return true; }

  bool is_complex_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool is_float_type (void) const { return true; }

  DiagMatrix diag_matrix_value (bool = false) const;

  FloatDiagMatrix float_diag_matrix_value (bool = false) const;

  ComplexDiagMatrix complex_diag_matrix_value (bool = false) const;

  FloatComplexDiagMatrix float_complex_diag_matrix_value (bool = false) const;

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap, 
		    oct_mach_info::float_format fmt);

  octave_value abs (void) const;
  octave_value conj (void) const;
  octave_value imag (void) const;
  octave_value real (void) const;
  octave_value sqrt (void) const;

private:

  bool chk_valid_scalar (const octave_value&, 
                         Complex&) const;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
