////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_ov_cx_diag_h)
#define octave_ov_cx_diag_h 1

#include "octave-config.h"

#include "ov-base.h"
#include "ov-base-diag.h"
#include "ov-cx-mat.h"
#include "ov-typeinfo.h"

// Real diagonal matrix values.

class
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

  ~octave_complex_diag_matrix (void) = default;

  octave_base_value * clone (void) const
  { return new octave_complex_diag_matrix (*this); }
  octave_base_value * empty_clone (void) const
  { return new octave_complex_diag_matrix (); }

  OCTINTERP_API type_conv_info numeric_conversion_function (void) const;

  OCTINTERP_API type_conv_info numeric_demotion_function (void) const;

  OCTINTERP_API octave_base_value * try_narrowing_conversion (void);

  builtin_type_t builtin_type (void) const { return btyp_complex; }

  bool is_complex_matrix (void) const { return true; }

  bool iscomplex (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool isfloat (void) const { return true; }

  OCTINTERP_API DiagMatrix diag_matrix_value (bool = false) const;

  OCTINTERP_API FloatDiagMatrix float_diag_matrix_value (bool = false) const;

  OCTINTERP_API ComplexDiagMatrix
  complex_diag_matrix_value (bool = false) const;

  OCTINTERP_API FloatComplexDiagMatrix
  float_complex_diag_matrix_value (bool = false) const;

  OCTINTERP_API octave_value as_double (void) const;
  OCTINTERP_API octave_value as_single (void) const;

  OCTINTERP_API bool save_binary (std::ostream& os, bool save_as_floats);

  OCTINTERP_API bool
  load_binary (std::istream& is, bool swap,
               octave::mach_info::float_format fmt);

  OCTINTERP_API octave_value map (unary_mapper_t umap) const;

private:

  OCTINTERP_API bool chk_valid_scalar (const octave_value&, Complex&) const;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
