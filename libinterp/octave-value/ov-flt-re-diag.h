////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2024 The Octave Project Developers
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

#if ! defined (octave_ov_flt_re_diag_h)
#define octave_ov_flt_re_diag_h 1

#include "octave-config.h"

#include "ov-base.h"
#include "ov-base-diag.h"
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"

// Real diagonal matrix values.

class
OCTINTERP_API
octave_float_diag_matrix
  : public octave_base_diag<FloatDiagMatrix, FloatMatrix>
{
public:

  octave_float_diag_matrix ()
    : octave_base_diag<FloatDiagMatrix, FloatMatrix> () { }

  octave_float_diag_matrix (const FloatDiagMatrix& m)
    : octave_base_diag<FloatDiagMatrix, FloatMatrix> (m) { }

  octave_float_diag_matrix (const octave_float_diag_matrix& m)
    : octave_base_diag<FloatDiagMatrix, FloatMatrix> (m) { }

  ~octave_float_diag_matrix () = default;

  octave_base_value * clone () const
  { return new octave_float_diag_matrix (*this); }
  octave_base_value * empty_clone () const
  { return new octave_float_diag_matrix (); }

  type_conv_info numeric_conversion_function () const;

  octave_base_value * try_narrowing_conversion ();

  builtin_type_t builtin_type () const { return btyp_float; }

  bool is_real_matrix () const { return true; }

  bool isreal () const { return true; }

  bool is_single_type () const { return true; }

  bool isfloat () const { return true; }

  DiagMatrix diag_matrix_value (bool = false) const;

  FloatDiagMatrix float_diag_matrix_value (bool = false) const;

  ComplexDiagMatrix complex_diag_matrix_value (bool = false) const;

  FloatComplexDiagMatrix float_complex_diag_matrix_value (bool = false) const;

  octave_value as_double () const;
  octave_value as_single () const;

  octave_value as_int8 () const;
  octave_value as_int16 () const;
  octave_value as_int32 () const;
  octave_value as_int64 () const;

  octave_value as_uint8 () const;
  octave_value as_uint16 () const;
  octave_value as_uint32 () const;
  octave_value as_uint64 () const;

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  octave_value map (unary_mapper_t umap) const;

private:

  bool chk_valid_scalar (const octave_value&,
                         float&) const;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
