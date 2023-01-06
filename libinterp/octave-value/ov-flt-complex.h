////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_ov_flt_complex_h)
#define octave_ov_flt_complex_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "lo-ieee.h"
#include "mx-base.h"
#include "str-vec.h"

#include "errwarn.h"
#include "error.h"
#include "ov-base.h"
#include "ov-flt-cx-mat.h"
#include "ov-base-scalar.h"
#include "ov-typeinfo.h"

class octave_value_list;

// Complex scalar values.

class
OCTINTERP_API
octave_float_complex : public octave_base_scalar<FloatComplex>
{
public:

  octave_float_complex (void)
    : octave_base_scalar<FloatComplex> () { }

  octave_float_complex (const FloatComplex& c)
    : octave_base_scalar<FloatComplex> (c) { }

  octave_float_complex (const octave_float_complex& c)
    : octave_base_scalar<FloatComplex> (c) { }

  ~octave_float_complex (void) = default;

  octave_base_value * clone (void) const
  { return new octave_float_complex (*this); }

  // We return an octave_float_complex_matrix object here instead of an
  // octave_float_complex object so that in expressions like A(2,2,2) = 2
  // (for A previously undefined), A will be empty instead of a 1x1
  // object.
  octave_base_value * empty_clone (void) const
  { return new octave_float_complex_matrix (); }

  octave_base_value * try_narrowing_conversion (void);

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  octave_value any (int = 0) const
  {
    return (scalar != FloatComplex (0, 0)
            && ! (lo_ieee_isnan (scalar.real ())
                  || lo_ieee_isnan (scalar.imag ())));
  }

  builtin_type_t builtin_type (void) const { return btyp_float_complex; }

  bool is_complex_scalar (void) const { return true; }

  bool iscomplex (void) const { return true; }

  bool is_single_type (void) const { return true; }

  bool isfloat (void) const { return true; }

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  float float_scalar_value (bool frc_str_conv = false) const
  { return float_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const;

  FloatMatrix float_matrix_value (bool = false) const;

  NDArray array_value (bool = false) const;

  FloatNDArray float_array_value (bool = false) const;

  SparseMatrix sparse_matrix_value (bool = false) const
  { return SparseMatrix (matrix_value ()); }

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
  { return SparseComplexMatrix (complex_matrix_value ()); }

  octave_value resize (const dim_vector& dv, bool fill = false) const;

  Complex complex_value (bool = false) const;

  FloatComplex float_complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  FloatComplexMatrix float_complex_matrix_value (bool = false) const;

  ComplexNDArray complex_array_value (bool = false) const;

  FloatComplexNDArray float_complex_array_value (bool = false) const;

  bool bool_value (bool warn = false) const
  {
    if (octave::math::isnan (scalar))
      octave::err_nan_to_logical_conversion ();
    if (warn && scalar != 0.0f && scalar != 1.0f)
      warn_logical_conversion ();

    return scalar != 0.0f;
  }

  boolNDArray bool_array_value (bool warn = false) const
  {
    if (octave::math::isnan (scalar))
      octave::err_nan_to_logical_conversion ();
    if (warn && scalar != 0.0f && scalar != 1.0f)
      warn_logical_conversion ();

    return boolNDArray (dim_vector (1, 1), scalar != 1.0f);
  }

  octave_value as_double (void) const;
  octave_value as_single (void) const;

  // We don't need to override both forms of the diag method.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_scalar<FloatComplex>::diag;

  octave_value diag (octave_idx_type m, octave_idx_type n) const;

  void increment (void) { scalar += 1.0; }

  void decrement (void) { scalar -= 1.0; }

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  int write (octave::stream& os, int block_size,
             oct_data_conv::data_type output_type, int skip,
             octave::mach_info::float_format flt_fmt) const
  {
    // Yes, for compatibility, we drop the imaginary part here.
    return os.write (array_value (true), block_size, output_type,
                     skip, flt_fmt);
  }

  mxArray * as_mxArray (bool interleaved) const;

  octave_value map (unary_mapper_t umap) const;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

typedef octave_float_complex octave_float_complex_scalar;

#endif
