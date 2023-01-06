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

#if ! defined (octave_ov_flt_re_mat_h)
#define octave_ov_flt_re_mat_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-typeinfo.h"

#include "MatrixType.h"

class octave_value_list;

// Real matrix values.

class
OCTINTERP_API
octave_float_matrix : public octave_base_matrix<FloatNDArray>
{
public:

  octave_float_matrix (void)
    : octave_base_matrix<FloatNDArray> () { }

  octave_float_matrix (const FloatMatrix& m)
    : octave_base_matrix<FloatNDArray> (m) { }

  octave_float_matrix (const FloatMatrix& m, const MatrixType& t)
    : octave_base_matrix<FloatNDArray> (m, t) { }

  octave_float_matrix (const FloatNDArray& nda)
    : octave_base_matrix<FloatNDArray> (nda) { }

  octave_float_matrix (const Array<float>& m)
    : octave_base_matrix<FloatNDArray> (FloatNDArray (m)) { }

  octave_float_matrix (const FloatDiagMatrix& d)
    : octave_base_matrix<FloatNDArray> (FloatMatrix (d)) { }

  octave_float_matrix (const FloatRowVector& v)
    : octave_base_matrix<FloatNDArray> (FloatMatrix (v)) { }

  octave_float_matrix (const FloatColumnVector& v)
    : octave_base_matrix<FloatNDArray> (FloatMatrix (v)) { }

  octave_float_matrix (const octave_float_matrix& m)
    : octave_base_matrix<FloatNDArray> (m) { }

  ~octave_float_matrix (void) = default;

  octave_base_value * clone (void) const
  { return new octave_float_matrix (*this); }
  octave_base_value * empty_clone (void) const
  { return new octave_float_matrix (); }

  octave_base_value * try_narrowing_conversion (void);

  octave::idx_vector index_vector (bool /* require_integers */ = false) const
  {
    return m_idx_cache ? *m_idx_cache
           : set_idx_cache (octave::idx_vector (m_matrix));
  }

  builtin_type_t builtin_type (void) const { return btyp_float; }

  bool is_real_matrix (void) const { return true; }

  bool isreal (void) const { return true; }

  bool is_single_type (void) const { return true; }

  bool isfloat (void) const { return true; }

  int8NDArray
  int8_array_value (void) const { return int8NDArray (m_matrix); }

  int16NDArray
  int16_array_value (void) const { return int16NDArray (m_matrix); }

  int32NDArray
  int32_array_value (void) const { return int32NDArray (m_matrix); }

  int64NDArray
  int64_array_value (void) const { return int64NDArray (m_matrix); }

  uint8NDArray
  uint8_array_value (void) const { return uint8NDArray (m_matrix); }

  uint16NDArray
  uint16_array_value (void) const { return uint16NDArray (m_matrix); }

  uint32NDArray
  uint32_array_value (void) const { return uint32NDArray (m_matrix); }

  uint64NDArray
  uint64_array_value (void) const { return uint64NDArray (m_matrix); }

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  float float_scalar_value (bool frc_str_conv = false) const
  { return float_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const;

  FloatMatrix float_matrix_value (bool = false) const;

  Complex complex_value (bool = false) const;

  FloatComplex float_complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  FloatComplexMatrix float_complex_matrix_value (bool = false) const;

  ComplexNDArray complex_array_value (bool = false) const;

  FloatComplexNDArray float_complex_array_value (bool = false) const;

  boolNDArray bool_array_value (bool warn = false) const;

  charNDArray char_array_value (bool = false) const;

  NDArray array_value (bool = false) const;

  FloatNDArray float_array_value (bool = false) const { return m_matrix; }

  SparseMatrix sparse_matrix_value (bool = false) const;

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  octave_value as_double (void) const;
  octave_value as_single (void) const;

  octave_value as_int8 (void) const;
  octave_value as_int16 (void) const;
  octave_value as_int32 (void) const;
  octave_value as_int64 (void) const;

  octave_value as_uint8 (void) const;
  octave_value as_uint16 (void) const;
  octave_value as_uint32 (void) const;
  octave_value as_uint64 (void) const;

  octave_value diag (octave_idx_type k = 0) const;

  octave_value diag (octave_idx_type m, octave_idx_type n) const;

  // Use matrix_ref here to clear index cache.
  void increment (void) { matrix_ref () += 1.0; }

  void decrement (void) { matrix_ref () -= 1.0; }

  void changesign (void) { matrix_ref ().changesign (); }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

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
  { return os.write (m_matrix, block_size, output_type, skip, flt_fmt); }

  mxArray * as_mxArray (bool interleaved) const;

  octave_value map (unary_mapper_t umap) const;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
