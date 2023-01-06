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

#if ! defined (octave_ov_perm_h)
#define octave_ov_perm_h 1

#include "octave-config.h"

#include "mx-base.h"
#include "str-vec.h"

#include "ov-base.h"
#include "ov-typeinfo.h"
#include "ovl.h"

class
OCTINTERP_API
octave_perm_matrix : public octave_base_value
{
public:
  octave_perm_matrix (void) : m_matrix (), m_dense_cache () { }

  octave_perm_matrix (const PermMatrix& p) : m_matrix (p), m_dense_cache () { }

  octave_base_value * clone (void) const
  { return new octave_perm_matrix (*this); }
  octave_base_value * empty_clone (void) const
  { return new octave_perm_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_base_value * try_narrowing_conversion (void);

  std::size_t byte_size (void) const { return m_matrix.byte_size (); }

  octave_value squeeze (void) const { return m_matrix; }

  octave_value full_value (void) const { return to_dense (); }

  // We don't need to override all three forms of subsref.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_value::subsref;

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx, int)
  { return subsref (type, idx); }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  dim_vector dims (void) const { return m_matrix.dims (); }

  octave_idx_type nnz (void) const { return m_matrix.rows (); }

  octave_value reshape (const dim_vector& new_dims) const
  { return to_dense ().reshape (new_dims); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
  { return to_dense ().permute (vec, inv); }

  octave_value resize (const dim_vector& dv, bool fill = false) const
  { return to_dense ().resize (dv, fill); }

  octave_value all (int dim = 0) const { return to_dense ().all (dim); }
  octave_value any (int dim = 0) const { return to_dense ().any (dim); }

  MatrixType matrix_type (void) const { return MatrixType::Permuted_Diagonal; }
  MatrixType matrix_type (const MatrixType&) const
  { return matrix_type (); }

  // We don't need to override both forms of the diag method.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_value::diag;

  octave_value diag (octave_idx_type k = 0) const
  { return to_dense () .diag (k); }

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
  { return to_dense ().sort (dim, mode); }
  octave_value sort (Array<octave_idx_type>& sidx, octave_idx_type dim = 0,
                     sortmode mode = ASCENDING) const
  { return to_dense ().sort (sidx, dim, mode); }

  sortmode issorted (sortmode mode = UNSORTED) const
  { return to_dense ().issorted (mode); }

  Array<octave_idx_type> sort_rows_idx (sortmode mode = ASCENDING) const
  { return to_dense ().sort_rows_idx (mode); }

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const
  { return to_dense ().is_sorted_rows (mode); }

  builtin_type_t builtin_type (void) const { return btyp_double; }

  bool is_perm_matrix (void) const { return true; }

  bool is_matrix_type (void) const { return true; }

  bool isnumeric (void) const { return true; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_real_matrix (void) const { return true; }

  bool isreal (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool isfloat (void) const { return true; }

  bool is_true (void) const;

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  octave::idx_vector index_vector (bool require_integers = false) const;

  PermMatrix perm_matrix_value (void) const
  { return m_matrix; }

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

  FloatNDArray float_array_value (bool = false) const;

  SparseMatrix sparse_matrix_value (bool = false) const;

  SparseBoolMatrix sparse_bool_matrix_value (bool = false) const;

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  int8NDArray
  int8_array_value (void) const { return to_dense ().int8_array_value (); }

  int16NDArray
  int16_array_value (void) const { return to_dense ().int16_array_value (); }

  int32NDArray
  int32_array_value (void) const { return to_dense ().int32_array_value (); }

  int64NDArray
  int64_array_value (void) const { return to_dense ().int64_array_value (); }

  uint8NDArray
  uint8_array_value (void) const { return to_dense ().uint8_array_value (); }

  uint16NDArray
  uint16_array_value (void) const { return to_dense ().uint16_array_value (); }

  uint32NDArray
  uint32_array_value (void) const { return to_dense ().uint32_array_value (); }

  uint64NDArray
  uint64_array_value (void) const { return to_dense ().uint64_array_value (); }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

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

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  float_display_format get_edit_display_format (void) const;

  std::string edit_display (const float_display_format& fmt,
                            octave_idx_type i, octave_idx_type j) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  int write (octave::stream& os, int block_size,
             oct_data_conv::data_type output_type, int skip,
             octave::mach_info::float_format flt_fmt) const;

  mxArray * as_mxArray (bool interleaved) const;

  bool print_as_scalar (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_info (std::ostream& os, const std::string& prefix) const;

  void short_disp (std::ostream& os) const;

  octave_value map (unary_mapper_t umap) const
  { return to_dense ().map (umap); }

  octave_value fast_elem_extract (octave_idx_type n) const;

protected:

  PermMatrix m_matrix;

  virtual octave_value to_dense (void) const;

  mutable octave_value m_dense_cache;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
