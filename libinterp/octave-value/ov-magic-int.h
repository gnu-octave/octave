////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020-2023 The Octave Project Developers
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

#if ! defined (octave_ov_magic_int_h)
#define octave_ov_magic_int_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

#include "oct-inttypes-fwd.h"

#include "ov-base.h"
#include "ov-re-mat.h"
#include "ov-base-scalar.h"
#include "ov-typeinfo.h"

class octave_value_list;

// Large integer scalar values.  The uint64 or int64 value they contain may be
// accessed without loss of precision when needed (for example, when
// directly converted to a uint64 or int64 value).  Otherwise, they
// behave like real scalars, so any operation on them will result in
// type conversion.

template <typename T>
class
octave_base_magic_int : public octave_base_scalar<T>
{
public:

  octave_base_magic_int (void)
    : octave_base_scalar<T> (0) { }

  octave_base_magic_int (const T& val)
    : octave_base_scalar<T> (val) { }

  ~octave_base_magic_int (void) = default;

  // We return an octave_matrix here instead of an octave_scalar so
  // that in expressions like A(2,2,2) = 2 (for A previously
  // undefined), A will be empty instead of a 1x1 object.
  octave_base_value * empty_clone (void) const { return new octave_matrix (); }

  // Although SCALAR is a protected member of the base class, it is not
  // directly visible here without the explicit octave_base_slalar<T>::
  // qualification.  Why not?

  const T& scalar_ref (void) const { return octave_base_scalar<T>::scalar; }

  T& scalar_ref (void) { return octave_base_scalar<T>::scalar; }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  octave::idx_vector index_vector (bool require_integers = false) const;

  octave_value any (int = 0) const { return scalar_ref () != T (0); }

  builtin_type_t builtin_type (void) const { return btyp_double; }

  bool is_storable (void) const { return false; }

  bool is_magic_int (void) const { return true; }

  bool is_real_scalar (void) const { return true; }

  bool isreal (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool isfloat (void) const { return true; }

  int8NDArray int8_array_value (void) const
  { return int8NDArray (dim_vector (1, 1), double_value ()); }

  int16NDArray int16_array_value (void) const
  { return int16NDArray (dim_vector (1, 1), double_value ()); }

  int32NDArray int32_array_value (void) const
  { return int32NDArray (dim_vector (1, 1), double_value ()); }

  int64NDArray int64_array_value (void) const
  { return int64NDArray (dim_vector (1, 1), double_value ()); }

  uint8NDArray uint8_array_value (void) const
  { return uint8NDArray (dim_vector (1, 1), double_value ()); }

  uint16NDArray uint16_array_value (void) const
  { return uint16NDArray (dim_vector (1, 1), double_value ()); }

  uint32NDArray uint32_array_value (void) const
  { return uint32NDArray (dim_vector (1, 1), double_value ()); }

  uint64NDArray uint64_array_value (void) const
  { return uint64NDArray (dim_vector (1, 1), double_value ()); }

  octave_int8 int8_scalar_value (void) const
  { return octave_int8 (double_value ()); }

  octave_int16 int16_scalar_value (void) const
  { return octave_int16 (double_value ()); }

  octave_int32 int32_scalar_value (void) const
  { return octave_int32 (double_value ()); }

  octave_int64 int64_scalar_value (void) const
  { return octave_int64 (double_value ()); }

  octave_uint8 uint8_scalar_value (void) const
  { return octave_uint8 (double_value ()); }

  octave_uint16 uint16_scalar_value (void) const
  { return octave_uint16 (double_value ()); }

  octave_uint32 uint32_scalar_value (void) const
  { return octave_uint32 (double_value ()); }

  octave_uint64 uint64_scalar_value (void) const
  { return octave_uint64 (double_value ()); }

  double double_value (bool = false) const
  {
    return scalar_ref ().double_value ();
  }

  float float_value (bool = false) const
  { return static_cast<float> (double_value ()); }

  double scalar_value (bool = false) const
  { return double_value (); }

  float float_scalar_value (bool = false) const
  { return float_value (); }

  Matrix matrix_value (bool = false) const
  { return Matrix (1, 1, double_value ()); }

  FloatMatrix float_matrix_value (bool = false) const
  { return FloatMatrix (1, 1, float_value ()); }

  NDArray array_value (bool = false) const
  { return NDArray (dim_vector (1, 1), double_value ()); }

  FloatNDArray float_array_value (bool = false) const
  { return FloatNDArray (dim_vector (1, 1), float_value ()); }

  SparseMatrix sparse_matrix_value (bool = false) const
  { return SparseMatrix (Matrix (1, 1, double_value ())); }

  // FIXME: Need SparseComplexMatrix (Matrix) constructor!
  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
  { return SparseComplexMatrix (sparse_matrix_value ()); }

  octave_value resize (const dim_vector& dv, bool fill = false) const;

  Complex complex_value (bool = false) const { return double_value (); }

  FloatComplex float_complex_value (bool = false) const
  { return FloatComplex (float_value ()); }

  ComplexMatrix complex_matrix_value (bool = false) const
  { return ComplexMatrix (1, 1, Complex (double_value ())); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
  { return FloatComplexMatrix (1, 1, FloatComplex (float_value ())); }

  ComplexNDArray complex_array_value (bool = false) const
  { return ComplexNDArray (dim_vector (1, 1), Complex (double_value ())); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
  {
    return FloatComplexNDArray (dim_vector (1, 1),
                                FloatComplex (float_value ()));
  }

  charNDArray
  char_array_value (bool = false) const
  {
    charNDArray retval (dim_vector (1, 1));
    retval(0) = static_cast<char> (double_value ());
    return retval;
  }

  bool bool_value (bool warn = false) const
  {
    if (warn && scalar_ref () != T (0) && scalar_ref () != T (1))
      warn_logical_conversion ();

    return double_value ();
  }

  boolNDArray bool_array_value (bool warn = false) const
  {
    if (warn && scalar_ref () != T (0) && scalar_ref () != T (1))
      warn_logical_conversion ();

    return boolNDArray (dim_vector (1, 1), double_value ());
  }

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

  // We don't need to override both forms of the diag method.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_scalar<T>::diag;

  octave_value diag (octave_idx_type m, octave_idx_type n) const;

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void increment (void) { scalar_ref () += T (1); }

  void decrement (void) { scalar_ref () -= T (1); }

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
    return os.write (array_value (), block_size, output_type,
                     skip, flt_fmt);
  }

  mxArray * as_mxArray (bool interleaved) const;

  octave_value map (octave_base_value::unary_mapper_t umap) const;
};

class
OCTINTERP_API
octave_magic_uint : public octave_base_magic_int<octave_uint64>
{
public:

  octave_magic_uint (void)
    : octave_base_magic_int<octave_uint64> (0) { }

  octave_magic_uint (const octave_uint64& val)
    : octave_base_magic_int<octave_uint64> (val) { }

  ~octave_magic_uint (void) = default;

  octave_base_value * clone (void) const
  {
    return new octave_magic_uint (*this);
  }

  type_conv_info numeric_conversion_function (void) const;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

class
OCTINTERP_API
octave_magic_int : public octave_base_magic_int<octave_int64>
{
public:

  octave_magic_int (void)
    : octave_base_magic_int<octave_int64> (0) { }

  octave_magic_int (const octave_int64& val)
    : octave_base_magic_int<octave_int64> (val) { }

  ~octave_magic_int (void) = default;

  octave_base_value * clone (void) const
  {
    return new octave_magic_int (*this);
  }

  type_conv_info numeric_conversion_function (void) const;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
