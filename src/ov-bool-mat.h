/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2006,
              2007, 2008 John W. Eaton

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

#if !defined (octave_bool_matrix_h)
#define octave_bool_matrix_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "oct-alloc.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"

#include "MatrixType.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Character matrix values.

class
octave_bool_matrix : public octave_base_matrix<boolNDArray>
{
public:

  octave_bool_matrix (void)
    : octave_base_matrix<boolNDArray> () { }

  octave_bool_matrix (const boolNDArray& bnda)
    : octave_base_matrix<boolNDArray> (bnda) { }

  octave_bool_matrix (const boolMatrix& bm)
    : octave_base_matrix<boolNDArray> (bm) { }

  octave_bool_matrix (const boolMatrix& bm, const MatrixType& t)
    : octave_base_matrix<boolNDArray> (bm, t) { }

  octave_bool_matrix (const octave_bool_matrix& bm)
    : octave_base_matrix<boolNDArray> (bm) { }

  ~octave_bool_matrix (void) { }

  octave_base_value *clone (void) const { return new octave_bool_matrix (*this); }
  octave_base_value *empty_clone (void) const { return new octave_bool_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  idx_vector index_vector (void) const { return idx_vector (matrix); }

  bool is_bool_matrix (void) const { return true; }

  bool is_bool_type (void) const { return true; }

  bool is_real_type (void) const { return true; }

  int8NDArray
  int8_array_value (void) const { return int8NDArray (matrix); }

  int16NDArray
  int16_array_value (void) const { return int16NDArray (matrix); }

  int32NDArray
  int32_array_value (void) const { return int32NDArray (matrix); }

  int64NDArray
  int64_array_value (void) const { return int64NDArray (matrix); }

  uint8NDArray
  uint8_array_value (void) const { return uint8NDArray (matrix); }

  uint16NDArray
  uint16_array_value (void) const { return uint16NDArray (matrix); }

  uint32NDArray
  uint32_array_value (void) const { return uint32NDArray (matrix); }

  uint64NDArray
  uint64_array_value (void) const { return uint64NDArray (matrix); }

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const
    { return Matrix (matrix.matrix_value ()); }

  FloatMatrix float_matrix_value (bool = false) const
    { return FloatMatrix (matrix.matrix_value ()); }

  NDArray array_value (bool = false) const
    { return NDArray (matrix); }

  FloatNDArray float_array_value (bool = false) const
    { return FloatNDArray (matrix); }

  Complex complex_value (bool = false) const;

  FloatComplex float_complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const
    { return ComplexMatrix (matrix.matrix_value ( )); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
    { return FloatComplexMatrix (matrix.matrix_value ( )); }

  ComplexNDArray complex_array_value (bool = false) const
    { return ComplexNDArray (matrix); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
    { return FloatComplexNDArray (matrix); }

  charNDArray
  char_array_value (bool = false) const
  {
    charNDArray retval (dims ());

    octave_idx_type nel = numel ();
  
    for (octave_idx_type i = 0; i < nel; i++)
      retval(i) = static_cast<char>(matrix(i));

    return retval;
  }

  boolMatrix bool_matrix_value (bool = false) const
    { return matrix.matrix_value (); }

  boolNDArray bool_array_value (bool = false) const
    { return matrix; }

  SparseMatrix sparse_matrix_value (bool = false) const
  { return SparseMatrix (Matrix (matrix.matrix_value ())); }

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
  { return SparseComplexMatrix (ComplexMatrix (matrix.matrix_value ())); }

  SparseBoolMatrix sparse_bool_matrix_value (bool = false) const
  { return SparseBoolMatrix (matrix.matrix_value ()); }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap, 
		    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  bool save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (hid_t loc_id, const char *name, bool have_h5giterate_bug);
#endif

  int write (octave_stream& os, int block_size,
	     oct_data_conv::data_type output_type, int skip,
	     oct_mach_info::float_format flt_fmt) const
    { return os.write (matrix, block_size, output_type, skip, flt_fmt); }

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return matrix.mex_get_data (); }

  mxArray *as_mxArray (void) const;

  // Mapper functions are converted to double for treatment
#define BOOL_MAT_MAPPER(MAP) \
  octave_value MAP (void) const \
    { \
      octave_matrix m (array_value ()); \
      return m.MAP (); \
    }

  BOOL_MAT_MAPPER (abs)
  BOOL_MAT_MAPPER (acos)
  BOOL_MAT_MAPPER (acosh)
  BOOL_MAT_MAPPER (angle)
  BOOL_MAT_MAPPER (arg)
  BOOL_MAT_MAPPER (asin)
  BOOL_MAT_MAPPER (asinh)
  BOOL_MAT_MAPPER (atan)
  BOOL_MAT_MAPPER (atanh)
  BOOL_MAT_MAPPER (ceil)
  BOOL_MAT_MAPPER (conj)
  BOOL_MAT_MAPPER (cos)
  BOOL_MAT_MAPPER (cosh)
  BOOL_MAT_MAPPER (erf)
  BOOL_MAT_MAPPER (erfc)
  BOOL_MAT_MAPPER (exp)
  BOOL_MAT_MAPPER (expm1)
  BOOL_MAT_MAPPER (finite)
  BOOL_MAT_MAPPER (fix)
  BOOL_MAT_MAPPER (floor)
  BOOL_MAT_MAPPER (gamma)
  BOOL_MAT_MAPPER (imag)
  BOOL_MAT_MAPPER (isinf)
  BOOL_MAT_MAPPER (isna)
  BOOL_MAT_MAPPER (isnan)
  BOOL_MAT_MAPPER (lgamma)
  BOOL_MAT_MAPPER (log)
  BOOL_MAT_MAPPER (log2)
  BOOL_MAT_MAPPER (log10)
  BOOL_MAT_MAPPER (log1p)
  BOOL_MAT_MAPPER (real)
  BOOL_MAT_MAPPER (round)
  BOOL_MAT_MAPPER (roundb)
  BOOL_MAT_MAPPER (signum)
  BOOL_MAT_MAPPER (sin)
  BOOL_MAT_MAPPER (sinh)
  BOOL_MAT_MAPPER (sqrt)
  BOOL_MAT_MAPPER (tan)
  BOOL_MAT_MAPPER (tanh)

#undef BOOL_MAT_MAPPER

protected:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
