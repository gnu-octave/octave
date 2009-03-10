/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2006,
              2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_scalar_h)
#define octave_scalar_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "gripes.h"
#include "ov-base.h"
#include "ov-re-mat.h"
#include "ov-base-scalar.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Real scalar values.

class
OCTINTERP_API
octave_scalar : public octave_base_scalar<double>
{
public:

  octave_scalar (void)
    : octave_base_scalar<double> (0.0) { }

  octave_scalar (double d)
    : octave_base_scalar<double> (d) { }

  octave_scalar (const octave_scalar& s)
    : octave_base_scalar<double> (s) { }

  ~octave_scalar (void) { }

  octave_base_value *clone (void) const { return new octave_scalar (*this); }

  // We return an octave_matrix here instead of an octave_scalar so
  // that in expressions like A(2,2,2) = 2 (for A previously
  // undefined), A will be empty instead of a 1x1 object.
  octave_base_value *empty_clone (void) const { return new octave_matrix (); }

  octave_value do_index_op (const octave_value_list& idx,
			    bool resize_ok = false);

  type_conv_info numeric_demotion_function (void) const;

  idx_vector index_vector (void) const { return idx_vector (scalar); }

  octave_value any (int = 0) const
    { return (scalar != 0 && ! lo_ieee_isnan (scalar)); }

  bool is_real_scalar (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool is_float_type (void) const { return true; }

  int8NDArray
  int8_array_value (void) const
    { return int8NDArray (dim_vector (1, 1), scalar); }

  int16NDArray
  int16_array_value (void) const
    { return int16NDArray (dim_vector (1, 1), scalar); }

  int32NDArray
  int32_array_value (void) const
    { return int32NDArray (dim_vector (1, 1), scalar); }

  int64NDArray
  int64_array_value (void) const
    { return int64NDArray (dim_vector (1, 1), scalar); }

  uint8NDArray
  uint8_array_value (void) const
    { return uint8NDArray (dim_vector (1, 1), scalar); }

  uint16NDArray
  uint16_array_value (void) const
    { return uint16NDArray (dim_vector (1, 1), scalar); }

  uint32NDArray
  uint32_array_value (void) const
    { return uint32NDArray (dim_vector (1, 1), scalar); }

  uint64NDArray
  uint64_array_value (void) const
    { return uint64NDArray (dim_vector (1, 1), scalar); }

#define DEFINE_INT_SCALAR_VALUE(TYPE) \
  octave_ ## TYPE \
  TYPE ## _scalar_value (void) const \
    { return octave_ ## TYPE (scalar); }

  DEFINE_INT_SCALAR_VALUE (int8)
  DEFINE_INT_SCALAR_VALUE (int16)
  DEFINE_INT_SCALAR_VALUE (int32)
  DEFINE_INT_SCALAR_VALUE (int64)
  DEFINE_INT_SCALAR_VALUE (uint8)
  DEFINE_INT_SCALAR_VALUE (uint16)
  DEFINE_INT_SCALAR_VALUE (uint32)
  DEFINE_INT_SCALAR_VALUE (uint64)

#undef DEFINE_INT_SCALAR_VALUE

  double double_value (bool = false) const { return scalar; }

  float float_value (bool = false) const { return static_cast<float> (scalar); }

  double scalar_value (bool = false) const { return scalar; }

  float float_scalar_value (bool = false) const { return static_cast<float> (scalar); }

  Matrix matrix_value (bool = false) const
    { return Matrix (1, 1, scalar); }

  FloatMatrix float_matrix_value (bool = false) const
    { return FloatMatrix (1, 1, scalar); }

  NDArray array_value (bool = false) const
    { return NDArray (dim_vector (1, 1), scalar); }

  FloatNDArray float_array_value (bool = false) const
    { return FloatNDArray (dim_vector (1, 1), scalar); }

  SparseMatrix sparse_matrix_value (bool = false) const
    { return SparseMatrix (Matrix (1, 1, scalar)); }

  // FIXME Need SparseComplexMatrix (Matrix) constructor!!!
  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
    { return SparseComplexMatrix (sparse_matrix_value ()); }

  octave_value resize (const dim_vector& dv, bool fill = false) const;

  Complex complex_value (bool = false) const { return scalar; }

  FloatComplex float_complex_value (bool = false) const { return scalar; }

  ComplexMatrix complex_matrix_value (bool = false) const
    { return  ComplexMatrix (1, 1, Complex (scalar)); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
    { return  FloatComplexMatrix (1, 1, FloatComplex (scalar)); }

  ComplexNDArray complex_array_value (bool = false) const
    { return ComplexNDArray (dim_vector (1, 1), Complex (scalar)); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
    { return FloatComplexNDArray (dim_vector (1, 1), FloatComplex (scalar)); }

  charNDArray
  char_array_value (bool = false) const
  {
    charNDArray retval (dim_vector (1, 1));
    retval(0) = static_cast<char> (scalar);
    return retval;
  }

  bool bool_value (bool warn = false) const
  {
    if (xisnan (scalar))
      error ("invalid conversion from NaN to logical");
    else if (warn && scalar != 0 && scalar != 1)
      gripe_logical_conversion ();

    return scalar;
  }

  boolNDArray bool_array_value (bool warn = false) const
  {
    if (xisnan (scalar))
      error ("invalid conversion from NaN to logical");
    else if (warn && scalar != 0 && scalar != 1)
      gripe_logical_conversion ();

    return boolNDArray (dim_vector (1, 1), scalar);
  }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void increment (void) { ++scalar; }

  void decrement (void) { --scalar; }

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
    {
      return os.write (array_value (), block_size, output_type,
		       skip, flt_fmt);
    }

  mxArray *as_mxArray (void) const;

  octave_value erf (void) const;
  octave_value erfc (void) const;
  octave_value gamma (void) const;
  octave_value lgamma (void) const;
  octave_value abs (void) const;
  octave_value acos (void) const;
  octave_value acosh (void) const;
  octave_value angle (void) const;
  octave_value arg (void) const;
  octave_value asin (void) const;
  octave_value asinh (void) const;
  octave_value atan (void) const;
  octave_value atanh (void) const;
  octave_value ceil (void) const;
  octave_value conj (void) const;
  octave_value cos (void) const;
  octave_value cosh (void) const;
  octave_value exp (void) const;
  octave_value expm1 (void) const;
  octave_value fix (void) const;
  octave_value floor (void) const;
  octave_value imag (void) const;
  octave_value log (void) const;
  octave_value log2 (void) const;
  octave_value log10 (void) const;
  octave_value log1p (void) const;
  octave_value real (void) const;
  octave_value round (void) const;
  octave_value roundb (void) const;
  octave_value signum (void) const;
  octave_value sin (void) const;
  octave_value sinh (void) const;
  octave_value sqrt (void) const;
  octave_value tan (void) const;
  octave_value tanh (void) const;
  octave_value finite (void) const;
  octave_value isinf (void) const;
  octave_value isna (void) const;
  octave_value isnan (void) const;

private:
  octave_value map (double (*fcn) (double)) const;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
