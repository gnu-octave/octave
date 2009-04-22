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

#if !defined (octave_range_h)
#define octave_range_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "Range.h"

#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Range values.

class
octave_range : public octave_base_value
{
public:

  octave_range (void)
    : octave_base_value () { }

  octave_range (double base, double limit, double inc)
    : octave_base_value (), range (base, limit, inc)
      {
	if (range.nelem () < 0)
	  ::error ("invalid range");
      }

  octave_range (const Range& r)
    : octave_base_value (), range (r)
      {
	if (range.nelem () < 0 && range.nelem () != -2)
	  ::error ("invalid range");
      }

  octave_range (const octave_range& r)
    : octave_base_value (), range (r.range) { }

  ~octave_range (void) { }

  octave_base_value *clone (void) const { return new octave_range (*this); }

  // A range is really just a special kind of real matrix object.  In
  // the places where we need to call empty_clone, it makes more sense
  // to create an empty matrix (0x0) instead of an empty range (1x0).
  octave_base_value *empty_clone (void) const { return new octave_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx, int)
    { return subsref (type, idx); }

  octave_value do_index_op (const octave_value_list& idx,
			    bool resize_ok = false);

  idx_vector index_vector (void) const { return idx_vector (range); }

  dim_vector dims (void) const
    { 
      octave_idx_type n = range.nelem ();
      return dim_vector (n > 0, n);
    }

  octave_value resize (const dim_vector& dv, bool fill = false) const;


  size_t byte_size (void) const { return 3 * sizeof (double); }

  octave_value reshape (const dim_vector& new_dims) const
    { return NDArray (array_value().reshape (new_dims)); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
    { return NDArray (array_value().permute (vec, inv)); }

  octave_value squeeze (void) const { return range; }

  octave_value full_value (void) const { return range.matrix_value (); }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_range (void) const { return true; }

  octave_value all (int dim = 0) const;

  octave_value any (int dim = 0) const;

  octave_value diag (octave_idx_type k = 0) const;

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
    { return range.sort (dim, mode); }

  octave_value sort (Array<octave_idx_type>& sidx, octave_idx_type dim = 0,
		     sortmode mode = ASCENDING) const
    { return range.sort (sidx, dim, mode); }

  sortmode is_sorted (sortmode mode = UNSORTED) const
    { return range.is_sorted (mode); }

  Array<octave_idx_type> sort_rows_idx (sortmode) const
    { return Array<octave_idx_type> (1, 0); }

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const
    { return mode ? mode : ASCENDING; }

  bool is_real_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool is_float_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool is_true (void) const;

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  float float_scalar_value (bool frc_str_conv = false) const
    { return float_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const
    { return range.matrix_value (); }

  FloatMatrix float_matrix_value (bool = false) const
    { return range.matrix_value (); }

  NDArray array_value (bool = false) const
    { return range.matrix_value (); }

  FloatNDArray float_array_value (bool = false) const
    { return FloatMatrix (range.matrix_value ()); }

  charNDArray char_array_value (bool = false) const;
  
  // FIXME -- it would be better to have Range::intXNDArray_value
  // functions to avoid the intermediate conversion to a matrix
  // object.

  int8NDArray
  int8_array_value (void) const { return int8NDArray (array_value ()); }

  int16NDArray
  int16_array_value (void) const { return int16NDArray (array_value ()); }

  int32NDArray
  int32_array_value (void) const { return int32NDArray (array_value ()); }

  int64NDArray
  int64_array_value (void) const { return int64NDArray (array_value ()); }

  uint8NDArray
  uint8_array_value (void) const { return uint8NDArray (array_value ()); }

  uint16NDArray
  uint16_array_value (void) const { return uint16NDArray (array_value ()); }

  uint32NDArray
  uint32_array_value (void) const { return uint32NDArray (array_value ()); }

  uint64NDArray
  uint64_array_value (void) const { return uint64NDArray (array_value ()); }

  SparseMatrix sparse_matrix_value (bool = false) const
    { return SparseMatrix (range.matrix_value ()); }

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
    { return SparseComplexMatrix (sparse_matrix_value ()); }

  Complex complex_value (bool = false) const;

  FloatComplex float_complex_value (bool = false) const;

  boolNDArray bool_array_value (bool warn = false) const
  {
    Matrix m = range.matrix_value ();

    if (m.any_element_is_nan ())
      error ("invalid conversion from NaN to logical");
    else if (warn && m.any_element_not_one_or_zero ())
      gripe_logical_conversion ();

    return boolNDArray (m);
  }

  ComplexMatrix complex_matrix_value (bool = false) const
    { return ComplexMatrix (range.matrix_value ()); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
    { return FloatComplexMatrix (range.matrix_value ()); }

  ComplexNDArray complex_array_value (bool = false) const
    { return ComplexMatrix (range.matrix_value ()); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
    { return FloatComplexMatrix (range.matrix_value ()); }

  Range range_value (void) const { return range; }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

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
      // FIXME -- could be more memory efficient by having a
      // special case of the octave_stream::write method for ranges.

      return os.write (matrix_value (), block_size, output_type, skip,
		       flt_fmt);
    }

  mxArray *as_mxArray (void) const;

  // Mapper functions are converted to double for treatment
#define RANGE_MAPPER(MAP) \
  octave_value MAP (void) const \
    { \
      octave_matrix m (array_value ()); \
      return m.MAP (); \
    }

  RANGE_MAPPER (abs)
  RANGE_MAPPER (acos)
  RANGE_MAPPER (acosh)
  RANGE_MAPPER (angle)
  RANGE_MAPPER (arg)
  RANGE_MAPPER (asin)
  RANGE_MAPPER (asinh)
  RANGE_MAPPER (atan)
  RANGE_MAPPER (atanh)
  RANGE_MAPPER (ceil)
  RANGE_MAPPER (conj)
  RANGE_MAPPER (cos)
  RANGE_MAPPER (cosh)
  RANGE_MAPPER (erf)
  RANGE_MAPPER (erfc)
  RANGE_MAPPER (exp)
  RANGE_MAPPER (expm1)
  RANGE_MAPPER (finite)
  RANGE_MAPPER (fix)
  RANGE_MAPPER (floor)
  RANGE_MAPPER (gamma)
  RANGE_MAPPER (imag)
  RANGE_MAPPER (isinf)
  RANGE_MAPPER (isna)
  RANGE_MAPPER (isnan)
  RANGE_MAPPER (lgamma)
  RANGE_MAPPER (log)
  RANGE_MAPPER (log2)
  RANGE_MAPPER (log10)
  RANGE_MAPPER (log1p)
  RANGE_MAPPER (real)
  RANGE_MAPPER (round)
  RANGE_MAPPER (roundb)
  RANGE_MAPPER (signum)
  RANGE_MAPPER (sin)
  RANGE_MAPPER (sinh)
  RANGE_MAPPER (sqrt)
  RANGE_MAPPER (tan)
  RANGE_MAPPER (tanh)

private:

  Range range;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
