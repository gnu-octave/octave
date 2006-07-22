/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_range_h)
#define octave_range_h 1

#include <cstdlib>

#include <iostream>
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
	if (range.nelem () < 0)
	  ::error ("invalid range");
      }

  octave_range (const octave_range& r)
    : octave_base_value (), range (r.range) { }

  ~octave_range (void) { }

  octave_base_value *clone (void) const { return new octave_range (*this); }
  octave_base_value *empty_clone (void) const { return new octave_range (); }

  type_conv_fcn numeric_conversion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string&,
			     const std::list<octave_value_list>&, int)
    {
      panic_impossible ();
      return octave_value_list ();
    }

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

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_range (void) const { return true; }

  octave_value all (int dim = 0) const;

  octave_value any (int dim = 0) const;

  bool is_real_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool valid_as_scalar_index (void) const
    {
      double b = range.base ();
      return (range.nelem () == 1
	      && ! xisnan (b) && D_NINT (b) == b && NINTbig (b) == 1);
    }

  bool valid_as_zero_index (void) const
    {
      double b = range.base ();
      return (range.nelem () == 1
	      && ! xisnan (b) && D_NINT (b) == b && NINTbig (b) == 0);
    }

  bool is_numeric_type (void) const { return true; }

  bool is_true (void) const;

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const
    { return range.matrix_value (); }

  NDArray array_value (bool = false) const
    { return range.matrix_value (); }

  Complex complex_value (bool = false) const;

  boolNDArray bool_array_value (void) const
    { return boolNDArray (range.matrix_value ()); }

  ComplexMatrix complex_matrix_value (bool = false) const
    { return ComplexMatrix (range.matrix_value ()); }

  ComplexNDArray complex_array_value (bool = false) const
    { return ComplexMatrix (range.matrix_value ()); }

  Range range_value (void) const { return range; }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  bool save_ascii (std::ostream& os, bool& infnan_warned,
		 bool strip_nan_and_inf);

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
