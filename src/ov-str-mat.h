/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_char_matrix_str_h)
#define octave_char_matrix_str_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-stream.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-ch-mat.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Character matrix values with special properties for use as
// strings.

class
OCTINTERP_API
octave_char_matrix_str : public octave_char_matrix
{
public:

  octave_char_matrix_str (void)
    : octave_char_matrix () { }

  octave_char_matrix_str (const charMatrix& chm)
    : octave_char_matrix (chm) { }

  octave_char_matrix_str (const charNDArray& chm)
    : octave_char_matrix (chm) { }

  octave_char_matrix_str (char c)
    : octave_char_matrix (c) { }

  octave_char_matrix_str (const char *s)
    : octave_char_matrix (s) { }

  octave_char_matrix_str (const std::string& s)
    : octave_char_matrix (s) { }

  octave_char_matrix_str (const string_vector& s)
    : octave_char_matrix (s) { }

  octave_char_matrix_str (const octave_char_matrix& chm)
    : octave_char_matrix (chm) { }

  octave_char_matrix_str (const octave_char_matrix_str& chms)
    : octave_char_matrix (chms) { }

  ~octave_char_matrix_str (void) { }

  octave_base_value *clone (void) const { return new octave_char_matrix_str (*this); }
  octave_base_value *empty_clone (void) const { return new octave_char_matrix_str (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_value do_index_op (const octave_value_list& idx,
			    bool resize_ok = false)
    { return do_index_op_internal (idx, resize_ok); }

  octave_value reshape (const dim_vector& new_dims) const
    { return octave_value (charNDArray (matrix.reshape (new_dims)), true); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
    { return octave_value (charNDArray (matrix.permute (vec, inv)), true); }

  octave_value resize (const dim_vector& dv, bool fill = false) const;

  bool is_string (void) const { return true; }

  bool is_real_type (void) const { return false; }

  bool is_matrix_type (void) const { return false; }

  bool is_numeric_type (void) const { return false; }

  double double_value (bool = false) const;

  Matrix matrix_value (bool = false) const;

  NDArray array_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  ComplexNDArray complex_array_value (bool = false) const;

  string_vector all_strings (bool pad = false) const;

  std::string string_value (bool force = false) const;

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
    { return octave_value (matrix.sort (dim, mode), true); }

  octave_value sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
		     sortmode mode = ASCENDING) const
    { return octave_value (matrix.sort (sidx, dim, mode), true); }

  bool print_as_scalar (void) const { return (rows () <= 1); }

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

  octave_value xisalnum (void) const;
  octave_value xisalpha (void) const;
  octave_value xisascii (void) const;
  octave_value xiscntrl (void) const;
  octave_value xisdigit (void) const;
  octave_value xisgraph (void) const;
  octave_value xislower (void) const;
  octave_value xisprint (void) const;
  octave_value xispunct (void) const;
  octave_value xisspace (void) const;
  octave_value xisupper (void) const;
  octave_value xisxdigit (void) const;
  octave_value xtoascii (void) const;
  octave_value xtolower (void) const;
  octave_value xtoupper (void) const;

#define MAT_MAPPER(MAP) \
  octave_value MAP (void) const \
    { \
      octave_matrix m (array_value (true)); \
      return m.MAP (); \
    }

  MAT_MAPPER (abs)
  MAT_MAPPER (angle)
  MAT_MAPPER (arg)
  MAT_MAPPER (ceil)
  MAT_MAPPER (conj)
  MAT_MAPPER (fix)
  MAT_MAPPER (floor)
  MAT_MAPPER (imag)
  MAT_MAPPER (real)
  MAT_MAPPER (round)
  MAT_MAPPER (signum)

#undef MAT_MAPPER

#define BOOL_MAT_MAPPER(MAP, VAL)	\
  octave_value MAP (void) const \
    { \
      return boolNDArray (matrix.dims (), VAL); \
    }

  BOOL_MAT_MAPPER (finite, true)
  BOOL_MAT_MAPPER (isinf, false)
  BOOL_MAT_MAPPER (isna, false)
  BOOL_MAT_MAPPER (isnan, false)

#undef BOOL_MAT_MAPPER

protected:

  octave_value do_index_op_internal (const octave_value_list& idx,
				     bool resize_ok, char type = '"');

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

typedef octave_char_matrix_str octave_char_matrix_dq_str;

class
octave_char_matrix_sq_str : public octave_char_matrix_str
{
public:

  octave_char_matrix_sq_str (void)
    : octave_char_matrix_str () { }

  octave_char_matrix_sq_str (const charMatrix& chm)
    : octave_char_matrix_str (chm) { }

  octave_char_matrix_sq_str (const charNDArray& chm)
    : octave_char_matrix_str (chm) { }

  octave_char_matrix_sq_str (char c)
    : octave_char_matrix_str (c) { }

  octave_char_matrix_sq_str (const char *s)
    : octave_char_matrix_str (s) { }

  octave_char_matrix_sq_str (const std::string& s)
    : octave_char_matrix_str (s) { }

  octave_char_matrix_sq_str (const string_vector& s)
    : octave_char_matrix_str (s) { }

  octave_char_matrix_sq_str (const octave_char_matrix_str& chm)
    : octave_char_matrix_str (chm) { }

  octave_char_matrix_sq_str (const octave_char_matrix_sq_str& chms)
    : octave_char_matrix_str (chms) { }

  ~octave_char_matrix_sq_str (void) { }

  octave_base_value *clone (void) const { return new octave_char_matrix_sq_str (*this); }
  octave_base_value *empty_clone (void) const { return new octave_char_matrix_sq_str (); }

  octave_value reshape (const dim_vector& new_dims) const
    { return octave_value (charNDArray (matrix.reshape (new_dims)), true, '\''); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
    { return octave_value (charNDArray (matrix.permute (vec, inv)), true, '\''); }

  octave_value resize (const dim_vector& dv, bool = false) const
    {
      charNDArray retval (matrix);
      retval.resize (dv);
      return octave_value (retval, true, '\'');
    }

  bool is_sq_string (void) const { return true; }

  octave_value do_index_op (const octave_value_list& idx,
			    bool resize_ok = false)
    { return do_index_op_internal (idx, resize_ok, '\''); }


  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
    { return octave_value (matrix.sort (dim, mode), true, '\''); }

  octave_value sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
		     sortmode mode = ASCENDING) const
    { return octave_value (matrix.sort (sidx, dim, mode), true, '\''); }

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
