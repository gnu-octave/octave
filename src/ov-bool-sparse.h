/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_sparse_bool_matrix_h)
#define octave_sparse_bool_matrix_h 1

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "oct-alloc.h"
#include "so-array.h"
#include "str-vec.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

#include "boolSparse.h"
#include "ov-base-sparse.h"
#include "ov-re-sparse.h"

class Octave_map;
class octave_value_list;

class tree_walker;

class
octave_sparse_bool_matrix : public octave_base_sparse<SparseBoolMatrix>
{
public:

  octave_sparse_bool_matrix (void)
    : octave_base_sparse<SparseBoolMatrix> () { }

  octave_sparse_bool_matrix (const SparseBoolMatrix& bnda)
    : octave_base_sparse<SparseBoolMatrix> (bnda) { }

  octave_sparse_bool_matrix (const SparseBoolMatrix& bnda,
			     const SparseType& t)
    : octave_base_sparse<SparseBoolMatrix> (bnda, t) { }

  octave_sparse_bool_matrix (const boolNDArray& m)
    : octave_base_sparse<SparseBoolMatrix> (SparseBoolMatrix (m)) { }

  octave_sparse_bool_matrix (const boolMatrix& m)
    : octave_base_sparse<SparseBoolMatrix> (SparseBoolMatrix (m)) { }

  octave_sparse_bool_matrix (const Sparse<bool>& a)
    : octave_base_sparse<SparseBoolMatrix> (a) { }

  octave_sparse_bool_matrix (const octave_sparse_bool_matrix& bm)
    : octave_base_sparse<SparseBoolMatrix> (bm) { }

  ~octave_sparse_bool_matrix (void) { }

  octave_value *clone (void) const { return new octave_sparse_bool_matrix (*this); }
  octave_value *empty_clone (void) const { return new octave_sparse_bool_matrix (); }

  type_conv_fcn numeric_conversion_function (void) const;

  octave_value *try_narrowing_conversion (void);

#if 0
  idx_vector index_vector (void) const { return idx_vector (matrix); }
#endif

  bool is_bool_matrix (void) const { return true; }

  bool is_bool_type (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool valid_as_scalar_index (void) const;

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const;

  NDArray array_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  ComplexNDArray complex_array_value (bool = false) const;

  boolMatrix bool_matrix_value (void) const;

  boolNDArray bool_array_value (void) const;

  SparseMatrix sparse_matrix_value (bool = false) const;

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  SparseBoolMatrix sparse_bool_matrix_value (void) const
    { return matrix; }

  octave_value convert_to_str_internal (bool pad, bool force) const;

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap, 
		    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  bool save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (hid_t loc_id, const char *name, bool have_h5giterate_bug);
#endif

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
