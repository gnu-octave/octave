/*

Copyright (C) 1998 John W. Eaton

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

#if !defined (octave_base_matrix_h)
#define octave_base_matrix_h 1

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-obj.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

#include "MatrixType.h"

class Octave_map;

class tree_walker;

// Real matrix values.

template <class MT>
class
octave_base_matrix : public octave_base_value
{
public:

  octave_base_matrix (void)
    : octave_base_value (), typ (MatrixType ()) { }

  octave_base_matrix (const MT& m)
    : octave_base_value (), matrix (m), typ (MatrixType ())
  {
    if (matrix.ndims () == 0)
      matrix.resize (dim_vector (0, 0));
  }

  octave_base_matrix (const MT& m, const MatrixType& t)
    : octave_base_value (), matrix (m), typ (t)
  {
    if (matrix.ndims () == 0)
      matrix.resize (dim_vector (0, 0));
  }

  octave_base_matrix (const octave_base_matrix& m)
    : octave_base_value (), matrix (m.matrix), typ (m.typ) { }

  ~octave_base_matrix (void) { }

  octave_base_value *clone (void) const { return new octave_base_matrix (*this); }
  octave_base_value *empty_clone (void) const { return new octave_base_matrix (); }

  size_t byte_size (void) const { return matrix.byte_size (); }

  octave_value squeeze (void) const { return MT (matrix.squeeze ()); }

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string&,
			     const std::list<octave_value_list>&, int)
    {
      panic_impossible ();
      return octave_value_list ();
    }

  octave_value subsasgn (const std::string& type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs);

  octave_value do_index_op (const octave_value_list& idx, int resize_ok);

  octave_value do_index_op (const octave_value_list& idx)
    { return do_index_op (idx, 0); }

  void assign (const octave_value_list& idx, const MT& rhs);

  dim_vector dims (void) const { return matrix.dims (); }

  octave_idx_type nnz (void) const { return matrix.nnz (); }

  octave_value reshape (const dim_vector& new_dims) const
    { return MT (matrix.reshape (new_dims)); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
    { return MT (matrix.permute (vec, inv)); }

  octave_value resize (const dim_vector& dv, bool fill = false) const;

  octave_value all (int dim = 0) const { return matrix.all (dim); }
  octave_value any (int dim = 0) const { return matrix.any (dim); }

  MatrixType matrix_type (void) const { return typ; }
  MatrixType matrix_type (const MatrixType& _typ) const
    { MatrixType ret = typ; typ = _typ; return ret; }

  bool is_matrix_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_true (void) const;

  bool print_as_scalar (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_info (std::ostream& os, const std::string& prefix) const;

protected:

  MT matrix;

  mutable MatrixType typ;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
