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

#if !defined (octave_base_sparse_h)
#define octave_base_sparse_h 1

#include <cstdlib>

#include <iostream>
#include <string>

#include "str-vec.h"

#include "error.h"
#include "oct-obj.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

#include "boolSparse.h"
#include "SparseType.h"

class Octave_map;

class tree_walker;

class octave_sparse_bool_matrix;

template <class T>
class
octave_base_sparse : public octave_base_value
{
 public:
 
  octave_base_sparse (void) : octave_base_value (), typ (SparseType ()) { }

  octave_base_sparse (const T& a) : octave_base_value (), matrix (a),
				    typ (SparseType ())
  {
    if (matrix.ndims () == 0)
      matrix.resize (dim_vector (0, 0));
  }

  octave_base_sparse (const T& a, const SparseType& t) : octave_base_value (), 
				matrix (a), typ (t)
  {
    if (matrix.ndims () == 0)
      matrix.resize (dim_vector (0, 0));
  }

  octave_base_sparse (const octave_base_sparse& a) : 
    octave_base_value (), matrix (a.matrix), typ (a.typ) { }

  ~octave_base_sparse (void) { }

  octave_value *clone (void) const { return new octave_base_sparse (*this); }
  octave_value *empty_clone (void) const 
    { return new octave_base_sparse (); }

  int nnz (void) const { return matrix.nnz (); }
  int nonzero (void) const { return matrix.nonzero (); }

  size_t byte_size (void) const { return matrix.byte_size (); }

  octave_value squeeze (void) const { return matrix.squeeze (); }

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

  void assign (const octave_value_list& idx, const T& rhs);

  dim_vector dims (void) const { return matrix.dims (); }

  octave_value do_index_op (const octave_value_list& idx, int resize_ok);

  octave_value do_index_op (const octave_value_list& idx)
    { return do_index_op (idx, 0); }

  octave_value reshape (const dim_vector& new_dims) const
    { return T (matrix.reshape (new_dims)); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
    { return T (matrix.permute (vec, inv)); }

  octave_value resize (const dim_vector& dv) const
    { T retval (matrix); retval.resize (dv); return retval; }

  octave_value all (int dim = 0) const { return matrix.all (dim); }
  octave_value any (int dim = 0) const { return matrix.any (dim); }

  bool is_matrix_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_true (void) const;

  int capacity (void) const { return matrix.capacity (); }

  bool print_as_scalar (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_info (std::ostream& os, const std::string& prefix) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool save_ascii (std::ostream& os, bool& infnan_warned,
		 bool strip_nan_and_inf);

  bool load_ascii (std::istream& is);

protected:

  T matrix;

  SparseType typ;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
