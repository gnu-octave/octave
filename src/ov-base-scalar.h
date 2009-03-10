/*

Copyright (C) 1996, 1997, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_base_scalar_h)
#define octave_base_scalar_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-alloc.h"
#include "str-vec.h"
#include "MatrixType.h"

#include "ov-base.h"
#include "ov-typeinfo.h"

// Real scalar values.

template <class ST>
class
octave_base_scalar : public octave_base_value
{
public:

  octave_base_scalar (void)
    : octave_base_value (), typ (MatrixType ()) { }

  octave_base_scalar (const ST& s, const MatrixType& t = MatrixType ())
    : octave_base_value (), scalar (s), typ (t) { }

  octave_base_scalar (const octave_base_scalar& s)
    : octave_base_value (), scalar (s.scalar), typ (s.typ) { }

  ~octave_base_scalar (void) { }

  octave_base_value *clone (void) const { return new octave_base_scalar (*this); }
  octave_base_value *empty_clone (void) const { return new octave_base_scalar (); }

  octave_value squeeze (void) const { return scalar; }

  octave_value full_value (void) const { return scalar; }

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx, int)
    { return subsref (type, idx); }

  octave_value subsasgn (const std::string& type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs);

  bool is_constant (void) const { return true; }

  bool is_defined (void) const { return true; }

  dim_vector dims (void) const { static dim_vector dv (1, 1); return dv; }

  octave_idx_type nnz (void) const { return (scalar != ST ()) ? 1 : 0; }

  octave_value permute (const Array<int>&, bool = false) const
    { return scalar; }

  octave_value reshape (const dim_vector& new_dims) const
    { return array_value ().reshape (new_dims); }

  size_t byte_size (void) const { return sizeof (ST); }

  octave_value all (int = 0) const { return (scalar != ST ()); }

  octave_value any (int = 0) const { return (scalar != ST ()); }

  octave_value diag (octave_idx_type k = 0) const 
    { return octave_value (matrix_value (). diag (k)); }

  octave_value sort (octave_idx_type, sortmode) const
    { return octave_value (scalar); }
  octave_value sort (Array<octave_idx_type> &sidx, octave_idx_type,
		     sortmode) const
    { 
      sidx.resize (dim_vector (1, 1)); 
      sidx(0) = 0; 
      return octave_value (scalar); 
    }

  sortmode is_sorted (sortmode mode = UNSORTED) const
    { return mode ? mode : ASCENDING; }

  Array<octave_idx_type> sort_rows_idx (sortmode) const
    { return Array<octave_idx_type> (1, 0); }

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const
    { return mode ? mode : ASCENDING; }

  MatrixType matrix_type (void) const { return typ; }
  MatrixType matrix_type (const MatrixType& _typ) const
    { MatrixType ret = typ; typ = _typ; return ret; }

  bool is_scalar_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool is_true (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return const_cast<ST *> (&scalar); }

protected:

  // The value of this scalar.
  ST scalar;

  mutable MatrixType typ;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
