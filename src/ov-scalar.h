/*

Copyright (C) 1996 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_scalar_h)
#define octave_scalar_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "lo-utils.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "mappers.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Real scalar values.

class
octave_scalar : public octave_base_value
{
public:

  octave_scalar (void)
    : octave_base_value (), scalar (0.0) { }

  octave_scalar (double d)
    : octave_base_value (), scalar (d) { }

  octave_scalar (const octave_scalar& s)
    : octave_base_value (), scalar (s.scalar) { }

  ~octave_scalar (void) { }

  octave_value *clone (void) { return new octave_scalar (*this); }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  octave_value index (const octave_value_list& idx) const;

  idx_vector index_vector (void) const { return idx_vector (scalar); }

  int rows (void) const { return 1; }
  int columns (void) const { return 1; }

  bool is_defined (void) const { return true; }
  bool is_real_scalar (void) const { return true; }

  octave_value all (void) const { return (scalar != 0.0); }
  octave_value any (void) const { return (scalar != 0.0); }

  bool is_real_type (void) const { return true; }
  bool is_scalar_type (void) const { return true; }
  bool is_numeric_type (void) const { return true; }

  bool valid_as_scalar_index (void) const
    { return (! xisnan (scalar) && NINT (scalar) == 1); }

  bool valid_as_zero_index (void) const
    { return (! xisnan (scalar) && NINT (scalar) == 0); }

  bool is_true (void) const { return (scalar != 0.0); }

  double double_value (bool = false) const { return scalar; }

  Matrix matrix_value (bool = false) const { return Matrix (1, 1, scalar); }

  Complex complex_value (bool = false) const { return scalar; }

  ComplexMatrix complex_matrix_value (bool = false) const
    { return  ComplexMatrix (1, 1, Complex (scalar)); }

  octave_value not (void) const { return octave_value (! scalar); }

  octave_value uminus (void) const { return octave_value (- scalar); }

  octave_value transpose (void) const { return octave_value (scalar); }

  octave_value hermitian (void) const { return octave_value (scalar); }

  void increment (void) { ++scalar; }

  void decrement (void) { --scalar; }

  octave_value convert_to_str (void) const;

  void print (ostream& os, bool pr_as_read_syntax = false);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // The value of this scalar.
  double scalar;

  // For custom memory management.
  static octave_allocator allocator;

  // Type id of scalar objects, set by register_type().
  static int t_id;

  // Type name of scalar objects, defined in ov-scalar.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
