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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_char_matrix_h)
#define octave_char_matrix_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Character matrix values.

class
octave_char_matrix : public octave_base_value
{
public:

  octave_char_matrix (void)
    : octave_base_value () { }

  octave_char_matrix (const charMatrix& chm, bool = false)
    : octave_base_value (), matrix (chm) { }

  octave_char_matrix (const char *s)
    : octave_base_value (), matrix (s) { }

  octave_char_matrix (const string& s)
    : octave_base_value (), matrix (s) { }

  octave_char_matrix (const string_vector& s)
    : octave_base_value (), matrix (s) { }

  octave_char_matrix (const octave_char_matrix& chm)
    : octave_base_value (), matrix (chm.matrix) { }

  ~octave_char_matrix (void) { }

  octave_value *clone (void) { return new octave_char_matrix (*this); }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  int rows (void) const { return matrix.rows (); }
  int columns (void) const { return matrix.columns (); }

  bool is_defined (void) const { return true; }

  bool is_char_matrix (void) const { return true; }

  // XXX FIXME XXX
  octave_value all (void) const { return 0.0; }
  octave_value any (void) const { return 0.0; }

  bool is_real_type (void) const { return true; }

  bool is_matrix_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool valid_as_scalar_index (void) const;
  bool valid_as_zero_index (void) const;

  bool is_true (void) const;

  double double_value (bool = false) const;

  Matrix matrix_value (bool = false) const { return matrix; }

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const { return matrix; }

  charMatrix char_matrix_value (bool = false) const { return matrix; }

  octave_value convert_to_str (void) const
    { return octave_value (matrix); }

  octave_value transpose (void) const
    { return octave_value (matrix.transpose ()); }

  octave_value hermitian (void) const
    { return octave_value (matrix.transpose ()); }

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

protected:

  charMatrix matrix;

  static octave_allocator allocator;

  // Type id of character matrix objects, set by register_type().
  static int t_id;

  // Type name of character matrix objects, defined in ov-ch-mat.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
