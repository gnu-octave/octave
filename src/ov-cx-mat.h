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

#if !defined (octave_complex_matrix_h)
#define octave_complex_matrix_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Real scalar values.

class
octave_complex_matrix : public octave_base_value
{
public:

  octave_complex_matrix (void)
    : octave_base_value () { }

  octave_complex_matrix (const ComplexMatrix& m)
    : octave_base_value (), matrix (m) { }

  octave_complex_matrix (const ComplexDiagMatrix& d)
    : octave_base_value (), matrix (d) { }

  octave_complex_matrix (const ComplexRowVector& v, int pcv = -1);

  octave_complex_matrix (const ComplexColumnVector& v, int pcv = -1);

  octave_complex_matrix (const octave_complex_matrix& cm)
    : octave_base_value (), matrix (cm.matrix) { }

  ~octave_complex_matrix (void) { }

  octave_value *clone (void) { return new octave_complex_matrix (*this); }

#if 0
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  octave_value *try_narrowing_conversion (void);

  octave_value index (const octave_value_list& idx) const;

  void assign (const octave_value_list& idx, const ComplexMatrix& rhs);

  void assign (const octave_value_list& idx, const Matrix& rhs);

  int rows (void) const { return matrix.rows (); }
  int columns (void) const { return matrix.columns (); }

  bool is_defined (void) const { return true; }

  bool is_complex_matrix (void) const { return true; }

  octave_value all (void) const { return matrix.all (); }
  octave_value any (void) const { return matrix.any (); }

  bool is_complex_type (void) const { return true; }

  bool is_matrix_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool valid_as_scalar_index (void) const;
  bool valid_as_zero_index (void) const;

  bool is_true (void) const;

  bool is_empty (void) const { return (rows () == 0 && columns () == 0); }

  double double_value (bool) const;

  Matrix matrix_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  octave_value not (void) const { return octave_value (! matrix); }

  octave_value uminus (void) const { return octave_value (- matrix); }

  octave_value transpose (void) const
    { return octave_value (matrix.transpose ()); }

  octave_value hermitian (void) const
    { return octave_value (matrix.hermitian ()); }

  void increment (void) { matrix += 1.0; }

  void decrement (void) { matrix -= 1.0; }

  void print (ostream& os, bool pr_as_read_syntax = false);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  ComplexMatrix matrix;

  static int t_id;

  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
