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

#if !defined (octave_tree_const_h)
#define octave_tree_const_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

class ostream;

#include "Range.h"
#include "mx-base.h"
#include "str-vec.h"

#include "pt-fvc.h"

class Octave_map;
class octave_value_list;

class tree_walker;

#include "ov.h"

class
tree_constant : public tree_fvc
{
public:

  enum magic_colon { magic_colon_t };
  enum all_va_args { all_va_args_t };

  // Constructors.  It is possible to create the following types of
  // constants:
  //
  // constant type    constructor arguments
  // -------------    ---------------------
  // unknown          none
  // real scalar      double
  // real matrix      Matrix
  //                  DiagMatrix
  //                  RowVector
  //                  ColumnVector
  // complex scalar   Complex
  // complex matrix   ComplexMatrix
  //                  ComplexDiagMatrix
  //                  ComplexRowVector
  //                  ComplexColumnVector
  // char matrix      charMatrix
  // string           char* (null terminated)
  //                  string
  //                  charMatrix
  // range            double, double, double
  //                  Range
  // map              Octave_map
  // magic colon      tree_constant::magic_colon
  // all_va_args      tree_constant::all_va_args

  tree_constant (void)
    : tree_fvc (), val (), orig_text () { }

  tree_constant (double d, int l = -1, int c = -1)
    : tree_fvc (l, c), val (d), orig_text () { }

  tree_constant (const Matrix& m)
    : tree_fvc (), val (m), orig_text () { }

  tree_constant (const DiagMatrix& d)
    : tree_fvc (), val (d), orig_text () { }

  tree_constant (const RowVector& v, int pcv = -1)
    : tree_fvc (), val (v, pcv), orig_text () { }

  tree_constant (const ColumnVector& v, int pcv = -1)
    : tree_fvc (), val (v, pcv), orig_text () { }

  tree_constant (const Complex& C, int l = -1, int c = -1)
    : tree_fvc (l, c), val (C), orig_text () { }

  tree_constant (const ComplexMatrix& m)
    : tree_fvc (), val (m), orig_text () { }

  tree_constant (const ComplexDiagMatrix& d)
    : tree_fvc (), val (d), orig_text () { }

  tree_constant (const ComplexRowVector& v, int pcv = -1)
    : tree_fvc (), val (v, pcv), orig_text () { }

  tree_constant (const ComplexColumnVector& v, int pcv = -1)
    : tree_fvc (), val (v, pcv), orig_text () { }

  tree_constant (const char *s, int l = -1, int c = -1)
    : tree_fvc (l, c), val (s), orig_text () { }

  tree_constant (const string& s, int l = -1, int c = -1)
    : tree_fvc (l, c), val (s), orig_text () { }

  tree_constant (const string_vector& s, int l = -1, int c = -1)
    : tree_fvc (l, c), val (s), orig_text () { }

  tree_constant (const charMatrix& chm, bool is_string = false)
    : tree_fvc (), val (chm, is_string), orig_text () { }

  tree_constant (double base, double limit, double inc)
    : tree_fvc (), val (base, limit, inc), orig_text () { }

  tree_constant (const Range& r)
    : tree_fvc (), val (r), orig_text () { }

  tree_constant (const Octave_map& m)
    : tree_fvc (), val (m), orig_text () { }

  tree_constant (tree_constant::magic_colon, int l = -1, int c = -1)
    : tree_fvc (l, c), val (octave_value::magic_colon_t), orig_text () { }

  tree_constant (tree_constant::all_va_args, int l = -1, int c = -1)
    : tree_fvc (l, c), val (octave_value::all_va_args_t), orig_text () { }

  tree_constant (const octave_value& v, int l = -1, int c = -1)
    : tree_fvc (l, c), val (v), orig_text () { }

  tree_constant (const tree_constant& a)
    : tree_fvc (), val (a.val), orig_text () { }

  ~tree_constant (void) { }

  tree_constant& operator = (const tree_constant& a)
    {
      if (this != &a)
	{
	  tree_fvc::operator = (a);
	  val = a.val;
	}
      return *this;
    }

#if 0
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  // Indexed assignment.

  octave_value index (const octave_value_list& idx) const
    { return val.index (idx); }

  octave_value& reference (void)
    {
      val.make_unique ();
      return val;
    }

  octave_value value (void) const
    {
      return val;
    }

  octave_value assign (const octave_value_list& idx, const octave_value& rhs)
    {
      val.assign (idx, rhs);
      return val;
    }

  // Type.  It would be nice to eliminate the need for this.

  bool is_constant (void) const { return true; }

  // Size.

  int rows (void) const { return val.rows (); }
  int columns (void) const { return val.columns (); }

  // Does this constant have a type?  Both of these are provided since
  // it is sometimes more natural to write is_undefined() instead of
  // ! is_defined().

  bool is_defined (void) const { return val.is_defined (); }
  bool is_undefined (void) const { return val.is_undefined (); }

  // Is this constant a particular type, or does it belong to a
  // particular class of types?

  bool is_real_scalar (void) const { return val.is_real_scalar (); }
  bool is_real_matrix (void) const { return val.is_real_matrix (); }
  bool is_complex_scalar (void) const { return val.is_complex_scalar (); }
  bool is_complex_matrix (void) const { return val.is_complex_matrix (); }
  bool is_char_matrix (void) const { return val.is_char_matrix (); }
  bool is_string (void) const { return val.is_string (); }
  bool is_range (void) const { return val.is_range (); }
  bool is_map (void) const { return val.is_map (); }
  bool is_magic_colon (void) const { return val.is_magic_colon (); }
  bool is_all_va_args (void) const { return val.is_all_va_args (); }

  // Are any or all of the elements in this constant nonzero?

  octave_value all (void) const { return val.all (); }
  octave_value any (void) const { return val.any (); }

  // Other type stuff.

  bool is_real_type (void) const { return val.is_real_type (); }

  bool is_complex_type (void) const { return val.is_complex_type (); }

  bool is_scalar_type (void) const { return val.is_scalar_type (); }
  bool is_matrix_type (void) const { return val.is_matrix_type (); }

  bool is_numeric_type (void) const
    { return val.is_numeric_type (); }

  bool valid_as_scalar_index (void) const
    { return val.valid_as_scalar_index (); }

  bool valid_as_zero_index (void) const
    { return val.valid_as_zero_index (); }

  // Does this constant correspond to a truth value?

  bool is_true (void) const { return val.is_true (); }

  // Is at least one of the dimensions of this constant zero?

  bool is_empty (void) const
    { return val.is_empty (); }

  // Are the dimensions of this constant zero by zero?

  bool is_zero_by_zero (void) const
    { return val.is_zero_by_zero (); }

  // Values.

  double double_value (bool frc_str_conv = false) const
    { return val.double_value (frc_str_conv); }

  Matrix matrix_value (bool frc_str_conv = false) const
    { return val.matrix_value (frc_str_conv); }

  Complex complex_value (bool frc_str_conv = false) const
    { return val.complex_value (frc_str_conv); }

  ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const
    { return val.complex_matrix_value (frc_str_conv); }

  charMatrix char_matrix_value (bool frc_str_conv = false) const
    { return val.char_matrix_value (frc_str_conv); }

  charMatrix all_strings (void) const
    { return val.all_strings (); }

  string string_value (void) const
    { return val.string_value (); }

  Range range_value (void) const
    { return val.range_value (); }

  Octave_map map_value (void) const;

  octave_value lookup_map_element (const string& ref,
				    bool insert = false,
				    bool silent = false);

  octave_value lookup_map_element (SLList<string>& list,
				    bool insert = false,
				    bool silent = false);

  ColumnVector vector_value (bool /* frc_str_conv */ = false,
			     bool /* frc_vec_conv */ = false) const 
    { return val.vector_value (); }

  ComplexColumnVector
  complex_vector_value (bool /* frc_str_conv */ = false,
			bool /* frc_vec_conv */ = false) const
    { return val.complex_vector_value (); }

  // Binary and unary operations.

  friend octave_value do_binary_op (octave_value& a, octave_value& b,
				    tree_expression::type t);

  friend octave_value do_unary_op (octave_value& a,
				   tree_expression::type t);

  // Conversions.  These should probably be private.  If a user of this
  // class wants a certain kind of constant, he should simply ask for
  // it, and we should convert it if possible.

  octave_value convert_to_str (void) const
    { return val.convert_to_str (); }

  void convert_to_row_or_column_vector (void)
    { val.convert_to_row_or_column_vector (); }

  void maybe_mutate (void)
    { val.maybe_mutate (); }

  // Increment or decrement this constant.

  void increment (void) { val.increment (); }

  void decrement (void) { val.decrement (); }

  void print (void);
  void print (ostream& os) { val.print (os); }

  void print_with_name (const string& name, bool print_padding = true);
  void print_with_name (ostream& os, const string& name,
			bool print_padding = true);

  octave_value eval (bool print_result);

  octave_value_list eval (bool, int, const octave_value_list&);

  // Store the original text corresponding to this constant for later
  // pretty printing.

  void stash_original_text (const string& s) { orig_text = s; }

  string original_text (void) { return orig_text; }

  void accept (tree_walker& tw);

  string type_name (void) const { return val.type_name (); }

private:

  octave_value val;

  string orig_text;

  void convert_to_matrix_type (bool make_complex)
    { val.convert_to_matrix_type (make_complex); }

  // Can we make these go away?

  // These need better names, since a range really is a numeric type.

  void force_numeric (bool frc_str_conv = false)
    { val.force_numeric (frc_str_conv); }

  octave_value make_numeric (bool frc_str_conv = false) const;

  bool print_as_scalar (void) { return val.print_as_scalar (); }

  bool print_as_structure (void) { return val.print_as_structure (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
