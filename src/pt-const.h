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

#include <cstdlib>

#include <string>

class ostream;

#include "Range.h"
#include "mx-base.h"
#include "str-vec.h"

#include "pt-fvc.h"

class Octave_map;
class octave_value_list;

// Constants.

class
octave_value : public tree_fvc
{
private:

// The actual representation of the octave_value.

  class
  octave_value_rep
  {
  public:

    enum constant_type
      {
	unknown_constant,
	scalar_constant,
	matrix_constant,
	complex_scalar_constant,
	complex_matrix_constant,
	char_matrix_constant,
	char_matrix_constant_str,
	range_constant,
	map_constant,
	magic_colon,
	all_va_args,
      };

    enum force_orient
      {
	no_orient,
	row_orient,
	column_orient,
      };

    octave_value_rep (void);

    octave_value_rep (double d);
    octave_value_rep (const Matrix& m);
    octave_value_rep (const DiagMatrix& d);
    octave_value_rep (const RowVector& v, int pcv);
    octave_value_rep (const ColumnVector& v, int pcv);

    octave_value_rep (const Complex& c);
    octave_value_rep (const ComplexMatrix& m);
    octave_value_rep (const ComplexDiagMatrix& d);
    octave_value_rep (const ComplexRowVector& v, int pcv);
    octave_value_rep (const ComplexColumnVector& v, int pcv);

    octave_value_rep (const char *s);
    octave_value_rep (const string& s);
    octave_value_rep (const string_vector& s);
    octave_value_rep (const charMatrix& chm, bool is_string);

    octave_value_rep (double base, double limit, double inc);
    octave_value_rep (const Range& r);

    octave_value_rep (const Octave_map& m);

    octave_value_rep (octave_value_rep::constant_type t);

    octave_value_rep (const octave_value_rep& t);

    ~octave_value_rep (void);

    void *operator new (size_t size);
    void operator delete (void *p, size_t size);

    int rows (void) const;
    int columns (void) const;

    bool is_defined (void) const
      { return type_tag != unknown_constant; }

    bool is_undefined (void) const
      { return type_tag == unknown_constant; }

    bool is_unknown (void) const
      { return type_tag == unknown_constant; }

    bool is_real_scalar (void) const
      { return type_tag == scalar_constant; }

    bool is_real_matrix (void) const
      { return type_tag == matrix_constant; }

    bool is_complex_scalar (void) const
      { return type_tag == complex_scalar_constant; }

    bool is_complex_matrix (void) const
      { return type_tag == complex_matrix_constant; }

    bool is_char_matrix (void) const
      { return type_tag == char_matrix_constant; }

    bool is_string (void) const
      { return type_tag == char_matrix_constant_str; }

    bool is_range (void) const
      { return type_tag == range_constant; }

    bool is_map (void) const
      { return type_tag == map_constant; }

    bool is_magic_colon (void) const
      { return type_tag == magic_colon; }

    bool is_all_va_args (void) const
      { return type_tag == all_va_args; }

    octave_value all (void) const;
    octave_value any (void) const;

    bool is_real_type (void) const
      {
	return (type_tag == scalar_constant
		|| type_tag == matrix_constant
		|| type_tag == range_constant
		|| type_tag == char_matrix_constant
		|| type_tag == char_matrix_constant_str);
      }

    bool is_complex_type (void) const
      {
	return (type_tag == complex_matrix_constant
		|| type_tag == complex_scalar_constant);
      }

    // Would be nice to get rid of the next four functions:

    bool is_scalar_type (void) const
      {
	return (type_tag == scalar_constant
		|| type_tag == complex_scalar_constant);
      }

    bool is_matrix_type (void) const
      {
	return (type_tag == matrix_constant
		|| type_tag == complex_matrix_constant);
      }

    bool is_numeric_type (void) const
      {
	return (type_tag == scalar_constant
		|| type_tag == matrix_constant
		|| type_tag == complex_matrix_constant
		|| type_tag == complex_scalar_constant);
      }

    bool valid_as_scalar_index (void) const;
    bool valid_as_zero_index (void) const;

    bool is_true (void) const;

    bool is_empty (void) const
      {
	return ((! (is_magic_colon ()
		    || is_all_va_args ()
		    || is_unknown ()))
		&& (rows () == 0
		    || columns () == 0));
      }

    double double_value (bool frc_str_conv = false) const;
    Matrix matrix_value (bool frc_str_conv = false) const;
    Complex complex_value (bool frc_str_conv = false) const;
    ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const;
    charMatrix char_matrix_value (bool frc_str_conv = false) const;
    charMatrix all_strings (void) const;
    string string_value (void) const;
    Range range_value (void) const;
    Octave_map map_value (void) const;

    octave_value& lookup_map_element (const string& name,
				       bool insert = false,
				       bool silent = false);

    ColumnVector vector_value (bool frc_str_conv = false,
			       bool frc_vec_conv = false) const;

    ComplexColumnVector
    complex_vector_value (bool frc_str_conv = false,
			  bool frc_vec_conv = false) const;

    octave_value convert_to_str (void) const;

    void convert_to_row_or_column_vector (void);

    void bump_value (tree_expression::type);

    void resize (int i, int j);
    void resize (int i, int j, double val);

    void stash_original_text (const string& s);

    void maybe_mutate (void);

    void print (void);
    void print (ostream& os);

    void print_code (ostream& os);

    void gripe_wrong_type_arg (const char *name,
			       const octave_value_rep& tcr) const;

    char *type_as_string (void) const;

    // Binary and unary operations.

    friend octave_value do_binary_op (octave_value& a, octave_value& b,
				      tree_expression::type t);

    friend octave_value do_unary_op (octave_value& a,
				     tree_expression::type t);

    // We want to eliminate this.

    constant_type const_type (void) const { return type_tag; }

    // We want to get rid of these too:

    void force_numeric (bool frc_str_conv = false);
    octave_value make_numeric (bool frc_str_conv = false) const;

    // But not this.

    void convert_to_matrix_type (bool make_complex);

    // Indexing and assignment.

    void clear_index (void);

    // void set_index (double d);
    void set_index (const Range& r);
    void set_index (const ColumnVector& v);
    void set_index (const Matrix& m);
    void set_index (char c);

    void set_index (const octave_value_list& args,
		    bool rhs_is_complex = false);

    octave_value do_index (const octave_value_list& args);

    void maybe_widen (constant_type t);

    void assign (octave_value& rhs, const octave_value_list& args);

    bool print_as_scalar (void);

    bool print_as_structure (void);

    // Data.

    union
      {
	double scalar;			// A real scalar constant.
	Matrix *matrix;			// A real matrix constant.
	Complex *complex_scalar;	// A real scalar constant.
	ComplexMatrix *complex_matrix;	// A real matrix constant.
	charMatrix *char_matrix;	// A character string constant.
	Range *range;			// A set of evenly spaced values.
	Octave_map *a_map;		// An associative array.

	octave_value_rep *freeptr;	// For custom memory management.
      };

    constant_type type_tag;

    int count;

    string orig_text;
  };

  union
    {
      octave_value *freeptr;  // For custom memory management.
      octave_value_rep *rep;  // The real representation.
    };

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
  // magic colon      octave_value::magic_colon
  // all_va_args      octave_value::all_va_args

  octave_value (void) : tree_fvc ()
    { rep = new octave_value_rep (); rep->count = 1; }

  octave_value (double d, int l = -1, int c = -1) : tree_fvc (l, c)
    { rep = new octave_value_rep (d); rep->count = 1; }

  octave_value (const Matrix& m) : tree_fvc ()
    { rep = new octave_value_rep (m); rep->count = 1; }

  octave_value (const DiagMatrix& d) : tree_fvc ()
    { rep = new octave_value_rep (d); rep->count = 1; }

  octave_value (const RowVector& v, int pcv = -1) : tree_fvc ()
    { rep = new octave_value_rep (v, pcv); rep->count = 1; }

  octave_value (const ColumnVector& v, int pcv = -1) : tree_fvc ()
    { rep = new octave_value_rep (v, pcv); rep->count = 1; }

  octave_value (const Complex& C, int l = -1, int c = -1) : tree_fvc (l, c)
    { rep = new octave_value_rep (C); rep->count = 1; }

  octave_value (const ComplexMatrix& m) : tree_fvc ()
    { rep = new octave_value_rep (m); rep->count = 1; }

  octave_value (const ComplexDiagMatrix& d) : tree_fvc ()
    { rep = new octave_value_rep (d); rep->count = 1; }

  octave_value (const ComplexRowVector& v, int pcv = -1) : tree_fvc ()
    { rep = new octave_value_rep (v, pcv); rep->count = 1; }

  octave_value (const ComplexColumnVector& v, int pcv = -1) : tree_fvc () 
    { rep = new octave_value_rep (v, pcv); rep->count = 1; }

  octave_value (const char *s, int l = -1, int c = -1) : tree_fvc (l, c)
    { rep = new octave_value_rep (s); rep->count = 1; }

  octave_value (const string& s, int l = -1, int c = -1) : tree_fvc (l, c)
    { rep = new octave_value_rep (s); rep->count = 1; }

  octave_value (const string_vector& s, int l = -1, int c = -1)
    : tree_fvc (l, c)
    { rep = new octave_value_rep (s); rep->count = 1; }

  octave_value (const charMatrix& chm, bool is_string = false) : tree_fvc ()
    { rep = new octave_value_rep (chm, is_string); rep->count = 1; }

  octave_value (double base, double limit, double inc) : tree_fvc ()
    { rep = new octave_value_rep (base, limit, inc); rep->count = 1; }

  octave_value (const Range& r) : tree_fvc ()
    { rep = new octave_value_rep (r); rep->count = 1; }

  octave_value (const Octave_map& m) : tree_fvc ()
    { rep = new octave_value_rep (m); rep->count = 1; }

  octave_value (octave_value::magic_colon) : tree_fvc ()
    {
      octave_value_rep::constant_type tmp;
      tmp = octave_value_rep::magic_colon;
      rep = new octave_value_rep (tmp);
      rep->count = 1;
    }

  octave_value (octave_value::all_va_args) : tree_fvc ()
    {
      octave_value_rep::constant_type tmp;
      tmp = octave_value_rep::all_va_args;
      rep = new octave_value_rep (tmp);
      rep->count = 1;
    }

  // Copy constructor.

  octave_value (const octave_value& a) : tree_fvc ()
    { rep = a.rep; rep->count++; }

  // Delete the representation of this constant if the count drops to
  // zero.

  ~octave_value (void);

  void *operator new (size_t size);
  void operator delete (void *p, size_t size);

  // Simple assignment.

  octave_value operator = (const octave_value& a);

  // Indexed assignment.

  octave_value assign (octave_value& rhs, const octave_value_list& args)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new octave_value_rep (*rep);
	  rep->count = 1;
	}

      rep->assign (rhs, args);

      return *this;
    }

  // Simple structure assignment.

  octave_value assign_map_element (SLList<string>& list,
				    octave_value& rhs);

  // Indexed structure assignment.

  octave_value assign_map_element (SLList<string>& list,
				    octave_value& rhs,
				    const octave_value_list& args);

  // Type.  It would be nice to eliminate the need for this.

  bool is_constant (void) const { return true; }

  // Size.

  int rows (void) const { return rep->rows (); }
  int columns (void) const { return rep->columns (); }

  // Does this constant have a type?  Both of these are provided since
  // it is sometimes more natural to write is_undefined() instead of
  // ! is_defined().

  bool is_defined (void) const { return rep->is_defined (); }
  bool is_undefined (void) const { return rep->is_undefined (); }

  // Is this constant a particular type, or does it belong to a
  // particular class of types?

  bool is_unknown (void) const { return rep->is_unknown (); }
  bool is_real_scalar (void) const { return rep->is_real_scalar (); }
  bool is_real_matrix (void) const { return rep->is_real_matrix (); }
  bool is_complex_scalar (void) const { return rep->is_complex_scalar (); }
  bool is_complex_matrix (void) const { return rep->is_complex_matrix (); }
  bool is_string (void) const { return rep->is_string (); }
  bool is_range (void) const { return rep->is_range (); }
  bool is_map (void) const { return rep->is_map (); }
  bool is_magic_colon (void) const { return rep->is_magic_colon (); }
  bool is_all_va_args (void) const { return rep->is_all_va_args (); }

  // Are any or all of the elements in this constant nonzero?

  octave_value all (void) const { return rep->all (); }
  octave_value any (void) const { return rep->any (); }

  // Other type stuff.

  bool is_real_type (void) const { return rep->is_real_type (); }

  bool is_complex_type (void) const { return rep->is_complex_type (); }

  bool is_scalar_type (void) const { return rep->is_scalar_type (); }
  bool is_matrix_type (void) const { return rep->is_matrix_type (); }

  bool is_numeric_type (void) const
    { return rep->is_numeric_type (); }

  bool valid_as_scalar_index (void) const
    { return rep->valid_as_scalar_index (); }

  bool valid_as_zero_index (void) const
    { return rep->valid_as_zero_index (); }

  // Does this constant correspond to a truth value?

  bool is_true (void) const { return rep->is_true (); }

  // Is at least one of the dimensions of this constant zero?

  bool is_empty (void) const
    { return rep->is_empty (); }

  // Are the dimensions of this constant zero by zero?

  bool is_zero_by_zero (void) const
    {
      return ((! (is_magic_colon () || is_all_va_args () || is_unknown ()))
	      && rows () == 0 && columns () == 0);
    } 

  // Values.

  double double_value (bool frc_str_conv = false) const
    { return rep->double_value (frc_str_conv); }

  Matrix matrix_value (bool frc_str_conv = false) const
    { return rep->matrix_value (frc_str_conv); }

  Complex complex_value (bool frc_str_conv = false) const
    { return rep->complex_value (frc_str_conv); }

  ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const
    { return rep->complex_matrix_value (frc_str_conv); }

  charMatrix char_matrix_value (bool frc_str_conv = false) const
    { return rep->char_matrix_value (frc_str_conv); }

  charMatrix all_strings (void) const
    { return rep->all_strings (); }

  string string_value (void) const
    { return rep->string_value (); }

  Range range_value (void) const
    { return rep->range_value (); }

  Octave_map map_value (void) const;

  octave_value lookup_map_element (const string& ref,
				    bool insert = false,
				    bool silent = false);

  octave_value lookup_map_element (SLList<string>& list,
				    bool insert = false,
				    bool silent = false);

  ColumnVector vector_value (bool /* frc_str_conv */ = false,
			     bool /* frc_vec_conv */ = false) const 
    { return rep->vector_value (); }

  ComplexColumnVector
  complex_vector_value (bool /* frc_str_conv */ = false,
			bool /* frc_vec_conv */ = false) const
    { return rep->complex_vector_value (); }

  // Binary and unary operations.

  friend octave_value do_binary_op (octave_value& a, octave_value& b,
				     tree_expression::type t);

  friend octave_value do_unary_op (octave_value& a,
				    tree_expression::type t);

  // Conversions.  These should probably be private.  If a user of this
  // class wants a certain kind of constant, he should simply ask for
  // it, and we should convert it if possible.

  octave_value convert_to_str (void)
    { return rep->convert_to_str (); }

  void convert_to_row_or_column_vector (void)
    { rep->convert_to_row_or_column_vector (); }

  // Increment or decrement this constant.

  void bump_value (tree_expression::type et)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new octave_value_rep (*rep);
	  rep->count = 1;
	}

      rep->bump_value (et);
    }

  void print (void);
  void print (ostream& os) { rep->print (os); }

  void print_with_name (const string& name, bool print_padding = true);
  void print_with_name (ostream& os, const string& name,
			bool print_padding = true);

  // Evaluate this constant, possibly converting complex to real, or
  // matrix to scalar, etc.

  octave_value eval (bool print_result)
    {
      if (print_result)
	{
	  rep->maybe_mutate ();  // XXX FIXME XXX -- is this necessary?
	  print ();
	}

      return *this;
    }

  octave_value_list eval (bool, int, const octave_value_list&);

  // Store the original text corresponding to this constant for later
  // pretty printing.

  void stash_original_text (const string& s)
    { rep->stash_original_text (s); }

  // Pretty print this constant.
 
  void print_code (ostream& os);

  char *type_as_string (void) const
    { return rep->type_as_string (); }

  // We really do need this, and it should be private:

private:

  void make_unique (void);

  octave_value_rep *make_unique_map (void);

  // We want to eliminate this, or at least make it private.

  octave_value_rep::constant_type const_type (void) const
    { return rep->const_type (); }

  void convert_to_matrix_type (bool make_complex)
    { rep->convert_to_matrix_type (make_complex); }

  // Can we make these go away?

  // These need better names, since a range really is a numeric type.

  void force_numeric (bool frc_str_conv = false)
    { rep->force_numeric (frc_str_conv); }

  octave_value make_numeric (bool frc_str_conv = false) const
    {
      if (is_numeric_type ())
	return *this;
      else
	return rep->make_numeric (frc_str_conv);
    }

  bool print_as_scalar (void) { return rep->print_as_scalar (); }

  bool print_as_structure (void) { return rep->print_as_structure (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
