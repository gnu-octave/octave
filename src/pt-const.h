// tree-const.h                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#include <iostream.h>

#include "Range.h"
#include "mx-base.h"

#include "oct-obj.h"
#include "oct-str.h"
#include "tree-base.h"
#include "tree-expr.h"

class idx_vector;
class Octave_map;

struct Mapper_fcn;

// Constants.

class
tree_constant : public tree_fvc
{
private:

// The real representation of a constant, declared in tc-rep.h

#include "tc-rep.h"

  union
    {
      tree_constant *freeptr;  // For custom memory management.
      tree_constant_rep *rep;  // The real representation.
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
// string           char* (null terminated)
//                  Octave_str_obj
// range            double, double, double
//                  Range
// map              Octave_map
// magic colon      tree_constant::magic_colon
// all_va_args      tree_constant::all_va_args

  tree_constant (void) : tree_fvc ()
    { rep = new tree_constant_rep (); rep->count = 1; }

  tree_constant (double d) : tree_fvc ()
    { rep = new tree_constant_rep (d); rep->count = 1; }

  tree_constant (const Matrix& m) : tree_fvc ()
    { rep = new tree_constant_rep (m); rep->count = 1; }

  tree_constant (const DiagMatrix& d) : tree_fvc ()
    { rep = new tree_constant_rep (d); rep->count = 1; }

  tree_constant (const RowVector& v, int pcv = -1) : tree_fvc ()
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const ColumnVector& v, int pcv = -1) : tree_fvc ()
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const Complex& c) : tree_fvc ()
    { rep = new tree_constant_rep (c); rep->count = 1; }

  tree_constant (const ComplexMatrix& m) : tree_fvc ()
    { rep = new tree_constant_rep (m); rep->count = 1; }

  tree_constant (const ComplexDiagMatrix& d) : tree_fvc ()
    { rep = new tree_constant_rep (d); rep->count = 1; }

  tree_constant (const ComplexRowVector& v, int pcv = -1) : tree_fvc ()
      { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const ComplexColumnVector& v, int pcv = -1) : tree_fvc () 
      { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const char *s) : tree_fvc ()
    { rep = new tree_constant_rep (s); rep->count = 1; }

  tree_constant (const Octave_str_obj& s) : tree_fvc ()
    { rep = new tree_constant_rep (s); rep->count = 1; }

  tree_constant (double base, double limit, double inc) : tree_fvc ()
    { rep = new tree_constant_rep (base, limit, inc); rep->count = 1; }

  tree_constant (const Range& r) : tree_fvc ()
    { rep = new tree_constant_rep (r); rep->count = 1; }

  tree_constant (const Octave_map& m) : tree_fvc ()
    { rep = new tree_constant_rep (m); rep->count = 1; }

  tree_constant (tree_constant::magic_colon) : tree_fvc ()
    {
      tree_constant_rep::constant_type tmp;
      tmp = tree_constant_rep::magic_colon;
      rep = new tree_constant_rep (tmp);
      rep->count = 1;
    }

  tree_constant (tree_constant::all_va_args) : tree_fvc ()
    {
      tree_constant_rep::constant_type tmp;
      tmp = tree_constant_rep::all_va_args;
      rep = new tree_constant_rep (tmp);
      rep->count = 1;
    }

// Copy constructor.

  tree_constant (const tree_constant& a) : tree_fvc ()
    { rep = a.rep; rep->count++; }

// Delete the representation of this constant if the count drops to
// zero.

  ~tree_constant (void);

  void *operator new (size_t size);
  void operator delete (void *p, size_t size);

// Simple assignment.

  tree_constant operator = (const tree_constant& a);

// Indexed assignment.

  tree_constant assign (tree_constant& rhs, const Octave_object& args)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new tree_constant_rep (*rep);
	  rep->count = 1;
	}

      rep->assign (rhs, args);

      return *this;
    }

// Simple structure assignment.

  tree_constant assign_map_element (SLList<char*>& list,
				    tree_constant& rhs);

// Indexed structure assignment.

  tree_constant assign_map_element (SLList<char*>& list,
				    tree_constant& rhs,
				    const Octave_object& args);

// Type.  It would be nice to eliminate the need for this.

  int is_constant (void) const { return 1; }

// Size.

  int rows (void) const { return rep->rows (); }
  int columns (void) const { return rep->columns (); }

// Does this constant have a type?  Both of these are provided since
// it is sometimes more natural to write is_undefined() instead of
// ! is_defined().

  int is_defined (void) const { return rep->is_defined (); }
  int is_undefined (void) const { return rep->is_undefined (); }

// What type is this constant?

  int is_unknown (void) const { return rep->is_unknown (); }
  int is_real_scalar (void) const { return rep->is_real_scalar (); }
  int is_real_matrix (void) const { return rep->is_real_matrix (); }
  int is_complex_scalar (void) const { return rep->is_complex_scalar (); }
  int is_complex_matrix (void) const { return rep->is_complex_matrix (); }
  int is_string (void) const { return rep->is_string (); }
  int is_range (void) const { return rep->is_range (); }
  int is_map (void) const { return rep->is_map (); }
  int is_magic_colon (void) const { return rep->is_magic_colon (); }
  int is_all_va_args (void) const { return rep->is_all_va_args (); }

// Are any or all of the elements in this constant nonzero?

  tree_constant all (void) const { return rep->all (); }
  tree_constant any (void) const { return rep->any (); }

  int is_real_type (void) const { return rep->is_real_type (); }

  int is_complex_type (void) const { return rep->is_complex_type (); }

// Would be nice to get rid of the next four functions:

  int is_scalar_type (void) const { return rep->is_scalar_type (); }
  int is_matrix_type (void) const { return rep->is_matrix_type (); }

  int is_numeric_type (void) const
    { return rep->is_numeric_type (); }

  int is_numeric_or_range_type (void) const
    { return rep->is_numeric_or_range_type (); }

// Is this constant valid as a scalar index?

  int valid_as_scalar_index (void) const
    { return rep->valid_as_scalar_index (); }

// Is this constant valid as a zero scalar index?

  int valid_as_zero_index (void) const
    { return rep->valid_as_zero_index (); }

// Does this constant correspond to a truth value?

  int is_true (void) const { return rep->is_true (); }

// Is at least one of the dimensions of this constant zero?

  int is_empty (void) const
    { return rep->is_empty (); }

// Are the dimensions of this constant zero by zero?

  int is_zero_by_zero (void) const
    {
      return ((! (is_magic_colon () || is_all_va_args () || is_unknown ()))
	      && rows () == 0 && columns () == 0);
    } 

// Values.

  double double_value (int force_string_conversion = 0) const
    { return rep->double_value (force_string_conversion); }

  Matrix matrix_value (int force_string_conversion = 0) const
    { return rep->matrix_value (force_string_conversion); }

  Complex complex_value (int force_string_conversion = 0) const
    { return rep->complex_value (force_string_conversion); }

  ComplexMatrix complex_matrix_value (int force_string_conversion = 0) const
    { return rep->complex_matrix_value (force_string_conversion); }

  Octave_str_obj all_strings (void) const
    { return rep->all_strings (); }

  const char *string_value (void) const
    { return rep->string_value (); }

  Range range_value (void) const
    { return rep->range_value (); }

  Octave_map map_value (void) const;

  tree_constant lookup_map_element (const char *ref, int insert = 0,
				    int silent = 0);

  tree_constant lookup_map_element (SLList<char*>& list,
				    int insert = 0, int silent = 0);

  ColumnVector vector_value (int /* force_string_conversion */ = 0,
			     int /* force_vector_conversion */ = 0) const 
    { return rep->vector_value (); }

  ComplexColumnVector complex_vector_value (int /* force_string_conv */ = 0,
					    int /* force_vec_conv */ = 0) const
    { return rep->complex_vector_value (); }

// Binary and unary operations.

  friend tree_constant do_binary_op (tree_constant& a, tree_constant& b,
				     tree_expression::type t);

  friend tree_constant do_unary_op (tree_constant& a,
				    tree_expression::type t);

// Conversions.  These should probably be private.  If a user of this
// class wants a certain kind of constant, he should simply ask for
// it, and we should convert it if possible.

  tree_constant convert_to_str (void)
    { return rep->convert_to_str (); }

  void convert_to_row_or_column_vector (void)
    { rep->convert_to_row_or_column_vector (); }

// Increment or decrement this constant.

  void bump_value (tree_expression::type et)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new tree_constant_rep (*rep);
	  rep->count = 1;
	}

      rep->bump_value (et);
    }

  void print (void);
  void print (ostream& os) { rep->print (os); }

// Evaluate this constant, possibly converting complex to real, or
// matrix to scalar, etc.

  tree_constant eval (int print_result)
    {
      if (! is_scalar_type ())
	rep->maybe_mutate ();

      if (print_result)
	print ();

      return *this;
    }

  Octave_object eval (int print, int /* nargout */, const Octave_object& args)
    {
      Octave_object retval;

// XXX FIXME XXX -- make it safe to call do_index() with
// args.length () == 0

      if (args.length () > 0)
	retval(0) = rep->do_index (args);
      else
	retval(0) = *this;

      if (retval(0).is_defined ())
	retval(0).eval (print);

      return retval;
    }

// Store the original text corresponding to this constant for later
// pretty printing.

  void stash_original_text (char *s)
    { rep->stash_original_text (s); }

// Pretty print this constant.
 
  void print_code (ostream& os);

  char *type_as_string (void) const
    { return rep->type_as_string (); }

// We really do need this, and it should be private:

private:

  void make_unique (void);

  tree_constant_rep *make_unique_map (void);

public:

// -------------------------------------------------------------------

// We want to eliminate this, or at least make it private.

  tree_constant_rep::constant_type const_type (void) const
    { return rep->const_type (); }

private:

// Can we make these go away?

// These need better names, since a range really is a numeric type.

  void force_numeric (int force_str_conv = 0)
    { rep->force_numeric (force_str_conv); }

  tree_constant make_numeric (int force_str_conv = 0) const
    {
      if (is_numeric_type ())
	return *this;
      else
	return rep->make_numeric (force_str_conv);
    }

#if 0
  tree_constant make_numeric_or_range (void) const
    {
      if (is_numeric_type () || is_range ())
	return *this;
      else
	return rep->make_numeric ();
    }
#endif

  tree_constant make_numeric_or_magic (void) const
    {
      if (is_numeric_type () || is_all_va_args () || is_magic_colon ())
	return *this;
      else
	return rep->make_numeric ();
    }

  tree_constant make_numeric_or_range_or_magic (void) const
    {
      if (is_numeric_type () || is_range () || is_all_va_args ()
	  || is_magic_colon ())
	return *this;
      else
	return rep->make_numeric ();
    }
};

extern int print_as_scalar (const tree_constant& val);

extern int print_as_structure (const tree_constant& val);

// XXX FIXME XXX -- this is not used very much now.  Perhaps it can be
// eliminated.
extern Octave_object vector_of_empties (int nargout, const char *fcn_name);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
