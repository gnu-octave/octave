// tree-const.h                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_tree_const_h)
#define octave_tree_const_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <stdlib.h>

#include "mx-base.h"
#include "Range.h"

#include "tree-base.h"
#include "tree-expr.h"
#include "tc-rep.h"
#include "oct-obj.h"

class idx_vector;

struct Mapper_fcn;

/*
 * Constants.
 */
class
tree_constant : public tree_fvc
{
friend class tree_constant_rep;

public:
  tree_constant (void)
    { rep = new tree_constant_rep (); rep->count = 1; }

  tree_constant (double d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (const Matrix& m)
    { rep = new tree_constant_rep (m); rep->count = 1; }
  tree_constant (const DiagMatrix& d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (const RowVector& v, int pcv = -1)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }
  tree_constant (const ColumnVector& v, int pcv = -1)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const Complex& c)
    { rep = new tree_constant_rep (c); rep->count = 1; }
  tree_constant (const ComplexMatrix& m)
    { rep = new tree_constant_rep (m); rep->count = 1; }
  tree_constant (const ComplexDiagMatrix& d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (const ComplexRowVector& v, int pcv = -1)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }
  tree_constant (const ComplexColumnVector& v, int pcv = -1)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const char *s)
    { rep = new tree_constant_rep (s); rep->count = 1; }

  tree_constant (double base, double limit, double inc)
    { rep = new tree_constant_rep (base, limit, inc); rep->count = 1; }
  tree_constant (const Range& r)
    { rep = new tree_constant_rep (r); rep->count = 1; }

  tree_constant (tree_constant_rep::constant_type t)
    { rep = new tree_constant_rep (t); rep->count = 1; }

  tree_constant (const tree_constant& a)
    { rep = a.rep; rep->count++; }
  tree_constant (tree_constant_rep& r)
    { rep = &r; rep->count++; }

  ~tree_constant (void);

#if defined (MDEBUG)
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  tree_constant operator = (const tree_constant& a)
    {
      if (--rep->count <= 0 && rep != a.rep)
	delete rep;

      rep = a.rep;
      rep->count++;
      return *this;  
    }

  int is_constant (void) const { return 1; }

  int is_scalar_type (void) const { return rep->is_scalar_type (); }
  int is_matrix_type (void) const { return rep->is_matrix_type (); }

  int is_real_type (void) const { return rep->is_real_type (); }
  int is_complex_type (void) const { return rep->is_complex_type (); }

  int is_numeric_type (void) const { return rep->is_numeric_type (); }

  int is_numeric_or_range_type (void) const
    { return rep->is_numeric_or_range_type (); }

  int is_string_type (void) const { return rep->is_string_type (); }

  int valid_as_scalar_index (void) const
    { return rep->valid_as_scalar_index (); }

  int is_defined (void) const { return rep->is_defined (); }
  int is_undefined (void) const { return rep->is_undefined (); }

  double to_scalar (void) const { return rep->to_scalar (); }
  ColumnVector to_vector (void) const { return rep->to_vector (); }
  Matrix to_matrix (void) const { return rep->to_matrix (); }

  tree_constant_rep::constant_type force_numeric (int force_str_conv = 0)
    { return rep->force_numeric (force_str_conv); }

  tree_constant make_numeric (int force_str_conv = 0) const
    {
      if (is_numeric_type ())
	return *this;
      else
	return rep->make_numeric (force_str_conv);
    }

  tree_constant make_numeric_or_range (void) const
    {
      if (is_numeric_type ()
	  || rep->type_tag == tree_constant_rep::range_constant)
	return *this;
      else
	return rep->make_numeric ();
    }

  tree_constant make_numeric_or_magic (void) const
    {
      if (is_numeric_type ()
	  || rep->type_tag == tree_constant_rep::magic_colon)
	return *this;
      else
	return rep->make_numeric ();
    }

  tree_constant make_numeric_or_range_or_magic (void) const
    {
      if (is_numeric_type ()
	  || rep->type_tag == tree_constant_rep::magic_colon
	  || rep->type_tag == tree_constant_rep::range_constant)
	return *this;
      else
	return rep->make_numeric ();
    }

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

  int save (ostream& os, int mark_as_global = 0, int precision = 17)
    { return rep->save (os, mark_as_global, precision); }
  int save_three_d (ostream& os, int parametric = 0)
    { return rep->save_three_d (os, parametric); }

  int load (istream& is) { return rep->load (is); }
  tree_constant_rep::constant_type load
    (istream& is, tree_constant_rep::constant_type t)
      { return rep->load (is, t); }

  double double_value (void) const { return rep->double_value (); }
  Matrix matrix_value (void) const { return rep->matrix_value (); }
  Complex complex_value (void) const { return rep->complex_value (); }
  ComplexMatrix complex_matrix_value (void) const
    { return rep->complex_matrix_value (); }
  char *string_value (void) const { return rep->string_value (); }
  Range range_value (void) const { return rep->range_value (); }

  int rows (void) const { return rep->rows (); }
  int columns (void) const { return rep->columns (); }

  int is_empty (void) const
    {
      return (rep->type_tag != tree_constant_rep::magic_colon
	      && rep->type_tag != tree_constant_rep::unknown_constant
	      && (rows () == 0 || columns () == 0));
    }

  int is_zero_by_zero (void) const
    {
      return (rep->type_tag != tree_constant_rep::magic_colon
	      && rep->type_tag != tree_constant_rep::unknown_constant
	      && rows () == 0
	      && columns () == 0);
    } 


  tree_constant all (void) const { return rep->all (); }
  tree_constant any (void) const { return rep->any (); }
  tree_constant isstr (void) const { return rep->isstr (); }

  tree_constant convert_to_str (void) { return rep->convert_to_str (); }

  void convert_to_row_or_column_vector (void)
    { rep->convert_to_row_or_column_vector (); }

  int is_true (void) const { return rep->is_true (); }

  tree_constant cumprod (void) const { return rep->cumprod (); }
  tree_constant cumsum (void) const { return rep->cumsum (); }
  tree_constant prod (void) const { return rep->prod (); }
  tree_constant sum (void) const { return rep->sum (); }
  tree_constant sumsq (void) const { return rep->sumsq (); }

  tree_constant diag (void) const { return rep->diag (); }
  tree_constant diag (const tree_constant& a) const { return rep->diag (a); }

  tree_constant_rep::constant_type const_type (void) const
    { return rep->const_type (); }

  tree_constant mapper (Mapper_fcn& m_fcn, int print) const
    { return rep->mapper (m_fcn, print); }

  void bump_value (tree::expression_type et)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new tree_constant_rep (*rep);
	  rep->count = 1;
	}
      rep->bump_value (et);
    }

  tree_constant eval (int print)
    {
      rep->maybe_mutate ();
      if (print)
	rep->print ();
      return *this;
    }

  Octave_object eval (int print, int nargout, const Octave_object& args)
    {
      Octave_object retval (1);

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

private:
  tree_constant_rep *rep;
};

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
