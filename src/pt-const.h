// The rest of the tree classes.                          -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_tree_const_h)
#define _tree_const_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include <stdlib.h>
#include <String.h>

#include "Range.h"
#include "builtins.h"
#include "Matrix.h"
#include "idx-vector.h"
#include "tree-base.h"

/*
 * How about a few macros?
 */

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef ABS
#define ABS(x) (((x) < 0) ? (-x) : (x))
#endif

#ifndef NULL_TREE
#define NULL_TREE (tree *)NULL
#endif

#ifndef NULL_TREE_CONST
#define NULL_TREE_CONST (tree_constant *)NULL
#endif

/*
 * The following are used by some of the functions in the
 * tree_constant_rep class that must deal with real and complex
 * matrices.  This was not done with overloaded or virtual functions
 * from the Matrix class because there is no clean way to do that --
 * the necessary functions (like elem) need to return values of
 * different types...
 */

// Given a tree_constant, and the names to be used for the real and
// complex matrix and their dimensions, declare a real or complex
// matrix, and initialize it from the tree_constant.  Note that m, cm,
// nr, and nc must not be previously declared, and they must not be
// expressions.  Since only one of the matrices will be defined after
// this macro is used, only one set of dimesions is declared.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class

#define REP_RHS_MATRIX(tc,m,cm,nr,nc) \
  int nr, nc; \
  Matrix m; \
  ComplexMatrix cm; \
  if ((tc).const_type () == tree_constant_rep::complex_matrix_constant) \
    { \
      cm = (tc).complex_matrix_value (); \
      nr = (cm).rows (); \
      nc = (cm).columns (); \
    } \
  else \
    { \
      m = (tc).matrix_value (); \
      nr = (m).rows (); \
      nc = (m).columns (); \
    }

// Assign a real or complex value to a tree_constant.
//
// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define REP_ELEM_ASSIGN(i,j,rval,cval,real_type) \
  do \
    { \
      if (type_tag == tree_constant_rep::matrix_constant) \
        { \
          if (real_type) \
            matrix->elem ((i), (j)) = (rval); \
          else \
            abort (); \
        } \
      else \
        { \
          if (real_type) \
            complex_matrix->elem ((i), (j)) = (rval); \
          else \
            complex_matrix->elem ((i), (j)) = (cval); \
        } \
    } \
  while (0)

// Given a real and complex matrix and row and column dimensions,
// declare both and size one of them.  Only one of the matrices should
// be used after this macro has been used.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define CRMATRIX(m,cm,nr,nc) \
  Matrix m; \
  ComplexMatrix cm; \
  if (type_tag == tree_constant_rep::matrix_constant) \
    { \
      (m).resize ((nr), (nc)); \
    } \
  else if (type_tag == complex_matrix_constant) \
    { \
      (cm).resize ((nr), (nc)); \
    } \
  else \
    { \
      abort (); \
    }

// Assign a real or complex matrix to a tree constant.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define ASSIGN_CRMATRIX_TO(tc,m,cm) \
  do \
    { \
      if (type_tag == matrix_constant) \
        tc = tree_constant (m); \
      else \
        tc = tree_constant (cm); \
    } \
  while (0)

// Assign an element of this tree_constant_rep's real or complex
// matrix to another real or complex matrix.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define CRMATRIX_ASSIGN_REP_ELEM(m,cm,i1,j1,i2,j2) \
  do \
    { \
      if (type_tag == matrix_constant) \
        (m).elem ((i1), (j1)) = matrix->elem ((i2), (j2)); \
      else \
        (cm).elem ((i1), (j1)) = complex_matrix->elem ((i2), (j2)); \
    } \
  while (0)

// Assign a value to an element of a real or complex matrix.  Assumes
// that the lhs and rhs are either both real or both complex types.

#define CRMATRIX_ASSIGN_ELEM(m,cm,i,j,rval,cval,real_type) \
  do \
    { \
      if (real_type) \
        (m).elem ((i), (j)) = (rval); \
      else \
        (cm).elem ((i), (j)) = (cval); \
    } \
  while (0)


/*
 * Forward class declarations.
 */
class tree;
class tree_constant;

#ifndef TREE_FCN_TYPEDEFS
#define TREE_FCN_TYPEDEFS 1

typedef tree_constant (*Text_fcn)(int, char **);
typedef tree_constant* (*General_fcn)(tree_constant *, int, int);

#endif

/*
 * The actual representation of the tree_constant.
 */
class
tree_constant_rep
{
friend class tree_constant;

  enum force_orient
    {
      no_orient,
      row_orient,
      column_orient,
    };

public:
  enum constant_type
    {
      unknown_constant,
      scalar_constant,
      matrix_constant,
      complex_scalar_constant,
      complex_matrix_constant,
      string_constant,
      range_constant,
      magic_colon,
    };

  tree_constant_rep (void);

  tree_constant_rep (double d);
  tree_constant_rep (Matrix& m);
  tree_constant_rep (DiagMatrix& d);
  tree_constant_rep (RowVector& v);
  tree_constant_rep (RowVector& v, int pcv);
  tree_constant_rep (ColumnVector& v);
  tree_constant_rep (ColumnVector& v, int pcv);

  tree_constant_rep (Complex c);
  tree_constant_rep (ComplexMatrix& m);
  tree_constant_rep (ComplexDiagMatrix& d);
  tree_constant_rep (ComplexRowVector& v);
  tree_constant_rep (ComplexRowVector& v, int pcv);
  tree_constant_rep (ComplexColumnVector& v);
  tree_constant_rep (ComplexColumnVector& v, int pcv);

  tree_constant_rep (const char *s);
  tree_constant_rep (String& s);

  tree_constant_rep (double base, double limit, double inc);
  tree_constant_rep (Range& r);

  tree_constant_rep (tree_constant_rep::constant_type t);

  tree_constant_rep (tree_constant_rep& t);

  ~tree_constant_rep (void);

#if defined (MDEBUG)
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  void resize (int i, int j);
  void resize (int i, int j, double val);

  void maybe_resize (int imax, force_orient fo = no_orient);
  void maybe_resize (int imax, int jmax);

  int valid_as_scalar_index (void);

  int is_defined (void)
    { return type_tag != tree_constant_rep::unknown_constant; }

  int is_undefined (void)
    { return type_tag == tree_constant_rep::unknown_constant; }

  int is_string_type (void)
    { return type_tag == tree_constant_rep::string_constant; }

  int is_scalar_type (void)
    { return type_tag == scalar_constant
             || type_tag == complex_scalar_constant; }

  int is_matrix_type (void)
    { return type_tag == matrix_constant
             || type_tag == complex_matrix_constant; }

  int is_real_type (void)
    { return type_tag == scalar_constant
             || type_tag == matrix_constant
	     || type_tag == range_constant; }

  int is_complex_type (void)
    { return type_tag == complex_matrix_constant
             || type_tag == complex_scalar_constant; }


  int is_numeric_type (void)
    { return type_tag == scalar_constant
             || type_tag == matrix_constant
	     || type_tag == complex_matrix_constant
             || type_tag == complex_scalar_constant; }

  int is_numeric_or_range_type (void)
    { return type_tag == scalar_constant
             || type_tag == matrix_constant
	     || type_tag == complex_matrix_constant
             || type_tag == complex_scalar_constant
	     || type_tag == range_constant; }

  double to_scalar (void);
  ColumnVector to_vector (void);
  Matrix to_matrix (void);

  tree_constant_rep::constant_type force_numeric (int force_str_conv = 0);
  tree_constant make_numeric (int force_str_conv = 0);

  friend tree_constant
    do_binary_op (tree_constant& a, tree_constant& b, tree::expression_type t);

  friend tree_constant
    do_unary_op (tree_constant& a, tree::expression_type t);

  void assign (tree_constant& rhs, tree_constant *args, int nargs);

  void do_scalar_assignment
    (tree_constant& rhs, tree_constant *args, int nargin);

  void do_matrix_assignment
    (tree_constant& rhs, tree_constant *args, int nargin);

  void do_matrix_assignment
    (tree_constant& rhs, tree_constant& i_arg);

  void do_matrix_assignment
    (tree_constant& rhs, tree_constant& i_arg, tree_constant& j_arg);

  void fortran_style_matrix_assignment (tree_constant& rhs,
					tree_constant& i_arg);

  void fortran_style_matrix_assignment (tree_constant& rhs, constant_type ci);

  void fortran_style_matrix_assignment (tree_constant& rhs, idx_vector& i);

  void vector_assignment (tree_constant& rhs, tree_constant& i_arg);

  void check_vector_assign (int rhs_nr, int rhs_nc, int ilen, char *rm);

  void do_vector_assign (tree_constant& rhs, int i);
  void do_vector_assign (tree_constant& rhs, idx_vector& i);
  void do_vector_assign (tree_constant& rhs, Range& i, int imax);

  void do_matrix_assignment
    (tree_constant& rhs, int i, tree_constant& j_arg);
  void do_matrix_assignment
    (tree_constant& rhs, idx_vector& i, tree_constant& j_arg);
  void do_matrix_assignment
    (tree_constant& rhs, Range& i, int imax, tree_constant& j_arg);
  void do_matrix_assignment
    (tree_constant& rhs, constant_type i, tree_constant& j_arg);

  void do_matrix_assignment (tree_constant& rhs, int i, int j);
  void do_matrix_assignment (tree_constant& rhs, int i, idx_vector& jv);
  void do_matrix_assignment (tree_constant& rhs, int i, Range& j);
  void do_matrix_assignment (tree_constant& rhs, int i, constant_type cj);

  void do_matrix_assignment (tree_constant& rhs, idx_vector& iv, int j);
  void do_matrix_assignment (tree_constant& rhs, idx_vector& iv,
			     idx_vector& jv);
  void do_matrix_assignment (tree_constant& rhs, idx_vector& iv, Range& j);
  void do_matrix_assignment (tree_constant& rhs, idx_vector& iv,
			     constant_type j);

  void do_matrix_assignment (tree_constant& rhs, Range& i, int j);
  void do_matrix_assignment (tree_constant& rhs, Range& i, idx_vector& jv);
  void do_matrix_assignment (tree_constant& rhs, Range& i, Range& j);
  void do_matrix_assignment (tree_constant& rhs, Range& i, constant_type j);

  void do_matrix_assignment (tree_constant& rhs, constant_type i, int j);
  void do_matrix_assignment (tree_constant& rhs, constant_type i,
			     idx_vector& jv);
  void do_matrix_assignment (tree_constant& rhs, constant_type i, Range& j);
  void do_matrix_assignment (tree_constant& rhs, constant_type i,
			     constant_type j);

  void bump_value (tree::expression_type);

  void eval (int print);

  tree_constant *eval (tree_constant *args, int n_in, int n_out, int print);

  tree_constant do_scalar_index (tree_constant *args, int nargin);

  tree_constant do_matrix_index (tree_constant *args, int nargin);

  tree_constant do_matrix_index (tree_constant& i_arg);

  tree_constant do_matrix_index (tree_constant& i_arg, tree_constant& j_arg);

  tree_constant do_matrix_index (constant_type i);

  tree_constant fortran_style_matrix_index (tree_constant& i_arg);
  tree_constant fortran_style_matrix_index (Matrix& mi);

  tree_constant do_vector_index (tree_constant& i_arg);

  tree_constant do_matrix_index (int i, tree_constant& i_arg);
  tree_constant do_matrix_index (idx_vector& i, tree_constant& i_arg);
  tree_constant do_matrix_index (Range& i, int imax, tree_constant& i_arg);
  tree_constant do_matrix_index (constant_type i, tree_constant& i_arg);

  tree_constant do_matrix_index (int i, int j);
  tree_constant do_matrix_index (int i, idx_vector& j);
  tree_constant do_matrix_index (int i, Range& j);
  tree_constant do_matrix_index (int i, constant_type cj);

  tree_constant do_matrix_index (idx_vector& i, int j);
  tree_constant do_matrix_index (idx_vector& i, idx_vector& j);
  tree_constant do_matrix_index (idx_vector& i, Range& j);
  tree_constant do_matrix_index (idx_vector& i, constant_type j);

  tree_constant do_matrix_index (Range& i, int j);
  tree_constant do_matrix_index (Range& i, idx_vector& j);
  tree_constant do_matrix_index (Range& i, Range& j);
  tree_constant do_matrix_index (Range& i, constant_type j);

  tree_constant do_matrix_index (constant_type i, int j);
  tree_constant do_matrix_index (constant_type i, idx_vector& j);
  tree_constant do_matrix_index (constant_type i, Range& j);
  tree_constant do_matrix_index (constant_type i, constant_type j);

  int save (ostream& os, int mark_as_global);
  int save_three_d (ostream& os, int parametric);
  int load (istream& is);
  constant_type load (istream& is, constant_type t);

  double double_value (void);
  Matrix matrix_value (void);
  Complex complex_value (void);
  ComplexMatrix complex_matrix_value (void);
  char *string_value (void);
  Range range_value (void);

  int rows (void);
  int columns (void);

  tree_constant all (void);
  tree_constant any (void);
  tree_constant isstr (void);

  tree_constant convert_to_str (void);

  tree_constant cumprod (void);
  tree_constant cumsum (void);
  tree_constant prod (void);
  tree_constant sum (void);
  tree_constant sumsq (void);

  tree_constant diag (void);
  tree_constant diag (tree_constant& a);

  friend tree_constant fill_matrix (tree_constant& a, double d,
				    char *warn_for);
  friend tree_constant fill_matrix (tree_constant& a, tree_constant& b,
				    double d, char *warn_for);

  friend tree_constant identity_matrix (tree_constant& a);
  friend tree_constant identity_matrix (tree_constant& a, tree_constant& b);

  friend tree_constant find_nonzero_elem_idx (tree_constant& a);

  friend tree_constant *matrix_log (tree_constant& a);
  friend tree_constant *matrix_sqrt (tree_constant& a);

  friend tree_constant *column_max (tree_constant *args, int nargin,
				    int nargout);

  friend tree_constant *column_min (tree_constant *args, int nargin,
				    int nargout);
  
  friend tree_constant *sort (tree_constant *args, int nargin, int nargout);
 
  friend tree_constant *feval (tree_constant *args, int nargin, int nargout);

  friend tree_constant eval_string (tree_constant& arg, int& parse_status);

  friend tree_constant get_user_input (tree_constant *args, int nargin,
				       int nargout, int debug = 0);

  void print_if_string (ostream& os, int warn);

  constant_type const_type (void) { return type_tag; }

  tree_constant mapper (Mapper_fcn& m_fcn, int print);

private:
  int count;
  constant_type type_tag;
  union
    {
      double scalar;			// A real scalar constant.
      Matrix *matrix;			// A real matrix constant.
      Complex *complex_scalar;		// A real scalar constant.
      ComplexMatrix *complex_matrix;	// A real matrix constant.
      char *string;			// A character string constant.
      Range *range;			// A set of evenly spaced values.
    };
};

/*
 * Constants.  Nice -- No need to interpret them anymore.  Logically,
 * this should be ahead of the tree_constant_rep class, but that
 * causes problems with my version of g++ (~2.2.2)...
 */
class tree_constant : public tree
{
friend class tree_constant_rep;

public:
  tree_constant (void)
    { rep = new tree_constant_rep (); rep->count = 1; }

  tree_constant (double d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (Matrix& m)
    { rep = new tree_constant_rep (m); rep->count = 1; }
  tree_constant (DiagMatrix& d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (RowVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (RowVector& v, int pcv)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }
  tree_constant (ColumnVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (ColumnVector& v, int pcv)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (Complex c)
    { rep = new tree_constant_rep (c); rep->count = 1; }
  tree_constant (ComplexMatrix& m)
    { rep = new tree_constant_rep (m); rep->count = 1; }
  tree_constant (ComplexDiagMatrix& d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (ComplexRowVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (ComplexRowVector& v, int pcv)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }
  tree_constant (ComplexColumnVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (ComplexColumnVector& v, int pcv)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const char *s)
    { rep = new tree_constant_rep (s); rep->count = 1; }
  tree_constant (String& s)
    { rep = new tree_constant_rep (s); rep->count = 1; }

  tree_constant (double base, double limit, double inc)
    { rep = new tree_constant_rep (base, limit, inc); rep->count = 1; }
  tree_constant (Range& r)
    { rep = new tree_constant_rep (r); rep->count = 1; }

  tree_constant (tree_constant_rep::constant_type t)
    { rep = new tree_constant_rep (t); rep->count = 1; }

  tree_constant (tree_constant& a)
    { rep = a.rep; rep->count++; }
  tree_constant (tree_constant_rep& r)
    { rep = &r; rep->count++; }

  ~tree_constant (void);

#if defined (MDEBUG)
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  tree_constant operator = (tree_constant& a)
    {
      if (--rep->count <= 0 && rep != a.rep)
	delete rep;

      rep = a.rep;
      rep->count++;
      return *this;  
    }

  int is_constant (void) { return 1; }

  int is_scalar_type (void) { return rep->is_scalar_type (); }
  int is_matrix_type (void) { return rep->is_matrix_type (); }

  int is_real_type (void) { return rep->is_real_type (); }
  int is_complex_type (void) { return rep->is_complex_type (); }

  int is_numeric_type (void) { return rep->is_numeric_type (); }

  int is_numeric_or_range_type (void)
    { return rep->is_numeric_or_range_type (); }

  int is_string_type (void) { return rep->is_string_type (); }

  int valid_as_scalar_index (void) { return rep->valid_as_scalar_index (); }

  int is_defined (void) { return rep->is_defined (); }
  int is_undefined (void) { return rep->is_undefined (); }

  double to_scalar (void) { return rep->to_scalar (); }
  ColumnVector to_vector (void) { return rep->to_vector (); }
  Matrix to_matrix (void) { return rep->to_matrix (); }

  tree_constant_rep::constant_type force_numeric (int force_str_conv = 0)
    { return rep->force_numeric (force_str_conv); }

  tree_constant make_numeric (int force_str_conv = 0)
    {
      if (is_numeric_type ())
	return *this;
      else
	return rep->make_numeric (force_str_conv);
    }

  tree_constant make_numeric_or_range (void)
    {
      if (is_numeric_type ()
	  || rep->type_tag == tree_constant_rep::range_constant)
	return *this;
      else
	return rep->make_numeric ();
    }

  tree_constant make_numeric_or_magic (void)
    {
      if (is_numeric_type ()
	  || rep->type_tag == tree_constant_rep::magic_colon)
	return *this;
      else
	return rep->make_numeric ();
    }

  tree_constant make_numeric_or_range_or_magic (void)
    {
      if (is_numeric_type ()
	  || rep->type_tag == tree_constant_rep::magic_colon
	  || rep->type_tag == tree_constant_rep::range_constant)
	return *this;
      else
	return rep->make_numeric ();
    }

  tree_constant assign (tree_constant& rhs, tree_constant *args, int nargs)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new tree_constant_rep (*rep);
	  rep->count = 1;
	}
      rep->assign (rhs, args, nargs);
      return *this;
    }

  int save (ostream& os, int mark_as_global = 0)
    { return rep->save (os, mark_as_global); }
  int save_three_d (ostream& os, int parametric = 0)
    { return rep->save_three_d (os, parametric); }

  int load (istream& is) { return rep->load (is); }
  tree_constant_rep::constant_type load
    (istream& is, tree_constant_rep::constant_type t)
      { return rep->load (is, t); }

  double double_value (void) { return rep->double_value (); }
  Matrix matrix_value (void) { return rep->matrix_value (); }
  Complex complex_value (void) { return rep->complex_value (); }
  ComplexMatrix complex_matrix_value (void)
    { return rep->complex_matrix_value (); }
  char *string_value (void) { return rep->string_value (); }
  Range range_value (void) { return rep->range_value (); }

  int rows (void) { return rep->rows (); }
  int columns (void) { return rep->columns (); }

  int is_empty (void) { return (rows () == 0 || columns () == 0); }

  tree_constant all (void) { return rep->all (); }
  tree_constant any (void) { return rep->any (); }
  tree_constant isstr (void) { return rep->isstr (); }

  tree_constant convert_to_str (void) { return rep->convert_to_str (); }

  tree_constant cumprod (void) { return rep->cumprod (); }
  tree_constant cumsum (void) { return rep->cumsum (); }
  tree_constant prod (void) { return rep->prod (); }
  tree_constant sum (void) { return rep->sum (); }
  tree_constant sumsq (void) { return rep->sumsq (); }

  tree_constant diag (void) { return rep->diag (); }
  tree_constant diag (tree_constant& a) { return rep->diag (a); }

  void print_if_string (ostream& os, int warn)
    { rep->print_if_string (os, warn); }

  tree_constant_rep::constant_type const_type (void)
    { return rep->const_type (); }

  tree_constant mapper (Mapper_fcn& m_fcn, int print)
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
    { rep->eval (print); return *this; }

// A tree constant can have one and only one value to return.
  tree_constant *eval (int print, int nargout)
    {
      rep->eval (print);
      tree_constant *retval = new tree_constant [2];
      retval[0] = *this;
      return retval;
    }

  tree_constant *eval (tree_constant *args, int n_in, int n_out, int print)
    { return rep->eval (args, n_in, n_out, print); }

private:
  tree_constant_rep *rep;
};

/*
 * Here are some extra functions that are related to the tree_constant
 * class but that don't need to be class members or friends.
 */

extern tree_constant *vector_of_empties (int nargout, char *fcn_name);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
