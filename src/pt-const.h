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

#include <stdlib.h>

#include "builtins.h"
#include "tree-base.h"
#include "Matrix.h" // Needed for some inline functions.
#include "Range.h"  // Ditto.

class idx_vector;

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
  tree_constant_rep (const Matrix& m);
  tree_constant_rep (const DiagMatrix& d);
  tree_constant_rep (const RowVector& v);
  tree_constant_rep (const RowVector& v, int pcv);
  tree_constant_rep (const ColumnVector& v);
  tree_constant_rep (const ColumnVector& v, int pcv);

  tree_constant_rep (const Complex& c);
  tree_constant_rep (const ComplexMatrix& m);
  tree_constant_rep (const ComplexDiagMatrix& d);
  tree_constant_rep (const ComplexRowVector& v);
  tree_constant_rep (const ComplexRowVector& v, int pcv);
  tree_constant_rep (const ComplexColumnVector& v);
  tree_constant_rep (const ComplexColumnVector& v, int pcv);

  tree_constant_rep (const char *s);

  tree_constant_rep (double base, double limit, double inc);
  tree_constant_rep (const Range& r);

  tree_constant_rep (tree_constant_rep::constant_type t);

  tree_constant_rep (const tree_constant_rep& t);

  ~tree_constant_rep (void);

#if defined (MDEBUG)
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  void resize (int i, int j);
  void resize (int i, int j, double val);

  void maybe_resize (int imax, force_orient fo = no_orient);
  void maybe_resize (int imax, int jmax);

  int valid_as_scalar_index (void) const;

  int is_defined (void) const
    { return type_tag != tree_constant_rep::unknown_constant; }

  int is_undefined (void) const
    { return type_tag == tree_constant_rep::unknown_constant; }

  int is_string_type (void) const
    { return type_tag == tree_constant_rep::string_constant; }

  int is_scalar_type (void) const
    { return type_tag == scalar_constant
             || type_tag == complex_scalar_constant; }

  int is_matrix_type (void) const
    { return type_tag == matrix_constant
             || type_tag == complex_matrix_constant; }

  int is_real_type (void) const
    { return type_tag == scalar_constant
             || type_tag == matrix_constant
	     || type_tag == range_constant; }

  int is_complex_type (void) const
    { return type_tag == complex_matrix_constant
             || type_tag == complex_scalar_constant; }


  int is_numeric_type (void) const
    { return type_tag == scalar_constant
             || type_tag == matrix_constant
	     || type_tag == complex_matrix_constant
             || type_tag == complex_scalar_constant; }

  int is_numeric_or_range_type (void) const
    { return type_tag == scalar_constant
             || type_tag == matrix_constant
	     || type_tag == complex_matrix_constant
             || type_tag == complex_scalar_constant
	     || type_tag == range_constant; }

  double to_scalar (void) const;
  ColumnVector to_vector (void) const;
  Matrix to_matrix (void) const;

  tree_constant_rep::constant_type force_numeric (int force_str_conv = 0);
  tree_constant make_numeric (int force_str_conv = 0) const;

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

  void check_vector_assign (int rhs_nr, int rhs_nc, int ilen,
			    const char *rm);

  void do_vector_assign (tree_constant& rhs, int i);
  void do_vector_assign (tree_constant& rhs, idx_vector& i);
  void do_vector_assign (tree_constant& rhs, Range& i);

  void do_matrix_assignment
    (tree_constant& rhs, int i, tree_constant& j_arg);
  void do_matrix_assignment
    (tree_constant& rhs, idx_vector& i, tree_constant& j_arg);
  void do_matrix_assignment
    (tree_constant& rhs, Range& i, tree_constant& j_arg);
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

  void delete_row (int);
  void delete_rows (idx_vector& i);
  void delete_rows (Range& i);

  void delete_column (int);
  void delete_columns (idx_vector& j);
  void delete_columns (Range& j);

  void bump_value (tree::expression_type);

  void eval (int print);

  tree_constant *eval (const tree_constant *args, int n_in, int n_out,
		       int print);

  tree_constant do_scalar_index (const tree_constant *args,
				 int nargin) const;

  tree_constant do_matrix_index (const tree_constant *args, int nargin) const;

  tree_constant do_matrix_index (const tree_constant& i_arg) const;

  tree_constant do_matrix_index (const tree_constant& i_arg,
				 const tree_constant& j_arg) const; 

  tree_constant do_matrix_index (constant_type i) const;

  tree_constant fortran_style_matrix_index (const tree_constant& i_arg) const;
  tree_constant fortran_style_matrix_index (const Matrix& mi) const;

  tree_constant do_vector_index (const tree_constant& i_arg) const;

  tree_constant do_matrix_index (int i, const tree_constant& i_arg) const;
  tree_constant do_matrix_index (const idx_vector& i,
				 const tree_constant& i_arg) const; 
  tree_constant do_matrix_index (const Range& i,
				 const tree_constant& i_arg) const;
  tree_constant do_matrix_index (constant_type i,
				 const tree_constant& i_arg) const;

  tree_constant do_matrix_index (int i, int j) const;
  tree_constant do_matrix_index (int i, const idx_vector& j) const;
  tree_constant do_matrix_index (int i, const Range& j) const;
  tree_constant do_matrix_index (int i, constant_type cj) const;

  tree_constant do_matrix_index (const idx_vector& i, int j) const;
  tree_constant do_matrix_index (const idx_vector& i,
				 const idx_vector& j) const;
  tree_constant do_matrix_index (const idx_vector& i, const Range& j) const;
  tree_constant do_matrix_index (const idx_vector& i, constant_type j) const;

  tree_constant do_matrix_index (const Range& i, int j) const;
  tree_constant do_matrix_index (const Range& i, const idx_vector& j) const;
  tree_constant do_matrix_index (const Range& i, const Range& j) const;
  tree_constant do_matrix_index (const Range& i, constant_type j) const;

  tree_constant do_matrix_index (constant_type i, int j) const;
  tree_constant do_matrix_index (constant_type i, const idx_vector& j) const;
  tree_constant do_matrix_index (constant_type i, const Range& j) const;
  tree_constant do_matrix_index (constant_type i, constant_type j) const;

  int save (ostream& os, int mark_as_global, int precision);
  int save_three_d (ostream& os, int parametric);
  int load (istream& is);
  constant_type load (istream& is, constant_type t);

  double double_value (void) const;
  Matrix matrix_value (void) const;
  Complex complex_value (void) const;
  ComplexMatrix complex_matrix_value (void) const;
  char *string_value (void) const;
  Range range_value (void) const;

  int rows (void) const;
  int columns (void) const;

  tree_constant all (void) const;
  tree_constant any (void) const;
  tree_constant isstr (void) const;

  tree_constant convert_to_str (void);

  tree_constant cumprod (void) const;
  tree_constant cumsum (void) const;
  tree_constant prod (void) const;
  tree_constant sum (void) const;
  tree_constant sumsq (void) const;

  tree_constant diag (void) const;
  tree_constant diag (const tree_constant& a) const;

  friend tree_constant fill_matrix (const tree_constant& a,
				    double d, const char *warn_for);
  friend tree_constant fill_matrix (const tree_constant& a,
				    const tree_constant& b,
				    double d, const char *warn_for);

  friend tree_constant identity_matrix (const tree_constant& a);
  friend tree_constant identity_matrix (const tree_constant& a,
					const tree_constant& b);

  friend tree_constant find_nonzero_elem_idx (const tree_constant& a);

  friend tree_constant *matrix_log (const tree_constant& a);
  friend tree_constant *matrix_sqrt (const tree_constant& a);

  friend tree_constant *column_max (const tree_constant *args, int nargin,
				    int nargout);

  friend tree_constant *column_min (const tree_constant *args, int nargin,
				    int nargout);
  
  friend tree_constant *sort (const tree_constant *args, int nargin,
			      int nargout);
 
  friend tree_constant *feval (const tree_constant *args, int nargin,
			       int nargout);

  friend tree_constant eval_string (const tree_constant& arg, int&
				    parse_status);

  friend tree_constant get_user_input (const tree_constant *args,
				       int nargin, int nargout,
				       int debug = 0);

  constant_type const_type (void) const { return type_tag; }

  tree_constant mapper (Mapper_fcn& m_fcn, int print) const;

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
  tree_constant (const Matrix& m)
    { rep = new tree_constant_rep (m); rep->count = 1; }
  tree_constant (const DiagMatrix& d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (const RowVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (const RowVector& v, int pcv)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }
  tree_constant (const ColumnVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (const ColumnVector& v, int pcv)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }

  tree_constant (const Complex& c)
    { rep = new tree_constant_rep (c); rep->count = 1; }
  tree_constant (const ComplexMatrix& m)
    { rep = new tree_constant_rep (m); rep->count = 1; }
  tree_constant (const ComplexDiagMatrix& d)
    { rep = new tree_constant_rep (d); rep->count = 1; }
  tree_constant (const ComplexRowVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (const ComplexRowVector& v, int pcv)
    { rep = new tree_constant_rep (v, pcv); rep->count = 1; }
  tree_constant (const ComplexColumnVector& v)
    { rep = new tree_constant_rep (v); rep->count = 1; }
  tree_constant (const ComplexColumnVector& v, int pcv)
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

  tree_constant operator = (tree_constant& a)
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
    { rep->eval (print); return *this; }

// A tree constant can have one and only one value to return.
  tree_constant *eval (int print, int nargout)
    {
      rep->eval (print);
      tree_constant *retval = new tree_constant [2];
      retval[0] = *this;
      return retval;
    }

  tree_constant eval (int argc, char **argv, int print);

  tree_constant *eval (const tree_constant *args, int n_in, int n_out,
		       int print)
    { return rep->eval (args, n_in, n_out, print); }

private:
  tree_constant_rep *rep;
};

/*
 * Here are some extra functions that are related to the tree_constant
 * class but that don't need to be class members or friends.
 */

extern tree_constant *vector_of_empties (int nargout, const char *fcn_name);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
