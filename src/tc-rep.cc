// The constants for the tree class.                      -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined (__GNUG__)
#pragma implementation
#endif

#include <ctype.h>
#include <string.h>
#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#include "mx-base.h"
#include "Range.h"

#include "variables.h"
#include "error.h"
#include "gripes.h"
#include "user-prefs.h"
#include "utils.h"
#include "pager.h"
#include "pr-output.h"
#include "tree-const.h"
#include "idx-vector.h"

#include "tc-inlines.cc"

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
  int nr = 0; \
  int nc = 0; \
  Matrix m; \
  ComplexMatrix cm; \
  if ((tc).const_type () == tree_constant_rep::complex_matrix_constant) \
    { \
      cm = (tc).complex_matrix_value (); \
      nr = (cm).rows (); \
      nc = (cm).columns (); \
    } \
  else if ((tc).const_type () == tree_constant_rep::matrix_constant) \
    { \
      m = (tc).matrix_value (); \
      nr = (m).rows (); \
      nc = (m).columns (); \
    } \
  else \
    abort ();

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
    (m).resize ((nr), (nc)); \
  else if (type_tag == complex_matrix_constant) \
    (cm).resize ((nr), (nc)); \
  else \
    abort (); \

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


// A couple of handy helper functions.

static int
any_element_less_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) < val)
	return 1;
  return 0;
}

static int
any_element_greater_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) > val)
	return 1;
  return 0;
}

static int
any_element_is_complex (const ComplexMatrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (imag (a.elem (i, j)) != 0.0)
	return 1;
  return 0;
}

// Now, the classes.

/*
 * The real representation of constants.
 */
tree_constant_rep::tree_constant_rep (void)
{
  type_tag = unknown_constant;
}

tree_constant_rep::tree_constant_rep (double d)
{
  scalar = d;
  type_tag = scalar_constant;
}

tree_constant_rep::tree_constant_rep (const Matrix& m)
{
  if (m.rows () == 1 && m.columns () == 1)
    {
      scalar = m.elem (0, 0);
      type_tag = scalar_constant;
    }
  else
    {
      matrix = new Matrix (m);
      type_tag = matrix_constant;
    }
}

tree_constant_rep::tree_constant_rep (const DiagMatrix& d)
{
  if (d.rows () == 1 && d.columns () == 1)
    {
      scalar = d.elem (0, 0);
      type_tag = scalar_constant;
    }
  else
    {
      matrix = new Matrix (d);
      type_tag = matrix_constant;
    }
}

tree_constant_rep::tree_constant_rep (const RowVector& v, int
				      prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v.elem (0);
      type_tag = scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  Matrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
      else
	{
	  Matrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
    }
}

tree_constant_rep::tree_constant_rep (const ColumnVector& v,
				      int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v.elem (0);
      type_tag = scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  Matrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
      else
	{
	  Matrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
    }
}

tree_constant_rep::tree_constant_rep (const Complex& c)
{
  complex_scalar = new Complex (c);
  type_tag = complex_scalar_constant;
}

tree_constant_rep::tree_constant_rep (const ComplexMatrix& m)
{
  if (m.rows () == 1 && m.columns () == 1)
    {
      complex_scalar = new Complex (m.elem (0, 0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      complex_matrix = new ComplexMatrix (m);
      type_tag = complex_matrix_constant;
    }
}

tree_constant_rep::tree_constant_rep (const ComplexDiagMatrix& d)
{
  if (d.rows () == 1 && d.columns () == 1)
    {
      complex_scalar = new Complex (d.elem (0, 0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      complex_matrix = new ComplexMatrix (d);
      type_tag = complex_matrix_constant;
    }
}

tree_constant_rep::tree_constant_rep (const ComplexRowVector& v,
				      int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      complex_scalar = new Complex (v.elem (0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  ComplexMatrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  ComplexMatrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
    }
}

tree_constant_rep::tree_constant_rep (const ComplexColumnVector& v,
				      int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      complex_scalar = new Complex (v.elem (0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  ComplexMatrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  ComplexMatrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
    }
}

tree_constant_rep::tree_constant_rep (const char *s)
{
  string = strsave (s);
  type_tag = string_constant;
}

tree_constant_rep::tree_constant_rep (double b, double l, double i)
{
  range = new Range (b, l, i);
  int nel = range->nelem ();
  if (nel < 0)
    {
      delete range;
      type_tag = unknown_constant;
      if (nel == -1)
	::error ("number of elements in range exceeds INT_MAX");
      else
	::error ("invalid range");
    }
  else if (nel > 1)
    type_tag = range_constant;
  else
    {
      delete range;
      if (nel == 1)
	{
	  scalar = b;
	  type_tag = scalar_constant;
	}
      else if (nel == 0)
	{
	  matrix = new Matrix ();
	  type_tag = matrix_constant;
	}
      else
	panic_impossible ();
    }
}

tree_constant_rep::tree_constant_rep (const Range& r)
{
  if (r.nelem () > 1)
    {
      range = new Range (r);
      type_tag = range_constant;
    }
  else if (r.nelem () == 1)
    {
      scalar = r.base ();
      type_tag = scalar_constant;
    }
  else if (r.nelem () == 0)
    {
      matrix = new Matrix ();
      type_tag = matrix_constant;
    }
  else
    panic_impossible ();
}

tree_constant_rep::tree_constant_rep (tree_constant_rep::constant_type t)
{
  assert (t == magic_colon);

  type_tag = magic_colon;
}

tree_constant_rep::tree_constant_rep (const tree_constant_rep& t)
{
  type_tag = t.type_tag;

  switch (t.type_tag)
    {
    case unknown_constant:
      break;
    case scalar_constant:
      scalar = t.scalar;
      break;
    case matrix_constant:
      matrix = new Matrix (*(t.matrix));
      break;
    case string_constant:
      string = strsave (t.string);
      break;
    case complex_matrix_constant:
      complex_matrix = new ComplexMatrix (*(t.complex_matrix));
      break;
    case complex_scalar_constant:
      complex_scalar = new Complex (*(t.complex_scalar));
      break;
    case range_constant:
      range = new Range (*(t.range));
      break;
    case magic_colon:
      break;
    default:
      panic_impossible ();
      break;
    }
}

tree_constant_rep::~tree_constant_rep (void)
{
  switch (type_tag)
    {
    case unknown_constant:
      break;
    case scalar_constant:
      break;
    case matrix_constant:
      delete matrix;
      break;
    case complex_scalar_constant:
      delete complex_scalar;
      break;
    case complex_matrix_constant:
      delete complex_matrix;
      break;
    case string_constant:
      delete [] string;
      break;
    case range_constant:
      delete range;
      break;
    case magic_colon:
      break;
    default:
      panic_impossible ();
      break;
    }
}

#if defined (MDEBUG)
void *
tree_constant_rep::operator new (size_t size)
{
  tree_constant_rep *p = ::new tree_constant_rep;
  cerr << "tree_constant_rep::new(): " << p << "\n";
  return p;
}

void
tree_constant_rep::operator delete (void *p, size_t size)
{
  cerr << "tree_constant_rep::delete(): " << p << "\n";
  ::delete p;
}
#endif

void
tree_constant_rep::resize (int i, int j)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->resize (i, j);
      break;
    case complex_matrix_constant:
      complex_matrix->resize (i, j);
      break;
    default:
      panic_impossible ();
      break;
    }
}

void
tree_constant_rep::resize (int i, int j, double val)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->resize (i, j, val);
      break;
    case complex_matrix_constant:
      complex_matrix->resize (i, j, val);
      break;
    default:
      panic_impossible ();
      break;
    }
}

void
tree_constant_rep::maybe_resize (int i, int j)
{
  int nr = rows ();
  int nc = columns ();

  i++;
  j++;

  assert (i > 0 && j > 0);

  if (i > nr || j > nc)
    {
      if (user_pref.resize_on_range_error)
	resize (MAX (i, nr), MAX (j, nc), 0.0);
      else
	{
	  if (i > nr)
	    ::error ("row index = %d exceeds max row dimension = %d", i, nr);

	  if (j > nc)
	    ::error ("column index = %d exceeds max column dimension = %d",
		     j, nc);
	}
    }
}

void
tree_constant_rep::maybe_resize (int i, force_orient f_orient = no_orient)
{
  int nr = rows ();
  int nc = columns ();

  i++;

  assert (i >= 0 && (nr <= 1 || nc <= 1));

// This function never reduces the size of a vector, and all vectors
// have dimensions of at least 0x0.  If i is 0, it is either because
// a vector has been indexed with a vector of all zeros (in which case
// the index vector is empty and nothing will happen) or a vector has
// been indexed with 0 (an error which will be caught elsewhere).
  if (i == 0)
    return;

  if (nr <= 1 && nc <= 1 && i >= 1)
    {
      if (user_pref.resize_on_range_error)
	{
	  if (f_orient == row_orient)
	    resize (1, i, 0.0);
	  else if (f_orient == column_orient)
	    resize (i, 1, 0.0);
	  else if (user_pref.prefer_column_vectors)
	    resize (i, 1, 0.0);
	  else
	    resize (1, i, 0.0);
	}
      else
	::error ("matrix index = %d exceeds max dimension = %d", i, nc);
    }
  else if (nr == 1 && i > nc)
    {
      if (user_pref.resize_on_range_error)
	resize (1, i, 0.0);
      else
	::error ("matrix index = %d exceeds max dimension = %d", i, nc);
    }
  else if (nc == 1 && i > nr)
    {
      if (user_pref.resize_on_range_error)
	resize (i, 1, 0.0);
      else
	::error ("matrix index = %d exceeds max dimension = ", i, nc);
    }
}

double
tree_constant_rep::to_scalar (void) const
{
  tree_constant tmp = make_numeric ();

  double retval = 0.0;

  switch (tmp.const_type ())
    {
    case tree_constant_rep::scalar_constant:
    case tree_constant_rep::complex_scalar_constant:
      retval = tmp.double_value ();
      break;
    case tree_constant_rep::matrix_constant:
      if (user_pref.do_fortran_indexing)
	{
	  Matrix m = tmp.matrix_value ();
	  retval = m (0, 0);
	}
      break;
    case tree_constant_rep::complex_matrix_constant:
      if (user_pref.do_fortran_indexing)
	{
	  int flag = user_pref.ok_to_lose_imaginary_part;
	  if (flag == -1)
	    warning ("implicit conversion of complex value to real value");

	  if (flag != 0)
	    {
	      ComplexMatrix m = tmp.complex_matrix_value ();
	      return ::real (m (0, 0));
	    }
	  else
	    jump_to_top_level ();
	}
      else
	{
	  ::error ("complex matrix used in invalid context");
	  jump_to_top_level ();
	}
      break;
    default:
      break;
    }
  return retval;
}

ColumnVector
tree_constant_rep::to_vector (void) const
{
  tree_constant tmp = make_numeric ();

  ColumnVector retval;

  switch (tmp.const_type ())
    {
    case tree_constant_rep::scalar_constant:
    case tree_constant_rep::complex_scalar_constant:
      retval.resize (1);
      retval.elem (0) = tmp.double_value ();
      break;
    case tree_constant_rep::complex_matrix_constant:
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();
	int nr = m.rows ();
	int nc = m.columns ();
	if (nr == 1)
	  {
	    retval.resize (nc);
	    for (int i = 0; i < nc; i++)
	      retval.elem (i) = m (0, i);
	  }
	else if (nc == 1)
	  {
	    retval.resize (nr);
	    for (int i = 0; i < nr; i++)
	      retval.elem (i) = m.elem (i, 0);
	  }
      }
      break;
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

Matrix
tree_constant_rep::to_matrix (void) const
{
  tree_constant tmp = make_numeric ();

  Matrix retval;

  switch (tmp.const_type ())
    {
    case tree_constant_rep::scalar_constant:
      retval.resize (1, 1);
      retval.elem (0, 0) = tmp.double_value ();
      break;
    case tree_constant_rep::matrix_constant:
      retval = tmp.matrix_value ();
      break;
    default:
      break;
    }
  return retval;
}

tree_constant_rep::constant_type
tree_constant_rep::force_numeric (int force_str_conv = 0)
{
  switch (type_tag)
    {
    case scalar_constant:
    case matrix_constant:
    case complex_scalar_constant:
    case complex_matrix_constant:
      break;
    case string_constant:
      {
	if (! force_str_conv && ! user_pref.implicit_str_to_num_ok)
	  {
	    ::error ("failed to convert `%s' to a numeric type --", string);
	    ::error ("default conversion turned off");
// Abort!
	    jump_to_top_level ();
	  }

	int len = strlen (string);
	if (len > 1)
	  {
	    type_tag = matrix_constant;
	    Matrix *tm = new Matrix (1, len);
	    for (int i = 0; i < len; i++)
	      tm->elem (0, i) = toascii ((int) string[i]);
	    matrix = tm;
	  }
	else if (len == 1)
	  {
	    type_tag = scalar_constant;
	    scalar = toascii ((int) string[0]);
	  }
	else if (len == 0)
	  {
	    type_tag = matrix_constant;
	    matrix = new Matrix (0, 0);
	  }
	else
	  panic_impossible ();
      }
      break;
    case range_constant:
      {
	int len = range->nelem ();
	if (len > 1)
	  {
	    type_tag = matrix_constant;
	    Matrix *tm = new Matrix (1, len);
	    double b = range->base ();
	    double increment = range->inc ();
	    for (int i = 0; i < len; i++)
	      tm->elem (0, i) = b + i * increment;
	    matrix = tm;
	  }
	else if (len == 1)
	  {
	    type_tag = scalar_constant;
	    scalar = range->base ();
	  }
      }
      break;
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return type_tag;
}

tree_constant
tree_constant_rep::make_numeric (int force_str_conv = 0) const
{
  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar);
      break;
    case matrix_constant:
      retval = tree_constant (*matrix);
      break;
    case complex_scalar_constant:
      retval = tree_constant (*complex_scalar);
      break;
    case complex_matrix_constant:
      retval = tree_constant (*complex_matrix);
      break;
    case string_constant:
      retval = tree_constant (string);
      retval.force_numeric (force_str_conv);
      break;
    case range_constant:
      retval = tree_constant (*range);
      retval.force_numeric (force_str_conv);
      break;
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
do_binary_op (tree_constant& a, tree_constant& b, tree::expression_type t)
{
  tree_constant ans;

  int first_empty = (a.rows () == 0 || a.columns () == 0);
  int second_empty = (b.rows () == 0 || b.columns () == 0);

  if (first_empty || second_empty)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	warning ("binary operation on empty matrix");
      else if (flag == 0)
	{
	  ::error ("invalid binary operation on empty matrix");
	  return ans;
	}
    }

  tree_constant tmp_a = a.make_numeric ();
  tree_constant tmp_b = b.make_numeric ();

  tree_constant_rep::constant_type a_type = tmp_a.const_type ();
  tree_constant_rep::constant_type b_type = tmp_b.const_type ();

  double d1, d2;
  Matrix m1, m2;
  Complex c1, c2;
  ComplexMatrix cm1, cm2;

  switch (a_type)
    {
    case tree_constant_rep::scalar_constant:
      d1 = tmp_a.double_value ();
      switch (b_type)
	{
	case tree_constant_rep::scalar_constant:
	  d2 = tmp_b.double_value ();
	  ans = do_binary_op (d1, d2, t);
	  break;
	case tree_constant_rep::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  ans = do_binary_op (d1, m2, t);
	  break;
	case tree_constant_rep::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  ans = do_binary_op (d1, c2, t);
	  break;
	case tree_constant_rep::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  ans = do_binary_op (d1, cm2, t);
	  break;
	case tree_constant_rep::magic_colon:
	default:
	  panic_impossible ();
	  break;
	}
      break;
    case tree_constant_rep::matrix_constant:
      m1 = tmp_a.matrix_value ();
      switch (b_type)
	{
	case tree_constant_rep::scalar_constant:
	  d2 = tmp_b.double_value ();
	  ans = do_binary_op (m1, d2, t);
	  break;
	case tree_constant_rep::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  ans = do_binary_op (m1, m2, t);
	  break;
	case tree_constant_rep::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  ans = do_binary_op (m1, c2, t);
	  break;
	case tree_constant_rep::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  ans = do_binary_op (m1, cm2, t);
	  break;
	case tree_constant_rep::magic_colon:
	default:
	  panic_impossible ();
	  break;
	}
      break;
    case tree_constant_rep::complex_scalar_constant:
      c1 = tmp_a.complex_value ();
      switch (b_type)
	{
	case tree_constant_rep::scalar_constant:
	  d2 = tmp_b.double_value ();
	  ans = do_binary_op (c1, d2, t);
	  break;
	case tree_constant_rep::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  ans = do_binary_op (c1, m2, t);
	  break;
	case tree_constant_rep::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  ans = do_binary_op (c1, c2, t);
	  break;
	case tree_constant_rep::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  ans = do_binary_op (c1, cm2, t);
	  break;
	case tree_constant_rep::magic_colon:
	default:
	  panic_impossible ();
	  break;
	}
      break;
    case tree_constant_rep::complex_matrix_constant:
      cm1 = tmp_a.complex_matrix_value ();
      switch (b_type)
	{
	case tree_constant_rep::scalar_constant:
	  d2 = tmp_b.double_value ();
	  ans = do_binary_op (cm1, d2, t);
	  break;
	case tree_constant_rep::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  ans = do_binary_op (cm1, m2, t);
	  break;
	case tree_constant_rep::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  ans = do_binary_op (cm1, c2, t);
	  break;
	case tree_constant_rep::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  ans = do_binary_op (cm1, cm2, t);
	  break;
	case tree_constant_rep::magic_colon:
	default:
	  panic_impossible ();
	  break;
	}
      break;
    case tree_constant_rep::magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return ans;
}

tree_constant
do_unary_op (tree_constant& a, tree::expression_type t)
{
  tree_constant ans;

  if (a.rows () == 0 || a.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	warning ("unary operation on empty matrix");
      else if (flag == 0)
	{
	  ::error ("invalid unary operation on empty matrix");
	  return ans;
	}
    }

  tree_constant tmp_a = a.make_numeric ();

  switch (tmp_a.const_type ())
    {
    case tree_constant_rep::scalar_constant:
      ans = do_unary_op (tmp_a.double_value (), t);
      break;
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp_a.matrix_value ();
	ans = do_unary_op (m, t);
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      ans = do_unary_op (tmp_a.complex_value (), t);
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp_a.complex_matrix_value ();
	ans = do_unary_op (m, t);
      }
      break;
    case tree_constant_rep::magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return ans;
}

void
tree_constant_rep::bump_value (tree::expression_type etype)
{
  switch (etype)
    {
    case tree::increment:
      switch (type_tag)
	{
	case scalar_constant:
	  scalar++;
	  break;
	case matrix_constant:
	  *matrix = *matrix + 1.0;
	  break;
	case complex_scalar_constant:
	  *complex_scalar = *complex_scalar + 1.0;
	  break;
	case complex_matrix_constant:
	  *complex_matrix = *complex_matrix + 1.0;
	  break;
	case string_constant:
	  ::error ("string++ and ++string not implemented yet, ok?");
	  break;
	case range_constant:
	  range->set_base (range->base () + 1.0);
	  range->set_limit (range->limit () + 1.0);
	  break;
	case magic_colon:
	default:
	  panic_impossible ();
	  break;
	}
      break;
    case tree::decrement:
      switch (type_tag)
	{
	case scalar_constant:
	  scalar--;
	  break;
	case matrix_constant:
	  *matrix = *matrix - 1.0;
	  break;
	case string_constant:
	  ::error ("string-- and -- string not implemented yet, ok?");
	  break;
	case range_constant:
	  range->set_base (range->base () - 1.0);
	  range->set_limit (range->limit () - 1.0);
	  break;
	case magic_colon:
	default:
	  panic_impossible ();
	  break;
	}
      break;
    default:
      panic_impossible ();
      break;
    }
}

void
tree_constant_rep::maybe_mutate (void)
{
  if (error_state)
    return;

  switch (type_tag)
    {
    case complex_scalar_constant:
      if (::imag (*complex_scalar) == 0.0)
	{
	  double d = ::real (*complex_scalar);
	  delete complex_scalar;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;
    case complex_matrix_constant:
      if (! any_element_is_complex (*complex_matrix))
	{
	  Matrix *m = new Matrix (::real (*complex_matrix));
	  delete complex_matrix;
	  matrix = m;
	  type_tag = matrix_constant;
	}
      break;
    case scalar_constant:
    case matrix_constant:
    case string_constant:
    case range_constant:
    case magic_colon:
      break;
    default:
      panic_impossible ();
      break;
    }

// Avoid calling rows() and columns() for things like magic_colon.

  int nr = 1;
  int nc = 1;
  if (type_tag == matrix_constant
      || type_tag == complex_matrix_constant
      || type_tag == range_constant)
    {
      nr = rows ();
      nc = columns ();
    }

  switch (type_tag)
    {
    case matrix_constant:
      if (nr == 1 && nc == 1)
	{
	  double d = matrix->elem (0, 0);
	  delete matrix;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;
    case complex_matrix_constant:
      if (nr == 1 && nc == 1)
	{
	  Complex c = complex_matrix->elem (0, 0);
	  delete complex_matrix;
	  complex_scalar = new Complex (c);
	  type_tag = complex_scalar_constant;
	}
      break;
    case range_constant:
      if (nr == 1 && nc == 1)
	{
	  double d = range->base ();
	  delete range;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;
    default:
      break;
    }
}

void
tree_constant_rep::print (void)
{
  if (error_state)
    return;

  int nr = rows ();
  int nc = columns ();

  if (print)
    {
      ostrstream output_buf;
      switch (type_tag)
	{
	case scalar_constant:
	  octave_print_internal (output_buf, scalar);
	  break;
	case matrix_constant:
	  if (nr == 0 || nc == 0)
	    {
	      output_buf << "[]";
	      if (user_pref.print_empty_dimensions)
		output_buf << "(" << nr << "x" << nc << ")";
	      output_buf << "\n";
	    }
	  else
	    octave_print_internal (output_buf, *matrix);
	  break;
	case complex_scalar_constant:
	  octave_print_internal (output_buf, *complex_scalar);
	  break;
	case complex_matrix_constant:
	  if (nr == 0 || nc == 0)
	    {
	      output_buf << "[]";
	      if (user_pref.print_empty_dimensions)
		output_buf << "(" << nr << "x" << nc << ")";
	      output_buf << "\n";
	    }
	  else
	    octave_print_internal (output_buf, *complex_matrix);
	  break;
	case string_constant:
	  output_buf << string << "\n";
	  break;
	case range_constant:
	  octave_print_internal (output_buf, *range);
	  break;
	case magic_colon:
	default:
	  panic_impossible ();
	  break;
	}

      output_buf << ends;
      maybe_page_output (output_buf);
    }
}

tree_constant
tree_constant_rep::do_index (const tree_constant *args, int nargin)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (rows () == 0 || columns () == 0)
    {
      ::error ("attempt to index empty matrix");
      return retval;
    }

  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
      retval = do_scalar_index (args, nargin);
      break;
    case complex_matrix_constant:
    case matrix_constant:
      retval = do_matrix_index (args, nargin);
      break;
    case string_constant:
      gripe_string_invalid ();
//      retval = do_string_index (args, nargin);
      break;
    case magic_colon:
    case range_constant:
// This isn\'t great, but it\'s easier than implementing a lot of
// range indexing functions.
      force_numeric ();
      assert (type_tag != magic_colon && type_tag != range_constant);
      retval = do_index (args, nargin);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

int
tree_constant_rep::save (ostream& os, int mark_as_global, int precision)
{
  switch (type_tag)
    {
    case scalar_constant:
    case matrix_constant:
    case complex_scalar_constant:
    case complex_matrix_constant:
    case string_constant:
    case range_constant:
      if (mark_as_global)
	os << "# type: global ";
      else
	os << "# type: ";
      break;
    case magic_colon:
    default:
      break;
    }

  long old_precision = os.precision ();
  os.precision (precision);

  switch (type_tag)
    {
    case scalar_constant:
      os << "scalar\n"
	 << scalar << "\n";
      break;
    case matrix_constant:
      os << "matrix\n"
	 << "# rows: " << rows () << "\n"
	 << "# columns: " << columns () << "\n"
	 << *matrix ;
      break;
    case complex_scalar_constant:
      os << "complex scalar\n"
	 << *complex_scalar << "\n";
      break;
    case complex_matrix_constant:
      os << "complex matrix\n"
	 << "# rows: " << rows () << "\n"
	 << "# columns: " << columns () << "\n"
	 << *complex_matrix ;
      break;
    case string_constant:
      os << "string\n"
	 << "# length: " << strlen (string) << "\n"
	 << string << "\n";
      break;
    case range_constant:
      {
	os << "range\n"
	   << "# base, limit, increment\n"
	   << range->base () << " "
	   << range->limit () << " "
	   << range->inc () << "\n";
      }
      break;
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }

  os.precision (old_precision);

// Really want to return 1 only if write is successful.
  return 1;
}

int
tree_constant_rep::save_three_d (ostream& os, int parametric)
{
  int nr = rows ();
  int nc = columns ();

  switch (type_tag)
    {
    case matrix_constant:
      os << "# 3D data...\n"
	 << "# type: matrix\n"
	 << "# total rows: " << nr << "\n"
	 << "# total columns: " << nc << "\n";

      if (parametric)
	{
	  int extras = nc % 3;
	  if (extras)
	    warning ("ignoring last %d columns", extras);

	  for (int i = 0; i < nc-extras; i += 3)
	    {
	      os << matrix->extract (0, i, nr-1, i+2);
	      if (i+3 < nc-extras)
		os << "\n";
	    }
	}
      else
	{
	  for (int i = 0; i < nc; i++)
	    {
	      os << matrix->extract (0, i, nr-1, i);
	      if (i+1 < nc)
		os << "\n";
	    }
	}
      break;
    default:
      ::error ("for now, I can only save real matrices in 3D format");
      return 0;
      break;
    }
// Really want to return 1 only if write is successful.
  return 1;
}

int
tree_constant_rep::load (istream& is)
{
  int is_global = 0;

  type_tag = unknown_constant;

// Look for type keyword

  char *tag = extract_keyword (is, "type");

  if (tag != (char *) NULL && *tag != '\0')
    {
      char *ptr = strchr (tag, ' ');
      if (ptr != (char *) NULL)
	{
	  *ptr = '\0';
	  is_global = (strncmp (tag, "global", 6) == 0);
	  *ptr = ' ';
	  if (is_global)
	    ptr++;
	  else
	    ptr = tag;
	}
      else
	ptr = tag;

      if (strncmp (ptr, "scalar", 6) == 0)
	type_tag = load (is, scalar_constant);
      else if (strncmp (ptr, "matrix", 6) == 0)
	type_tag = load (is, matrix_constant);
      else if (strncmp (ptr, "complex scalar", 14) == 0)
	type_tag = load (is, complex_scalar_constant);
      else if (strncmp (ptr, "complex matrix", 14) == 0)
	type_tag = load (is, complex_matrix_constant);
      else if (strncmp (ptr, "string", 6) == 0)
	type_tag = load (is, string_constant);
      else if (strncmp (ptr, "range", 5) == 0)
	type_tag = load (is, range_constant);
      else
	::error ("unknown constant type `%s'", tag);
    }
  else
    ::error ("failed to extract keyword specifying value type");

  delete [] tag;

  return is_global;
}

tree_constant_rep::constant_type
tree_constant_rep::load (istream& is, tree_constant_rep::constant_type t)
{
  tree_constant_rep::constant_type status = unknown_constant;

  switch (t)
    {
    case scalar_constant:
      is >> scalar;
      if (is)
	status = scalar_constant;
      else
	::error ("failed to load scalar constant");
      break;
    case matrix_constant:
      {
	int nr = 0, nc = 0;

	if (extract_keyword (is, "rows", nr) && nr > 0
	    && extract_keyword (is, "columns", nc) && nc > 0)
	  {
	    matrix = new Matrix (nr, nc);
	    is >> *matrix;
	    if (is)
	      status = matrix_constant;
	    else
	      ::error ("failed to load matrix constant");
	  }
	else
	  ::error ("failed to extract number of rows and columns");
      }
      break;
    case complex_scalar_constant:
      complex_scalar = new Complex;
      is >> *complex_scalar;
      if (is)
	status = complex_scalar_constant;
      else
	::error ("failed to load complex scalar constant");
      break;
    case complex_matrix_constant:
      {
	int nr = 0, nc = 0;

	if (extract_keyword (is, "rows", nr) && nr > 0
	    && extract_keyword (is, "columns", nc) && nc > 0)
	  {
	    complex_matrix = new ComplexMatrix (nr, nc);
	    is >> *complex_matrix;
	    if (is)
	      status = complex_matrix_constant;
	    else
	      ::error ("failed to load complex matrix constant");
	  }
	else
	  ::error ("failed to extract number of rows and columns");
      }
      break;
    case string_constant:
      {
	int len;
	if (extract_keyword (is, "length", len) && len > 0)
	  {
	    string = new char [len+1];
	    is.get (string, len+1, EOF);
	    if (is)
	      status = string_constant;
	    else
	      ::error ("failed to load string constant");
	  }
	else
	  ::error ("failed to extract string length");
      }
      break;
    case range_constant:
      skip_comments (is);
      range = new Range ();
      is >> *range;
      if (is)
	status = range_constant;
      else
	::error ("failed to load range constant");
      break;
    default:
      panic_impossible ();
      break;
    }
  return status;
}

double
tree_constant_rep::double_value (void) const
{
  switch (type_tag)
    {
    case scalar_constant:
      return scalar;
    case complex_scalar_constant:
      {
	int flag = user_pref.ok_to_lose_imaginary_part;
	if (flag == -1)
	  warning ("implicit conversion of complex value to real value");

	if (flag != 0)
	  return ::real (*complex_scalar);

	::error ("implicit conversion of complex value to real value");
	::error ("not allowed");
	jump_to_top_level ();
      }
    default:
      panic_impossible ();
      break;
    }
}

Matrix
tree_constant_rep::matrix_value (void) const
{
  switch (type_tag)
    {
    case scalar_constant:
      return Matrix (1, 1, scalar);
    case matrix_constant:
      return *matrix;
    case complex_scalar_constant:
    case complex_matrix_constant:
      {
	int flag = user_pref.ok_to_lose_imaginary_part;
	if (flag == -1)
	  warning ("implicit conversion of complex matrix to real matrix");

	if (flag != 0)
	  {
	    if (type_tag == complex_scalar_constant)
	      return Matrix (1, 1, ::real (*complex_scalar));
	    else if (type_tag == complex_matrix_constant)
	      return ::real (*complex_matrix);
	    else
	      panic_impossible ();
	  }
	else
	  {
	    ::error ("implicit conversion of complex matrix to real matrix");
	    ::error ("not allowed");
	  }
	jump_to_top_level ();
      }
    default:
      panic_impossible ();
      break;
    }
}

Complex
tree_constant_rep::complex_value (void) const
{
  switch (type_tag)
    {
    case complex_scalar_constant:
      return *complex_scalar;
    case scalar_constant:
      return Complex (scalar);
    default:
      panic_impossible ();
      break;
    }
}

ComplexMatrix
tree_constant_rep::complex_matrix_value (void) const
{
  switch (type_tag)
    {
    case scalar_constant:
      {
	return ComplexMatrix (1, 1, Complex (scalar));
      }
    case complex_scalar_constant:
      {
	return ComplexMatrix (1, 1, *complex_scalar);
      }
    case matrix_constant:
      {
        return ComplexMatrix (*matrix);
      }
    case complex_matrix_constant:
      return *complex_matrix;
      break;
    default:
      panic_impossible ();
      break;
    }
}

char *
tree_constant_rep::string_value (void) const
{
  assert (type_tag == string_constant);
  return string;
}

Range
tree_constant_rep::range_value (void) const
{
  assert (type_tag == range_constant);
  return *range;
}

int
tree_constant_rep::rows (void) const
{
  int retval = -1;
  switch (type_tag)
    {
    case scalar_constant:
    case complex_scalar_constant:
      retval = 1;
      break;
    case string_constant:
    case range_constant:
      retval = (columns () > 0);
      break;
    case matrix_constant:
      retval = matrix->rows ();
      break;
    case complex_matrix_constant:
      retval = complex_matrix->rows ();
      break;
    case magic_colon:
      ::error ("invalid use of colon operator");
      break;
    case unknown_constant:
      retval = 0;
      break;
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

int
tree_constant_rep::columns (void) const
{
  int retval = -1;
  switch (type_tag)
    {
    case scalar_constant:
    case complex_scalar_constant:
      retval = 1;
      break;
    case matrix_constant:
      retval = matrix->columns ();
      break;
    case complex_matrix_constant:
      retval = complex_matrix->columns ();
      break;
    case string_constant:
      retval = strlen (string);
      break;
    case range_constant:
      retval = range->nelem ();
      break;
    case magic_colon:
      ::error ("invalid use of colon operator");
      break;
    case unknown_constant:
      retval = 0;
      break;
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::all (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.all ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      {
	double status = (scalar != 0.0);
	retval = tree_constant (status);
      }
      break;
    case matrix_constant:
      {
	Matrix m = matrix->all ();
	retval = tree_constant (m);
      }
      break;
    case complex_scalar_constant:
      {
	double status = (*complex_scalar != 0.0);
	retval = tree_constant (status);
      }
      break;
    case complex_matrix_constant:
      {
	Matrix m = complex_matrix->all ();
	retval = tree_constant (m);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::any (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.any ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      {
	double status = (scalar != 0.0);
	retval = tree_constant (status);
      }
      break;
    case matrix_constant:
      {
	Matrix m = matrix->any ();
	retval = tree_constant (m);
      }
      break;
    case complex_scalar_constant:
      {
	double status = (*complex_scalar != 0.0);
	retval = tree_constant (status);
      }
      break;
    case complex_matrix_constant:
      {
	Matrix m = complex_matrix->any ();
	retval = tree_constant (m);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::isstr (void) const
{
  double status = 0.0;
  if (type_tag == string_constant)
    status = 1.0;
  tree_constant retval (status);
  return retval;
}

tree_constant
tree_constant_rep::convert_to_str (void)
{
  tree_constant retval;

  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	double d = double_value ();
	int i = NINT (d);
// Warn about out of range conversions?
	char s[2];
	s[0] = (char) i;
	s[1] = '\0';
	retval = tree_constant (s);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	ColumnVector v = to_vector ();
	int len = v.length ();
	if (len == 0)
	  ::error ("can only convert vectors and scalars to strings");
	else
	  {
	    char *s = new char [len+1];
	    s[len] = '\0';
	    for (int i = 0; i < len; i++)
	      {
		double d = v.elem (i);
		int ival = NINT (d);
// Warn about out of range conversions?
		s[i] = (char) ival;
	      }
	    retval = tree_constant (s);
	    delete [] s;
	  }
      }
      break;
    case range_constant:
      {
	Range r = range_value ();
	double b = r.base ();
	double incr = r.inc ();
	int nel = r.nelem ();
	char *s = new char [nel+1];
	s[nel] = '\0';
	for (int i = 0; i < nel; i++)
	  {
	    double d = b + i * incr;
	    int ival = NINT (d);
// Warn about out of range conversions?
	    s[i] = (char) ival;
	  }
	retval = tree_constant (s);
	delete [] s;
      }
      break;
    case string_constant:
      retval = tree_constant (*this);
      break;
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

void
tree_constant_rep::convert_to_row_or_column_vector (void)
{
  assert (type_tag == matrix_constant || type_tag == complex_matrix_constant);

  int nr = rows ();
  int nc = columns ();

  int len = nr * nc;

  assert (len > 0);

  int new_nr = 1;
  int new_nc = 1;

  if (user_pref.prefer_column_vectors)
    new_nr = len;
  else
    new_nc = len;

  if (type_tag == matrix_constant)
    {
      Matrix *m = new Matrix (new_nr, new_nc);

      double *cop_out = matrix->fortran_vec ();

      for (int i = 0; i < len; i++)
	{
	  if (new_nr == 1)
	    m->elem (0, i) = *cop_out++;
	  else
	    m->elem (i, 0) = *cop_out++;
	}

      delete matrix;
      matrix = m;
    }
  else
    {
      ComplexMatrix *cm = new ComplexMatrix (new_nr, new_nc);

      Complex *cop_out = complex_matrix->fortran_vec ();

      for (int i = 0; i < len; i++)
	{
	  if (new_nr == 1)
	    cm->elem (0, i) = *cop_out++;
	  else
	    cm->elem (i, 0) = *cop_out++;
	}

      delete complex_matrix;
      complex_matrix = cm;
    }
}

int
tree_constant_rep::is_true (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.is_true ();
    }

  int retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = (scalar != 0.0);
      break;
    case matrix_constant:
      {
	Matrix m = (matrix->all ()) . all ();
	retval = (m.rows () == 1
		  && m.columns () == 1
		  && m.elem (0, 0) != 0.0);
      }
      break;
    case complex_scalar_constant:
      retval = (*complex_scalar != 0.0);
      break;
    case complex_matrix_constant:
      {
	Matrix m = (complex_matrix->all ()) . all ();
	retval = (m.rows () == 1
		  && m.columns () == 1
		  && m.elem (0, 0) != 0.0);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::cumprod (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.cumprod ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar);
      break;
    case matrix_constant:
      {
	Matrix m = matrix->cumprod ();
	retval = tree_constant (m);
      }
      break;
    case complex_scalar_constant:
      retval = tree_constant (*complex_scalar);
      break;
    case complex_matrix_constant:
      {
	ComplexMatrix m = complex_matrix->cumprod ();
	retval = tree_constant (m);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::cumsum (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.cumsum ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar);
      break;
    case matrix_constant:
      {
	Matrix m = matrix->cumsum ();
	retval = tree_constant (m);
      }
      break;
    case complex_scalar_constant:
      retval = tree_constant (*complex_scalar);
      break;
    case complex_matrix_constant:
      {
	ComplexMatrix m = complex_matrix->cumsum ();
	retval = tree_constant (m);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::prod (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.prod ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar);
      break;
    case matrix_constant:
      {
	Matrix m = matrix->prod ();
	retval = tree_constant (m);
      }
      break;
    case complex_scalar_constant:
      retval = tree_constant (*complex_scalar);
      break;
    case complex_matrix_constant:
      {
	ComplexMatrix m = complex_matrix->prod ();
	retval = tree_constant (m);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::sum (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.sum ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar);
      break;
    case matrix_constant:
      {
	Matrix m = matrix->sum ();
	retval = tree_constant (m);
      }
      break;
    case complex_scalar_constant:
      retval = tree_constant (*complex_scalar);
      break;
    case complex_matrix_constant:
      {
	ComplexMatrix m = complex_matrix->sum ();
	retval = tree_constant (m);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::sumsq (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.sumsq ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar * scalar);
      break;
    case matrix_constant:
      {
	Matrix m = matrix->sumsq ();
	retval = tree_constant (m);
      }
      break;
    case complex_scalar_constant:
      {
	Complex c (*complex_scalar);
	retval = tree_constant (c * c);
      }
      break;
    case complex_matrix_constant:
      {
	ComplexMatrix m = complex_matrix->sumsq ();
	retval = tree_constant (m);
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

static tree_constant
make_diag (const Matrix& v, int k)
{
  int nr = v.rows ();
  int nc = v.columns ();
  assert (nc == 1 || nr == 1);

  tree_constant retval;

  int roff = 0;
  int coff = 0;
  if (k > 0)
    {
      roff = 0;
      coff = k;
    }
  else if (k < 0)
    {
      roff = -k;
      coff = 0;
    }

  if (nr == 1)
    {
      int n = nc + ABS (k);
      Matrix m (n, n, 0.0);
      for (int i = 0; i < nc; i++)
	m.elem (i+roff, i+coff) = v.elem (0, i);
      retval = tree_constant (m);
    }
  else
    {
      int n = nr + ABS (k);
      Matrix m (n, n, 0.0);
      for (int i = 0; i < nr; i++)
	m.elem (i+roff, i+coff) = v.elem (i, 0);
      retval = tree_constant (m);
    }

  return retval;
}

static tree_constant
make_diag (const ComplexMatrix& v, int k)
{
  int nr = v.rows ();
  int nc = v.columns ();
  assert (nc == 1 || nr == 1);

  tree_constant retval;

  int roff = 0;
  int coff = 0;
  if (k > 0)
    {
      roff = 0;
      coff = k;
    }
  else if (k < 0)
    {
      roff = -k;
      coff = 0;
    }

  if (nr == 1)
    {
      int n = nc + ABS (k);
      ComplexMatrix m (n, n, 0.0);
      for (int i = 0; i < nc; i++)
	m.elem (i+roff, i+coff) = v.elem (0, i);
      retval = tree_constant (m);
    }
  else
    {
      int n = nr + ABS (k);
      ComplexMatrix m (n, n, 0.0);
      for (int i = 0; i < nr; i++)
	m.elem (i+roff, i+coff) = v.elem (i, 0);
      retval = tree_constant (m);
    }

  return retval;
}

tree_constant
tree_constant_rep::diag (void) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.diag ();
    }

  tree_constant retval;
  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar);
      break;
    case matrix_constant:
      {
	int nr = rows ();
	int nc = columns ();
	if (nr == 0 || nc == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else if (nr == 1 || nc == 1)
	  retval = make_diag (matrix_value (), 0);
	else
	  {
	    ColumnVector v = matrix->diag ();
	    if (v.capacity () > 0)
	      retval = tree_constant (v);
	  }
      }
      break;
    case complex_scalar_constant:
      retval = tree_constant (*complex_scalar);
      break;
    case complex_matrix_constant:
      {
	int nr = rows ();
	int nc = columns ();
	if (nr == 0 || nc == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else if (nr == 1 || nc == 1)
	  retval = make_diag (complex_matrix_value (), 0);
	else
	  {
	    ComplexColumnVector v = complex_matrix->diag ();
	    if (v.capacity () > 0)
	      retval = tree_constant (v);
	  }
      }
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::diag (const tree_constant& a) const
{
  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.diag (a);
    }

  tree_constant tmp_a = a.make_numeric ();

  tree_constant_rep::constant_type a_type = tmp_a.const_type ();

  tree_constant retval;

  switch (type_tag)
    {
    case scalar_constant:
      if (a_type == scalar_constant)
	{
	  int k = NINT (tmp_a.double_value ());
	  int n = ABS (k) + 1;
	  if (k == 0)
	    retval = tree_constant (scalar);
	  else if (k > 0)
	    {
	      Matrix m (n, n, 0.0);
	      m.elem (0, k) = scalar;
	      retval = tree_constant (m);
	    }
	  else if (k < 0)
	    {
	      Matrix m (n, n, 0.0);
	      m.elem (-k, 0) = scalar;
	      retval = tree_constant (m);
	    }
	}
      break;
    case matrix_constant:
      if (a_type == scalar_constant)
	{
	  int k = NINT (tmp_a.double_value ());
	  int nr = rows ();
	  int nc = columns ();
	  if (nr == 0 || nc == 0)
	    {
	      Matrix mtmp;
	      retval = tree_constant (mtmp);
	    }
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (matrix_value (), k);
	  else
	    {
	      ColumnVector d = matrix->diag (k);
	      retval = tree_constant (d);
	    }
	}
      else
	::error ("diag: invalid second argument");

      break;
    case complex_scalar_constant:
      if (a_type == scalar_constant)
	{
	  int k = NINT (tmp_a.double_value ());
	  int n = ABS (k) + 1;
	  if (k == 0)
	    retval = tree_constant (*complex_scalar);
	  else if (k > 0)
	    {
	      ComplexMatrix m (n, n, 0.0);
	      m.elem (0, k) = *complex_scalar;
	      retval = tree_constant (m);
	    }
	  else if (k < 0)
	    {
	      ComplexMatrix m (n, n, 0.0);
	      m.elem (-k, 0) = *complex_scalar;
	      retval = tree_constant (m);
	    }
	}
      break;
    case complex_matrix_constant:
      if (a_type == scalar_constant)
	{
	  int k = NINT (tmp_a.double_value ());
	  int nr = rows ();
	  int nc = columns ();
	  if (nr == 0 || nc == 0)
	    {
	      Matrix mtmp;
	      retval = tree_constant (mtmp);
	    }
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (complex_matrix_value (), k);
	  else
	    {
	      ComplexColumnVector d = complex_matrix->diag (k);
	      retval = tree_constant (d);
	    }
	}
      else
	::error ("diag: invalid second argument");

      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

tree_constant
tree_constant_rep::mapper (Mapper_fcn& m_fcn, int print) const
{
  tree_constant retval;

  if (type_tag == string_constant || type_tag == range_constant)
    {
      tree_constant tmp = make_numeric ();
      return tmp.mapper (m_fcn, print);
    }

  switch (type_tag)
    {
    case scalar_constant:
      if (m_fcn.can_return_complex_for_real_arg
	  && (scalar < m_fcn.lower_limit
	      || scalar > m_fcn.upper_limit))
	{
	  if (m_fcn.c_c_mapper != NULL)
	    {
	      Complex c = m_fcn.c_c_mapper (Complex (scalar));
	      retval = tree_constant (c);
	    }
	  else
	    panic_impossible ();
	}
      else
	{
	  if (m_fcn.d_d_mapper != NULL)
	    {
	      double d = m_fcn.d_d_mapper (scalar);
	      retval = tree_constant (d);
	    }
	  else
	    panic_impossible ();
	}
      break;
    case matrix_constant:
      if (m_fcn.can_return_complex_for_real_arg
	  && (any_element_less_than (*matrix, m_fcn.lower_limit)
	      || any_element_greater_than (*matrix, m_fcn.upper_limit)))
	{
	  if (m_fcn.c_c_mapper != NULL)
	    {
	      ComplexMatrix cm = map (m_fcn.c_c_mapper,
				      ComplexMatrix (*matrix));
	      retval = tree_constant (cm);
	    }
	  else
	    panic_impossible ();
	}
      else
	{
	  if (m_fcn.d_d_mapper != NULL)
	    {
	      Matrix m = map (m_fcn.d_d_mapper, *matrix);
	      retval = tree_constant (m);
	    }
	  else
	    panic_impossible ();
	}
      break;
    case complex_scalar_constant:
      if (m_fcn.d_c_mapper != NULL)
	{
	  double d;
	  d = m_fcn.d_c_mapper (*complex_scalar);
	  retval = tree_constant (d);
	}
      else if (m_fcn.c_c_mapper != NULL)
	{
	  Complex c;
	  c = m_fcn.c_c_mapper (*complex_scalar);
	  retval = tree_constant (c);
	}
      else
	panic_impossible ();
      break;
    case complex_matrix_constant:
      if (m_fcn.d_c_mapper != NULL)
	{
	  Matrix m;
	  m = map (m_fcn.d_c_mapper, *complex_matrix);
	  retval = tree_constant (m);
	}
      else if (m_fcn.c_c_mapper != NULL)
	{
	  ComplexMatrix cm;
	  cm = map (m_fcn.c_c_mapper, *complex_matrix);
	  retval = tree_constant (cm);
	}
      else
	panic_impossible ();
      break;
    case string_constant:
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

/*
 * Top-level tree-constant function that handles assignments.  Only
 * decide if the left-hand side is currently a scalar or a matrix and
 * hand off to other functions to do the real work.
 */
void
tree_constant_rep::assign (tree_constant& rhs, tree_constant *args, int nargs)
{
  tree_constant rhs_tmp = rhs.make_numeric ();

// This is easier than actually handling assignments to strings.
// An assignment to a range will normally require a conversion to a
// vector since it will normally destroy the equally-spaced property
// of the range elements.

  if (type_tag == string_constant || type_tag == range_constant)
    force_numeric ();

  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
    case unknown_constant:
      do_scalar_assignment (rhs_tmp, args, nargs);
      break;
    case complex_matrix_constant:
    case matrix_constant:
      do_matrix_assignment (rhs_tmp, args, nargs);
      break;
    case string_constant:
      ::error ("invalid assignment to string type");
      break;
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
}

/*
 * Assignments to scalars.  If resize_on_range_error is true,
 * this can convert the left-hand side to a matrix.
 */
void
tree_constant_rep::do_scalar_assignment (tree_constant& rhs,
					 tree_constant *args, int nargs)
{
  assert (type_tag == unknown_constant
	  || type_tag == scalar_constant
	  || type_tag == complex_scalar_constant);

  if ((rhs.is_scalar_type () || rhs.is_zero_by_zero ())
      && valid_scalar_indices (args, nargs))
    {
      if (rhs.is_zero_by_zero ())
	{
	  if (type_tag == complex_scalar_constant)
	    delete complex_scalar;

	  matrix = new Matrix (0, 0);
	  type_tag = matrix_constant;
	}
      else if (type_tag == unknown_constant || type_tag == scalar_constant)
	{
	  if (rhs.const_type () == scalar_constant)
	    {
	      scalar = rhs.double_value ();
	      type_tag = scalar_constant;
	    }
	  else if (rhs.const_type () == complex_scalar_constant)
	    {
	      complex_scalar = new Complex (rhs.complex_value ());
	      type_tag = complex_scalar_constant;
	    }
	  else
	    {
	      ::error ("invalid assignment to scalar");
	      return;
	    }
	}
      else
	{
	  if (rhs.const_type () == scalar_constant)
	    {
	      delete complex_scalar;
	      scalar = rhs.double_value ();
	      type_tag = scalar_constant;
	    }
	  else if (rhs.const_type () == complex_scalar_constant)
	    {
	      *complex_scalar = rhs.complex_value ();
	      type_tag = complex_scalar_constant;
	    }
	  else
	    {
	      ::error ("invalid assignment to scalar");
	      return;
	    }
	}
    }
  else if (user_pref.resize_on_range_error)
    {
      tree_constant_rep::constant_type old_type_tag = type_tag;

      if (type_tag == complex_scalar_constant)
	{
	  Complex *old_complex = complex_scalar;
	  complex_matrix = new ComplexMatrix (1, 1, *complex_scalar);
	  type_tag = complex_matrix_constant;
	  delete old_complex;
	}
      else if (type_tag == scalar_constant)
	{
	  matrix = new Matrix (1, 1, scalar);
	  type_tag = matrix_constant;
	}

// If there is an error, the call to do_matrix_assignment should not
// destroy the current value.  tree_constant_rep::eval(int) will take
// care of converting single element matrices back to scalars.

      do_matrix_assignment (rhs, args, nargs);

// I don't think there's any other way to revert back to unknown
// constant types, so here it is.

      if (old_type_tag == unknown_constant && error_state)
	{
	  if (type_tag == matrix_constant)
	    delete matrix;
	  else if (type_tag == complex_matrix_constant)
	    delete complex_matrix;

	  type_tag = unknown_constant;
	}
    }
  else if (nargs > 3 || nargs < 2)
    ::error ("invalid index expression for scalar type");
  else
    ::error ("index invalid or out of range for scalar type");
}

/*
 * Assignments to matrices (and vectors).
 *
 * For compatibility with Matlab, we allow assignment of an empty
 * matrix to an expression with empty indices to do nothing.
 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant *args, int nargs)
{
  assert (type_tag == unknown_constant
	  || type_tag == matrix_constant
	  || type_tag == complex_matrix_constant);

  if (type_tag == matrix_constant && rhs.is_complex_type ())
    {
      Matrix *old_matrix = matrix;
      complex_matrix = new ComplexMatrix (*matrix);
      type_tag = complex_matrix_constant;
      delete old_matrix;
    }
  else if (type_tag == unknown_constant)
    {
      if (rhs.is_complex_type ())
	{
	  complex_matrix = new ComplexMatrix ();
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  matrix = new Matrix ();
	  type_tag = matrix_constant;
	}
    }

// The do_matrix_assignment functions can't handle empty matrices, so
// don't let any pass through here.
  switch (nargs)
    {
    case 2:
      if (! args)
	::error ("matrix index is null");
      else if (args[1].is_undefined ())
	::error ("matrix index is undefined");
      else
	do_matrix_assignment (rhs, args[1]);
      break;
    case 3:
      if (! args)
	::error ("matrix indices are null");
      else if (args[1].is_undefined ())
	::error ("first matrix index is undefined");
      else if (args[2].is_undefined ())
	::error ("second matrix index is undefined");
      else if (args[1].is_empty () || args[2].is_empty ())
	{
	  if (! rhs.is_empty ())
	    {
	      ::error ("in assignment expression, a matrix index is empty");
	      ::error ("but hte right hand side is not an empty matrix");
	    }
// XXX FIXME XXX -- to really be correct here, we should probably
// check to see if the assignment conforms, but that seems like more
// work than it's worth right now...
	}
      else
	do_matrix_assignment (rhs, args[1], args[2]);
      break;
    default:
      ::error ("too many indices for matrix expression");
      break;
    }
}

/*
 * Matrix assignments indexed by a single value.
 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant& i_arg)
{
  int nr = rows ();
  int nc = columns ();

  if (user_pref.do_fortran_indexing || nr <= 1 || nc <= 1)
    {
      if (i_arg.is_empty ())
	{
	  if (! rhs.is_empty ())
	    {
	      ::error ("in assignment expression, matrix index is empty but");
	      ::error ("right hand side is not an empty matrix");
	    }
// XXX FIXME XXX -- to really be correct here, we should probably
// check to see if the assignment conforms, but that seems like more
// work than it's worth right now...

// The assignment functions can't handle empty matrices, so don't let
// any pass through here.
	  return;
	}

// We can't handle the case of assigning to a vector first, since even
// then, the two operations are not equivalent.  For example, the
// expression V(:) = M is handled differently depending on whether the
// user specified do_fortran_indexing = "true".

      if (user_pref.do_fortran_indexing)
	fortran_style_matrix_assignment (rhs, i_arg);
      else if (nr <= 1 || nc <= 1)
	vector_assignment (rhs, i_arg);
      else
	panic_impossible ();
    }
  else
    ::error ("single index only valid for row or column vector");
}

/*
 * Fortran-style assignments.  Matrices are assumed to be stored in
 * column-major order and it is ok to use a single index for
 * multi-dimensional matrices.
 */
void
tree_constant_rep::fortran_style_matrix_assignment (tree_constant& rhs,
						    tree_constant& i_arg)
{
  tree_constant tmp_i = i_arg.make_numeric_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = NINT (tmp_i.double_value ());
	int idx = i - 1;

	if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (idx < nr * nc)
	      {
		convert_to_row_or_column_vector ();

		nr = rows ();
		nc = columns ();

		if (nr == 1)
		  delete_column (idx);
		else if (nc == 1)
		  delete_row (idx);
		else
		  panic_impossible ();
	      }
	    return;
	  }

	if (index_check (idx, "") < 0)
	  return;

	if (nr <= 1 || nc <= 1)
	  {
	    maybe_resize (idx);
	    if (error_state)
	      return;
	  }
	else if (range_max_check (idx, nr * nc) < 0)
	  return;

	nr = rows ();
	nc = columns ();

	if (! indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
	  {
	    ::error ("for A(int) = X: X must be a scalar");
	    return;
	  }
	int ii = fortran_row (i, nr) - 1;
	int jj = fortran_column (i, nr) - 1;
	do_matrix_assignment (rhs, ii, jj);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	int len = nr * nc;
	idx_vector ii (mi, 1, "", len);  // Always do fortran indexing here...
	if (! ii)
	  return;

	if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    ii.sort_uniq ();
	    int num_to_delete = 0;
	    for (int i = 0; i < ii.length (); i++)
	      {
		if (ii.elem (i) < len)
		  num_to_delete++;
		else
		  break;
	      }

	    if (num_to_delete > 0)
	      {
		if (num_to_delete != ii.length ())
		  ii.shorten (num_to_delete);

		convert_to_row_or_column_vector ();

		nr = rows ();
		nc = columns ();

		if (nr == 1)
		  delete_columns (ii);
		else if (nc == 1)
		  delete_rows (ii);
		else
		  panic_impossible ();
	      }
	    return;
	  }

	if (nr <= 1 || nc <= 1)
	  {
	    maybe_resize (ii.max ());
	    if (error_state)
	      return;
	  }
	else if (range_max_check (ii.max (), len) < 0)
	  return;

	int ilen = ii.capacity ();

	if (ilen != rhs_nr * rhs_nc)
	  {
	    ::error ("A(matrix) = X: X and matrix must have the same number");
	    ::error ("of elements");
	  }
	else if (ilen == 1 && rhs.is_scalar_type ())
	  {
	    int nr = rows ();
	    int idx = ii.elem (0);
	    int ii = fortran_row (idx + 1, nr) - 1;
	    int jj = fortran_column (idx + 1, nr) - 1;

	    if (rhs.const_type () == scalar_constant)
	      matrix->elem (ii, jj) = rhs.double_value ();
	    else if (rhs.const_type () == complex_scalar_constant)
	      complex_matrix->elem (ii, jj) = rhs.complex_value ();
	    else
	      panic_impossible ();
	  }
	else
	  fortran_style_matrix_assignment (rhs, ii);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      gripe_range_invalid ();
      break;
    case magic_colon:
// a(:) = [] is equivalent to a(:,:) = [].
      if (rhs_nr == 0 && rhs_nc == 0)
	do_matrix_assignment (rhs, magic_colon, magic_colon);
      else
	fortran_style_matrix_assignment (rhs, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }
}

/*
 * Fortran-style assignment for vector index.
 */
void
tree_constant_rep::fortran_style_matrix_assignment (tree_constant& rhs,
						    idx_vector& i)
{
  assert (rhs.is_matrix_type ());

  int ilen = i.capacity ();

  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  int len = rhs_nr * rhs_nc;

  if (len == ilen)
    {
      int nr = rows ();
      if (rhs.const_type () == matrix_constant)
	{
	  double *cop_out = rhs_m.fortran_vec ();
	  for (int k = 0; k < len; k++)
	    {
	      int ii = fortran_row (i.elem (k) + 1, nr) - 1;
	      int jj = fortran_column (i.elem (k) + 1, nr) - 1;

	      matrix->elem (ii, jj) = *cop_out++;
	    }
	}
      else
	{
	  Complex *cop_out = rhs_cm.fortran_vec ();
	  for (int k = 0; k < len; k++)
	    {
	      int ii = fortran_row (i.elem (k) + 1, nr) - 1;
	      int jj = fortran_column (i.elem (k) + 1, nr) - 1;

	      complex_matrix->elem (ii, jj) = *cop_out++;
	    }
	}
    }
  else
    ::error ("number of rows and columns must match for indexed assignment");
}

/*
 * Fortran-style assignment for colon index.
 */
void
tree_constant_rep::fortran_style_matrix_assignment
  (tree_constant& rhs, tree_constant_rep::constant_type mci)
{
  assert (rhs.is_matrix_type () && mci == tree_constant_rep::magic_colon);

  int nr = rows ();
  int nc = columns ();

  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  int rhs_size = rhs_nr * rhs_nc;
  if (rhs_size == 0)
    {
      if (rhs.const_type () == matrix_constant)
	{
	  delete matrix;
	  matrix = new Matrix (0, 0);
	  return;
	}
      else
	panic_impossible ();
    }
  else if (nr*nc != rhs_size)
    {
      ::error ("A(:) = X: X and A must have the same number of elements");
      return;
    }

  if (rhs.const_type () == matrix_constant)
    {
      double *cop_out = rhs_m.fortran_vec ();
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  matrix->elem (i, j) = *cop_out++;
    }
  else
    {
      Complex *cop_out = rhs_cm.fortran_vec ();
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  complex_matrix->elem (i, j) = *cop_out++;
    }
}

/*
 * Assignments to vectors.  Hand off to other functions once we know
 * what kind of index we have.  For a colon, it is the same as
 * assignment to a matrix indexed by two colons.
 */
void
tree_constant_rep::vector_assignment (tree_constant& rhs, tree_constant& i_arg)
{
  int nr = rows ();
  int nc = columns ();

  assert ((nr == 1 || nc == 1 || (nr == 0 && nc == 0))
	  && ! user_pref.do_fortran_indexing);

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = tree_to_mat_idx (tmp_i.double_value ());
	if (index_check (i, "") < 0)
	  return;
	do_vector_assign (rhs, i);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	int len = nr * nc;
	idx_vector iv (mi, user_pref.do_fortran_indexing, "", len);
	if (! iv)
	  return;

	do_vector_assign (rhs, iv);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range ri = tmp_i.range_value ();
	int len = nr * nc;
	if (len == 2 && is_zero_one (ri))
	  {
	    do_vector_assign (rhs, 1);
	  }
	else if (len == 2 && is_one_zero (ri))
	  {
	    do_vector_assign (rhs, 0);
	  }
	else
	  {
	    if (index_check (ri, "") < 0)
	      return;
	    do_vector_assign (rhs, ri);
	  }
      }
      break;
    case magic_colon:
      {
	int rhs_nr = rhs.rows ();
	int rhs_nc = rhs.columns ();

	if (! indexed_assign_conforms (nr, nc, rhs_nr, rhs_nc))
	  {
	    ::error ("A(:) = X: X and A must have the same dimensions");
	    return;
	  }
	do_matrix_assignment (rhs, magic_colon, magic_colon);
      }
      break;
    default:
      panic_impossible ();
      break;
    }
}

/*
 * Check whether an indexed assignment to a vector is valid.
 */
void
tree_constant_rep::check_vector_assign (int rhs_nr, int rhs_nc,
					int ilen, const char *rm)
{
  int nr = rows ();
  int nc = columns ();

  if ((nr == 1 && nc == 1) || nr == 0 || nc == 0)  // No orientation.
    {
      if (! (ilen == rhs_nr || ilen == rhs_nc))
	{
	  ::error ("A(%s) = X: X and %s must have the same number of elements",
		 rm, rm);
	}
    }
  else if (nr == 1)  // Preserve current row orientation.
    {
      if (! (rhs_nr == 1 && rhs_nc == ilen))
	{
	  ::error ("A(%s) = X: where A is a row vector, X must also be a", rm);
	  ::error ("row vector with the same number of elements as %s", rm);
	}
    }
  else if (nc == 1)  // Preserve current column orientation.
    {
      if (! (rhs_nc == 1 && rhs_nr == ilen))
	{
	  ::error ("A(%s) = X: where A is a column vector, X must also be", rm);
	  ::error ("a column vector with the same number of elements as %s", rm);
	}
    }
  else
    panic_impossible ();
}

/*
 * Assignment to a vector with an integer index.
 */
void
tree_constant_rep::do_vector_assign (tree_constant& rhs, int i)
{
  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  if (indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
    {
      maybe_resize (i);
      if (error_state)
	return;

      int nr = rows ();
      int nc = columns ();

      if (nr == 1)
	{
	  REP_ELEM_ASSIGN (0, i, rhs.double_value (), rhs.complex_value (),
			   rhs.is_real_type ());
	}
      else if (nc == 1)
	{
	  REP_ELEM_ASSIGN (i, 0, rhs.double_value (), rhs.complex_value (),
			   rhs.is_real_type ());
	}
      else
	panic_impossible ();
    }
  else if (rhs_nr == 0 && rhs_nc == 0)
    {
      int nr = rows ();
      int nc = columns ();

      int len = MAX (nr, nc);

      if (i < 0 || i >= len)
	{
	  ::error ("A(int) = []: index out of range");
	  return;
	}

      if (nr == 1)
	delete_column (i);
      else if (nc == 1)
	delete_row (i);
      else
	panic_impossible ();
    }
  else
    {
      ::error ("for A(int) = X: X must be a scalar");
      return;
    }
}

/*
 * Assignment to a vector with a vector index.
 */
void
tree_constant_rep::do_vector_assign (tree_constant& rhs, idx_vector& iv)
{
  if (rhs.is_zero_by_zero ())
    {
      int nr = rows ();
      int nc = columns ();

      int len = MAX (nr, nc);

      if (iv.max () >= len)
	{
	  ::error ("A(matrix) = []: index out of range");
	  return;
	}

      if (nr == 1)
	delete_columns (iv);
      else if (nc == 1)
	delete_rows (iv);
      else
	panic_impossible ();
    }
  else if (rhs.is_scalar_type ())
    {
      int nr = rows ();
      int nc = columns ();

      if (iv.capacity () == 1)
	{
	  int idx = iv.elem (0);

	  if (nr == 1)
	    {
	      REP_ELEM_ASSIGN (0, idx, rhs.double_value (),
			       rhs.complex_value (), rhs.is_real_type ());
	    }
	  else if (nc == 1)
	    {
	      REP_ELEM_ASSIGN (idx, 0, rhs.double_value (),
			       rhs.complex_value (), rhs.is_real_type ());
	    }
	  else
	    panic_impossible ();
	}
      else
	{
	  if (nr == 1)
	    {
	      ::error ("A(matrix) = X: where A is a row vector, X must also be a");
	      ::error ("row vector with the same number of elements as matrix");
	    }
	  else if (nc == 1)
	    {
	      ::error ("A(matrix) = X: where A is a column vector, X must also be a");
	      ::error ("column vector with the same number of elements as matrix");
	    }
	  else
	    panic_impossible ();
	}
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int ilen = iv.capacity ();
      check_vector_assign (rhs_nr, rhs_nc, ilen, "matrix");
      if (error_state)
	return;

      force_orient f_orient = no_orient;
      if (rhs_nr == 1 && rhs_nc != 1)
	f_orient = row_orient;
      else if (rhs_nc == 1 && rhs_nr != 1)
	f_orient = column_orient;

      maybe_resize (iv.max (), f_orient);
      if (error_state)
	return;

      int nr = rows ();
      int nc = columns ();

      if (nr == 1)
	{
	  for (int i = 0; i < iv.capacity (); i++)
	    REP_ELEM_ASSIGN (0, iv.elem (i), rhs_m.elem (0, i),
			     rhs_cm.elem (0, i), rhs.is_real_type ());
	}
      else if (nc == 1)
	{
	  for (int i = 0; i < iv.capacity (); i++)
	    REP_ELEM_ASSIGN (iv.elem (i), 0, rhs_m.elem (i, 0),
			     rhs_cm.elem (i, 0), rhs.is_real_type ());
	}
      else
	panic_impossible ();
    }
  else
    panic_impossible ();
}

/*
 * Assignment to a vector with a range index.
 */
void
tree_constant_rep::do_vector_assign (tree_constant& rhs, Range& ri)
{
  if (rhs.is_zero_by_zero ())
    {
      int nr = rows ();
      int nc = columns ();

      int len = MAX (nr, nc);

      int b = tree_to_mat_idx (ri.min ());
      int l = tree_to_mat_idx (ri.max ());
      if (b < 0 || l >= len)
	{
	  ::error ("A(range) = []: index out of range");
	  return;
	}

      if (nr == 1)
	delete_columns (ri);
      else if (nc == 1)
	delete_rows (ri);
      else
	panic_impossible ();
    }
  else if (rhs.is_scalar_type ())
    {
      int nr = rows ();
      int nc = columns ();

      if (nr == 1)
	{
	  ::error ("A(range) = X: where A is a row vector, X must also be a");
	  ::error ("row vector with the same number of elements as range");
	}
      else if (nc == 1)
	{
	  ::error ("A(range) = X: where A is a column vector, X must also be a");
	  ::error ("column vector with the same number of elements as range");
	}
      else
	panic_impossible ();
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int ilen = ri.nelem ();
      check_vector_assign (rhs_nr, rhs_nc, ilen, "range");
      if (error_state)
	return;

      force_orient f_orient = no_orient;
      if (rhs_nr == 1 && rhs_nc != 1)
	f_orient = row_orient;
      else if (rhs_nc == 1 && rhs_nr != 1)
	f_orient = column_orient;

      maybe_resize (tree_to_mat_idx (ri.max ()), f_orient);
      if (error_state)
	return;

      int nr = rows ();
      int nc = columns ();

      double b = ri.base ();
      double increment = ri.inc ();

      if (nr == 1)
	{
	  for (int i = 0; i < ri.nelem (); i++)
	    {
	      double tmp = b + i * increment;
	      int col = tree_to_mat_idx (tmp);
	      REP_ELEM_ASSIGN (0, col, rhs_m.elem (0, i), rhs_cm.elem (0, i),
			       rhs.is_real_type ());
	    }
	}
      else if (nc == 1)
	{
	  for (int i = 0; i < ri.nelem (); i++)
	    {
	      double tmp = b + i * increment;
	      int row = tree_to_mat_idx (tmp);
	      REP_ELEM_ASSIGN (row, 0, rhs_m.elem (i, 0), rhs_cm.elem (i, 0),
			       rhs.is_real_type ());
	    }
	}
      else
	panic_impossible ();
    }
  else
    panic_impossible ();
}

/*
 * Matrix assignment indexed by two values.  This function determines
 * the type of the first arugment, checks as much as possible, and
 * then calls one of a set of functions to handle the specific cases:
 *
 *   M (integer, arg2) = RHS  (MA1)
 *   M (vector,  arg2) = RHS  (MA2)
 *   M (range,   arg2) = RHS  (MA3)
 *   M (colon,   arg2) = RHS  (MA4)
 *
 * Each of those functions determines the type of the second argument
 * and calls another function to handle the real work of doing the
 * assignment.
 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant& i_arg,
					 tree_constant& j_arg)
{
  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = tree_to_mat_idx (tmp_i.double_value ());
	if (index_check (i, "row") < 0)
	  return;
	do_matrix_assignment (rhs, i, j_arg);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	idx_vector iv (mi, user_pref.do_fortran_indexing, "row", rows ());
	if (! iv)
	  return;

	do_matrix_assignment (rhs, iv, j_arg);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range ri = tmp_i.range_value ();
	int nr = rows ();
	if (nr == 2 && is_zero_one (ri))
	  {
	    do_matrix_assignment (rhs, 1, j_arg);
	  }
	else if (nr == 2 && is_one_zero (ri))
	  {
	    do_matrix_assignment (rhs, 0, j_arg);
	  }
	else
	  {
	    if (index_check (ri, "row") < 0)
	      return;
	    do_matrix_assignment (rhs, ri, j_arg);
	  }
      }
      break;
    case magic_colon:
      do_matrix_assignment (rhs, magic_colon, j_arg);
      break;
    default:
      panic_impossible ();
      break;
    }
}

/* MA1 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i,
					 tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return;
	if (! indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
	  {
	    ::error ("A(int,int) = X, X must be a scalar");
	    return;
	  }
	maybe_resize (i, j);
	if (error_state)
	  return;

	do_matrix_assignment (rhs, i, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	if (! indexed_assign_conforms (1, jv.capacity (), rhs_nr, rhs_nc))
	  {
	    ::error ("A(int,matrix) = X: X must be a row vector with the same");
	    ::error ("number of elements as matrix");
	    return;
	  }
	maybe_resize (i, jv.max ());
	if (error_state)
	  return;

	do_matrix_assignment (rhs, i, jv);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (! indexed_assign_conforms (1, rj.nelem (), rhs_nr, rhs_nc))
	  {
	    ::error ("A(int,range) = X: X must be a row vector with the same");
	    ::error ("number of elements as range");
	    return;
	  }

	int nc = columns ();
	if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, i, 1);
	  }
	else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, i, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return;
	    maybe_resize (i, tree_to_mat_idx (rj.max ()));
	    if (error_state)
	      return;

	    do_matrix_assignment (rhs, i, rj);
	  }
      }
      break;
    case magic_colon:
      {
	int nc = columns ();
	int nr = rows ();
	if (nc == 0 && nr == 0 && rhs_nr == 1)
	  {
	    if (rhs.is_complex_type ())
	      {
		complex_matrix = new ComplexMatrix ();
		type_tag = complex_matrix_constant;
	      }
	    else
	      {
		matrix = new Matrix ();
		type_tag = matrix_constant;
	      }
	    maybe_resize (i, rhs_nc-1);
	    if (error_state)
	      return;
	  }
	else if (indexed_assign_conforms (1, nc, rhs_nr, rhs_nc))
	  {
	    maybe_resize (i, nc-1);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (i < 0 || i >= nr)
	      {
		::error ("A(int,:) = []: row index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(int,:) = X: X must be a row vector with the same");
	    ::error ("number of columns as A");
	    return;
	  }

	do_matrix_assignment (rhs, i, magic_colon);
      }
      break;
    default:
      panic_impossible ();
      break;
    }
}

/* MA2 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, idx_vector& iv,
					 tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return;
	if (! indexed_assign_conforms (iv.capacity (), 1, rhs_nr, rhs_nc))
	  {
	    ::error ("A(matrix,int) = X: X must be a column vector with the");
	    ::error ("same number of elements as matrix");
	    return;
	  }
	maybe_resize (iv.max (), j);
	if (error_state)
	  return;

	do_matrix_assignment (rhs, iv, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	if (! indexed_assign_conforms (iv.capacity (), jv.capacity (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(r_mat,c_mat) = X: the number of rows in X must match");
	    ::error ("the number of elements in r_mat and the number of");
	    ::error ("columns in X must match the number of elements in c_mat");
	    return;
	  }
	maybe_resize (iv.max (), jv.max ());
	if (error_state)
	  return;

	do_matrix_assignment (rhs, iv, jv);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (! indexed_assign_conforms (iv.capacity (), rj.nelem (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(matrix,range) = X: the number of rows in X must match");
	    ::error ("the number of elements in matrix and the number of");
	    ::error ("columns in X must match the number of elements in range");
	    return;
	  }

	int nc = columns ();
	if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, iv, 1);
	  }
	else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, iv, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return;
	    maybe_resize (iv.max (), tree_to_mat_idx (rj.max ()));
	    if (error_state)
	      return;

	    do_matrix_assignment (rhs, iv, rj);
	  }
      }
      break;
    case magic_colon:
      {
	int nc = columns ();
	int new_nc = nc;
	if (nc == 0)
	  new_nc = rhs_nc;

	if (indexed_assign_conforms (iv.capacity (), new_nc,
				     rhs_nr, rhs_nc))
	  {
	    maybe_resize (iv.max (), new_nc-1);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (iv.max () >= rows ())
	      {
		::error ("A(matrix,:) = []: row index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(matrix,:) = X: the number of rows in X must match the");
	    ::error ("number of elements in matrix, and the number of columns");
	    ::error ("in X must match the number of columns in A");
	    return;
	  }

	do_matrix_assignment (rhs, iv, magic_colon);
      }
      break;
    default:
      panic_impossible ();
      break;
    }
}

/* MA3 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 Range& ri, tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return;
	if (! indexed_assign_conforms (ri.nelem (), 1, rhs_nr, rhs_nc))
	  {
	    ::error ("A(range,int) = X: X must be a column vector with the");
	    ::error ("same number of elements as range");
	    return;
	  }
	maybe_resize (tree_to_mat_idx (ri.max ()), j);
	if (error_state)
	  return;

	do_matrix_assignment (rhs, ri, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	if (! indexed_assign_conforms (ri.nelem (), jv.capacity (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(range,matrix) = X: the number of rows in X must match");
	    ::error ("the number of elements in range and the number of");
	    ::error ("columns in X must match the number of elements in matrix");
	    return;
	  }
	maybe_resize (tree_to_mat_idx (ri.max ()), jv.max ());
	if (error_state)
	  return;

	do_matrix_assignment (rhs, ri, jv);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (! indexed_assign_conforms (ri.nelem (), rj.nelem (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(r_range,c_range) = X: the number of rows in X must");
	    ::error ("match the number of elements in r_range and the number");
	    ::error ("of columns in X must match the number of elements in");
	    ::error ("c_range");
	    return;
	  }

	int nc = columns ();
	if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, ri, 1);
	  }
	else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, ri, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return;

	    maybe_resize (tree_to_mat_idx (ri.max ()),
			  tree_to_mat_idx (rj.max ()));

	    if (error_state)
	      return;

	    do_matrix_assignment (rhs, ri, rj);
	  }
      }
      break;
    case magic_colon:
      {
	int nc = columns ();
	int new_nc = nc;
	if (nc == 0)
	  new_nc = rhs_nc;

	if (indexed_assign_conforms (ri.nelem (), new_nc, rhs_nr, rhs_nc))
	  {
	    maybe_resize (tree_to_mat_idx (ri.max ()), new_nc-1);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    int b = tree_to_mat_idx (ri.min ());
	    int l = tree_to_mat_idx (ri.max ());
	    if (b < 0 || l >= rows ())
	      {
		::error ("A(range,:) = []: row index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(range,:) = X: the number of rows in X must match the");
	    ::error ("number of elements in range, and the number of columns");
	    ::error ("in X must match the number of columns in A");
	    return;
	  }

	do_matrix_assignment (rhs, ri, magic_colon);
      }
      break;
    default:
      panic_impossible ();
      break;
    }
}

/* MA4 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type i,
					 tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return;
	int nr = rows ();
	int nc = columns ();
	if (nr == 0 && nc == 0 && rhs_nc == 1)
	  {
	    if (rhs.is_complex_type ())
	      {
		complex_matrix = new ComplexMatrix ();
		type_tag = complex_matrix_constant;
	      }
	    else
	      {
		matrix = new Matrix ();
		type_tag = matrix_constant;
	      }
	    maybe_resize (rhs_nr-1, j);
	    if (error_state)
	      return;
	  }
	else if (indexed_assign_conforms (nr, 1, rhs_nr, rhs_nc))
	  {
	    maybe_resize (nr-1, j);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (j < 0 || j >= nc)
	      {
		::error ("A(:,int) = []: column index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(:,int) = X: X must be a column vector with the same");
	    ::error ("number of rows as A");
	    return;
	  }

	do_matrix_assignment (rhs, magic_colon, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	int nr = rows ();
	int new_nr = nr;
	if (nr == 0)
	  new_nr = rhs_nr;

	if (indexed_assign_conforms (new_nr, jv.capacity (),
				     rhs_nr, rhs_nc))
	  {
	    maybe_resize (new_nr-1, jv.max ());
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (jv.max () >= columns ())
	      {
		::error ("A(:,matrix) = []: column index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(:,matrix) = X: the number of rows in X must match the");
	    ::error ("number of rows in A, and the number of columns in X must");
	    ::error ("match the number of elements in matrix");
	    return;
	  }

	do_matrix_assignment (rhs, magic_colon, jv);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	int nr = rows ();
	int new_nr = nr;
	if (nr == 0)
	  new_nr = rhs_nr;

	if (indexed_assign_conforms (new_nr, rj.nelem (), rhs_nr, rhs_nc))
	  {
	    int nc = columns ();
	    if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	      {
		do_matrix_assignment (rhs, magic_colon, 1);
	      }
	    else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	      {
		do_matrix_assignment (rhs, magic_colon, 0);
	      }
	    else
	      {
		if (index_check (rj, "column") < 0)
		  return;
		maybe_resize (new_nr-1, tree_to_mat_idx (rj.max ()));
		if (error_state)
		  return;
	      }
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    int b = tree_to_mat_idx (rj.min ());
	    int l = tree_to_mat_idx (rj.max ());
	    if (b < 0 || l >= columns ())
	      {
		::error ("A(:,range) = []: column index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(:,range) = X: the number of rows in X must match the");
	    ::error ("number of rows in A, and the number of columns in X");
	    ::error ("must match the number of elements in range");
	    return;
	  }

	do_matrix_assignment (rhs, magic_colon, rj);
      }
      break;
    case magic_colon:
// a(:,:) = foo is equivalent to a = foo.
      do_matrix_assignment (rhs, magic_colon, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }
}

/*
 * Functions that actually handle assignment to a matrix using two
 * index values.
 *
 *                   idx2
 *            +---+---+----+----+
 *   idx1     | i | v |  r | c  |
 *   ---------+---+---+----+----+
 *   integer  | 1 | 5 |  9 | 13 |
 *   ---------+---+---+----+----+
 *   vector   | 2 | 6 | 10 | 14 |
 *   ---------+---+---+----+----+
 *   range    | 3 | 7 | 11 | 15 |
 *   ---------+---+---+----+----+
 *   colon    | 4 | 8 | 12 | 16 |
 *   ---------+---+---+----+----+
 */

/* 1 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i, int j)
{
  REP_ELEM_ASSIGN (i, j, rhs.double_value (), rhs.complex_value (),
		   rhs.is_real_type ());
}

/* 2 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i,
					 idx_vector& jv)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int j = 0; j < jv.capacity (); j++)
    REP_ELEM_ASSIGN (i, jv.elem (j), rhs_m.elem (0, j),
		     rhs_cm.elem (0, j), rhs.is_real_type ());
}

/* 3 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i, Range& rj)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = rj.base ();
  double increment = rj.inc ();

  for (int j = 0; j < rj.nelem (); j++)
    {
      double tmp = b + j * increment;
      int col = tree_to_mat_idx (tmp);
      REP_ELEM_ASSIGN (i, col, rhs_m.elem (0, j), rhs_cm.elem (0, j),
		       rhs.is_real_type ());
    }
}

/* 4 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i,
					 tree_constant_rep::constant_type mcj)
{
  assert (mcj == magic_colon);

  int nc = columns ();

  if (rhs.is_zero_by_zero ())
    {
      delete_row (i);
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      for (int j = 0; j < nc; j++)
	REP_ELEM_ASSIGN (i, j, rhs_m.elem (0, j), rhs_cm.elem (0, j),
			 rhs.is_real_type ());
    }
  else if (rhs.is_scalar_type () && nc == 1)
    {
      REP_ELEM_ASSIGN (i, 0, rhs.double_value (),
		       rhs.complex_value (), rhs.is_real_type ());
    }
  else
    panic_impossible ();
}

/* 5 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 idx_vector& iv, int j)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int i = 0; i < iv.capacity (); i++)
    {
      int row = iv.elem (i);
      REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, 0),
		       rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
}

/* 6 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 idx_vector& iv, idx_vector& jv)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int i = 0; i < iv.capacity (); i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < jv.capacity (); j++)
	{
	  int col = jv.elem (j);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem (i, j),
			   rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 7 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 idx_vector& iv, Range& rj)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = rj.base ();
  double increment = rj.inc ();

  for (int i = 0; i < iv.capacity (); i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < rj.nelem (); j++)
	{
	  double tmp = b + j * increment;
	  int col = tree_to_mat_idx (tmp);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem (i, j),
			   rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 8 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, idx_vector& iv,
					 tree_constant_rep::constant_type mcj)
{
  assert (mcj == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_rows (iv);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int nc = columns ();

      for (int j = 0; j < nc; j++)
	{
	  for (int i = 0; i < iv.capacity (); i++)
	    {
	      int row = iv.elem (i);
	      REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, j),
			       rhs_cm.elem (i, j), rhs.is_real_type ());
	    }
	}
    }
}

/* 9 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri, int j)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = ri.base ();
  double increment = ri.inc ();

  for (int i = 0; i < ri.nelem (); i++)
    {
      double tmp = b + i * increment;
      int row = tree_to_mat_idx (tmp);
      REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, 0),
		       rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
}

/* 10 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri,
					 idx_vector& jv)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = ri.base ();
  double increment = ri.inc ();

  for (int j = 0; j < jv.capacity (); j++)
    {
      int col = jv.elem (j);
      for (int i = 0; i < ri.nelem (); i++)
	{
	  double tmp = b + i * increment;
	  int row = tree_to_mat_idx (tmp);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem (i, j),
			   rhs_m.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 11 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri,
					 Range& rj)
{
  double ib = ri.base ();
  double iinc = ri.inc ();
  double jb = rj.base ();
  double jinc = rj.inc ();

  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int i = 0; i < ri.nelem (); i++)
    {
      double itmp = ib + i * iinc;
      int row = tree_to_mat_idx (itmp);
      for (int j = 0; j < rj.nelem (); j++)
	{
	  double jtmp = jb + j * jinc;
	  int col = tree_to_mat_idx (jtmp);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem  (i, j),
			   rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 12 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri,
					 tree_constant_rep::constant_type mcj)
{
  assert (mcj == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_rows (ri);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      double ib = ri.base ();
      double iinc = ri.inc ();

      int nc = columns ();

      for (int i = 0; i < ri.nelem (); i++)
	{
	  double itmp = ib + i * iinc;
	  int row = tree_to_mat_idx (itmp);
	  for (int j = 0; j < nc; j++)
	    REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, j),
			     rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 13 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 int j)
{
  assert (mci == magic_colon);

  int nr = rows ();

  if (rhs.is_zero_by_zero ())
    {
      delete_column (j);
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      for (int i = 0; i < nr; i++)
	REP_ELEM_ASSIGN (i, j, rhs_m.elem (i, 0),
			 rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
  else if (rhs.is_scalar_type () && nr == 1)
    {
      REP_ELEM_ASSIGN (0, j, rhs.double_value (),
		       rhs.complex_value (), rhs.is_real_type ());
    }
  else
    panic_impossible ();
}

/* 14 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 idx_vector& jv)
{
  assert (mci == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_columns (jv);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int nr = rows ();

      for (int i = 0; i < nr; i++)
	{
	  for (int j = 0; j < jv.capacity (); j++)
	    {
	      int col = jv.elem (j);
	      REP_ELEM_ASSIGN (i, col, rhs_m.elem (i, j),
			       rhs_cm.elem (i, j), rhs.is_real_type ());
	    }
	}
    }
}

/* 15 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 Range& rj)
{
  assert (mci == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_columns (rj);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int nr = rows ();

      double jb = rj.base ();
      double jinc = rj.inc ();

      for (int j = 0; j < rj.nelem (); j++)
	{
	  double jtmp = jb + j * jinc;
	  int col = tree_to_mat_idx (jtmp);
	  for (int i = 0; i < nr; i++)
	    {
	      REP_ELEM_ASSIGN (i, col, rhs_m.elem (i, j),
			       rhs_cm.elem (i, j), rhs.is_real_type ());
	    }
	}
    }
}

/* 16 */
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 tree_constant_rep::constant_type mcj)
{
  assert (mci == magic_colon && mcj == magic_colon);

  switch (type_tag)
    {
    case scalar_constant:
      break;
    case matrix_constant:
      delete matrix;
      break;
    case complex_scalar_constant:
      delete complex_scalar;
      break;
    case complex_matrix_constant:
      delete complex_matrix;
      break;
    case string_constant:
      delete [] string;
      break;
    case range_constant:
      delete range;
      break;
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }

  type_tag = rhs.const_type ();

  switch (type_tag)
    {
    case scalar_constant:
      scalar = rhs.double_value ();
      break;
    case matrix_constant:
      matrix = new Matrix (rhs.matrix_value ());
      break;
    case string_constant:
      string = strsave (rhs.string_value ());
      break;
    case complex_matrix_constant:
      complex_matrix = new ComplexMatrix (rhs.complex_matrix_value ());
      break;
    case complex_scalar_constant:
      complex_scalar = new Complex (rhs.complex_value ());
      break;
    case range_constant:
      range = new Range (rhs.range_value ());
      break;
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
}

/*
 * Functions for deleting rows or columns of a matrix.  These are used
 * to handle statements like
 *
 *   M (i, j) = []
 */
void
tree_constant_rep::delete_row (int idx)
{
  if (type_tag == matrix_constant)
    {
      int nr = matrix->rows ();
      int nc = matrix->columns ();
      Matrix *new_matrix = new Matrix (nr-1, nc);
      int ii = 0;
      for (int i = 0; i < nr; i++)
	{
	  if (i != idx)
	    {
	      for (int j = 0; j < nc; j++)
		new_matrix->elem (ii, j) = matrix->elem (i, j);
	      ii++;
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      int nr = complex_matrix->rows ();
      int nc = complex_matrix->columns ();
      ComplexMatrix *new_matrix = new ComplexMatrix (nr-1, nc);
      int ii = 0;
      for (int i = 0; i < nr; i++)
	{
	  if (i != idx)
	    {
	      for (int j = 0; j < nc; j++)
		new_matrix->elem (ii, j) = complex_matrix->elem (i, j);
	      ii++;
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::delete_rows (idx_vector& iv)
{
  iv.sort_uniq ();
  int num_to_delete = iv.length ();

  int nr = rows ();
  int nc = columns ();

// If deleting all rows of a column vector, make result 0x0.
  if (nc == 1 && num_to_delete == nr)
    nc = 0;

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (i == iv.elem (idx))
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (i == iv.elem (idx))
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = complex_matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::delete_rows (Range& ri)
{
  ri.sort ();
  int num_to_delete = ri.nelem ();

  int nr = rows ();
  int nc = columns ();

// If deleting all rows of a column vector, make result 0x0.
  if (nc == 1 && num_to_delete == nr)
    nc = 0;

  double ib = ri.base ();
  double iinc = ri.inc ();

  int max_idx = tree_to_mat_idx (ri.max ());

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      double itmp = ib + idx * iinc;
	      int row = tree_to_mat_idx (itmp);

	      if (i == row && row <= max_idx)
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      double itmp = ib + idx * iinc;
	      int row = tree_to_mat_idx (itmp);

	      if (i == row && row <= max_idx)
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = complex_matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::delete_column (int idx)
{
  if (type_tag == matrix_constant)
    {
      int nr = matrix->rows ();
      int nc = matrix->columns ();
      Matrix *new_matrix = new Matrix (nr, nc-1);
      int jj = 0;
      for (int j = 0; j < nc; j++)
	{
	  if (j != idx)
	    {
	      for (int i = 0; i < nr; i++)
		new_matrix->elem (i, jj) = matrix->elem (i, j);
	      jj++;
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      int nr = complex_matrix->rows ();
      int nc = complex_matrix->columns ();
      ComplexMatrix *new_matrix = new ComplexMatrix (nr, nc-1);
      int jj = 0;
      for (int j = 0; j < nc; j++)
	{
	  if (j != idx)
	    {
	      for (int i = 0; i < nr; i++)
		new_matrix->elem (i, jj) = complex_matrix->elem (i, j);
	      jj++;
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::delete_columns (idx_vector& jv)
{
  jv.sort_uniq ();
  int num_to_delete = jv.length ();

  int nr = rows ();
  int nc = columns ();

// If deleting all columns of a row vector, make result 0x0.
  if (nr == 1 && num_to_delete == nc)
    nr = 0;

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (j == jv.elem (idx))
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (j == jv.elem (idx))
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = complex_matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::delete_columns (Range& rj)
{
  rj.sort ();
  int num_to_delete = rj.nelem ();

  int nr = rows ();
  int nc = columns ();

// If deleting all columns of a row vector, make result 0x0.
  if (nr == 1 && num_to_delete == nc)
    nr = 0;

  double jb = rj.base ();
  double jinc = rj.inc ();

  int max_idx = tree_to_mat_idx (rj.max ());

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      double jtmp = jb + idx * jinc;
	      int col = tree_to_mat_idx (jtmp);

	      if (j == col && col <= max_idx)
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      double jtmp = jb + idx * jinc;
	      int col = tree_to_mat_idx (jtmp);

	      if (j == col && col <= max_idx)
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = complex_matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

/*
 * Indexing functions.
 */
int
tree_constant_rep::valid_as_scalar_index (void) const
{
  int valid = type_tag == magic_colon
    || (type_tag == scalar_constant && NINT (scalar) == 1)
    || (type_tag == range_constant
	&& range->nelem () == 1 && NINT (range->base ()) == 1);

  return valid;
}

tree_constant
tree_constant_rep::do_scalar_index (const tree_constant *args,
				    int nargs) const
{
  if (valid_scalar_indices (args, nargs))
    {
      if (type_tag == scalar_constant)
	return tree_constant (scalar);
      else if (type_tag == complex_scalar_constant)
	return tree_constant (*complex_scalar);
      else
	panic_impossible ();
    }
  else
    {
      int rows = 0;
      int cols = 0;

      switch (nargs)
	{
	case 3:
	  {
	    if (args[2].is_matrix_type ())
	      {
		Matrix mj = args[2].matrix_value ();

		idx_vector j (mj, user_pref.do_fortran_indexing, "");
		if (! j)
		  return tree_constant ();

		int len = j.length ();
		if (len == j.ones_count ())
		  cols = len;
	      }
	    else if (args[2].const_type () == magic_colon
		     || (args[2].is_scalar_type ()
			 && NINT (args[2].double_value ()) == 1))
	      {
		cols = 1;
	      }
	    else
	      break;
	  }
// Fall through...
	case 2:
	  {
	    if (args[1].is_matrix_type ())
	      {
		Matrix mi = args[1].matrix_value ();

		idx_vector i (mi, user_pref.do_fortran_indexing, "");
		if (! i)
		  return tree_constant ();

		int len = i.length ();
		if (len == i.ones_count ())
		  rows = len;
	      }
	    else if (args[1].const_type () == magic_colon
		     || (args[1].is_scalar_type ()
			 && NINT (args[1].double_value ()) == 1))
	      {
		rows = 1;
	      }
	    else if (args[1].is_scalar_type ()
		     && NINT (args[1].double_value ()) == 0)
	      {
		Matrix m (0, 0);
		return tree_constant (m);
	      }
	    else
	      break;

	    if (cols == 0)
	      {
		if (user_pref.prefer_column_vectors)
		  cols = 1;
		else
		  {
		    cols = rows;
		    rows = 1;
		  }
	      }

	    if (type_tag == scalar_constant)
	      {
		Matrix m (rows, cols, scalar);
		return tree_constant (m);
	      }
	    else if (type_tag == complex_scalar_constant)
	      {
		ComplexMatrix cm (rows, cols, *complex_scalar);
		return tree_constant (cm);
	      }
	    else
	      panic_impossible ();
	  }
	  break;
	default:
	  ::error ("illegal number of arguments for scalar type");
	  return tree_constant ();
	  break;
	}
    }

  ::error ("index invalid or out of range for scalar type");
  return tree_constant ();
}

tree_constant
tree_constant_rep::do_matrix_index (const tree_constant *args,
				    int nargin) const
{
  tree_constant retval;

  switch (nargin)
    {
    case 2:
      if (! args)
	::error ("matrix index is null");
      else if (args[1].is_undefined ())
	::error ("matrix index is a null expression");
      else
	retval = do_matrix_index (args[1]);
      break;
    case 3:
      if (! args)
	::error ("matrix indices are null");
      else if (args[1].is_undefined ())
	::error ("first matrix index is a null expression");
      else if (args[2].is_undefined ())
	::error ("second matrix index is a null expression");
      else
	retval = do_matrix_index (args[1], args[2]);
      break;
    default:
      ::error ("too many indices for matrix expression");
      break;
    }

  return  retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const tree_constant& i_arg) const
{
  tree_constant retval;

  int nr = rows ();
  int nc = columns ();

  if (user_pref.do_fortran_indexing)
    retval = fortran_style_matrix_index (i_arg);
  else if (nr <= 1 || nc <= 1)
    retval = do_vector_index (i_arg);
  else
    ::error ("single index only valid for row or column vector");

  return retval;
}

tree_constant
tree_constant_rep::fortran_style_matrix_index
  (const tree_constant& i_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = NINT (tmp_i.double_value ());
	int ii = fortran_row (i, nr) - 1;
	int jj = fortran_column (i, nr) - 1;
	if (index_check (i-1, "") < 0)
	  return tree_constant ();
	if (range_max_check (i-1, nr * nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (ii, jj);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	if (mi.rows () == 0 || mi.columns () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
// Yes, we really do want to call this with mi.
	    retval = fortran_style_matrix_index (mi);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      gripe_range_invalid ();
      break;
    case magic_colon:
      retval = do_matrix_index (magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::fortran_style_matrix_index (const Matrix& mi) const
{
  assert (is_matrix_type ());

  tree_constant retval;

  int nr = rows ();
  int nc = columns ();

  int len = nr * nc;

  int index_nr = mi.rows ();
  int index_nc = mi.columns ();

  if (index_nr >= 1 && index_nc >= 1)
    {
      const double *cop_out = (const double *) NULL;
      const Complex *c_cop_out = (const Complex *) NULL;
      int real_type = type_tag == matrix_constant;
      if (real_type)
	cop_out = matrix->data ();
      else
	c_cop_out = complex_matrix->data ();

      const double *cop_out_index = mi.data ();

      idx_vector iv (mi, 1, "", len);
      if (! iv)
	return tree_constant ();

      int result_size = iv.length ();

      if (nc == 1 || (nr != 1 && iv.one_zero_only ()))
	{
	  CRMATRIX (m, cm, result_size, 1);

	  for (int i = 0; i < result_size; i++)
	    {
	      int idx = iv.elem (i);
	      CRMATRIX_ASSIGN_ELEM (m, cm, i, 0, cop_out [idx],
				    c_cop_out [idx], real_type);
	    }

	  ASSIGN_CRMATRIX_TO (retval, m, cm);
	}
      else if (nr == 1)
	{
	  CRMATRIX (m, cm, 1, result_size);

	  for (int i = 0; i < result_size; i++)
	    {
	      int idx = iv.elem (i);
	      CRMATRIX_ASSIGN_ELEM (m, cm, 0, i, cop_out [idx],
				    c_cop_out [idx], real_type);
	    }

	  ASSIGN_CRMATRIX_TO (retval, m, cm);
	}
      else
	{
	  CRMATRIX (m, cm, index_nr, index_nc);

	  for (int j = 0; j < index_nc; j++)
	    for (int i = 0; i < index_nr; i++)
	      {
		double tmp = *cop_out_index++;
		int idx = tree_to_mat_idx (tmp);
		CRMATRIX_ASSIGN_ELEM (m, cm, i, j, cop_out [idx],
				      c_cop_out [idx], real_type);
	      }

	  ASSIGN_CRMATRIX_TO (retval, m, cm);
	}
    }
  else
    {
      if (index_nr == 0 || index_nc == 0)
	::error ("empty matrix invalid as index");
      else
	::error ("invalid matrix index");
      return tree_constant ();
    }

  return retval;
}

tree_constant
tree_constant_rep::do_vector_index (const tree_constant& i_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  int len = MAX (nr, nc);

  assert ((nr == 1 || nc == 1) && ! user_pref.do_fortran_indexing);

  int swap_indices = (nr == 1);

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
        int i = tree_to_mat_idx (tmp_i.double_value ());
        if (index_check (i, "") < 0)
	  return tree_constant ();
        if (swap_indices)
          {
	    if (range_max_check (i, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (0, i);
          }
        else
          {
	    if (range_max_check (i, nr) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (i, 0);
          }
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
        Matrix mi = tmp_i.matrix_value ();
	if (mi.rows () == 0 || mi.columns () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    idx_vector iv (mi, user_pref.do_fortran_indexing, "", len);
	    if (! iv)
	      return tree_constant ();

	    if (swap_indices)
	      {
		if (range_max_check (iv.max (), nc) < 0)
		  return tree_constant ();
		retval = do_matrix_index (0, iv);
	      }
	    else
	      {
		if (range_max_check (iv.max (), nr) < 0)
		  return tree_constant ();
		retval = do_matrix_index (iv, 0);
	      }
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
        Range ri = tmp_i.range_value ();
	if (len == 2 && is_zero_one (ri))
	  {
	    if (swap_indices)
	      retval = do_matrix_index (0, 1);
	    else
	      retval = do_matrix_index (1, 0);
	  }
	else if (len == 2 && is_one_zero (ri))
	  {
	    retval = do_matrix_index (0, 0);
	  }
	else
	  {
	    if (index_check (ri, "") < 0)
	      return tree_constant ();
	    if (swap_indices)
	      {
		if (range_max_check (tree_to_mat_idx (ri.max ()), nc) < 0)
		  return tree_constant ();
		retval = do_matrix_index (0, ri);
	      }
	    else
	      {
		if (range_max_check (tree_to_mat_idx (ri.max ()), nr) < 0)
		  return tree_constant ();
		retval = do_matrix_index (ri, 0);
	      }
	  }
      }
      break;
    case magic_colon:
      if (swap_indices)
        retval = do_matrix_index (0, magic_colon);
      else
        retval = do_matrix_index (magic_colon, 0);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const tree_constant& i_arg,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
        int i = tree_to_mat_idx (tmp_i.double_value ());
	if (index_check (i, "row") < 0)
	  return tree_constant ();
	retval = do_matrix_index (i, j_arg);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	idx_vector iv (mi, user_pref.do_fortran_indexing, "row", rows ());
	if (! iv)
	  return tree_constant ();

	if (iv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  retval = do_matrix_index (iv, j_arg);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range ri = tmp_i.range_value ();
	int nr = rows ();
	if (nr == 2 && is_zero_one (ri))
	  {
	    retval = do_matrix_index (1, j_arg);
	  }
	else if (nr == 2 && is_one_zero (ri))
	  {
	    retval = do_matrix_index (0, j_arg);
	  }
	else
	  {
	    if (index_check (ri, "row") < 0)
	      return tree_constant ();
	    retval = do_matrix_index (ri, j_arg);
	  }
      }
      break;
    case magic_colon:
      retval = do_matrix_index (magic_colon, j_arg);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (i, j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (i, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (i, jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (i, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (i, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (i, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (i, tree_to_mat_idx (rj.max ()), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (i, rj);
	  }
      }
      break;
    case magic_colon:
      if (range_max_check (i, 0, nr, nc) < 0)
	return tree_constant ();
      retval = do_matrix_index (i, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (iv.max (), j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (iv, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (iv.max (), jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (iv, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (iv, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (iv, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (iv.max (), tree_to_mat_idx (rj.max ()),
				 nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (iv, rj);
	  }
      }
      break;
    case magic_colon:
      if (range_max_check (iv.max (), 0, nr, nc) < 0)
	return tree_constant ();
      retval = do_matrix_index (iv, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (tree_to_mat_idx (ri.max ()), j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (ri, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (tree_to_mat_idx (ri.max ()),
				 jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (ri, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (ri, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (ri, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (tree_to_mat_idx (ri.max ()),
				 tree_to_mat_idx (rj.max ()), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (ri, rj);
	  }
      }
      break;
    case magic_colon:
      retval = do_matrix_index (ri, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (0, j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (magic_colon, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (0, jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (magic_colon, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (magic_colon, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (magic_colon, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (0, tree_to_mat_idx (rj.max ()), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (magic_colon, rj);
	  }
      }
      break;
    case magic_colon:
      retval = do_matrix_index (magic_colon, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, int j) const
{
  tree_constant retval;

  if (type_tag == matrix_constant)
    retval = tree_constant (matrix->elem (i, j));
  else
    retval = tree_constant (complex_matrix->elem (i, j));

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, const idx_vector& jv) const
{
  tree_constant retval;

  int jlen = jv.capacity ();

  CRMATRIX (m, cm, 1, jlen);

  for (int j = 0; j < jlen; j++)
    {
      int col = jv.elem (j);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, 0, j, i, col);
    }
  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, const Range& rj) const
{
  tree_constant retval;

  int jlen = rj.nelem ();

  CRMATRIX (m, cm, 1, jlen);

  double b = rj.base ();
  double increment = rj.inc ();
  for (int j = 0; j < jlen; j++)
    {
      double tmp = b + j * increment;
      int col = tree_to_mat_idx (tmp);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, 0, j, i, col);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index
  (int i, tree_constant_rep::constant_type mcj) const
{
  assert (mcj == magic_colon);

  tree_constant retval;

  int nc = columns ();

  CRMATRIX (m, cm, 1, nc);

  for (int j = 0; j < nc; j++)
    {
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, 0, j, i, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv, int j) const
{
  tree_constant retval;

  int ilen = iv.capacity ();

  CRMATRIX (m, cm, ilen, 1);

  for (int i = 0; i < ilen; i++)
    {
      int row = iv.elem (i);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, 0, row, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv,
				    const idx_vector& jv) const
{
  tree_constant retval;

  int ilen = iv.capacity ();
  int jlen = jv.capacity ();

  CRMATRIX (m, cm, ilen, jlen);

  for (int i = 0; i < ilen; i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < jlen; j++)
	{
	  int col = jv.elem (j);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv,
				    const Range& rj) const
{
  tree_constant retval;

  int ilen = iv.capacity ();
  int jlen = rj.nelem ();

  CRMATRIX (m, cm, ilen, jlen);

  double b = rj.base ();
  double increment = rj.inc ();

  for (int i = 0; i < ilen; i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < jlen; j++)
	{
	  double tmp = b + j * increment;
	  int col = tree_to_mat_idx (tmp);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index
  (const idx_vector& iv, tree_constant_rep::constant_type mcj) const
{
  assert (mcj == magic_colon);

  tree_constant retval;

  int nc = columns ();
  int ilen = iv.capacity ();

  CRMATRIX (m, cm, ilen, nc);

  for (int j = 0; j < nc; j++)
    {
      for (int i = 0; i < ilen; i++)
	{
	  int row = iv.elem (i);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, j);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri, int j) const
{
  tree_constant retval;

  int ilen = ri.nelem ();

  CRMATRIX (m, cm, ilen, 1);

  double b = ri.base ();
  double increment = ri.inc ();
  for (int i = 0; i < ilen; i++)
    {
      double tmp = b + i * increment;
      int row = tree_to_mat_idx (tmp);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, 0, row, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri,
				    const idx_vector& jv) const
{
  tree_constant retval;

  int ilen = ri.nelem ();
  int jlen = jv.capacity ();

  CRMATRIX (m, cm, ilen, jlen);

  double b = ri.base ();
  double increment = ri.inc ();
  for (int i = 0; i < ilen; i++)
    {
      double tmp = b + i * increment;
      int row = tree_to_mat_idx (tmp);
      for (int j = 0; j < jlen; j++)
	{
	  int col = jv.elem (j);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri, const Range& rj) const
{
  tree_constant retval;

  int ilen = ri.nelem ();
  int jlen = rj.nelem ();

  CRMATRIX (m, cm, ilen, jlen);

  double ib = ri.base ();
  double iinc = ri.inc ();
  double jb = rj.base ();
  double jinc = rj.inc ();

  for (int i = 0; i < ilen; i++)
    {
      double itmp = ib + i * iinc;
      int row = tree_to_mat_idx (itmp);
      for (int j = 0; j < jlen; j++)
	{
	  double jtmp = jb + j * jinc;
	  int col = tree_to_mat_idx (jtmp);

	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index
  (const Range& ri, tree_constant_rep::constant_type mcj) const
{
  assert (mcj == magic_colon);

  tree_constant retval;

  int nc = columns ();

  int ilen = ri.nelem ();

  CRMATRIX (m, cm, ilen, nc);

  double ib = ri.base ();
  double iinc = ri.inc ();

  for (int i = 0; i < ilen; i++)
    {
      double itmp = ib + i * iinc;
      int row = tree_to_mat_idx (itmp);
      for (int j = 0; j < nc; j++)
	{
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, j);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    int j) const
{
  assert (mci == magic_colon);

  tree_constant retval;

  int nr = rows ();

  CRMATRIX (m, cm, nr, 1);

  for (int i = 0; i < nr; i++)
    {
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, 0, i, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    const idx_vector& jv) const
{
  assert (mci == magic_colon);

  tree_constant retval;

  int nr = rows ();
  int jlen = jv.capacity ();

  CRMATRIX (m, cm, nr, jlen);

  for (int i = 0; i < nr; i++)
    {
      for (int j = 0; j < jlen; j++)
	{
	  int col = jv.elem (j);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, i, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    const Range& rj) const
{
  assert (mci == magic_colon);

  tree_constant retval;

  int nr = rows ();
  int jlen = rj.nelem ();

  CRMATRIX (m, cm, nr, jlen);

  double jb = rj.base ();
  double jinc = rj.inc ();

  for (int j = 0; j < jlen; j++)
    {
      double jtmp = jb + j * jinc;
      int col = tree_to_mat_idx (jtmp);
      for (int i = 0; i < nr; i++)
	{
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, i, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    tree_constant_rep::constant_type mcj) const
{
  assert (mci == magic_colon && mcj == magic_colon);

  return tree_constant (*this);
}

tree_constant
tree_constant_rep::do_matrix_index
  (tree_constant_rep::constant_type mci) const
{
  assert (mci == magic_colon);

  tree_constant retval;
  int nr =  rows ();
  int nc =  columns ();
  int size = nr * nc;
  if (size > 0)
    {
      CRMATRIX (m, cm, size, 1);
      int idx = 0;
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  {
	    CRMATRIX_ASSIGN_REP_ELEM (m, cm, idx, 0, i, j);
	    idx++;
	  }
      ASSIGN_CRMATRIX_TO (retval, m, cm);
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
