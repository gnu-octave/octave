// The constants for the tree class.                      -*- C++ -*-
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

#ifdef __GNUG__
#pragma implementation
#endif

#include <ctype.h>
#include <string.h>
#include <iostream.h>
#include <strstream.h>
#include <math.h>

#include "variables.h"
#include "error.h"
#include "gripes.h"
#include "user-prefs.h"
#include "utils.h"
#include "pager.h"
#include "mappers.h"
#include "pr-output.h"
#include "tree-const.h"
#include "arith-ops.h"

// A couple of handy helper functions.

static int
any_element_is_negative (const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) < 0.0)
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

tree_constant_rep::tree_constant_rep (Matrix& m)
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

tree_constant_rep::tree_constant_rep (DiagMatrix& d)
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

tree_constant_rep::tree_constant_rep (RowVector& v)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v.elem (0);
      type_tag = scalar_constant;
    }
  else
    {
      if (user_pref.prefer_column_vectors)
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

tree_constant_rep::tree_constant_rep (RowVector& v, int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v.elem (0);
      type_tag = scalar_constant;
    }
  else
    {
      if (prefer_column_vector)
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

tree_constant_rep::tree_constant_rep (ColumnVector& v)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v.elem (0);
      type_tag = scalar_constant;
    }
  else
    {
      if (user_pref.prefer_column_vectors)
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

tree_constant_rep::tree_constant_rep (ColumnVector& v,
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
      if (prefer_column_vector)
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

tree_constant_rep::tree_constant_rep (Complex c)
{
  complex_scalar = new Complex (c);
  type_tag = complex_scalar_constant;
}

tree_constant_rep::tree_constant_rep (ComplexRowVector& v)
{
  int len = v.capacity ();
  if (len == 1)
    {
      complex_scalar = new Complex (v.elem (0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      if (user_pref.prefer_column_vectors)
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

tree_constant_rep::tree_constant_rep (ComplexMatrix& m)
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

tree_constant_rep::tree_constant_rep (ComplexDiagMatrix& d)
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

tree_constant_rep::tree_constant_rep (ComplexRowVector& v,
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
      if (prefer_column_vector)
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

tree_constant_rep::tree_constant_rep (ComplexColumnVector& v)
{
  int len = v.capacity ();
  if (len == 1)
    {
      complex_scalar = new Complex (v.elem (0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      if (user_pref.prefer_column_vectors)
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

tree_constant_rep::tree_constant_rep (ComplexColumnVector& v,
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
      if (prefer_column_vector)
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

tree_constant_rep::tree_constant_rep (String& s)
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
      if (nel == -1)
	error ("number of elements in range exceeds INT_MAX");
      else
	error ("invalid range");

      jump_to_top_level ();
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

tree_constant_rep::tree_constant_rep (Range& r)
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

tree_constant_rep::tree_constant_rep (tree_constant_rep& t)
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
	    message ((char *) NULL,
		     "row index = %d exceeds max row dimension = %d", i, nr);
	  if (j > nc)
	    message ((char *) NULL,
		     "column index = %d exceeds max column dimension = %d",
		     j, nc);

	  jump_to_top_level ();
	}
    }
}

void
tree_constant_rep::maybe_resize (int i, force_orient f_orient = no_orient)
{
  int nr = rows ();
  int nc = columns ();

  i++;

  assert (i > 0 && (nr <= 1 || nc <= 1));

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
	{
	  message ((char *) NULL,
		   "matrix index = %d exceeds max dimension = %d", i, nc);
	  jump_to_top_level ();
	}
    }
  else if (nr == 1 && i > nc)
    {
      if (user_pref.resize_on_range_error)
	resize (1, i, 0.0);
      else
	{
	  message ((char *) NULL,
		   "matrix index = %d exceeds max dimension = %d", i, nc);
	  jump_to_top_level ();
	}
    }
  else if (nc == 1 && i > nr)
    {
      if (user_pref.resize_on_range_error)
	resize (i, 1, 0.0);
      else
	{
	  message ((char *) NULL,
		   "matrix index = %d exceeds max dimension = ", i, nc);
	  jump_to_top_level ();
	}
    }
}

double
tree_constant_rep::to_scalar (void)
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
	      return real (m (0, 0));
	    }
	  else
	    jump_to_top_level ();
	}
      else
	{
	  error ("complex matrix used in invalid context");
	  jump_to_top_level ();
	}
      break;
    default:
      break;
    }
  return retval;
}

ColumnVector
tree_constant_rep::to_vector (void)
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
tree_constant_rep::to_matrix (void)
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
	    error ("failed to convert `%s' to a numeric type -- default\
 conversion turned off", string);
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
tree_constant_rep::make_numeric (int force_str_conv = 0)
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
  int first_empty = (a.rows () == 0 || a.columns () == 0);
  int second_empty = (b.rows () == 0 || b.columns () == 0);

  if (first_empty || second_empty)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	warning ("binary operation on empty matrix");
      else if (flag == 0)
	{
	  error ("invalid binary operation on empty matrix");
	  jump_to_top_level ();
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

  tree_constant ans;

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
  if (a.rows () == 0 || a.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	warning ("unary operation on empty matrix");
      else if (flag == 0)
	{
	  error ("invalid unary operation on empty matrix");
	  jump_to_top_level ();
	}
    }

  tree_constant tmp_a = a.make_numeric ();

  tree_constant ans;

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
	  error ("string++ and ++string not implemented yet, ok?");
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
	  error ("string-- and -- string not implemented yet, ok?");
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
tree_constant_rep::eval (int print)
{
  switch (type_tag)
    {
    case complex_scalar_constant:
      if (imag (*complex_scalar) == 0.0)
	{
	  double d = real (*complex_scalar);
	  delete complex_scalar;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;
    case complex_matrix_constant:
      if (! any_element_is_complex (*complex_matrix))
	{
	  Matrix *m = new Matrix (real (*complex_matrix));
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

  if (print)
    {
      int nr = rows ();
      int nc = columns ();

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

tree_constant *
tree_constant_rep::eval (tree_constant *args, int nargin, int nargout,
			 int print)
{
  tree_constant *retval = new tree_constant [2];
  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
      retval[0] = do_scalar_index (args, nargin);
      break;
    case complex_matrix_constant:
    case matrix_constant:
      retval[0] = do_matrix_index (args, nargin);
      break;
    case string_constant:
      gripe_string_invalid ();
//      retval[0] = do_string_index (args, nargin);
      break;
    case magic_colon:
    case range_constant:
// This isn\'t great, but it\'s easier than implementing a lot of
// range indexing functions.
      force_numeric ();
      assert (type_tag != magic_colon && type_tag != range_constant);
      return eval (args, nargin, nargout, print);
      break;
    default:
      panic_impossible ();
      break;
    }

  if (retval[0].is_defined ())
    retval[0].eval (print);
  return retval;
}

int
tree_constant_rep::save (ostream& os, int mark_as_global)
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
      error ("for now, I can only save real matrices in 3D format");
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
  char tag [128];
  if (extract_keyword (is, "type", tag))
    {
      if (tag != (char *) NULL && *tag != '\0')
	{
	  char *ptr = strchr (tag, ' ');
	  if (ptr != (char *) NULL)
	    {
	      *ptr = '\0';
	      is_global = (strncmp (tag, "global", 6) == 0);
	      *ptr = ' ';
	      ptr++;
	    }
	  else
	    ptr = &tag[0];

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
	    error ("unknown constant type `%s'", tag);
	}
      else
	error ("failed to extract keyword specifying value type");
    }

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
	error ("failed to load scalar constant");
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
	      error ("failed to load matrix constant");
	  }
	else
	  error ("failed to extract number of rows and columns");
      }
      break;
    case complex_scalar_constant:
      is >> *complex_scalar;
      if (is)
	status = complex_scalar_constant;
      else
	error ("failed to load complex scalar constant");
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
	      error ("failed to load complex matrix constant");
	  }
	else
	  error ("failed to extract number of rows and columns");
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
	      error ("failed to load string constant");
	  }
	else
	  error ("failed to extract string length");
      }
      break;
    case range_constant:
      skip_comments (is);
      range = new Range ();
      is >> *range;
      if (is)
	status = range_constant;
      else
	error ("failed to load range constant");
      break;
    default:
      panic_impossible ();
      break;
    }
  return status;
}

double
tree_constant_rep::double_value (void)
{
  assert (type_tag == scalar_constant || type_tag == complex_scalar_constant);

  if (type_tag == scalar_constant)
    return scalar;
  else if (type_tag == complex_scalar_constant)
    {
      int flag = user_pref.ok_to_lose_imaginary_part;
      if (flag == -1)
	warning ("implicit conversion of complex value to real value");

      if (flag != 0)
	return real (*complex_scalar);
      else
	{
	  error ("implicit conversion of complex value to real value not allowed");
	  jump_to_top_level ();
	}
    }
}

Matrix
tree_constant_rep::matrix_value (void)
{
  assert (type_tag == matrix_constant || type_tag == complex_matrix_constant);

  if (type_tag == matrix_constant)
    return *matrix;
  else if (type_tag == complex_matrix_constant)
    {
      int flag = user_pref.ok_to_lose_imaginary_part;
      if (flag == -1)
	warning ("implicit conversion of complex matrix to real matrix"); 

      if (flag != 0)
	return real (*complex_matrix);
      else
	{
	  error ("implicit conversion of complex matrix to real matrix not allowed");
	  jump_to_top_level ();
	}
    }
}

Complex
tree_constant_rep::complex_value (void)
{
  assert (type_tag == complex_scalar_constant);
  return *complex_scalar;
}

ComplexMatrix
tree_constant_rep::complex_matrix_value (void)
{
  assert (type_tag == complex_matrix_constant);
  return *complex_matrix;
}

char *
tree_constant_rep::string_value (void)
{
  assert (type_tag == string_constant);
  return string;
}

Range
tree_constant_rep::range_value (void)
{
  assert (type_tag == range_constant);
  return *range;
}

int
tree_constant_rep::rows (void)
{
  int retval = -1;
  switch (type_tag)
    {
    case scalar_constant:
    case complex_scalar_constant:
    case string_constant:
    case range_constant:
      retval = 1;
      break;
    case matrix_constant:
      retval = matrix->rows ();
      break;
    case complex_matrix_constant:
      retval = complex_matrix->rows ();
      break;
    case magic_colon:
      error ("invalid use of colon operator");
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
tree_constant_rep::columns (void)
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
      error ("invalid use of colon operator");
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
tree_constant_rep::all (void)
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
tree_constant_rep::any (void)
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
tree_constant_rep::isstr (void)
{
  double status = 0.0;
  if (const_type () == string_constant)
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
	retval = tree_constant (s);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	ColumnVector v = to_vector ();
	int len = v.length ();
	if (len == 0)
	  error ("can only convert vectors and scalars to strings");
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

tree_constant
tree_constant_rep::cumprod (void)
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
tree_constant_rep::cumsum (void)
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
tree_constant_rep::prod (void)
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
tree_constant_rep::sum (void)
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
tree_constant_rep::sumsq (void)
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
make_diag (Matrix& v, int k)
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
make_diag (ComplexMatrix& v, int k)
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
tree_constant_rep::diag (void)
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
	if (nr == 1 || nc == 1)
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
	if (nr == 1 || nc == 1)
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
tree_constant_rep::diag (tree_constant& a)
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
	  if (nr == 1 || nc == 1)
	    retval = make_diag (matrix_value (), k);
	  else
	    {
	      ColumnVector d = matrix->diag (k);
	      retval = tree_constant (d);
	    }
	}
      else
	message ("diag", "invalid second argument");

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
	  if (nr == 1 || nc == 1)
	    retval = make_diag (complex_matrix_value (), k);
	  else
	    {
	      ComplexColumnVector d = complex_matrix->diag (k);
	      retval = tree_constant (d);
	    }
	}
      else
	message ("diag", "invalid second argument");

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

void
tree_constant_rep::print_if_string (ostream& os, int warn)
{
  if (type_tag == string_constant)
    os << string << "\n";
  else if (warn)
    warning ("expecting string, found numeric constant");
}

tree_constant
tree_constant_rep::mapper (Mapper_fcn& m_fcn, int print)
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
      if (m_fcn.neg_arg_complex && scalar < 0.0)
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
      if (m_fcn.neg_arg_complex && any_element_is_negative (*matrix))
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

  if (retval.is_defined ())
    return retval.eval (print);
  else
    return retval;
}

tree_constant::~tree_constant (void)
{
#if defined (MDEBUG)
  cerr << "~tree_constant: rep: " << rep
       << " rep->count: " << rep->count << "\n";
#endif

  if (--rep->count <= 0)
    {
      delete rep;
      rep = (tree_constant_rep *) NULL;
    }
}

#if defined (MDEBUG)
void *
tree_constant::operator new (size_t size)
{
  tree_constant *p = ::new tree_constant;
  cerr << "tree_constant::new(): " << p << "\n";
  return p;
}

void
tree_constant::operator delete (void *p, size_t size)
{
  cerr << "tree_constant::delete(): " << p << "\n";
  ::delete p;
}
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
