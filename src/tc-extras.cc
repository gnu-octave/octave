// Some extra friends of the tree constant class.        -*- C++ -*-
// See also the other tc-*.cc files.
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

#include <strstream.h>
#include <iostream.h>
#include <fstream.h>
#include <string.h>
#include <ctype.h>

#include "unwind-prot.h"
#include "tree-const.h"
#include "user-prefs.h"
#include "variables.h"
#include "octave.h"
#include "gripes.h"
#include "error.h"
#include "input.h"
#include "octave-hist.h"
#include "pager.h"
#include "utils.h"
#include "parse.h"
#include "lex.h"

Matrix
max (const Matrix& a, const Matrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    FAIL;

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double a_elem = a.elem (i, j);
	double b_elem = b.elem (i, j);
	result.elem (i, j) = MAX (a_elem, b_elem);
      }

  return result;
}

ComplexMatrix
max (const ComplexMatrix& a, const ComplexMatrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    FAIL;

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
        double abs_a_elem = abs (a.elem (i, j));
        double abs_b_elem = abs (b.elem (i, j));
        if (abs_a_elem > abs_b_elem)
          result.elem (i, j) = a.elem (i, j);
        else
          result.elem (i, j) = b.elem (i, j);
      }

  return result;
}

Matrix
min (const Matrix& a, const Matrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    FAIL;

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double a_elem = a.elem (i, j);
	double b_elem = b.elem (i, j);
	result.elem (i, j) = MIN (a_elem, b_elem);
      }

  return result;
}

ComplexMatrix
min (const ComplexMatrix& a, const ComplexMatrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    FAIL;

  ComplexMatrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
        double abs_a_elem = abs (a.elem (i, j));
        double abs_b_elem = abs (b.elem (i, j));
        if (abs_a_elem < abs_b_elem)
          result.elem (i, j) = a.elem (i, j);
        else
          result.elem (i, j) = b.elem (i, j);
      }

  return result;
}

static void
get_dimensions (const tree_constant& a, const char *warn_for,
		int& nr, int& nc)
{
  tree_constant tmpa = a.make_numeric ();

  if (tmpa.is_scalar_type ())
    {
      double tmp = tmpa.double_value ();
      nr = nc = NINT (tmp);
    }
  else
    {
      nr = a.rows ();
      nc = a.columns ();
    }

  check_dimensions (nr, nc, warn_for); // May set error_state.
}

static void
get_dimensions (const tree_constant& a, const tree_constant& b,
		const char *warn_for, int& nr, int& nc)
{
  tree_constant tmpa = a.make_numeric ();
  tree_constant tmpb = b.make_numeric ();

  if (tmpa.is_scalar_type () && tmpb.is_scalar_type ())
    {
      nr = NINT (tmpa.double_value ());
      nc = NINT (tmpb.double_value ());

      check_dimensions (nr, nc, warn_for); // May set error_state.
    }
  else
    error ("%s: expecting two scalar arguments", warn_for);
}

tree_constant
fill_matrix (const tree_constant& a, double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, warn_for, nr, nc);

  if (error_state)
    return  tree_constant ();

  Matrix m (nr, nc, val);

  return tree_constant (m);
}

tree_constant
fill_matrix (const tree_constant& a, const tree_constant& b,
	     double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, b, warn_for, nr, nc); // May set error_state.

  if (error_state)
    return tree_constant ();

  Matrix m (nr, nc, val);

  return tree_constant (m);
}

tree_constant
identity_matrix (const tree_constant& a)
{
  int nr, nc;
  get_dimensions (a, "eye", nr, nc); // May set error_state.

  if (error_state)
    return tree_constant ();

  Matrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      int n = MIN (nr, nc);
      for (int i = 0; i < n; i++)
	m.elem (i, i) = 1.0;
    }

  return tree_constant (m);
}

tree_constant
identity_matrix (const tree_constant& a, const tree_constant& b)
{
  int nr, nc;
  get_dimensions (a, b, "eye", nr, nc);  // May set error_state.

  if (error_state)
    return tree_constant ();

  Matrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      int n = MIN (nr, nc);
      for (int i = 0; i < n; i++)
	m.elem (i, i) = 1.0;
    }

  return tree_constant (m);
}

static tree_constant
find_nonzero_elem_idx (const Matrix& m)
{
  int count = 0;
  int m_nr = m.rows ();
  int m_nc = m.columns ();

  int i;
  for (int j = 0; j < m_nc; j++)
    for (i = 0; i < m_nr; i++)
      if (m.elem (i, j) != 0)
	count++;

  Matrix result;

  if (count == 0)
    return result;

  if (m_nr == 1)
    {
      result.resize (1, count);
      count = 0;
      for (j = 0; j < m_nc; j++)
	if (m.elem (0, j) != 0)
	  {
	    result (0, count) = j + 1;
	    count++;
	  }
      return tree_constant (result);
    }
  else
    {
      ColumnVector v (count);
      count = 0;
      for (j = 0; j < m_nc; j++)
	for (i = 0; i < m_nr; i++)
	  if (m.elem (i, j) != 0)
	    {
	      v.elem (count) = m_nr * j + i + 1;
	      count++;
	    }
      return tree_constant (v, 1);  // Always make a column vector.
    }
}

static tree_constant
find_nonzero_elem_idx (const ComplexMatrix& m)
{
  int count = 0;
  int m_nr = m.rows ();
  int m_nc = m.columns ();

  for (int j = 0; j < m_nc; j++)
    {
      for (int i = 0; i < m_nr; i++)
	if (m.elem (i, j) != 0)
	  count++;
    }

  Matrix result;

  if (count == 0)
    return result;

  if (m_nr == 1)
    {
      result.resize (1, count);
      count = 0;
      for (j = 0; j < m_nc; j++)
	if (m.elem (0, j) != 0)
	  {
	    result (0, count) = j + 1;
	    count++;
	  }
      return tree_constant (result);
    }
  else
    {
      ColumnVector v (count);
      count = 0;
      for (j = 0; j < m_nc; j++)
	{
	  for (int i = 0; i < m_nr; i++)
	    if (m.elem (i, j) != 0)
	      {
		v.elem (count) = m_nr * j + i + 1;
		count++;
	      }
	}
      return tree_constant (v, 1);  // Always make a column vector.
    }
}

tree_constant
find_nonzero_elem_idx (const tree_constant& a)
{
  tree_constant retval;

  tree_constant tmp = a.make_numeric ();

  Matrix result;
    
  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();
	return find_nonzero_elem_idx (m);
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	if (d != 0.0)
	  return tree_constant (1.0);
	else
	  return tree_constant (result);
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();
	return find_nonzero_elem_idx (m);
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	if (c != 0.0)
	  return tree_constant (1.0);
	else
	  return tree_constant (result);
      }
      break;
    default:
      break;
    }
  return retval;
}

// XXX FIXME XXX -- the next two functions (and expm) should really be just
// one...

tree_constant *
matrix_log (const tree_constant& a)
{
  tree_constant *retval = new tree_constant [2];

  tree_constant tmp = a.make_numeric ();;
    
  if (tmp.rows () == 0 || tmp.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("logm", 0);
	  Matrix m;
	  retval = new tree_constant [2];
	  retval[0] = tree_constant (m);
	  return retval;
	}
      else
	gripe_empty_arg ("logm", 1);
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("logm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = log (real (elt));
		else
		  lambda.elem (i) = log (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval[0] = tree_constant (result);
	  }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("logm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = log (real (elt));
		else
		  lambda.elem (i) = log (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval[0] = tree_constant (result);
	  }
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	if (d > 0.0)
	  retval[0] = tree_constant (log (d));
	else
	  {
	    Complex dtmp (d);
	    retval[0] = tree_constant (log (dtmp));
	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval[0] = tree_constant (log (c));
      }
      break;
    default:
      break;
    }
  return retval;
}

tree_constant *
matrix_sqrt (const tree_constant& a)
{
  tree_constant *retval = new tree_constant [2];

  tree_constant tmp = a.make_numeric ();;
    
  if (tmp.rows () == 0 || tmp.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("sqrtm", 0);
	  Matrix m;
	  retval = new tree_constant [2];
	  retval[0] = tree_constant (m);
	  return retval;
	}
      else
	gripe_empty_arg ("sqrtm", 1);
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("sqrtm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = sqrt (real (elt));
		else
		  lambda.elem (i) = sqrt (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval[0] = tree_constant (result);
	  }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();

	int nr = m.rows ();
	int nc = m.columns ();

	if (nr == 0 || nc == 0 || nr != nc)
	  gripe_square_matrix_required ("sqrtm");
	else
	  {
	    EIG m_eig (m);
	    ComplexColumnVector lambda (m_eig.eigenvalues ());
	    ComplexMatrix Q (m_eig.eigenvectors ());

	    for (int i = 0; i < nr; i++)
	      {
		Complex elt = lambda.elem (i);
		if (imag (elt) == 0.0 && real (elt) > 0.0)
		  lambda.elem (i) = sqrt (real (elt));
		else
		  lambda.elem (i) = sqrt (elt);
	      }

	    ComplexDiagMatrix D (lambda);
	    ComplexMatrix result = Q * D * Q.inverse ();

	    retval[0] = tree_constant (result);
	  }
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	if (d > 0.0)
	  retval[0] = tree_constant (sqrt (d));
	else
	  {
	    Complex dtmp (d);
	    retval[0] = tree_constant (sqrt (dtmp));
	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval[0] = tree_constant (log (c));
      }
      break;
    default:
      break;
    }
  return retval;
}

tree_constant *
column_max (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  tree_constant arg1;
  tree_constant arg2;
  tree_constant_rep::constant_type arg1_type =
    tree_constant_rep::unknown_constant;
  tree_constant_rep::constant_type arg2_type =
    tree_constant_rep::unknown_constant;

  switch (nargin)
    {
    case 3:
      arg2 = args[2].make_numeric ();
      arg2_type = arg2.const_type ();
// Fall through...
    case 2:
      arg1 = args[1].make_numeric ();
      arg1_type = arg1.const_type ();
      break;
    default:
      panic_impossible ();
      break;
    }

  if (nargin == 2 && nargout == 1)
    {
      retval = new tree_constant [2];
      switch (arg1_type)
	{
        case tree_constant_rep::scalar_constant:
	  retval[0] = tree_constant (arg1.double_value ());
          break;
        case tree_constant_rep::complex_scalar_constant:
          retval[0] = tree_constant (arg1.complex_value ());
          break;
        case tree_constant_rep::matrix_constant:
          {
  	    Matrix m = arg1.matrix_value ();
	    if (m.rows () == 1)
	      retval[0] = tree_constant (m.row_max ());
	    else
	      retval[0] = tree_constant (m.column_max (), 0);
 	  }
          break;
        case tree_constant_rep::complex_matrix_constant:
          {
            ComplexMatrix m = arg1.complex_matrix_value ();
            if (m.rows () == 1)
              retval[0] = tree_constant (m.row_max ());
            else
              retval[0] = tree_constant (m.column_max (), 0);
          }
	  break;
	default:
	  panic_impossible ();
	  break;
	}
    }
  else if (nargin == 2 && nargout == 2)
    {
      retval = new tree_constant [2];
      switch (arg1_type)
        {
	case tree_constant_rep::scalar_constant:
	  {
	    retval[0] = tree_constant (arg1.double_value ());
	    retval[1] = tree_constant (1);
	  }
          break;
	case tree_constant_rep::complex_scalar_constant:
	  {
	    retval[0] = tree_constant (arg1.complex_value ());
	    retval[1] = tree_constant (1);
	  }
          break;
	case tree_constant_rep::matrix_constant:
	  {
	    Matrix m = arg1.matrix_value ();
	    if (m.rows () == 1)
	      {
		retval[0] = tree_constant (m.row_max ());
		retval[1] = tree_constant (m.row_max_loc ());
	      }
	    else
	      {
		retval[0] = tree_constant (m.column_max (), 0);
		retval[1] = tree_constant (m.column_max_loc (), 0);
	      }
	  }
          break;
	case tree_constant_rep::complex_matrix_constant:
	  {
	    ComplexMatrix m = arg1.complex_matrix_value ();
	    if (m.rows () == 1)
	      {
		retval[0] = tree_constant (m.row_max ());
		retval[1] = tree_constant (m.row_max_loc ());
	      }
	    else
	      {
		retval[0] = tree_constant (m.column_max (), 0);
		retval[1] = tree_constant (m.column_max_loc (), 0);
	      }
	  }
          break;
	default:
	  panic_impossible ();
	  break;
        }
    }
  else if (nargin == 3)
    {
      if (arg1.rows () == arg2.rows ()
	  && arg1.columns () == arg2.columns ())
	{
	  retval = new tree_constant [2];
          switch (arg1_type)
            {
	    case tree_constant_rep::scalar_constant:
	      {
		double result;
		double a_elem = arg1.double_value ();
		double b_elem = arg2.double_value ();
		result = MIN (a_elem, b_elem);
		retval[0] = tree_constant (result);
	      }
              break;
	    case tree_constant_rep::complex_scalar_constant:
	      {
		Complex result;
		Complex a_elem = arg1.complex_value ();
		Complex b_elem = arg2.complex_value ();
		if (abs(a_elem) < abs(b_elem))
		  result = a_elem;
		else
		  result = b_elem;
		retval[0] = tree_constant (result);
	      }
              break;
	    case tree_constant_rep::matrix_constant:
	      {
		Matrix result;
		result = max (arg1.matrix_value (), arg2.matrix_value ());
		retval[0] = tree_constant (result);
	      }
              break;
	    case tree_constant_rep::complex_matrix_constant:
	      {
		ComplexMatrix result;
		result = max (arg1.complex_matrix_value (),
			      arg2.complex_matrix_value ());
		retval[0] = tree_constant (result);
	      }
	      break;
	    default:
	      panic_impossible ();
	      break;
	    }
	}
      else
	error ("max: nonconformant matrices");
    }
  else
    panic_impossible ();

  return retval;
}

tree_constant *
column_min (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  tree_constant arg1;
  tree_constant arg2;
  tree_constant_rep::constant_type arg1_type =
    tree_constant_rep::unknown_constant;
  tree_constant_rep::constant_type arg2_type =
    tree_constant_rep::unknown_constant;

  switch (nargin)
    {
    case 3:
      arg2 = args[2].make_numeric ();
      arg2_type = arg2.const_type ();
// Fall through...
    case 2:
      arg1 = args[1].make_numeric ();
      arg1_type = arg1.const_type ();
      break;
    default:
      panic_impossible ();
      break;
    }

  if (nargin == 2 && nargout == 1)
    {
      retval = new tree_constant [2];
      switch (arg1_type)
	{
        case tree_constant_rep::scalar_constant:
	  retval[0] = tree_constant (arg1.double_value ());
          break;
        case tree_constant_rep::complex_scalar_constant:
          retval[0] = tree_constant (arg1.complex_value ());
          break;
        case tree_constant_rep::matrix_constant:
          {
  	    Matrix m = arg1.matrix_value ();
	    if (m.rows () == 1)
	      retval[0] = tree_constant (m.row_min ());
	    else
	      retval[0] = tree_constant (m.column_min (), 0);
 	  }
          break;
        case tree_constant_rep::complex_matrix_constant:
          {
            ComplexMatrix m = arg1.complex_matrix_value ();
            if (m.rows () == 1)
              retval[0] = tree_constant (m.row_min ());
            else
              retval[0] = tree_constant (m.column_min (), 0);
          }
	  break;
	default:
	  panic_impossible ();
	  break;
	}
    }
  else if (nargin == 2 && nargout == 2)
    {
      retval = new tree_constant [2];
      switch (arg1_type)
        {
	case tree_constant_rep::scalar_constant:
	  {
	    retval[0] = tree_constant (arg1.double_value ());
	    retval[1] = tree_constant (1);
	  }
          break;
	case tree_constant_rep::complex_scalar_constant:
	  {
	    retval[0] = tree_constant (arg1.complex_value ());
	    retval[1] = tree_constant (1);
	  }
          break;
	case tree_constant_rep::matrix_constant:
	  {
	    Matrix m = arg1.matrix_value ();
	    if (m.rows () == 1)
	      {
		retval[0] = tree_constant (m.row_min ());
		retval[1] = tree_constant (m.row_min_loc ());
	      }
	    else
	      {
		retval[0] = tree_constant (m.column_min (), 0);
		retval[1] = tree_constant (m.column_min_loc (), 0);
	      }
	  }
          break;
	case tree_constant_rep::complex_matrix_constant:
	  {
	    ComplexMatrix m = arg1.complex_matrix_value ();
	    if (m.rows () == 1)
	      {
		retval[0] = tree_constant (m.row_min ());
		retval[1] = tree_constant (m.row_min_loc ());
	      }
	    else
	      {
		retval[0] = tree_constant (m.column_min (), 0);
		retval[1] = tree_constant (m.column_min_loc (), 0);
	      }
	  }
          break;
	default:
	  panic_impossible ();
	  break;
        }
    }
  else if (nargin == 3)
    {
      if (arg1.rows () == arg2.rows ()
	  && arg1.columns () == arg2.columns ())
	{
	  retval = new tree_constant [2];
          switch (arg1_type)
            {
	    case tree_constant_rep::scalar_constant:
	      {
		double result;
		double a_elem = arg1.double_value ();
		double b_elem = arg2.double_value ();
		result = MIN (a_elem, b_elem);
		retval[0] = tree_constant (result);
	      }
              break;
	    case tree_constant_rep::complex_scalar_constant:
	      {
		Complex result;
		Complex a_elem = arg1.complex_value ();
		Complex b_elem = arg2.complex_value ();
		if (abs(a_elem) < abs(b_elem))
		  result = a_elem;
		else
		  result = b_elem;
		retval[0] = tree_constant (result);
	      }
              break;
	    case tree_constant_rep::matrix_constant:
	      {
		Matrix result;
		result = min (arg1.matrix_value (), arg2.matrix_value ());
		retval[0] = tree_constant (result);
	      }
              break;
	    case tree_constant_rep::complex_matrix_constant:
	      {
		ComplexMatrix result;
		result = min (arg1.complex_matrix_value (),
			      arg2.complex_matrix_value ());
		retval[0] = tree_constant (result);
	      }
	      break;
	    default:
	      panic_impossible ();
	      break;
	    }
	}
      else
	error ("min: nonconformant matrices");
    }
  else
    panic_impossible ();

  return retval;
}

static void
mx_sort (Matrix& m, Matrix& idx, int return_idx)
{
  int nr = m.rows ();
  int nc = m.columns ();
  idx.resize (nr, nc);
  int i, j;

  if (return_idx)
    {
      for (j = 0; j < nc; j++)
	for (i = 0; i < nr; i++)
	  idx.elem (i, j) = i+1;
    }

  for (j = 0; j < nc; j++)
    {
      for (int gap = nr/2; gap > 0; gap /= 2)
	for (i = gap; i < nr; i++)
	  for (int k = i - gap;
	       k >= 0 && m.elem (k, j) > m.elem (k+gap, j);
	       k -= gap)
	    {
	      double tmp = m.elem (k, j);
	      m.elem (k, j) = m.elem (k+gap, j);
	      m.elem (k+gap, j) = tmp;

	      if (return_idx)
		{
		  double tmp = idx.elem (k, j);
		  idx.elem (k, j) = idx.elem (k+gap, j);
		  idx.elem (k+gap, j) = tmp;
		}
	    }
    }
}

static void
mx_sort (RowVector& v, RowVector& idx, int return_idx)
{
  int n = v.capacity ();
  idx.resize (n);
  int i;

  if (return_idx)
    for (i = 0; i < n; i++)
      idx.elem (i) = i+1;

  for (int gap = n/2; gap > 0; gap /= 2)
    for (i = gap; i < n; i++)
      for (int k = i - gap;
	   k >= 0 && v.elem (k) > v.elem (k+gap);
	   k -= gap)
	{
	  double tmp = v.elem (k);
	  v.elem (k) = v.elem (k+gap);
	  v.elem (k+gap) = tmp;

	  if (return_idx)
	    {
	      double tmp = idx.elem (k);
	      idx.elem (k) = idx.elem (k+gap);
	      idx.elem (k+gap) = tmp;
	    }
	}
}

static void
mx_sort (ComplexMatrix& cm, Matrix& idx, int return_idx)
{
  int nr = cm.rows ();
  int nc = cm.columns ();
  idx.resize (nr, nc);
  int i, j;

  if (return_idx)
    {
      for (j = 0; j < nc; j++)
	for (i = 0; i < nr; i++)
	  idx.elem (i, j) = i+1;
    }

  for (j = 0; j < nc; j++)
    {
      for (int gap = nr/2; gap > 0; gap /= 2)
	for (i = gap; i < nr; i++)
	  for (int k = i - gap;
	       k >= 0 && abs (cm.elem (k, j)) > abs (cm.elem (k+gap, j));
	       k -= gap)
	    {
	      Complex ctmp = cm.elem (k, j);
	      cm.elem (k, j) = cm.elem (k+gap, j);
	      cm.elem (k+gap, j) = ctmp;

	      if (return_idx)
		{
		  double tmp = idx.elem (k, j);
		  idx.elem (k, j) = idx.elem (k+gap, j);
		  idx.elem (k+gap, j) = tmp;
		}
	    }
    }
}

static void
mx_sort (ComplexRowVector& cv, RowVector& idx, int return_idx)
{
  int n = cv.capacity ();
  idx.resize (n);
  int i;

  if (return_idx)
    for (i = 0; i < n; i++)
      idx.elem (i) = i+1;

  for (int gap = n/2; gap > 0; gap /= 2)
    for (i = gap; i < n; i++)
      for (int k = i - gap;
	   k >= 0 && abs (cv.elem (k)) > abs (cv.elem (k+gap));
	   k -= gap)
	{
	  Complex tmp = cv.elem (k);
	  cv.elem (k) = cv.elem (k+gap);
	  cv.elem (k+gap) = tmp;

	  if (return_idx)
	    {
	      double tmp = idx.elem (k);
	      idx.elem (k) = idx.elem (k+gap);
	      idx.elem (k+gap) = tmp;
	    }
	}
}

tree_constant *
sort (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  int return_idx = nargout > 1;
  if (return_idx)
    retval = new tree_constant [3];
  else
    retval = new tree_constant [2];

  switch (args[1].const_type ())
    {
    case tree_constant_rep::scalar_constant:
      {
	retval [0] = tree_constant (args[1].double_value ());
	if (return_idx)
	  retval [1] = tree_constant (1.0);
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	retval [0] = tree_constant (args[1].complex_value ());
	if (return_idx)
	  retval [1] = tree_constant (1.0);
      }
      break;
    case tree_constant_rep::string_constant:
    case tree_constant_rep::range_constant:
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = args[1].to_matrix ();
	if (m.rows () == 1)
	  {
	    int nc = m.columns ();
	    RowVector v (nc);
	    for (int i = 0; i < nc; i++)
	      v.elem (i) = m.elem (0, i);
	    RowVector idx;
	    mx_sort (v, idx, return_idx);

	    retval [0] = tree_constant (v, 0);
	    if (return_idx)
	      retval [1] = tree_constant (idx, 0);
	  }
	else
	  {
// Sorts m in place, optionally computes index Matrix.
	    Matrix idx;
	    mx_sort (m, idx, return_idx);

	    retval [0] = tree_constant (m);
	    if (return_idx)
	      retval [1] = tree_constant (idx);
	  }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix cm = args[1].complex_matrix_value ();
	if (cm.rows () == 1)
	  {
	    int nc = cm.columns ();
	    ComplexRowVector cv (nc);
	    for (int i = 0; i < nc; i++)
	      cv.elem (i) = cm.elem (0, i);
	    RowVector idx;
	    mx_sort (cv, idx, return_idx);

	    retval [0] = tree_constant (cv, 0);
	    if (return_idx)
	      retval [1] = tree_constant (idx, 0);
	  }
	else
	  {
// Sorts cm in place, optionally computes index Matrix.
	    Matrix idx;
	    mx_sort (cm, idx, return_idx);

	    retval [0] = tree_constant (cm);
	    if (return_idx)
	      retval [1] = tree_constant (idx);
	  }
      }
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant *
feval (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  tree *fcn = is_valid_function (args[1], "feval", 1);
  if (fcn != NULL_TREE)
    {
      args++;
      nargin--;
      if (nargin > 1)
	retval = fcn->eval (args, nargin, nargout, 0);
      else
	retval = fcn->eval (0, nargout);
    }

  return retval;
}

tree_constant
eval_string (const char *string, int print, int ans_assign,
	     int& parse_status)
{
  begin_unwind_frame ("eval_string");

  unwind_protect_int (get_input_from_eval_string);
  unwind_protect_ptr (global_command);
  unwind_protect_ptr (current_eval_string);

  get_input_from_eval_string = 1;
  current_eval_string = string;

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer ((FILE *) NULL);

  add_unwind_protect (restore_input_buffer, (void *) old_buf);
  add_unwind_protect (delete_input_buffer, (void *) new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_ptr (curr_sym_tab);
  symbol_table *prev_sym_tab = curr_sym_tab;

  parse_status = yyparse ();

  curr_sym_tab = prev_sym_tab;

// Important to reset the idea of where input is coming from before
// trying to eval the command we just parsed -- it might contain the
// name of an m-file that still needs to be parsed!

  tree *command = global_command;

  run_unwind_frame ("eval_string");

  tree_constant retval;

  if (parse_status == 0 && command != NULL_TREE)
    {
      retval = command->eval (print);
      delete command;
    }

  return retval;
}

tree_constant
eval_string (const tree_constant& arg, int& parse_status)
{
  if (! arg.is_string_type ())
    {
      error ("eval: expecting string argument");
      return -1;
    }

  char *string = arg.string_value ();

// Yes Virginia, we always print here...

  return eval_string (string, 1, 1, parse_status);
}

static int
match_sans_spaces (const char *standard, const char *test)
{
  const char *tp = test;
  while (*tp == ' ' || *tp == '\t')
    tp++;

  const char *ep = test + strlen (test) - 1;
  while (*ep == ' ' || *ep == '\t')
    ep--;

  int len = ep - tp + 1;

  return (strncmp (standard, tp, len) == 0);
}

tree_constant
get_user_input (const tree_constant *args, int nargin, int nargout,
		int debug = 0)
{
  tree_constant retval;

  int read_as_string = 0;
  if (nargin == 3)
    {
      if (args[2].is_string_type ()
	  && strcmp ("s", args[2].string_value ()) == 0)
	read_as_string++;
      else
	{
	  error ("input: unrecognized second argument");
	  return retval;
	}
    }

  char *prompt = "debug> ";
  if (nargin > 1)
   {
      if (args[1].is_string_type ())
	prompt = args[1].string_value ();
      else
	{
	  error ("input: unrecognized argument");
	  return retval;
	}
    }

 again:

  flush_output_to_pager ();

  char *input_buf = gnu_readline (prompt);

  if (input_buf != (char *) NULL)
    {
      if (input_buf)
	maybe_save_history (input_buf);

      int len = strlen (input_buf);

      if (len < 1)
	{
	  if (debug)
	    goto again;
	  else
	    return retval;
	}

      if (match_sans_spaces ("exit", input_buf)
	  || match_sans_spaces ("quit", input_buf)
	  || match_sans_spaces ("return", input_buf))
	return tree_constant ();
      else if (read_as_string)
	retval = tree_constant (input_buf);
      else
	{
	  int parse_status;
	  retval = eval_string (input_buf, 0, 0, parse_status);
	  if (debug && retval.is_defined ())
	    retval.eval (1);
	}
    }
  else
    error ("input: reading user-input failed!");

  if (debug)
    goto again;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
