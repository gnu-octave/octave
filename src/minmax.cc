// f-minmax.cc						 -*- C++ -*-
/*

Copyright (C) 1994 John W. Eaton

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

#include <math.h>

#include "tree-const.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "defun-dld.h"

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

// This file could probably be condensed quite a bit by an appropriate
// amount of C preprocessor abuse.

static Matrix
min (double d, const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double m_elem = m.elem (i, j);
	result.elem (i, j) = MIN (d, m_elem);
      }

  return result;
}

static Matrix
min (const Matrix& m, double d)
{
  int nr = m.rows ();
  int nc = m.columns ();

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double m_elem = m.elem (i, j);
	result.elem (i, j) = MIN (m_elem, d);
      }

  return result;
}

static ComplexMatrix
min (const Complex& c, const ComplexMatrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  ComplexMatrix result (nr, nc);

  double abs_c = abs (c);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double abs_m_elem = abs (m.elem (i, j));
	if (abs_c < abs_m_elem)
	  result.elem (i, j) = c;
	else
	  result.elem (i, j) = m.elem (i, j);
      }

  return result;
}

static ComplexMatrix
min (const ComplexMatrix& m, const Complex& c)
{
  int nr = m.rows ();
  int nc = m.columns ();

  ComplexMatrix result (nr, nc);

  double abs_c = abs (c);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double abs_m_elem = abs (m.elem (i, j));
	if (abs_m_elem < abs_c)
	  result.elem (i, j) = m.elem (i, j);
	else
	  result.elem (i, j) = c;
      }

  return result;
}

static Matrix
min (const Matrix& a, const Matrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    {
      error ("two-arg min expecting args of same size");
      return Matrix ();
    }

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

static ComplexMatrix
min (const ComplexMatrix& a, const ComplexMatrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    {
      error ("two-arg min expecting args of same size");
      return ComplexMatrix ();
    }

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

static Matrix
max (double d, const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double m_elem = m.elem (i, j);
	result.elem (i, j) = MAX (d, m_elem);
      }

  return result;
}

static Matrix
max (const Matrix& m, double d)
{
  int nr = m.rows ();
  int nc = m.columns ();

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double m_elem = m.elem (i, j);
	result.elem (i, j) = MAX (m_elem, d);
      }

  return result;
}

static ComplexMatrix
max (const Complex& c, const ComplexMatrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  ComplexMatrix result (nr, nc);

  double abs_c = abs (c);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double abs_m_elem = abs (m.elem (i, j));
	if (abs_c > abs_m_elem)
	  result.elem (i, j) = c;
	else
	  result.elem (i, j) = m.elem (i, j);
      }

  return result;
}

static ComplexMatrix
max (const ComplexMatrix& m, const Complex& c)
{
  int nr = m.rows ();
  int nc = m.columns ();

  ComplexMatrix result (nr, nc);

  double abs_c = abs (c);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double abs_m_elem = abs (m.elem (i, j));
	if (abs_m_elem > abs_c)
	  result.elem (i, j) = m.elem (i, j);
	else
	  result.elem (i, j) = c;
      }

  return result;
}

static Matrix
max (const Matrix& a, const Matrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    {
      error ("two-arg max expecting args of same size");
      return Matrix ();
    }

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

static ComplexMatrix
max (const ComplexMatrix& a, const ComplexMatrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();
  if (nr != b.rows () || nc != b.columns ())
    {
      error ("two-arg max expecting args of same size");
      return ComplexMatrix ();
    }

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

DEFUN_DLD_BUILTIN ("min", Fmin, Smin, 3, 2,
  "min (X): minimum value(s) of a vector (matrix)")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ("min");
      return retval;
    }

  tree_constant arg1;
  tree_constant arg2;

  switch (nargin)
    {
    case 2:
      arg2 = args(1);
// Fall through...

    case 1:
      arg1 = args(0);
      break;

    default:
      panic_impossible ();
      break;
    }

  if (nargin == 1 && (nargout == 1 || nargout == 0))
    {
      if (arg1.is_real_scalar ())
	{
	  retval(0) = arg1.double_value ();
	}
      else if (arg1.is_complex_scalar ())
	{
	  retval(0) = arg1.complex_value ();
	}
      else if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		retval(0) = m.row_min ();
	      else
		retval(0) = tree_constant (m.column_min (), 0);
	    }
	}
      else if (arg1.is_complex_type ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		retval(0) = m.row_min ();
	      else
		retval(0) = tree_constant (m.column_min (), 0);
	    }
	}
      else
	{
	  gripe_wrong_type_arg ("min", arg1);
	  return retval;
	}
    }
  else if (nargin == 1 && nargout == 2)
    {
      if (arg1.is_real_scalar ())
	{
	  retval(1) = 1;
	  retval(0) = arg1.double_value ();
	}
      else if (arg1.is_complex_scalar ())
	{
	  retval(1) = 1;
	  retval(0) = arg1.complex_value ();
	}
      else if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		{
		  retval(1) = m.row_min_loc ();
		  retval(0) = m.row_min ();
		}
	      else
		{
		  retval(1) = tree_constant (m.column_min_loc (), 0);
		  retval(0) = tree_constant (m.column_min (), 0);
		}
	    }
	}
      else if (arg1.is_complex_type ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		{
		  retval(1) = m.row_min_loc ();
		  retval(0) = m.row_min ();
		}
	      else
		{
		  retval(1) = tree_constant (m.column_min_loc (), 0);
		  retval(0) = tree_constant (m.column_min (), 0);
		}
	    }
	}
      else
	{
	  gripe_wrong_type_arg ("min", arg1);
	  return retval;
	}
    }
  else if (nargin == 2)
    {
      int arg1_is_scalar = arg1.is_scalar_type ();
      int arg2_is_scalar = arg2.is_scalar_type ();

      int arg1_is_complex = arg1.is_complex_type ();
      int arg2_is_complex = arg2.is_complex_type ();

      if (arg1_is_scalar)
	{
	  if (arg1_is_complex || arg2_is_complex)
	    {
	      Complex c1 = arg1.complex_value ();
	      ComplexMatrix m2 = arg2.complex_matrix_value ();
	      if (! error_state)
		{
		  ComplexMatrix result = min (c1, m2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	  else
	    {
	      double d1 = arg1.double_value ();
	      Matrix m2 = arg2.matrix_value ();

	      if (! error_state)
		{
		  Matrix result = min (d1, m2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	}
      else if (arg2_is_scalar)
	{
	  if (arg1_is_complex || arg2_is_complex)
	    {
	      ComplexMatrix m1 = arg1.complex_matrix_value ();

	      if (! error_state)
		{
		  Complex c2 = arg2.complex_value ();
		  ComplexMatrix result = min (m1, c2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	  else
	    {
	      Matrix m1 = arg1.matrix_value ();

	      if (! error_state)
		{
		  double d2 = arg2.double_value ();
		  Matrix result = min (m1, d2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	}
      else
	{
	  if (arg1_is_complex || arg2_is_complex)
	    {
	      ComplexMatrix m1 = arg1.complex_matrix_value ();

	      if (! error_state)
		{
		  ComplexMatrix m2 = arg2.complex_matrix_value ();

		  if (! error_state)
		    {
		      ComplexMatrix result = min (m1, m2);
		      if (! error_state)
			retval(0) = result;
		    }
		}
	    }
	  else
	    {
	      Matrix m1 = arg1.matrix_value ();

	      if (! error_state)
		{
		  Matrix m2 = arg2.matrix_value ();

		  if (! error_state)
		    {
		      Matrix result = min (m1, m2);
		      if (! error_state)
			retval(0) = result;
		    }
		}
	    }
	}
    }
  else
    panic_impossible ();

  return retval;
}

DEFUN_DLD_BUILTIN ("max", Fmax, Smax, 3, 2,
  "max (X): maximum value(s) of a vector (matrix)")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ("max");
      return retval;
    }

  tree_constant arg1;
  tree_constant arg2;

  switch (nargin)
    {
    case 2:
      arg2 = args(1);
// Fall through...

    case 1:
      arg1 = args(0);
      break;

    default:
      panic_impossible ();
      break;
    }

  if (nargin == 1 && (nargout == 1 || nargout == 0))
    {
      if (arg1.is_real_scalar ())
	{
	  retval(0) = arg1.double_value ();
	}
      else if (arg1.is_complex_scalar ())
	{
	  retval(0) = arg1.complex_value ();
	}
      else if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		retval(0) = m.row_max ();
	      else
		retval(0) = tree_constant (m.column_max (), 0);
	    }
	}
      else if (arg1.is_complex_type ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		retval(0) = m.row_max ();
	      else
		retval(0) = tree_constant (m.column_max (), 0);
	    }
	}
      else
	{
	  gripe_wrong_type_arg ("max", arg1);
	  return retval;
	}
    }
  else if (nargin == 1 && nargout == 2)
    {
      if (arg1.is_real_scalar ())
	{
	  retval(1) = 1;
	  retval(0) = arg1.double_value ();
	}
      else if (arg1.is_complex_scalar ())
	{
	  retval(1) = 1;
	  retval(0) = arg1.complex_value ();
	}
      else if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		{
		  retval(1) = m.row_max_loc ();
		  retval(0) = m.row_max ();
		}
	      else
		{
		  retval(1) = tree_constant (m.column_max_loc (), 0);
		  retval(0) = tree_constant (m.column_max (), 0);
		}
	    }
	}
      else if (arg1.is_complex_type ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		{
		  retval(1) = m.row_max_loc ();
		  retval(0) = m.row_max ();
		}
	      else
		{
		  retval(1) = tree_constant (m.column_max_loc (), 0);
		  retval(0) = tree_constant (m.column_max (), 0);
		}
	    }
	}
      else
	{
	  gripe_wrong_type_arg ("max", arg1);
	  return retval;
	}
    }
  else if (nargin == 2)
    {
      int arg1_is_scalar = arg1.is_scalar_type ();
      int arg2_is_scalar = arg2.is_scalar_type ();

      int arg1_is_complex = arg1.is_complex_type ();
      int arg2_is_complex = arg2.is_complex_type ();

      if (arg1_is_scalar)
	{
	  if (arg1_is_complex || arg2_is_complex)
	    {
	      Complex c1 = arg1.complex_value ();
	      ComplexMatrix m2 = arg2.complex_matrix_value ();
	      if (! error_state)
		{
		  ComplexMatrix result = max (c1, m2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	  else
	    {
	      double d1 = arg1.double_value ();
	      Matrix m2 = arg2.matrix_value ();

	      if (! error_state)
		{
		  Matrix result = max (d1, m2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	}
      else if (arg2_is_scalar)
	{
	  if (arg1_is_complex || arg2_is_complex)
	    {
	      ComplexMatrix m1 = arg1.complex_matrix_value ();

	      if (! error_state)
		{
		  Complex c2 = arg2.complex_value ();
		  ComplexMatrix result = max (m1, c2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	  else
	    {
	      Matrix m1 = arg1.matrix_value ();

	      if (! error_state)
		{
		  double d2 = arg2.double_value ();
		  Matrix result = max (m1, d2);
		  if (! error_state)
		    retval(0) = result;
		}
	    }
	}
      else
	{
	  if (arg1_is_complex || arg2_is_complex)
	    {
	      ComplexMatrix m1 = arg1.complex_matrix_value ();

	      if (! error_state)
		{
		  ComplexMatrix m2 = arg2.complex_matrix_value ();

		  if (! error_state)
		    {
		      ComplexMatrix result = max (m1, m2);
		      if (! error_state)
			retval(0) = result;
		    }
		}
	    }
	  else
	    {
	      Matrix m1 = arg1.matrix_value ();

	      if (! error_state)
		{
		  Matrix m2 = arg2.matrix_value ();

		  if (! error_state)
		    {
		      Matrix result = max (m1, m2);
		      if (! error_state)
			retval(0) = result;
		    }
		}
	    }
	}
    }
  else
    panic_impossible ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
