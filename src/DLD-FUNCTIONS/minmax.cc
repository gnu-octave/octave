/*

Copyright (C) 1996, 1997 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "lo-ieee.h"
#include "oct-math.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

// XXX FIXME XXX -- it would be nice to share code among the min/max
// functions below.

static Matrix
min (double d, const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double m_elem = m (i, j);
	result (i, j) = MIN (d, m_elem);
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
	double m_elem = m (i, j);
	result (i, j) = MIN (m_elem, d);
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
    {
      for (int i = 0; i < nr; i++)
	{
	  double abs_m_elem = abs (m (i, j));
	  if (abs_c < abs_m_elem)
	    result (i, j) = c;
	  else
	    result (i, j) = m (i, j);
	}
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
	double abs_m_elem = abs (m (i, j));
	if (abs_m_elem < abs_c)
	  result (i, j) = m (i, j);
	else
	  result (i, j) = c;
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
	double a_elem = a (i, j);
	double b_elem = b (i, j);
	result (i, j) = MIN (a_elem, b_elem);
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
    {
      int columns_are_real_only = 1;
      for (int i = 0; i < nr; i++)
	if (imag (a (i, j)) != 0.0 && imag (b (i, j)) != 0.0)
	  {
	    columns_are_real_only = 0;
	    break;
	  }

      if (columns_are_real_only)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      double a_elem = real (a (i, j));
	      double b_elem = real (b (i, j));
	      if (a_elem < b_elem)
		result (i, j) = a_elem;
	      else
		result (i, j) = b_elem;
	    }
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    {
	      double abs_a_elem = abs (a (i, j));
	      double abs_b_elem = abs (b (i, j));
	      if (abs_a_elem < abs_b_elem)
		result (i, j) = a (i, j);
	      else
		result (i, j) = b (i, j);
	    }
	}
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
	double m_elem = m (i, j);
	result (i, j) = MAX (d, m_elem);
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
	double m_elem = m (i, j);
	result (i, j) = MAX (m_elem, d);
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
	double abs_m_elem = abs (m (i, j));
	if (abs_c > abs_m_elem)
	  result (i, j) = c;
	else
	  result (i, j) = m (i, j);
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
	double abs_m_elem = abs (m (i, j));
	if (abs_m_elem > abs_c)
	  result (i, j) = m (i, j);
	else
	  result (i, j) = c;
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
	double a_elem = a (i, j);
	double b_elem = b (i, j);
	result (i, j) = MAX (a_elem, b_elem);
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
    {
      int columns_are_real_only = 1;
      for (int i = 0; i < nr; i++)
	if (imag (a (i, j)) != 0.0 && imag (b (i, j)) != 0.0)
	  {
	    columns_are_real_only = 0;
	    break;
	  }

      if (columns_are_real_only)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      double a_elem = real (a (i, j));
	      double b_elem = real (b (i, j));
	      if (a_elem > b_elem)
		result (i, j) = a_elem;
	      else
		result (i, j) = b_elem;
	    }
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    {
	      double abs_a_elem = abs (a (i, j));
	      double abs_b_elem = abs (b (i, j));
	      if (abs_a_elem > abs_b_elem)
		result (i, j) = a (i, j);
	      else
		result (i, j) = b (i, j);
	    }
	}
    }

  return result;
}

DEFUN_DLD (min, args, nargout,
  "min (X): minimum value(s) of a vector (matrix)")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ("min");
      return retval;
    }

  octave_value arg1;
  octave_value arg2;

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
      if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		retval(0) = m.row_min ();
	      else
		retval(0) = octave_value (m.column_min (), 0);
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
		retval(0) = octave_value (m.column_min (), 0);
	    }
	}
      else
	gripe_wrong_type_arg ("min", arg1);
    }
  else if (nargin == 1 && nargout == 2)
    {
      Array<int> index;

      if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      retval.resize (2);

	      if (m.rows () == 1)
		retval(0) = m.row_min (index);
	      else
		retval(0) = octave_value (m.column_min (index), 0);
	    }
	}
      else if (arg1.is_complex_type ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();

	  if (! error_state)
	    {
	      retval.resize (2);

	      if (m.rows () == 1)
		retval(0) = m.row_min (index);
	      else
		retval(0) = octave_value (m.column_min (index), 0);
	    }
	}
      else
	gripe_wrong_type_arg ("min", arg1);

      int len = index.length ();

      if (len > 0)
	{
	  RowVector idx (len);

	  for (int i = 0; i < len; i++)
	    {
	      int tmp = index.elem (i) + 1;
	      idx.elem (i) = (tmp <= 0)
		? octave_NaN : static_cast<double> (tmp);
	    }

	  retval(1) = octave_value (idx, 0);
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

DEFUN_DLD (max, args, nargout,
  "max (X): maximum value(s) of a vector (matrix)")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ("max");
      return retval;
    }

  octave_value arg1;
  octave_value arg2;

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
      if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      if (m.rows () == 1)
		retval(0) = m.row_max ();
	      else
		retval(0) = octave_value (m.column_max (), 0);
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
		retval(0) = octave_value (m.column_max (), 0);
	    }
	}
      else
	gripe_wrong_type_arg ("max", arg1);
    }
  else if (nargin == 1 && nargout == 2)
    {
      Array<int> index;

      if (arg1.is_real_type ())
	{
	  Matrix m = arg1.matrix_value ();

	  if (! error_state)
	    {
	      retval.resize (2);

	      if (m.rows () == 1)
		retval(0) = m.row_max (index);
	      else
		retval(0) = octave_value (m.column_max (index), 0);
	    }
	}
      else if (arg1.is_complex_type ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();

	  if (! error_state)
	    {
	      retval.resize (2);

	      if (m.rows () == 1)
		retval(0) = m.row_max (index);
	      else
		retval(0) = octave_value (m.column_max (index), 0);
	    }
	}
      else
	gripe_wrong_type_arg ("max", arg1);

      int len = index.length ();

      if (len > 0)
	{
	  RowVector idx (len);

	  for (int i = 0; i < len; i++)
	    {
	      int tmp = index.elem (i) + 1;
	      idx.elem (i) = (tmp <= 0)
		? octave_NaN : static_cast<double> (tmp);
	    }

	  retval(1) = octave_value (idx, 0);
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
;;; End: ***
*/
