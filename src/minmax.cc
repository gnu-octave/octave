// f-minmax.cc                                           -*- C++ -*-
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
#include "help.h"
#include "defun-dld.h"

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

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
      if (arg1.rows () == arg2.rows ()
	  && arg1.columns () == arg2.columns ())
	{
	  if (arg1.is_real_type () && arg2.is_real_type ())
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
	  else if (arg1.is_complex_matrix () || arg2.is_complex_type ())
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
	      gripe_wrong_type_arg ("min", arg1);
	      return retval;
	    }
	}
      else
	error ("min: nonconformant matrices");
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
      else if (arg1.is_real_matrix ())
	{
	  Matrix m = arg1.matrix_value ();
	  if (m.rows () == 1)
	    retval(0) = m.row_max ();
	  else
	    retval(0) = tree_constant (m.column_max (), 0);
	}
      else if (arg1.is_complex_matrix ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();
	  if (m.rows () == 1)
	    retval(0) = m.row_max ();
	  else
	    retval(0) = tree_constant (m.column_max (), 0);
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
      else if (arg1.is_real_matrix ())
	{
	  Matrix m = arg1.matrix_value ();
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
      else if (arg1.is_complex_matrix ())
	{
	  ComplexMatrix m = arg1.complex_matrix_value ();
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
      else
	{
	  gripe_wrong_type_arg ("max", arg1);
	  return retval;
	}
    }
  else if (nargin == 2)
    {
      if (arg1.rows () == arg2.rows ()
	  && arg1.columns () == arg2.columns ())
	{
// XXX FIXME XXX -- I don't think this is quite right.
          if (arg1.is_real_scalar ())
            {
	      double result;
	      double a_elem = arg1.double_value ();
	      double b_elem = arg2.double_value ();
	      result = MAX (a_elem, b_elem);
	      retval(0) = result;
	    }
	  else if (arg1.is_complex_scalar ())
	    {
	      Complex result;
	      Complex a_elem = arg1.complex_value ();
	      Complex b_elem = arg2.complex_value ();
	      if (abs (a_elem) > abs (b_elem))
		result = a_elem;
	      else
		result = b_elem;
	      retval(0) = result;
	    }
	  else if (arg1.is_real_matrix ())
	    {
	      Matrix result;
	      result = max (arg1.matrix_value (), arg2.matrix_value ());
	      retval(0) = result;
	    }
	  else if (arg1.is_complex_matrix ())
	    {
	      ComplexMatrix result;
	      result = max (arg1.complex_matrix_value (),
			    arg2.complex_matrix_value ());
	      retval(0) = result;
	    }
	  else 
	    {
	      gripe_wrong_type_arg ("max", arg1);
	      return retval;
	    }
	}
      else
	error ("max: nonconformant matrices");
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
