// data.cc                                               -*- C++ -*-
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

/*

The function builtin_pwd adapted from a similar function from GNU
Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "tree-const.h"
#include "user-prefs.h"
#include "help.h"
#include "utils.h"
#include "error.h"
#include "defun.h"

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef ABS
#define ABS(x) (((x) < 0) ? (-x) : (x))
#endif

DEFUN ("all", Fall, Sall, 1, 1,
  "all (X): are all elements of X nonzero?")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).all ();
  else
    print_usage ("all");

  return retval;
}

DEFUN ("any", Fany, Sany, 1, 1,
  "any (X): are any elements of X nonzero?")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).any ();
  else
    print_usage ("any");

  return retval;
}

// These mapping functions may also be useful in other places, eh?

typedef double (*d_dd_fcn) (double, double);

static Matrix
map (d_dd_fcn f, double x, const Matrix& y)
{
  int nr = y.rows ();
  int nc = y.columns ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval.elem (i, j) = f (x, y.elem (i, j));

  return retval;
}

static Matrix
map (d_dd_fcn f, const Matrix& x, double y)
{
  int nr = x.rows ();
  int nc = x.columns ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval.elem (i, j) = f (x.elem (i, j), y);

  return retval;
}

static Matrix
map (d_dd_fcn f, const Matrix& x, const Matrix& y)
{
  int x_nr = x.rows ();
  int x_nc = x.columns ();

  int y_nr = y.rows ();
  int y_nc = y.columns ();

  assert (x_nr == y_nr && x_nc == y_nc);

  Matrix retval (x_nr, x_nc);

  for (int j = 0; j < x_nc; j++)
    for (int i = 0; i < x_nr; i++)
      retval.elem (i, j) = f (x.elem (i, j), y.elem (i, j));

  return retval;
}

DEFUN ("atan2", Fatan2, Satan2, 2, 1,
  "atan2 (Y, X): atan (Y / X) in range -pi to pi")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      tree_constant arg_y = args(0);
      tree_constant arg_x = args(1);

      int y_nr = arg_y.rows ();
      int y_nc = arg_y.columns ();

      int x_nr = arg_x.rows ();
      int x_nc = arg_x.columns ();

      int arg_y_empty = empty_arg ("atan2", y_nr, y_nc);
      int arg_x_empty = empty_arg ("atan2", x_nr, x_nc);

      if (arg_y_empty > 0 && arg_x_empty > 0)
	return Matrix ();
      else if (arg_y_empty || arg_x_empty)
	return retval;

      int y_is_scalar = (y_nr == 1 && y_nc == 1);
      int x_is_scalar = (x_nr == 1 && x_nc == 1);

      if (y_is_scalar && x_is_scalar)
	{
	  double y = arg_y.double_value ();

	  if (! error_state)
	    {
	      double x = arg_x.double_value ();

	      if (! error_state)
		retval = atan2 (y, x);
	    }
	}
      else if (y_is_scalar)
	{
	  double y = arg_y.double_value ();

	  if (! error_state)
	    {
	      Matrix x = arg_x.matrix_value ();

	      if (! error_state)
		retval = map (atan2, y, x);
	    }
	}
      else if (x_is_scalar)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      double x = arg_x.double_value ();

	      if (! error_state)
		retval = map (atan2, y, x);
	    }
	}
      else if (y_nr == x_nr && y_nc == x_nc)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      Matrix x = arg_x.matrix_value ();

	      if (! error_state)
		retval = map (atan2, y, x);
	    }
	}
      else
	error ("atan2: nonconformant matrices");
    }
  else
    print_usage ("atan2");

  return retval;
}

DEFUN ("cumprod", Fcumprod, Scumprod, 1, 1,
  "cumprod (X): cumulative products")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      tree_constant arg = args(0);

      if (arg.is_real_type ())
	{
	  Matrix tmp = arg.matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.cumprod ();
	}
      else if (arg.is_complex_type ())
	{
	  ComplexMatrix tmp = arg.complex_matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.cumprod ();
	}
      else
	{
	  gripe_wrong_type_arg ("cumprod", arg);
	  return retval;
	}
    }
  else
    print_usage ("cumprod");

  return retval;
}

DEFUN ("cumsum", Fcumsum, Scumsum, 1, 1,
  "cumsum (X): cumulative sums")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      tree_constant arg = args(0);

      if (arg.is_real_type ())
	{
	  Matrix tmp = arg.matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.cumsum ();
	}
      else if (arg.is_complex_type ())
	{
	  ComplexMatrix tmp = arg.complex_matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.cumsum ();
	}
      else
	{
	  gripe_wrong_type_arg ("cumsum", arg);
	  return retval;
	}
    }
  else
    print_usage ("cumsum");

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

static tree_constant
make_diag (const tree_constant& arg)
{
  tree_constant retval;

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  int nr = m.rows ();
	  int nc = m.columns ();

	  if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (m, 0);
	  else
	    {
	      ColumnVector v = m.diag ();
	      if (v.capacity () > 0)
		retval = v;
	    }
	}
      else
	gripe_wrong_type_arg ("diag", arg);
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix cm = arg.complex_matrix_value ();

      if (! error_state)
	{
	  int nr = cm.rows ();
	  int nc = cm.columns ();

	  if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (cm, 0);
	  else
	    {
	      ComplexColumnVector v = cm.diag ();
	      if (v.capacity () > 0)
		retval = v;
	    }
	}
      else
	gripe_wrong_type_arg ("diag", arg);
    }
  else
    gripe_wrong_type_arg ("diag", arg);

  return retval;
}

static tree_constant
make_diag (const tree_constant& a, const tree_constant& b)
{
  tree_constant retval;

  double tmp = b.double_value ();

  if (error_state)
    {
      error ("diag: invalid second argument");      
      return retval;
    }

  int k = NINT (tmp);
  int n = ABS (k) + 1;

  if (a.is_real_type ())
    {
      if (a.is_scalar_type ())
	{
	  double d = a.double_value ();

	  if (k == 0)
	    retval = d;
	  else if (k > 0)
	    {
	      Matrix m (n, n, 0.0);
	      m.elem (0, k) = d;
	      retval = m;
	    }
	  else if (k < 0)
	    {
	      Matrix m (n, n, 0.0);
	      m.elem (-k, 0) = d;
	      retval = m;
	    }
	}
      else if (a.is_matrix_type ())
	{
	  Matrix m = a.matrix_value ();

	  int nr = m.rows ();
	  int nc = m.columns ();

	  if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (m, k);
	  else
	    {
	      ColumnVector d = m.diag (k);
	      retval = d;
	    }
	}
      else
	gripe_wrong_type_arg ("diag", a);
    }
  else if (a.is_complex_type ())
    {
      if (a.is_scalar_type ())
	{
	  Complex c = a.complex_value ();

	  if (k == 0)
	    retval = c;
	  else if (k > 0)
	    {
	      ComplexMatrix m (n, n, 0.0);
	      m.elem (0, k) = c;
	      retval = m;
	    }
	  else if (k < 0)
	    {
	      ComplexMatrix m (n, n, 0.0);
	      m.elem (-k, 0) = c;
	      retval = m;
	    }
	}
      else if (a.is_matrix_type ())
	{
	  ComplexMatrix cm = a.complex_matrix_value ();

	  int nr = cm.rows ();
	  int nc = cm.columns ();

	  if (nr == 0 || nc == 0)
	    retval = Matrix ();
	  else if (nr == 1 || nc == 1)
	    retval = make_diag (cm, k);
	  else
	    {
	      ComplexColumnVector d = cm.diag (k);
	      retval = d;
	    }
	}
      else
	gripe_wrong_type_arg ("diag", a);
    }
  else
    gripe_wrong_type_arg ("diag", a);

  return retval;
}

DEFUN ("diag", Fdiag, Sdiag, 2, 1,
  "diag (X [,k]): form/extract diagonals")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = make_diag (args(0));
  else if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    retval = make_diag (args(0), args(1));
  else
    print_usage ("diag");

  return retval;
}

DEFUN ("isstr", Fisstr, Sisstr, 1, 1,
  "isstr (X): return 1 if X is a string, 0 otherwise")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = (double) args(0).is_string ();
  else
    print_usage ("isstr");

  return retval;
}

DEFUN ("prod", Fprod, Sprod, 1, 1,
  "prod (X): products")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      tree_constant arg = args(0);

      if (arg.is_real_type ())
	{
	  Matrix tmp = arg.matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.prod ();
	}
      else if (arg.is_complex_type ())
	{
	  ComplexMatrix tmp = arg.complex_matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.prod ();
	}
      else
	{
	  gripe_wrong_type_arg ("prod", arg);
	  return retval;
	}
    }
  else
    print_usage ("prod");

  return retval;
}

DEFUN ("setstr", Fsetstr, Ssetstr, 1, 1,
  "setstr (V): convert a vector to a string")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).convert_to_str ();
  else
    print_usage ("setstr");

  return retval;
}

DEFUN ("size", Fsize, Ssize, 1, 1,
  "[m, n] = size (x): return rows and columns of X")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    {
      int nr = args(0).rows ();
      int nc = args(0).columns ();
      if (nargout == 0 || nargout == 1)
	{
	  Matrix m (1, 2);
	  m.elem (0, 0) = nr;
	  m.elem (0, 1) = nc;
	  retval = m;
	}
      else if (nargout == 2)
	{
	  retval(1) = (double) nc;
	  retval(0) = (double) nr;
	}
      else
	print_usage ("size");
    }
  else
    print_usage ("size");

  return retval;
}

DEFUN ("sum", Fsum, Ssum, 1, 1,
  "sum (X): sum of elements")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      tree_constant arg = args(0);

      if (arg.is_real_type ())
	{
	  Matrix tmp = arg.matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.sum ();
	}
      else if (arg.is_complex_type ())
	{
	  ComplexMatrix tmp = arg.complex_matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.sum ();
	}
      else
	{
	  gripe_wrong_type_arg ("sum", arg);
	  return retval;
	}
    }
  else
    print_usage ("sum");

  return retval;
}

DEFUN ("sumsq", Fsumsq, Ssumsq, 1, 1,
  "sumsq (X): sum of squares of elements")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      tree_constant arg = args(0);

      if (arg.is_real_type ())
	{
	  Matrix tmp = arg.matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.sumsq ();
	}
      else if (arg.is_complex_type ())
	{
	  ComplexMatrix tmp = arg.complex_matrix_value ();

	  if (! error_state)
	    retval(0) = tmp.sumsq ();
	}
      else
	{
	  gripe_wrong_type_arg ("sumsq", arg);
	  return retval;
	}
    }
  else
    print_usage ("sumsq");

  return retval;
}

static void
check_dimensions (int& nr, int& nc, const char *warnfor)
{
  if (nr < 0 || nc < 0)
    {
      if (user_pref.treat_neg_dim_as_zero)
	{
	  nr = (nr < 0) ? 0 : nr;
	  nc = (nc < 0) ? 0 : nc;
	}
      else
	error ("%s: can't create a matrix with negative dimensions",
	       warnfor);
    }
}

static void
get_dimensions (const tree_constant& a, const char *warn_for,
		int& nr, int& nc)
{
  if (a.is_scalar_type ())
    {
      double tmp = a.double_value ();
      nr = nc = NINT (tmp);
    }
  else
    {
      nr = a.rows ();
      nc = a.columns ();

      if ((nr == 1 && nc == 2) || (nr == 2 && nc == 1))
	{
	  ColumnVector v = a.vector_value ();

	  if (error_state)
	    return;

	  nr = NINT (v.elem (0));
	  nc = NINT (v.elem (1));
	}
      else
	warning ("%s (A): use %s (size (A)) instead", warn_for, warn_for);
    }

  check_dimensions (nr, nc, warn_for); // May set error_state.
}

static void
get_dimensions (const tree_constant& a, const tree_constant& b,
		const char *warn_for, int& nr, int& nc)
{
  nr = NINT (a.double_value ());
  nc = NINT (b.double_value ());

  if (error_state)
    error ("%s: expecting two scalar arguments", warn_for);
  else
    check_dimensions (nr, nc, warn_for); // May set error_state.
}

static tree_constant
fill_matrix (const tree_constant& a, double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, warn_for, nr, nc);

  if (error_state)
    return  tree_constant ();

  Matrix m (nr, nc, val);

  return m;
}

static tree_constant
fill_matrix (const tree_constant& a, const tree_constant& b,
	     double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, b, warn_for, nr, nc); // May set error_state.

  if (error_state)
    return tree_constant ();

  Matrix m (nr, nc, val);

  return m;
}

DEFUN ("ones", Fones, Sones, 2, 1,
  "ones (N), ones (N, M), ones (X): create a matrix of all ones")
{
  Octave_object retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 0:
      retval = 1.0;
      break;
    case 1:
      retval = fill_matrix (args(0), 1.0, "ones");
      break;
    case 2:
      retval = fill_matrix (args(0), args(1), 1.0, "ones");
      break;
    default:
      print_usage ("ones");
      break;
    }

  return retval;
}

DEFUN ("zeros", Fzeros, Szeros, 2, 1,
  "zeros (N), zeros (N, M), zeros (X): create a matrix of all zeros")
{
  Octave_object retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 0:
      retval = 0.0;
      break;
    case 1:
      retval = fill_matrix (args(0), 0.0, "zeros");
      break;
    case 2:
      retval = fill_matrix (args(0), args(1), 0.0, "zeros");
      break;
    default:
      print_usage ("zeros");
      break;
    }

  return retval;
}

static tree_constant
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

  return m;
}

static tree_constant
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

  return m;
}

DEFUN ("eye", Feye, Seye, 2, 1,
  "eye (N), eye (N, M), eye (X): create an identity matrix")
{
  Octave_object retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 0:
      retval = 1.0;
      break;
    case 1:
      retval = identity_matrix (args(0));
      break;
    case 2:
      retval = identity_matrix (args(0), args(1));
      break;
    default:
      print_usage ("eye");
      break;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
