// data.cc                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

/*

The function builtin_pwd adapted from a similar function from GNU
Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-map.h"
#include "pt-const.h"
#include "oct-obj.h"
#include "user-prefs.h"
#include "utils.h"

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef ABS
#define ABS(x) (((x) < 0) ? (-x) : (x))
#endif

DEFUN ("all", Fall, Sall, 10,
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

DEFUN ("any", Fany, Sany, 10,
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

DEFUN ("atan2", Fatan2, Satan2, 10,
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

DEFUN ("cumprod", Fcumprod, Scumprod, 10,
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

DEFUN ("cumsum", Fcumsum, Scumsum, 10,
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

DEFUN ("diag", Fdiag, Sdiag, 10,
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

DEFUN ("prod", Fprod, Sprod, 10,
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

DEFUN ("size", Fsize, Ssize, 11,
  "[m, n] = size (x): return rows and columns of X\n\
\n\
d = size (x): return number of rows and columns of x as a row vector\n\
\n\
m = size (x, 1): return number of rows in x\n\
m = size (x, 2): return number of columns in x")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1 && nargout < 3)
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
    }
  else if (nargin == 2 && nargout < 2)
    {
      int nd = NINT (args(1).double_value ());

      if (error_state)
	error ("size: expecting scalar as second argument");
      else
	{
	  if (nd == 1)
	    retval(0) = (double) (args(0).rows ());
	  else if (nd == 2)
	    retval(0) = (double) (args(0).columns ());
	  else
	    error ("size: invalid second argument -- expecting 1 or 2");
	}
    }
  else
    print_usage ("size");

  return retval;
}

DEFUN ("sum", Fsum, Ssum, 10,
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

DEFUN ("sumsq", Fsumsq, Ssumsq, 10,
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

DEFUN ("is_struct", Fis_struct, Sis_struct, 10,
  "is_struct (x): return nonzero if x is a structure")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      tree_constant arg = args(0);

      if (arg.is_map ())
	retval = 1.0;
      else
	retval = 0.0;
    }
  else
    print_usage ("is_struct");

  return retval;
}

DEFUN ("struct_elements", Fstruct_elements, Sstruct_elements, 10,
  "struct_elements (S)\n\
\n\
Return a list of the names of the elements of the structure S.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args (0).is_map ())
	{
	  Octave_map m = args(0).map_value ();
	  char **names = m.make_name_list ();

	  char **ptr = names;
	  int max_len = 0;
	  while (*ptr)
	    {
	      int len = strlen (*ptr);
	      if (len > max_len)
		max_len = len;
	      ptr++;
	    }

	  charMatrix list (m.length (), max_len);

	  ptr = names;
	  int i = 0;
	  while (*ptr)
	    {
	      list.insert (*ptr, i++, 0);
	      delete [] *ptr++;
	    }

	  delete [] names;

	  retval(0) = list;
	}
      else
	gripe_wrong_type_arg ("struct_elements", args (0));
    }
  else
    print_usage ("struct_elements");

  return retval;
}

DEFUN ("struct_contains", Fstruct_contains, Sstruct_contains, 10,
  "struct_contains (S, NAME)\n\
\n\
return nonzero if S is a structure with element NAME")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      retval = 0.0;
      if (args(0).is_map () && args(1).is_string ())
	{
	  string tstr = args(1).string_value ();
	  const char *s = tstr.c_str ();
	  tree_constant tmp = args(0).lookup_map_element (s, 0, 1);
	  retval = (double) tmp.is_defined ();
	}
    }
  else
    print_usage ("struct_contains");

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

	  if (user_pref.treat_neg_dim_as_zero < 0)
	    warning ("%s: converting negative dimension to zero",
		     warnfor);
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

DEFUN ("ones", Fones, Sones, 10,
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

DEFUN ("zeros", Fzeros, Szeros, 10,
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

DEFUN ("eye", Feye, Seye, 10,
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

DEFUN ("linspace", Flinspace, Slinspace, 10,
  "usage: linspace (x1, x2, n)\n\
\n\
Return a vector of n equally spaced points between x1 and x2\n\
inclusive.\n\
\n\
If the final argument is omitted, n = 100 is assumed.\n\
\n\
All three arguments must be scalars.\n\
\n\
See also: logspace")
{
  Octave_object retval;

  int nargin = args.length ();

  int npoints = 100;

  if (nargin == 3)
    {
      double n = args(2).double_value ();

      if (! error_state)
	npoints = NINT (n);
    }
  else
    print_usage ("linspace");

  if (! error_state)
    {
      if (npoints > 1)
	{
	  tree_constant arg_1 = args(0);
	  tree_constant arg_2 = args(1);

	  if (arg_1.is_complex_type () || arg_2.is_complex_type ())
	    {
	      Complex x1 = arg_1.complex_value ();
	      Complex x2 = arg_2.complex_value ();

	      if (! error_state)
		{
		  ComplexRowVector rv = linspace (x1, x2, npoints);

		  if (! error_state)
		    retval (0) = tree_constant (rv, 0);
		}
	    }
	  else
	    {
	      double x1 = arg_1.double_value ();
	      double x2 = arg_2.double_value ();

	      if (! error_state)
		{
		  RowVector rv = linspace (x1, x2, npoints);

		  if (! error_state)
		    retval (0) = tree_constant (rv, 0);
		}
	    }
	}
      else
	error ("linspace: npoints must be greater than 2");
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
