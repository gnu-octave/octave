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

#include <cfloat>
#include <cmath>

#include <string>

#include "lo-ieee.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "ov.h"
#include "variables.h"
#include "oct-obj.h"
#include "utils.h"

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef ABS
#define ABS(x) (((x) < 0) ? (-x) : (x))
#endif

// Should expressions like ones (-1, 5) result in an empty matrix or
// an error?  A positive value means yes.  A negative value means
// yes, but print a warning message.  Zero means it should be
// considered an error.
static int Vtreat_neg_dim_as_zero;

DEFUN (all, args, ,
  "all (X): are all elements of X nonzero?")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).all ();
  else
    print_usage ("all");

  return retval;
}

DEFUN (any, args, ,
  "any (X): are any elements of X nonzero?")
{
  octave_value_list retval;

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
map_d_m (d_dd_fcn f, double x, const Matrix& y)
{
  int nr = y.rows ();
  int nc = y.columns ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval (i, j) = f (x, y (i, j));

  return retval;
}

static Matrix
map_m_d (d_dd_fcn f, const Matrix& x, double y)
{
  int nr = x.rows ();
  int nc = x.columns ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval (i, j) = f (x (i, j), y);

  return retval;
}

static Matrix
map_m_m (d_dd_fcn f, const Matrix& x, const Matrix& y)
{
  int x_nr = x.rows ();
  int x_nc = x.columns ();

  int y_nr = y.rows ();
  int y_nc = y.columns ();

  assert (x_nr == y_nr && x_nc == y_nc);

  Matrix retval (x_nr, x_nc);

  for (int j = 0; j < x_nc; j++)
    for (int i = 0; i < x_nr; i++)
      retval (i, j) = f (x (i, j), y (i, j));

  return retval;
}

DEFUN (atan2, args, ,
  "atan2 (Y, X): atan (Y / X) in range -pi to pi")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      octave_value arg_y = args(0);
      octave_value arg_x = args(1);

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
		retval = map_d_m (atan2, y, x);
	    }
	}
      else if (x_is_scalar)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      double x = arg_x.double_value ();

	      if (! error_state)
		retval = map_m_d (atan2, y, x);
	    }
	}
      else if (y_nr == x_nr && y_nc == x_nc)
	{
	  Matrix y = arg_y.matrix_value ();

	  if (! error_state)
	    {
	      Matrix x = arg_x.matrix_value ();

	      if (! error_state)
		retval = map_m_m (atan2, y, x);
	    }
	}
      else
	error ("atan2: nonconformant matrices");
    }
  else
    print_usage ("atan2");

  return retval;
}

DEFUN (cumprod, args, ,
  "cumprod (X): cumulative products")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

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

DEFUN (cumsum, args, ,
  "cumsum (X): cumulative sums")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

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

static octave_value
make_diag (const Matrix& v, int k)
{
  int nr = v.rows ();
  int nc = v.columns ();
  assert (nc == 1 || nr == 1);

  octave_value retval;

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
	m (i+roff, i+coff) = v (0, i);
      retval = octave_value (m);
    }
  else
    {
      int n = nr + ABS (k);
      Matrix m (n, n, 0.0);
      for (int i = 0; i < nr; i++)
	m (i+roff, i+coff) = v (i, 0);
      retval = octave_value (m);
    }

  return retval;
}

static octave_value
make_diag (const ComplexMatrix& v, int k)
{
  int nr = v.rows ();
  int nc = v.columns ();
  assert (nc == 1 || nr == 1);

  octave_value retval;

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
	m (i+roff, i+coff) = v (0, i);
      retval = octave_value (m);
    }
  else
    {
      int n = nr + ABS (k);
      ComplexMatrix m (n, n, 0.0);
      for (int i = 0; i < nr; i++)
	m (i+roff, i+coff) = v (i, 0);
      retval = octave_value (m);
    }

  return retval;
}

static octave_value
make_diag (const octave_value& arg)
{
  octave_value retval;

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

static octave_value
make_diag (const octave_value& a, const octave_value& b)
{
  octave_value retval;

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
	      m (0, k) = d;
	      retval = m;
	    }
	  else if (k < 0)
	    {
	      Matrix m (n, n, 0.0);
	      m (-k, 0) = d;
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
	      m (0, k) = c;
	      retval = m;
	    }
	  else if (k < 0)
	    {
	      ComplexMatrix m (n, n, 0.0);
	      m (-k, 0) = c;
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

DEFUN (diag, args, ,
  "diag (X [,k]): form/extract diagonals")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = make_diag (args(0));
  else if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    retval = make_diag (args(0), args(1));
  else
    print_usage ("diag");

  return retval;
}

DEFUN (prod, args, ,
  "prod (X): products")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

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

DEFUN (size, args, nargout,
  "[m, n] = size (x): return rows and columns of X\n\
\n\
d = size (x): return number of rows and columns of x as a row vector\n\
\n\
m = size (x, 1): return number of rows in x\n\
m = size (x, 2): return number of columns in x")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 && nargout < 3)
    {
      int nr = args(0).rows ();
      int nc = args(0).columns ();

      if (nargout == 0 || nargout == 1)
	{
	  Matrix m (1, 2);
	  m (0, 0) = nr;
	  m (0, 1) = nc;
	  retval = m;
	}
      else if (nargout == 2)
	{
	  retval(1) = static_cast<double> (nc);
	  retval(0) = static_cast<double> (nr);
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
	    retval(0) = static_cast<double> (args(0).rows ());
	  else if (nd == 2)
	    retval(0) = static_cast<double> (args(0).columns ());
	  else
	    error ("size: invalid second argument -- expecting 1 or 2");
	}
    }
  else
    print_usage ("size");

  return retval;
}

DEFUN (sum, args, ,
  "sum (X): sum of elements")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

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

DEFUN (sumsq, args, ,
  "sumsq (X): sum of squares of elements")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

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

DEFUN (is_struct, args, ,
  "is_struct (x): return nonzero if x is a structure")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

      if (arg.is_map ())
	retval = 1.0;
      else
	retval = 0.0;
    }
  else
    print_usage ("is_struct");

  return retval;
}

DEFUN (struct_elements, args, ,
  "struct_elements (S)\n\
\n\
Return a list of the names of the elements of the structure S.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args (0).is_map ())
	{
	  Octave_map m = args(0).map_value ();
	  retval(0) = m.make_name_list ();
	}
      else
	gripe_wrong_type_arg ("struct_elements", args (0));
    }
  else
    print_usage ("struct_elements");

  return retval;
}

DEFUN (struct_contains, args, ,
  "struct_contains (S, NAME)\n\
\n\
Return nonzero if S is a structure with element NAME.\n\
S must be a structure and NAME must be a string.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      retval = 0.0;

      // XXX FIXME XXX -- should this work for all types that can do
      // structure reference operations?

      if (args(0).is_map () && args(1).is_string ())
	{
	  string s = args(1).string_value ();
	  octave_value tmp = args(0).do_struct_elt_index_op (s, true);
	  retval = static_cast<double> (tmp.is_defined ());
	}
      else
	print_usage ("struct_contains");
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
      if (Vtreat_neg_dim_as_zero)
	{
	  nr = (nr < 0) ? 0 : nr;
	  nc = (nc < 0) ? 0 : nc;

	  if (Vtreat_neg_dim_as_zero < 0)
	    warning ("%s: converting negative dimension to zero",
		     warnfor);
	}
      else
	error ("%s: can't create a matrix with negative dimensions",
	       warnfor);
    }
}

static void
get_dimensions (const octave_value& a, const char *warn_for,
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

	  nr = NINT (v (0));
	  nc = NINT (v (1));
	}
      else
	warning ("%s (A): use %s (size (A)) instead", warn_for, warn_for);
    }

  check_dimensions (nr, nc, warn_for); // May set error_state.
}

static void
get_dimensions (const octave_value& a, const octave_value& b,
		const char *warn_for, int& nr, int& nc)
{
  nr = NINT (a.double_value ());
  nc = NINT (b.double_value ());

  if (error_state)
    error ("%s: expecting two scalar arguments", warn_for);
  else
    check_dimensions (nr, nc, warn_for); // May set error_state.
}

static octave_value
fill_matrix (const octave_value& a, double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, warn_for, nr, nc);

  if (error_state)
    return  octave_value ();

  Matrix m (nr, nc, val);

  return m;
}

static octave_value
fill_matrix (const octave_value& a, const octave_value& b,
	     double val, const char *warn_for)
{
  int nr, nc;
  get_dimensions (a, b, warn_for, nr, nc); // May set error_state.

  if (error_state)
    return octave_value ();

  Matrix m (nr, nc, val);

  return m;
}

DEFUN (ones, args, ,
  "ones (N), ones (N, M), ones (X): create a matrix of all ones")
{
  octave_value_list retval;

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

DEFUN (zeros, args, ,
  "zeros (N), zeros (N, M), zeros (X): create a matrix of all zeros")
{
  octave_value_list retval;

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

static octave_value
identity_matrix (const octave_value& a)
{
  int nr, nc;
  get_dimensions (a, "eye", nr, nc); // May set error_state.

  if (error_state)
    return octave_value ();

  Matrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      int n = MIN (nr, nc);
      for (int i = 0; i < n; i++)
	m (i, i) = 1.0;
    }

  return m;
}

static octave_value
identity_matrix (const octave_value& a, const octave_value& b)
{
  int nr, nc;
  get_dimensions (a, b, "eye", nr, nc);  // May set error_state.

  if (error_state)
    return octave_value ();

  Matrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      int n = MIN (nr, nc);
      for (int i = 0; i < n; i++)
	m (i, i) = 1.0;
    }

  return m;
}

DEFUN (eye, args, ,
  "eye (N), eye (N, M), eye (X): create an identity matrix")
{
  octave_value_list retval;

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

DEFUN (linspace, args, ,
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
  octave_value_list retval;

  int nargin = args.length ();

  int npoints = 100;

  if (nargin != 2 && nargin != 3)
    {
      print_usage ("linspace");
      return retval;
    }

  if (nargin == 3)
    {
      double n = args(2).double_value ();

      if (! error_state)
	npoints = NINT (n);
    }

  if (! error_state)
    {
      if (npoints > 1)
	{
	  octave_value arg_1 = args(0);
	  octave_value arg_2 = args(1);

	  if (arg_1.is_complex_type () || arg_2.is_complex_type ())
	    {
	      Complex x1 = arg_1.complex_value ();
	      Complex x2 = arg_2.complex_value ();

	      if (! error_state)
		{
		  ComplexRowVector rv = linspace (x1, x2, npoints);

		  if (! error_state)
		    retval (0) = octave_value (rv, 0);
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
		    retval (0) = octave_value (rv, 0);
		}
	    }
	}
      else
	error ("linspace: npoints must be greater than 2");
    }

  return retval;
}

static int
treat_neg_dim_as_zero (void)
{
  Vtreat_neg_dim_as_zero = check_preference ("treat_neg_dim_as_zero");

  return 0;
}

void
symbols_of_data (void)
{
  DEFCONST (I, Complex (0.0, 1.0), 0, 0,
    "sqrt (-1)");

  DEFCONST (Inf, octave_Inf, 0, 0,
    "infinity");

  DEFCONST (J, Complex (0.0, 1.0), 0, 0,
    "sqrt (-1)");

  DEFCONST (NaN, octave_NaN, 0, 0,
    "not a number");

#if defined (M_E)
  double e_val = M_E;
#else
  double e_val = exp (1.0);
#endif

  DEFCONST (e, e_val, 0, 0,
    "exp (1)");

  DEFCONST (eps, DBL_EPSILON, 0, 0,
    "machine precision");

  DEFCONST (i, Complex (0.0, 1.0), 1, 0,
    "sqrt (-1)");

  DEFCONST (inf, octave_Inf, 0, 0,
    "infinity");

  DEFCONST (j, Complex (0.0, 1.0), 1, 0,
    "sqrt (-1)");

  DEFCONST (nan, octave_NaN, 0, 0,
    "not a number");

#if defined (M_PI)
  double pi_val = M_PI;
#else
  double pi_val = 4.0 * atan (1.0);
#endif

  DEFCONST (pi, pi_val, 0, 0,
    "ratio of the circumference of a circle to its diameter");

  DEFCONST (realmax, DBL_MAX, 0, 0,
    "realmax (): return largest representable floating point number");

  DEFCONST (realmin, DBL_MIN, 0, 0,
    "realmin (): return smallest representable floating point number");

  DEFVAR (treat_neg_dim_as_zero, 0.0, 0, treat_neg_dim_as_zero,
    "convert negative dimensions to zero");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
