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

DEFUN ("all", Fall, Sall, 2, 1,
  "all (X): are all elements of X nonzero?")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("all");
  else if (nargin > 0 && args(1).is_defined ())
    retval = args(1).all ();

  return retval;
}

DEFUN ("any", Fany, Sany, 2, 1,
  "any (X): are any elements of X nonzero?")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("any");
  else if (nargin > 0 && args(1).is_defined ())
    retval = args(1).any ();

  return retval;
}

DEFUN ("atan2", Fatan2, Satan2, 3, 1,
  "atan2 (Y, X): atan (Y / X) in range -pi to pi")
{
  Octave_object retval;

  if (args.length () != 3)
    print_usage ("atan2");
  else
    {
      double y = args(1).double_value ();

      if (! error_state)
	{
	  double x = args(2).double_value ();

	  if (! error_state)
	    retval = atan2 (y, x);
	}
    }

  return retval;
}

DEFUN ("cumprod", Fcumprod, Scumprod, 2, 1,
  "cumprod (X): cumulative products")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("cumprod");
  else if (nargin > 0 && args(1).is_defined ())
    retval = args(1).cumprod ();

  return retval;
}

DEFUN ("cumsum", Fcumsum, Scumsum, 2, 1,
  "cumsum (X): cumulative sums")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("cumsum");
  else if (nargin > 0 && args(1).is_defined ())
    retval = args(1).cumsum ();

  return retval;
}

DEFUN ("diag", Fdiag, Sdiag, 3, 1,
  "diag (X [,k]): form/extract diagonals")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 2)
    retval = args(1).diag ();
  else if (nargin == 3)
    retval = args(1).diag (args(2));
  else
    print_usage ("diag");

  return retval;
}

DEFUN ("isstr", Fisstr, Sisstr, 2, 1,
  "isstr (X): return 1 if X is a string, 0 otherwise")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("isstr");
  else
    {
      if (nargin > 0 && args(1).is_defined ())
	retval = (double) args(1).is_string ();
    }

  return retval;
}

DEFUN ("prod", Fprod, Sprod, 2, 1,
  "prod (X): products")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("prod");
  else if (nargin > 0 && args(1).is_defined ())
    retval = args(1).prod ();

  return retval;
}

DEFUN ("setstr", Fsetstr, Ssetstr, 2, 1,
  "setstr (V): convert a vector to a string")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 2)
    retval = args(1).convert_to_str ();
  else
    print_usage ("setstr");

  return retval;
}

DEFUN ("size", Fsize, Ssize, 2, 1,
  "[m, n] = size (x): return rows and columns of X")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("size");
  else
    {
      if (nargin > 0 && args(1).is_defined ())
	{
	  int nr = args(1).rows ();
	  int nc = args(1).columns ();
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
    }

  return retval;
}

DEFUN ("sum", Fsum, Ssum, 2, 1,
  "sum (X): sum of elements")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("sum");
  else
    {
      if (nargin > 0 && args(1).is_defined ())
	retval = args(1).sum ();
    }

  return retval;
}

DEFUN ("sumsq", Fsumsq, Ssumsq, 2, 1,
  "sumsq (X): sum of squares of elements")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    print_usage ("sumsq");
  else if (nargin > 0 && args(1).is_defined ())
    retval = args(1).sumsq ();

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
  tree_constant tmpa = a;

  if (tmpa.is_scalar_type ())
    {
      double tmp = tmpa.double_value ();
      nr = nc = NINT (tmp);
    }
  else
    {
      nr = tmpa.rows ();
      nc = tmpa.columns ();

      if ((nr == 1 && nc == 2) || (nr == 2 && nc == 1))
	{
	  ColumnVector v = tmpa.vector_value ();

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
  tree_constant tmpa = a;
  tree_constant tmpb = b;

  if (tmpa.is_scalar_type () && tmpb.is_scalar_type ())
    {
      nr = NINT (tmpa.double_value ());
      nc = NINT (tmpb.double_value ());

      check_dimensions (nr, nc, warn_for); // May set error_state.
    }
  else
    error ("%s: expecting two scalar arguments", warn_for);
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

DEFUN ("ones", Fones, Sones, 3, 1,
  "ones (N), ones (N, M), ones (X): create a matrix of all ones")
{
  Octave_object retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 1:
      retval = 1.0;
      break;
    case 2:
      retval = fill_matrix (args(1), 1.0, "ones");
      break;
    case 3:
      retval = fill_matrix (args(1), args(2), 1.0, "ones");
      break;
    default:
      print_usage ("ones");
      break;
    }

  return retval;
}

DEFUN ("zeros", Fzeros, Szeros, 3, 1,
  "zeros (N), zeros (N, M), zeros (X): create a matrix of all zeros")
{
  Octave_object retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 1:
      retval = 0.0;
      break;
    case 2:
      retval = fill_matrix (args(1), 0.0, "zeros");
      break;
    case 3:
      retval = fill_matrix (args(1), args(2), 0.0, "zeros");
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

DEFUN ("eye", Feye, Seye, 3, 1,
  "eye (N), eye (N, M), eye (X): create an identity matrix")
{
  Octave_object retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 1:
      retval = 1.0;
      break;
    case 2:
      retval = identity_matrix (args(1));
      break;
    case 3:
      retval = identity_matrix (args(1), args(2));
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
