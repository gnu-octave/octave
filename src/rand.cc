// f-rand.cc                                           -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

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

#include "tree-const.h"
#include "f77-uscore.h"
#include "error.h"
#include "utils.h"
#include "f-rand.h"

// Possible distributions of random numbers.
enum rand_dist { uniform, normal };

// Current distribution of random numbers.
static rand_dist current_distribution = uniform;

extern "C"
{
  int *F77_FCN (dgennor) (double*, double*, double*);
  int *F77_FCN (dgenunf) (double*, double*, double*);
  int *F77_FCN (setall) (int*, int*);
  int *F77_FCN (getsd) (int*, int*);
}

#ifdef WITH_DLD
tree_constant *
builtin_rand_2 (tree_constant *args, int nargin, int nargout)
{
  return rand_internal (args, nargin, nargout);
}
#endif

static double
curr_rand_seed (void)
{
  union d2i { double d; int i[2]; };
  union d2i u;
  F77_FCN (getsd) (&(u.i[0]), &(u.i[1]));
  return u.d;
}

static int
force_to_fit_range (int i, int lo, int hi)
{
  assert (hi > lo && lo >= 0 && hi > lo);

  i = i > 0 ? i : -i;

  if (i < lo)
    i = lo;
  else if (i > hi)
    i = i % hi;

  return i;
}

static void
set_rand_seed (double val)
{
  union d2i { double d; int i[2]; };
  union d2i u;
  u.d = val;
  int i0 = force_to_fit_range (u.i[0], 1, 2147483563);
  int i1 = force_to_fit_range (u.i[1], 1, 2147483399);
  F77_FCN (setall) (&i0, &i1);
}

static char *
curr_rand_dist (void)
{
  if (current_distribution == uniform)
    return "uniform";
  else if (current_distribution == normal)
    return "normal";
  else
    {
      panic_impossible ();
      return (char *) NULL;
    }
}

tree_constant *
rand_internal (tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  static int initialized = 0;
  if (! initialized)
    {
// Make the random number generator give us a different sequence every
// time we start octave unless we specifically set the seed.
#if 0
      int s0 = 1234567890;
      int s1 = 123456789;
#else
      time_t now;
      struct tm *tm;
 
      time (&now);
      tm = localtime (&now);
 
      int s0 = tm->tm_min * 60 + tm->tm_sec;
      int s1 = (tm->tm_mday - 1) * 24 * 3600 + tm->tm_hour * 3600 + s0;
#endif
      s0 = force_to_fit_range (s0, 1, 2147483563);
      s1 = force_to_fit_range (s1, 1, 2147483399);

      F77_FCN (setall) (&s0, &s1);
      initialized = 1;
    }

  int n = 0;
  int m = 0;
  if (nargin == 1)
    {
      n = 1;
      m = 1;
      goto gen_matrix;
    }
  else if (nargin == 2)
    {
      switch (args[1].const_type ())
	{
	case tree_constant_rep::string_constant:
	  char *s_arg = args[1].string_value ();
	  if (strcmp (s_arg, "dist") == 0)
	    {
	      retval = new tree_constant [2];
	      char *s = curr_rand_dist ();
	      retval[0] = tree_constant (s);
	    }
	  else if (strcmp (s_arg, "seed") == 0)
	    {
	      retval = new tree_constant [2];
	      double d = curr_rand_seed ();
	      retval[0] = tree_constant (d);
	    }
	  else if (strcmp (s_arg, "uniform") == 0)
	    current_distribution = uniform;
	  else if (strcmp (s_arg, "normal") == 0)
	    current_distribution = normal;
	  else
	    {
	      delete [] retval;
	      retval = NULL_TREE_CONST;
	      message ("rand", "unrecognized string argument");
	    }
	  break;
	case tree_constant_rep::scalar_constant:
	case tree_constant_rep::complex_scalar_constant:
	  n = NINT (args[1].double_value ());
	  m = n;
	  goto gen_matrix;
	case tree_constant_rep::range_constant:
	  {
	    Range r = args[1].range_value ();
	    n = 1;
	    m = NINT (r.nelem ());
	  }
	  goto gen_matrix;
	case tree_constant_rep::matrix_constant:
	case tree_constant_rep::complex_matrix_constant:
	  n = NINT (args[1].rows ());
	  m = NINT (args[1].columns ());
	  goto gen_matrix;
	default:
	  panic_impossible ();
	  break;
	}
    }
  else if (nargin == 3)
    {
      if (args[1].is_string_type ()
	  && strcmp (args[1].string_value (), "seed") == 0)
	{
	  double d = args[2].to_scalar ();
	  set_rand_seed (d);
	}
      else
	{
	  n = NINT (args[1].to_scalar ());
	  m = NINT (args[2].to_scalar ());
	  goto gen_matrix;
	}
    }

  return retval;

 gen_matrix:

  if (n == 0 || m == 0)
    {
      retval = new tree_constant [2];
      Matrix m (0, 0);
      retval[0] = tree_constant (m);
    }
  else if (n > 0 && m > 0)
    {
      retval = new tree_constant [2];
      Matrix rand_mat (n, m);
      for (int j = 0; j < m; j++)
	for (int i = 0; i < n; i++)
	  {
	    double d_zero = 0.0;
	    double d_one = 1.0;
	    double val;
	    switch (current_distribution)
	      {
	      case uniform:
		F77_FCN (dgenunf) (&d_zero, &d_one, &val);
		rand_mat.elem (i, j) = val;
		break;
	      case normal:
		F77_FCN (dgennor) (&d_zero, &d_one, &val);
		rand_mat.elem (i, j) = val;
		break;
	      default:
		panic_impossible ();
		break;
	      }
	  }

      retval[0] = tree_constant (rand_mat);
    }
  else
    message ("rand", "invalid negative argument");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
