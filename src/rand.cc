// f-rand.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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

#include <time.h>

#include "tree-const.h"
#include "f77-uscore.h"
#include "error.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

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
      return 0;
    }
}

DEFUN_DLD_BUILTIN ("rand", Frand, Srand, 2, 1,
  "rand                  -- generate a random value\n\
\n\
rand (N)              -- generate N x N matrix\n\
rand (A)              -- generate matrix the size of A\n\
rand (N, M)           -- generate N x M matrix\n\
rand (\"dist\")         -- get current distribution\n\
rand (DISTRIBUTION)   -- set distribution type (\"normal\" or \"uniform\"\n\
rand (SEED)           -- get current seed\n\
rand (SEED, N)        -- set seed")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin > 3 || nargout > 1)
    {
      print_usage ("rand");
      return retval;
    }

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
      tree_constant tmp = args(1);

      if (tmp.is_string ())
	{
	  char *s_arg = tmp.string_value ();
	  if (strcmp (s_arg, "dist") == 0)
	    {
	      char *s = curr_rand_dist ();
	      retval(0) = s;
	    }
	  else if (strcmp (s_arg, "seed") == 0)
	    {
	      double d = curr_rand_seed ();
	      retval(0) = d;
	    }
	  else if (strcmp (s_arg, "uniform") == 0)
	    current_distribution = uniform;
	  else if (strcmp (s_arg, "normal") == 0)
	    current_distribution = normal;
	  else
	    error ("rand: unrecognized string argument");
	}
      else if (tmp.is_scalar_type ())
	{
	  m = n = NINT (tmp.double_value ());

	  if (! error_state)
	    goto gen_matrix;
	}
      else if (tmp.is_range ())
	{
	  Range r = tmp.range_value ();
	  n = 1;
	  m = NINT (r.nelem ());
	  goto gen_matrix;
	}
      else if (tmp.is_matrix_type ())
	{
	  n = NINT (args(1).rows ());
	  m = NINT (args(1).columns ());
	  goto gen_matrix;
	}
      else
	{
	  gripe_wrong_type_arg ("rand", tmp);
	  return retval;
	}
    }
  else if (nargin == 3)
    {
      if (args(1).is_string ()
	  && strcmp (args(1).string_value (), "seed") == 0)
	{
	  double d = args(2).double_value ();

	  if (! error_state)
	    set_rand_seed (d);
	}
      else
	{
	  n = NINT (args(1).double_value ());

	  if (! error_state)
	    {
	      m = NINT (args(2).double_value ());

	      if (! error_state)
		goto gen_matrix;
	    }
	}
    }

  return retval;

 gen_matrix:

  if (n == 0 || m == 0)
    {
      Matrix m;
      retval.resize (1, m);
    }
  else if (n > 0 && m > 0)
    {
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

      retval(0) = rand_mat;
    }
  else
    error ("rand: invalid negative argument");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
