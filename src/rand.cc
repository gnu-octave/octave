// f-rand.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

#include <time.h>

#include "tree-const.h"
#include "f77-uscore.h"
#include "error.h"
#include "gripes.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

// Possible distributions of random numbers.
enum rand_dist { uniform, normal };

// Current distribution of random numbers.
static rand_dist current_distribution = uniform;

extern "C"
{
  int *F77_FCN (dgennor, DGENNOR) (const double&, const double&,
				   double&);

  int *F77_FCN (dgenunf, DGENUNF) (const double&, const double&,
				   double&);

  int *F77_FCN (setall, SETALL) (const int&, const int&);

  int *F77_FCN (getsd, GETSD) (int&, int&);
}

static double
curr_rand_seed (void)
{
  union d2i { double d; int i[2]; };
  union d2i u;
  F77_FCN (getsd, GETSD) (u.i[0], u.i[1]);
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
  F77_FCN (setall, SETALL) (i0, i1);
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

  if (nargin > 2 || nargout > 1)
    {
      print_usage ("rand");
      return retval;
    }

  static int initialized = 0;
  if (! initialized)
    {
// Make the random number generator give us a different sequence every
// time we start octave unless we specifically set the seed.  The
// technique used below will cycle monthly, but it it does seem to
// work ok to give fairly different seeds each time Octave starts.

#if 0
      int s0 = 1234567890;
      int s1 = 123456789;
#else
      time_t now;
      struct tm *tm;
 
      time (&now);
      tm = localtime (&now);
 
      int hour = tm->tm_hour + 1;
      int minute = tm->tm_min + 1;
      int second = tm->tm_sec + 1;

      int s0 = tm->tm_mday * hour * minute * second;
      int s1 = hour * minute * second;
#endif
      s0 = force_to_fit_range (s0, 1, 2147483563);
      s1 = force_to_fit_range (s1, 1, 2147483399);

      F77_FCN (setall, SETALL) (s0, s1);
      initialized = 1;
    }

  int n = 0;
  int m = 0;
  if (nargin == 0)
    {
      n = 1;
      m = 1;
      goto gen_matrix;
    }
  else if (nargin == 1)
    {
      tree_constant tmp = args(0);

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
	  double dval = tmp.double_value ();

	  if (xisnan (dval))
	    {
	      error ("rand: NaN is invalid a matrix dimension");
	    }
	  else
	    {
	      m = n = NINT (tmp.double_value ());

	      if (! error_state)
		goto gen_matrix;
	    }
	}
      else if (tmp.is_range ())
	{
	  Range r = tmp.range_value ();
	  n = 1;
	  m = r.nelem ();
	  goto gen_matrix;
	}
      else if (tmp.is_matrix_type ())
	{
// XXX FIXME XXX -- this should probably use the function from data.cc.

	  Matrix a = args(0).matrix_value ();

	  if (error_state)
	    return retval;

	  n = args(0).rows ();
	  m = args(0).columns ();

	  if (n == 1 && m == 2)
	    {
	      n = NINT (a.elem (0, 0));
	      m = NINT (a.elem (0, 1));
	    }
	  else if (n == 2 && m == 1)
	    {
	      n = NINT (a.elem (0, 0));
	      m = NINT (a.elem (1, 0));
	    }
	  else
	    warning ("rand (A): use rand (size (A)) instead");

	  goto gen_matrix;
	}
      else
	{
	  gripe_wrong_type_arg ("rand", tmp);
	  return retval;
	}
    }
  else if (nargin == 2)
    {
      if (args(0).is_string ()
	  && strcmp (args(0).string_value (), "seed") == 0)
	{
	  double d = args(1).double_value ();

	  if (! error_state)
	    set_rand_seed (d);
	}
      else
	{
	  double dval = args(0).double_value ();

	  if (xisnan (dval))
	    {
	      error ("rand: NaN is invalid as a matrix dimension");
	    }
	  else
	    {
	      n = NINT (dval);

	      if (! error_state)
		{
		  m = NINT (args(1).double_value ());

		  if (! error_state)
		    goto gen_matrix;
		}
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
	    double val;
	    switch (current_distribution)
	      {
	      case uniform:
		F77_FCN (dgenunf, DGENUNF) (0.0, 1.0, val);
		rand_mat.elem (i, j) = val;
		break;

	      case normal:
		F77_FCN (dgennor, DGENNOR) (0.0, 1.0, val);
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
