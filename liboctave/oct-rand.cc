/*

Copyright (C) 2003 John W. Eaton

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

#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-rand.h"
#include "oct-time.h"

// Possible distributions of random numbers.  This was handled with an
// enum, but unwind_protecting that doesn't work so well.
#define uniform_dist 1
#define normal_dist 2

// Current distribution of random numbers.
static int current_distribution = uniform_dist;

// Has the seed been set yet?
static bool initialized = false;

extern "C"
{
  F77_RET_T
  F77_FUNC (dgennor, DGENNOR) (const double&, const double&, double&);

  F77_RET_T
  F77_FUNC (dgenunf, DGENUNF) (const double&, const double&, double&);

  F77_RET_T
  F77_FUNC (setall, SETALL) (const octave_idx_type&, const octave_idx_type&);

  F77_RET_T
  F77_FUNC (getsd, GETSD) (octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (setsd, SETSD) (const octave_idx_type&, const octave_idx_type&);

  F77_RET_T
  F77_FUNC (setcgn, SETCGN) (const octave_idx_type&);
}

static octave_idx_type
force_to_fit_range (octave_idx_type i, octave_idx_type lo, octave_idx_type hi)
{
  assert (hi > lo && lo >= 0 && hi > lo);

  i = i > 0 ? i : -i;

  if (i < lo)
    i = lo;
  else if (i > hi)
    i = i % hi;

  return i;
}

// Make the random number generator give us a different sequence every
// time we start octave unless we specifically set the seed.  The
// technique used below will cycle monthly, but it it does seem to
// work ok to give fairly different seeds each time Octave starts.

static void
do_initialization (void)
{
  octave_localtime tm;
 
  int hour = tm.hour() + 1;
  int minute = tm.min() + 1;
  int second = tm.sec() + 1;

  octave_idx_type s0 = tm.mday() * hour * minute * second;
  octave_idx_type s1 = hour * minute * second;

  s0 = force_to_fit_range (s0, 1, 2147483563);
  s1 = force_to_fit_range (s1, 1, 2147483399);

  F77_FUNC (setall, SETALL) (s0, s1);

  initialized = true;
}

static inline void
maybe_initialize (void)
{
  if (! initialized)
    do_initialization ();
}

double
octave_rand::seed (void)
{
  maybe_initialize ();

  union d2i { double d; octave_idx_type i[2]; };
  union d2i u;
  F77_FUNC (getsd, GETSD) (u.i[0], u.i[1]);
  return u.d;
}

void
octave_rand::seed (double s)
{
  maybe_initialize ();

  union d2i { double d; octave_idx_type i[2]; };
  union d2i u;
  u.d = s;
  int i0 = force_to_fit_range (u.i[0], 1, 2147483563);
  int i1 = force_to_fit_range (u.i[1], 1, 2147483399);
  F77_FUNC (setsd, SETSD) (i0, i1);
}

std::string
octave_rand::distribution (void)
{
  maybe_initialize ();

  if (current_distribution == uniform_dist)
    return "uniform";
  else if (current_distribution == normal_dist)
    return "normal";
  else
    {
      abort ();
      return "";
    }
}

void
octave_rand::distribution (const std::string& d)
{
  if (d == "uniform")
    octave_rand::uniform_distribution ();
  else if (d == "normal")
    octave_rand::normal_distribution ();
  else
    (*current_liboctave_error_handler) ("rand: invalid distribution");
}

void
octave_rand::uniform_distribution (void)
{
  maybe_initialize ();

  current_distribution = uniform_dist;

  F77_FUNC (setcgn, SETCGN) (uniform_dist);
}

void
octave_rand::normal_distribution (void)
{
  maybe_initialize ();

  current_distribution = normal_dist;

  F77_FUNC (setcgn, SETCGN) (normal_dist);
}

double
octave_rand::scalar (void)
{
  maybe_initialize ();

  double retval = 0.0;

  switch (current_distribution)
    {
    case uniform_dist:
      F77_FUNC (dgenunf, DGENUNF) (0.0, 1.0, retval);
      break;

    case normal_dist:
      F77_FUNC (dgennor, DGENNOR) (0.0, 1.0, retval);
      break;

    default:
      abort ();
      break;
    }

  return retval;
}

#define MAKE_RAND_MAT(mat, nr, nc, f, F) \
  do \
    { \
      double val; \
      for (volatile octave_idx_type j = 0; j < nc; j++) \
	for (volatile octave_idx_type i = 0; i < nr; i++) \
	  { \
	    OCTAVE_QUIT; \
	    F77_FUNC (f, F) (0.0, 1.0, val); \
	    mat(i,j) = val; \
	  } \
    } \
  while (0)

Matrix
octave_rand::matrix (octave_idx_type n, octave_idx_type m)
{
  maybe_initialize ();

  Matrix retval;

  if (n >= 0 && m >= 0)
    {
      retval.resize (n, m);

      if (n > 0 && m > 0)
	{
	  switch (current_distribution)
	    {
	    case uniform_dist:
	      MAKE_RAND_MAT (retval, n, m, dgenunf, DGENUNF);
	      break;

	    case normal_dist:
	      MAKE_RAND_MAT (retval, n, m, dgennor, DGENNOR);
	      break;

	    default:
	      abort ();
	      break;
	    }
	}
    }
  else
    (*current_liboctave_error_handler) ("rand: invalid negative argument");

  return retval;
}

#define MAKE_RAND_ND_ARRAY(mat, len, f, F) \
  do \
    { \
      double val; \
      for (volatile octave_idx_type i = 0; i < len; i++) \
	{ \
	  OCTAVE_QUIT; \
	  F77_FUNC (f, F) (0.0, 1.0, val); \
	  mat(i) = val; \
	} \
    } \
  while (0)

NDArray
octave_rand::nd_array (const dim_vector& dims)
{
  maybe_initialize ();

  NDArray retval;

  if (! dims.all_zero ())
    {
      retval.resize (dims);

      octave_idx_type len = retval.length ();

      switch (current_distribution)
	{
	case uniform_dist:
	  MAKE_RAND_ND_ARRAY (retval, len, dgenunf, DGENUNF);
	  break;

	case normal_dist:
	  MAKE_RAND_ND_ARRAY (retval, len, dgennor, DGENNOR);
	  break;

	default:
	  abort ();
	  break;
	}
    }

  return retval;
}

#define MAKE_RAND_ARRAY(array, n, f, F) \
  do \
    { \
      double val; \
      for (volatile octave_idx_type i = 0; i < n; i++) \
	{ \
	  OCTAVE_QUIT; \
	  F77_FUNC (f, F) (0.0, 1.0, val); \
	  array(i) = val; \
	} \
    } \
  while (0)

Array<double>
octave_rand::vector (octave_idx_type n)
{
  maybe_initialize ();

  Array<double> retval;

  if (n > 0)
    {
      retval.resize (n);

      switch (current_distribution)
	{
	case uniform_dist:
	  MAKE_RAND_ARRAY (retval, n, dgenunf, DGENUNF);
	  break;

	case normal_dist:
	  MAKE_RAND_ARRAY (retval, n, dgennor, DGENNOR);
	  break;

	default:
	  abort ();
	  break;
	}
    }
  else if (n < 0)
    (*current_liboctave_error_handler) ("rand: invalid negative argument");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
