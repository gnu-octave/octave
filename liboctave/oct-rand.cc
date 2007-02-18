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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <vector>

#include "f77-fcn.h"
#include "lo-ieee.h"
#include "lo-error.h"
#include "lo-mappers.h"
#include "oct-rand.h"
#include "oct-time.h"
#include "data-conv.h"
#include "randmtzig.h"
#include "randpoisson.h"
#include "randgamma.h"

// Possible distributions of random numbers.  This was handled with an
// enum, but unwind_protecting that doesn't work so well.
#define uniform_dist 1
#define normal_dist 2
#define expon_dist 3
#define poisson_dist 4
#define gamma_dist 5

// Current distribution of random numbers.
static int current_distribution = uniform_dist;

// Has the seed/state been set yet?
static bool old_initialized = false;
static bool new_initialized = false;
static bool use_old_generators = false;

extern "C"
{
  F77_RET_T
  F77_FUNC (dgennor, DGENNOR) (const double&, const double&, double&);

  F77_RET_T
  F77_FUNC (dgenunf, DGENUNF) (const double&, const double&, double&);

  F77_RET_T
  F77_FUNC (dgenexp, DGENEXP) (const double&, double&);

  F77_RET_T
  F77_FUNC (dignpoi, DIGNPOI) (const double&, double&);

  F77_RET_T
  F77_FUNC (dgengam, DGENGAM) (const double&, const double&, double&);

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
do_old_initialization (void)
{
  octave_localtime tm;
  int stored_distribution = current_distribution;
  F77_FUNC (setcgn, SETCGN) (uniform_dist);

  int hour = tm.hour() + 1;
  int minute = tm.min() + 1;
  int second = tm.sec() + 1;

  octave_idx_type s0 = tm.mday() * hour * minute * second;
  octave_idx_type s1 = hour * minute * second;

  s0 = force_to_fit_range (s0, 1, 2147483563);
  s1 = force_to_fit_range (s1, 1, 2147483399);

  F77_FUNC (setall, SETALL) (s0, s1);
  F77_FUNC (setcgn, SETCGN) (stored_distribution);

  old_initialized = true;
}

static inline void
maybe_initialize (void)
{
  if (use_old_generators)
    {
      if (! old_initialized)
	do_old_initialization ();
    }
  else
    {
      if (! new_initialized)
	{
	  oct_init_by_entropy ();
	  new_initialized = true;
	}
    }
}

double
octave_rand::seed (void)
{
  if (! old_initialized)
    do_old_initialization ();

  union d2i { double d; octave_idx_type i[2]; };
  union d2i u;
  F77_FUNC (getsd, GETSD) (u.i[0], u.i[1]);
  return u.d;
}

void
octave_rand::seed (double s)
{
  use_old_generators = true;
  maybe_initialize ();

  union d2i { double d; octave_idx_type i[2]; };
  union d2i u;
  u.d = s;
  int i0 = force_to_fit_range (u.i[0], 1, 2147483563);
  int i1 = force_to_fit_range (u.i[1], 1, 2147483399);
  F77_FUNC (setsd, SETSD) (i0, i1);
}

ColumnVector
octave_rand::state (void)
{
  ColumnVector s (MT_N + 1);
  if (! new_initialized)
    {
      oct_init_by_entropy ();
      new_initialized = true;
    }

  OCTAVE_LOCAL_BUFFER (uint32_t, tmp, MT_N + 1);
  oct_get_state (tmp);
  for (octave_idx_type i = 0; i <= MT_N; i++)
    s.elem (i) = static_cast<double>(tmp [i]);
  return s;
}

void
octave_rand::state (const ColumnVector &s)
{
  use_old_generators = false;
  maybe_initialize ();

  octave_idx_type len = s.length();
  octave_idx_type n = len < MT_N + 1 ? len : MT_N + 1;
  OCTAVE_LOCAL_BUFFER (uint32_t, tmp, MT_N + 1);
  for (octave_idx_type i = 0; i < n; i++)
    tmp[i] = static_cast<uint32_t> (s.elem(i));

  if (len == MT_N + 1 && tmp[MT_N] <= MT_N && tmp[MT_N] > 0)
    oct_set_state (tmp);
  else
    oct_init_by_array (tmp, len);
}

std::string
octave_rand::distribution (void)
{
  maybe_initialize ();

  if (current_distribution == uniform_dist)
    return "uniform";
  else if (current_distribution == normal_dist)
    return "normal";
  else if (current_distribution == expon_dist)
    return "exponential";
  else if (current_distribution == poisson_dist)
    return "poisson";
  else if (current_distribution == gamma_dist)
    return "gamma";
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
  else if (d == "exponential")
    octave_rand::exponential_distribution ();
  else if (d == "poisson")
    octave_rand::poisson_distribution ();
  else if (d == "gamma")
    octave_rand::gamma_distribution ();
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

void
octave_rand::exponential_distribution (void)
{
  maybe_initialize ();

  current_distribution = expon_dist;

  F77_FUNC (setcgn, SETCGN) (expon_dist);
}

void
octave_rand::poisson_distribution (void)
{
  maybe_initialize ();

  current_distribution = poisson_dist;

  F77_FUNC (setcgn, SETCGN) (poisson_dist);
}

void
octave_rand::gamma_distribution (void)
{
  maybe_initialize ();

  current_distribution = gamma_dist;

  F77_FUNC (setcgn, SETCGN) (gamma_dist);
}


double
octave_rand::scalar (double a)
{
  maybe_initialize ();

  double retval = 0.0;

  if (use_old_generators)
    {
      switch (current_distribution)
	{
	case uniform_dist:
	  F77_FUNC (dgenunf, DGENUNF) (0.0, 1.0, retval);
	  break;

	case normal_dist:
	  F77_FUNC (dgennor, DGENNOR) (0.0, 1.0, retval);
	  break;

	case expon_dist:
	  F77_FUNC (dgenexp, DGENEXP) (1.0, retval);
	  break;

	case poisson_dist:
	  if (a < 0.0 || xisnan(a) || xisinf(a))
	    retval = octave_NaN;
	  else
	    {
	      // workaround bug in ignpoi, by calling with different Mu
	      F77_FUNC (dignpoi, DIGNPOI) (a + 1, retval);
	      F77_FUNC (dignpoi, DIGNPOI) (a, retval);
	    }
	  break;

	case gamma_dist:
	  if (a <= 0.0 || xisnan(a) || xisinf(a))
	    retval = octave_NaN;
	  else
	    F77_FUNC (dgengam, DGENGAM) (1.0, a, retval);
	  break;

	default:
	  abort ();
	  break;
	}
    }
  else
    {
      switch (current_distribution)
	{
	case uniform_dist:
	  retval = oct_randu();
	  break;

	case normal_dist:
	  retval = oct_randn();
	  break;

	case expon_dist:
	  retval = oct_rande();
	  break;

	case poisson_dist:
	  retval = oct_randp(a);
	  break;

	case gamma_dist:
	  retval = oct_randg(a);
	  break;

	default:
	  abort ();
	  break;
	}
    }

  return retval;
}

#define MAKE_RAND(len) \
  do \
    { \
      double val; \
      for (volatile octave_idx_type i = 0; i < len; i++) \
	{ \
	  OCTAVE_QUIT; \
	  RAND_FUNC (val); \
	  v[i] = val; \
	} \
    } \
  while (0)

static void
fill_rand (octave_idx_type len, double *v, double a)
{
  maybe_initialize ();

  if (len < 1)
    return;

  switch (current_distribution)
    {
    case uniform_dist:
      if (use_old_generators)
	{
#define RAND_FUNC(x) F77_FUNC (dgenunf, DGENUNF) (0.0, 1.0, x)
	  MAKE_RAND (len);
#undef RAND_FUNC
	}
      else
	oct_fill_randu (len, v);
      break;

    case normal_dist:
      if (use_old_generators)
	{
#define RAND_FUNC(x) F77_FUNC (dgennor, DGENNOR) (0.0, 1.0, x)
	  MAKE_RAND (len);
#undef RAND_FUNC
	}
      else
	oct_fill_randn (len, v);
      break;

    case expon_dist:
      if (use_old_generators)
	{
#define RAND_FUNC(x) F77_FUNC (dgenexp, DGENEXP) (1.0, x)
	  MAKE_RAND (len);
#undef RAND_FUNC
	}
      else
	oct_fill_rande (len, v);
      break;

    case poisson_dist:
      if (use_old_generators)
	{
	  if (a < 0.0 || xisnan(a) || xisinf(a))
#define RAND_FUNC(x) x = octave_NaN;
	    MAKE_RAND (len);
#undef RAND_FUNC
	  else
	    {
	      // workaround bug in ignpoi, by calling with different Mu
	      double tmp;
	      F77_FUNC (dignpoi, DIGNPOI) (a + 1, tmp);
#define RAND_FUNC(x) F77_FUNC (dignpoi, DIGNPOI) (a, x)
		MAKE_RAND (len);
#undef RAND_FUNC
	    }
	}
      else
	oct_fill_randp (a, len, v);
      break;

    case gamma_dist:
      if (use_old_generators)
	{
	  if (a <= 0.0 || xisnan(a) || xisinf(a))
#define RAND_FUNC(x) x = octave_NaN;
	    MAKE_RAND (len);
#undef RAND_FUNC
	  else
#define RAND_FUNC(x) F77_FUNC (dgengam, DGENGAM) (1.0, a, x)
	    MAKE_RAND (len);
#undef RAND_FUNC
	}
      else
	oct_fill_randg (a, len, v);
      break;

    default:
      abort ();
      break;
    }

  return;
}

Matrix
octave_rand::matrix (octave_idx_type n, octave_idx_type m, double a)
{
  Matrix retval;

  if (n >= 0 && m >= 0)
    {
      retval.resize (n, m);

      if (n > 0 && m > 0)
	fill_rand (retval.capacity(), retval.fortran_vec(), a);
    }
  else
    (*current_liboctave_error_handler) ("rand: invalid negative argument");

  return retval;
}

NDArray
octave_rand::nd_array (const dim_vector& dims, double a)
{
  NDArray retval;

  if (! dims.all_zero ())
    {
      retval.resize (dims);

      fill_rand (retval.capacity(), retval.fortran_vec(), a);
    }

  return retval;
}

Array<double>
octave_rand::vector (octave_idx_type n, double a)
{
  maybe_initialize ();

  Array<double> retval;

  if (n > 0)
    {
      retval.resize (n);

      fill_rand (retval.capacity(), retval.fortran_vec(), a);
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
