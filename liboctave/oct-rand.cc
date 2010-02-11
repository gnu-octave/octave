/*

Copyright (C) 2003, 2005, 2006, 2007, 2008 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <map>
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
#include "mach-info.h"
#include "oct-locbuf.h"

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

octave_rand *octave_rand::instance = 0;

octave_rand::octave_rand (void)
  : current_distribution (uniform_dist), use_old_generators (false),
    rand_states ()
{
  initialize_ranlib_generators ();

  initialize_mersenne_twister ();
}

bool
octave_rand::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new octave_rand ();

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create octave_rand object!");

      retval = false;
    }

  return retval;
}

double
octave_rand::do_seed (void)
{
  union d2i { double d; octave_idx_type i[2]; };
  union d2i u;
    
  oct_mach_info::float_format ff = oct_mach_info::native_float_format ();

  switch (ff)
    {
    case oct_mach_info::flt_fmt_ieee_big_endian:
      F77_FUNC (getsd, GETSD) (u.i[1], u.i[0]);
      break;
    default:
      F77_FUNC (getsd, GETSD) (u.i[0], u.i[1]);
      break;
    }

  return u.d;
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

void
octave_rand::do_seed (double s)
{
  use_old_generators = true;

  int i0, i1;
  union d2i { double d; octave_idx_type i[2]; };
  union d2i u;
  u.d = s;

  oct_mach_info::float_format ff = oct_mach_info::native_float_format ();

  switch (ff)
    {
    case oct_mach_info::flt_fmt_ieee_big_endian:
      i1 = force_to_fit_range (u.i[0], 1, 2147483563);
      i0 = force_to_fit_range (u.i[1], 1, 2147483399);
      break;
    default:
      i0 = force_to_fit_range (u.i[0], 1, 2147483563);
      i1 = force_to_fit_range (u.i[1], 1, 2147483399);
      break;
    }

  F77_FUNC (setsd, SETSD) (i0, i1);
}

ColumnVector
octave_rand::do_state (const std::string& d)
{
  return rand_states[d.empty () ? current_distribution : get_dist_id (d)];
}

void
octave_rand::do_state (const ColumnVector& s, const std::string& d)
{
  use_old_generators = false;

  int old_dist = current_distribution;

  int new_dist = d.empty () ? current_distribution : get_dist_id (d);

  ColumnVector saved_state;

  if (old_dist != new_dist)
    saved_state = get_internal_state ();

  set_internal_state (s);

  rand_states[new_dist] = get_internal_state ();

  if (old_dist != new_dist)
    rand_states[old_dist] = saved_state;
}

std::string
octave_rand::do_distribution (void)
{
  std::string retval;

  switch (current_distribution)
    {
    case uniform_dist:
      retval = "uniform";
      break;

    case normal_dist:
      retval = "normal";
      break;

    case expon_dist:
      retval = "exponential";
      break;

    case poisson_dist:
      retval = "poisson";
      break;

    case gamma_dist:
      retval = "gamma";
      break;

    default:
      (*current_liboctave_error_handler)
        ("rand: invalid distribution ID = %d", current_distribution);
      break;
    }

  return retval;
}

void
octave_rand::do_distribution (const std::string& d)
{
  int id = get_dist_id (d);

  switch (id)
    {
    case uniform_dist:
      octave_rand::uniform_distribution ();
      break;

    case normal_dist:
      octave_rand::normal_distribution ();
      break;

    case expon_dist:
      octave_rand::exponential_distribution ();
      break;

    case poisson_dist:
      octave_rand::poisson_distribution ();
      break;

    case gamma_dist:
      octave_rand::gamma_distribution ();
      break;

    default:
      (*current_liboctave_error_handler)
        ("rand: invalid distribution ID = %d", id);
      break;
    }
}

void
octave_rand::do_uniform_distribution (void)
{
  switch_to_generator (uniform_dist);

  F77_FUNC (setcgn, SETCGN) (uniform_dist);
}

void
octave_rand::do_normal_distribution (void)
{
  switch_to_generator (normal_dist);

  F77_FUNC (setcgn, SETCGN) (normal_dist);
}

void
octave_rand::do_exponential_distribution (void)
{
  switch_to_generator (expon_dist);

  F77_FUNC (setcgn, SETCGN) (expon_dist);
}

void
octave_rand::do_poisson_distribution (void)
{
  switch_to_generator (poisson_dist);

  F77_FUNC (setcgn, SETCGN) (poisson_dist);
}

void
octave_rand::do_gamma_distribution (void)
{
  switch_to_generator (gamma_dist);

  F77_FUNC (setcgn, SETCGN) (gamma_dist);
}


double
octave_rand::do_scalar (double a)
{
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
          (*current_liboctave_error_handler) 
            ("rand: invalid distribution ID = %d", current_distribution);
          break;
        }
    }
  else
    {
      switch (current_distribution)
        {
        case uniform_dist:
          retval = oct_randu ();
          break;

        case normal_dist:
          retval = oct_randn ();
          break;

        case expon_dist:
          retval = oct_rande ();
          break;

        case poisson_dist:
          retval = oct_randp (a);
          break;

        case gamma_dist:
          retval = oct_randg (a);
          break;

        default:
          (*current_liboctave_error_handler)
            ("rand: invalid distribution ID = %d", current_distribution);
          break;
        }

      save_state ();
    }

  return retval;
}

Matrix
octave_rand::do_matrix (octave_idx_type n, octave_idx_type m, double a)
{
  Matrix retval;

  if (n >= 0 && m >= 0)
    {
      retval.clear (n, m);

      if (n > 0 && m > 0)
        fill (retval.capacity(), retval.fortran_vec(), a);
    }
  else
    (*current_liboctave_error_handler) ("rand: invalid negative argument");

  return retval;
}

NDArray
octave_rand::do_nd_array (const dim_vector& dims, double a)
{
  NDArray retval;

  if (! dims.all_zero ())
    {
      retval.clear (dims);

      fill (retval.capacity(), retval.fortran_vec(), a);
    }

  return retval;
}

Array<double>
octave_rand::do_vector (octave_idx_type n, double a)
{
  Array<double> retval;

  if (n > 0)
    {
      retval.clear (n);

      fill (retval.capacity (), retval.fortran_vec (), a);
    }
  else if (n < 0)
    (*current_liboctave_error_handler) ("rand: invalid negative argument");

  return retval;
}

// Make the random number generator give us a different sequence every
// time we start octave unless we specifically set the seed.  The
// technique used below will cycle monthly, but it it does seem to
// work ok to give fairly different seeds each time Octave starts.

void
octave_rand::initialize_ranlib_generators (void)
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
}

void
octave_rand::initialize_mersenne_twister (void)
{
  oct_init_by_entropy ();

  ColumnVector s = get_internal_state ();

  rand_states[uniform_dist] = s;

  oct_init_by_entropy ();
  s = get_internal_state ();
  rand_states[normal_dist] = s;

  oct_init_by_entropy ();
  s = get_internal_state ();
  rand_states[expon_dist] = s;

  oct_init_by_entropy ();
  s = get_internal_state ();
  rand_states[poisson_dist] = s;

  oct_init_by_entropy ();
  s = get_internal_state ();
  rand_states[gamma_dist] = s;
}

ColumnVector
octave_rand::get_internal_state (void)
{
  ColumnVector s (MT_N + 1);

  OCTAVE_LOCAL_BUFFER (uint32_t, tmp, MT_N + 1);

  oct_get_state (tmp);

  for (octave_idx_type i = 0; i <= MT_N; i++)
    s.elem (i) = static_cast<double> (tmp [i]);

  return s;
}

void
octave_rand::save_state (void)
{
  rand_states[current_distribution] = get_internal_state ();;
}

int
octave_rand::get_dist_id (const std::string& d)
{
  int retval = unknown_dist;

  if (d == "uniform" || d == "rand")
    retval = uniform_dist;
  else if (d == "normal" || d == "randn")
    retval = normal_dist;
  else if (d == "exponential" || d == "rande")
    retval = expon_dist;
  else if (d == "poisson" || d == "randp")
    retval = poisson_dist;
  else if (d == "gamma" || d == "randg")
    retval = gamma_dist;
  else
    (*current_liboctave_error_handler)
      ("rand: invalid distribution `%s'", d.c_str ());

  return retval;
}

void
octave_rand::set_internal_state (const ColumnVector& s)
{
  octave_idx_type len = s.length ();
  octave_idx_type n = len < MT_N + 1 ? len : MT_N + 1;

  OCTAVE_LOCAL_BUFFER (uint32_t, tmp, MT_N + 1);

  for (octave_idx_type i = 0; i < n; i++)
    tmp[i] = static_cast<uint32_t> (s.elem(i));

  if (len == MT_N + 1 && tmp[MT_N] <= MT_N && tmp[MT_N] > 0)
    oct_set_state (tmp);
  else
    oct_init_by_array (tmp, len);
}

void
octave_rand::switch_to_generator (int dist)
{
  if (dist != current_distribution)
    {
      current_distribution = dist;

      set_internal_state (rand_states[dist]);
    }
}

#define MAKE_RAND(len) \
  do \
    { \
      double val; \
      for (volatile octave_idx_type i = 0; i < len; i++) \
        { \
          octave_quit (); \
          RAND_FUNC (val); \
          v[i] = val; \
        } \
    } \
  while (0)

void
octave_rand::fill (octave_idx_type len, double *v, double a)
{
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
      (*current_liboctave_error_handler)
        ("rand: invalid distribution ID = %d", current_distribution);
      break;
    }

  save_state ();

  return;
}
