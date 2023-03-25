////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cassert>
#include <cstdint>

#include <limits>

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-ranlib-proto.h"
#include "mach-info.h"
#include "oct-locbuf.h"
#include "oct-rand.h"
#include "oct-time.h"
#include "quit.h"
#include "randgamma.h"
#include "randmtzig.h"
#include "randpoisson.h"
#include "singleton-cleanup.h"

OCTAVE_BEGIN_NAMESPACE(octave)

rand *rand::m_instance = nullptr;

rand::rand (void)
  : m_current_distribution (uniform_dist), m_use_old_generators (false),
    m_rand_states ()
{
  initialize_ranlib_generators ();

  initialize_mersenne_twister ();
}

bool rand::instance_ok (void)
{
  bool retval = true;

  if (! m_instance)
    {
      m_instance = new rand ();
      singleton_cleanup_list::add (cleanup_instance);
    }

  return retval;
}

double rand::do_seed (void)
{
  union d2i { double d; int32_t i[2]; };
  union d2i u;

  mach_info::float_format ff = mach_info::native_float_format ();

  switch (ff)
    {
    case mach_info::flt_fmt_ieee_big_endian:
      F77_FUNC (getsd, GETSD) (u.i[1], u.i[0]);
      break;

    default:
      F77_FUNC (getsd, GETSD) (u.i[0], u.i[1]);
      break;
    }

  return u.d;
}

static int32_t
force_to_fit_range (int32_t i, int32_t lo, int32_t hi)
{
  assert (hi > lo && lo >= 0);

  i = (i > 0 ? i : -i);

  if (i < lo)
    i = lo;
  else if (i > hi)
    i = i % hi;

  return i;
}

void rand::do_seed (double s)
{
  m_use_old_generators = true;

  int i0, i1;
  union d2i { double d; int32_t i[2]; };
  union d2i u;
  u.d = s;

  mach_info::float_format ff = mach_info::native_float_format ();

  switch (ff)
    {
    case mach_info::flt_fmt_ieee_big_endian:
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

void rand::do_reset (void)
{
  m_use_old_generators = true;
  initialize_ranlib_generators ();
}

uint32NDArray rand::do_state (const std::string& d)
{
  return m_rand_states[d.empty () ? m_current_distribution : get_dist_id (d)];
}

void rand::do_state (const uint32NDArray& s, const std::string& d)
{
  m_use_old_generators = false;

  int old_dist = m_current_distribution;

  int new_dist = (d.empty () ? m_current_distribution : get_dist_id (d));

  uint32NDArray saved_state;

  if (old_dist != new_dist)
    saved_state = get_internal_state ();

  set_internal_state (s);

  m_rand_states[new_dist] = get_internal_state ();

  if (old_dist != new_dist)
    m_rand_states[old_dist] = saved_state;
}

void rand::do_reset (const std::string& d)
{
  m_use_old_generators = false;

  int old_dist = m_current_distribution;

  int new_dist = (d.empty () ? m_current_distribution : get_dist_id (d));

  uint32NDArray saved_state;

  if (old_dist != new_dist)
    saved_state = get_internal_state ();

  init_mersenne_twister ();
  m_rand_states[new_dist] = get_internal_state ();

  if (old_dist != new_dist)
    m_rand_states[old_dist] = saved_state;
}

std::string rand::do_distribution (void)
{
  std::string retval;

  switch (m_current_distribution)
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
        ("rand: invalid distribution ID = %d", m_current_distribution);
      break;
    }

  return retval;
}

void rand::do_distribution (const std::string& d)
{
  int id = get_dist_id (d);

  switch (id)
    {
    case uniform_dist:
      rand::uniform_distribution ();
      break;

    case normal_dist:
      rand::normal_distribution ();
      break;

    case expon_dist:
      rand::exponential_distribution ();
      break;

    case poisson_dist:
      rand::poisson_distribution ();
      break;

    case gamma_dist:
      rand::gamma_distribution ();
      break;

    default:
      (*current_liboctave_error_handler)
        ("rand: invalid distribution ID = %d", id);
      break;
    }
}

void rand::do_uniform_distribution (void)
{
  switch_to_generator (uniform_dist);

  F77_FUNC (setcgn, SETCGN) (uniform_dist);
}

void rand::do_normal_distribution (void)
{
  switch_to_generator (normal_dist);

  F77_FUNC (setcgn, SETCGN) (normal_dist);
}

void rand::do_exponential_distribution (void)
{
  switch_to_generator (expon_dist);

  F77_FUNC (setcgn, SETCGN) (expon_dist);
}

void rand::do_poisson_distribution (void)
{
  switch_to_generator (poisson_dist);

  F77_FUNC (setcgn, SETCGN) (poisson_dist);
}

void rand::do_gamma_distribution (void)
{
  switch_to_generator (gamma_dist);

  F77_FUNC (setcgn, SETCGN) (gamma_dist);
}

template <>
OCTAVE_API double rand::uniform<double> (void)
{
  double retval;

  if (m_use_old_generators)
    F77_FUNC (dgenunf, DGENUNF) (0.0, 1.0, retval);
  else
    retval = rand_uniform<double> ();

  return retval;
}

template <>
OCTAVE_API double rand::normal<double> (void)
{
  double retval;

  if (m_use_old_generators)
    F77_FUNC (dgennor, DGENNOR) (0.0, 1.0, retval);
  else
    retval = rand_normal<double> ();

  return retval;
}

template <>
OCTAVE_API double rand::exponential<double> (void)
{
  double retval;

  if (m_use_old_generators)
    F77_FUNC (dgenexp, DGENEXP) (1.0, retval);
  else
    retval = rand_exponential<double> ();

  return retval;
}

template <>
OCTAVE_API double rand::poisson<double> (double a)
{
  double retval;

  if (m_use_old_generators)
    {
      if (a < 0.0 || ! math::isfinite (a))
        retval = numeric_limits<double>::NaN ();
      else
        {
          // workaround bug in ignpoi, by calling with different Mu
          F77_FUNC (dignpoi, DIGNPOI) (a + 1, retval);
          F77_FUNC (dignpoi, DIGNPOI) (a, retval);
        }
    }
  else
    retval = rand_poisson<double> (a);

  return retval;
}

template <>
OCTAVE_API double rand::gamma<double> (double a)
{
  double retval;

  if (m_use_old_generators)
    {
      if (a <= 0.0 || ! math::isfinite (a))
        retval = numeric_limits<double>::NaN ();
      else
        F77_FUNC (dgengam, DGENGAM) (1.0, a, retval);
    }
  else
    retval = rand_gamma<double> (a);

  return retval;
}

template <>
OCTAVE_API float rand::uniform<float> (void)
{
  float retval;

  if (m_use_old_generators)
    F77_FUNC (fgenunf, FGENUNF) (0.0f, 1.0f, retval);
  else
    retval = rand_uniform<float> ();

  return retval;
}

template <>
OCTAVE_API float rand::normal<float> (void)
{
  float retval;

  if (m_use_old_generators)
    F77_FUNC (fgennor, FGENNOR) (0.0f, 1.0f, retval);
  else
    retval = rand_normal<float> ();

  return retval;
}

template <>
OCTAVE_API float rand::exponential<float> (void)
{
  float retval;

  if (m_use_old_generators)
    F77_FUNC (fgenexp, FGENEXP) (1.0f, retval);
  else
    retval = rand_exponential<float> ();

  return retval;
}

template <>
OCTAVE_API float rand::poisson<float> (float a)
{
  float retval;

  if (m_use_old_generators)
    {
      if (a < 0.0f || ! math::isfinite (a))
        retval = numeric_limits<float>::NaN ();
      else
        {
          // workaround bug in ignpoi, by calling with different Mu
          F77_FUNC (fignpoi, FIGNPOI) (a + 1, retval);
          F77_FUNC (fignpoi, FIGNPOI) (a, retval);
        }
    }
  else
    {
      // Keep poisson distribution in double precision for accuracy
      retval = rand_poisson<double> (a);
    }

  return retval;
}

template <>
OCTAVE_API float rand::gamma<float> (float a)
{
  float retval;

  if (m_use_old_generators)
    {
      if (a <= 0.0f || ! math::isfinite (a))
        retval = numeric_limits<float>::NaN ();
      else
        F77_FUNC (fgengam, FGENGAM) (1.0f, a, retval);
    }
  else
    retval = rand_gamma<float> (a);

  return retval;
}

template <typename T>
T rand::do_scalar (T a)
{
  T retval = 0;

  switch (m_current_distribution)
    {
    case uniform_dist:
      retval = uniform<T> ();
      break;

    case normal_dist:
      retval = normal<T> ();
      break;

    case expon_dist:
      retval = exponential<T> ();
      break;

    case poisson_dist:
      retval = poisson<T> (a);
      break;

    case gamma_dist:
      retval = gamma<T> (a);
      break;

    default:
      (*current_liboctave_error_handler)
        ("rand: invalid distribution ID = %d", m_current_distribution);
      break;
    }

  if (! m_use_old_generators)
    save_state ();

  return retval;
}

template OCTAVE_API double rand::do_scalar<double> (double);
template OCTAVE_API float rand::do_scalar<float> (float);

template <typename T>
Array<T>
rand::do_vector (octave_idx_type n, T a)
{
  Array<T> retval;

  if (n > 0)
    {
      retval.clear (n, 1);

      fill (retval.numel (), retval.fortran_vec (), a);
    }
  else if (n < 0)
    (*current_liboctave_error_handler) ("rand: invalid negative argument");

  return retval;
}

template OCTAVE_API Array<double>
rand::do_vector<double> (octave_idx_type, double);
template OCTAVE_API Array<float>
rand::do_vector<float> (octave_idx_type, float);

NDArray rand::do_nd_array (const dim_vector& dims, double a)
{
  NDArray retval;

  if (! dims.all_zero ())
    {
      retval.clear (dims);

      fill (retval.numel (), retval.fortran_vec (), a);
    }

  return retval;
}

FloatNDArray rand::do_float_nd_array (const dim_vector& dims, float a)
{
  FloatNDArray retval;

  if (! dims.all_zero ())
    {
      retval.clear (dims);

      fill (retval.numel (), retval.fortran_vec (), a);
    }

  return retval;
}

// Make the random number generator give us a different sequence every
// time we start octave unless we specifically set the seed.  The
// technique used below will cycle monthly, but it does seem to
// work ok to give fairly different seeds each time Octave starts.

void rand::initialize_ranlib_generators (void)
{
  sys::localtime tm;
  int stored_distribution = m_current_distribution;
  F77_FUNC (setcgn, SETCGN) (uniform_dist);

  int hour = tm.hour () + 1;
  int minute = tm.min () + 1;
  int second = tm.sec () + 1;

  int32_t s0 = tm.mday () * hour * minute * second;
  int32_t s1 = hour * minute * second;

  s0 = force_to_fit_range (s0, 1, 2147483563);
  s1 = force_to_fit_range (s1, 1, 2147483399);

  F77_FUNC (setall, SETALL) (s0, s1);
  F77_FUNC (setcgn, SETCGN) (stored_distribution);
}

void rand::initialize_mersenne_twister (void)
{
  uint32NDArray s;

  init_mersenne_twister ();
  s = get_internal_state ();
  m_rand_states[uniform_dist] = s;

  init_mersenne_twister ();
  s = get_internal_state ();
  m_rand_states[normal_dist] = s;

  init_mersenne_twister ();
  s = get_internal_state ();
  m_rand_states[expon_dist] = s;

  init_mersenne_twister ();
  s = get_internal_state ();
  m_rand_states[poisson_dist] = s;

  init_mersenne_twister ();
  s = get_internal_state ();
  m_rand_states[gamma_dist] = s;

  // All of the initializations above have messed with the internal state.
  // Restore the state of the currently selected distribution.
  set_internal_state (m_rand_states[m_current_distribution]);
}

uint32NDArray rand::get_internal_state (void)
{
  uint32NDArray s (dim_vector (MT_N + 1, 1));

  get_mersenne_twister_state (reinterpret_cast<uint32_t *> (s.fortran_vec ()));

  return s;
}

void rand::save_state (void)
{
  m_rand_states[m_current_distribution] = get_internal_state ();
}

int rand::get_dist_id (const std::string& d)
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
      ("rand: invalid distribution '%s'", d.c_str ());

  return retval;
}

void rand::set_internal_state (const uint32NDArray& s)
{
  octave_idx_type len = s.numel ();

  const uint32_t *sdata = reinterpret_cast <const uint32_t *> (s.data ());

  if (len == MT_N + 1 && sdata[MT_N] <= MT_N && sdata[MT_N] > 0)
    set_mersenne_twister_state (sdata);
  else
    init_mersenne_twister (sdata, len);
}

void rand::switch_to_generator (int dist)
{
  if (dist != m_current_distribution)
    {
      m_current_distribution = dist;

      set_internal_state (m_rand_states[dist]);
    }
}

void rand::fill (octave_idx_type len, double *v, double a)
{
  if (len < 1)
    return;

  switch (m_current_distribution)
    {
    case uniform_dist:
      if (m_use_old_generators)
        std::generate_n (v, len, [](void) { double x; F77_FUNC (dgenunf, DGENUNF) (0.0, 1.0, x); return x; });
      else
        rand_uniform<double> (len, v);
      break;

    case normal_dist:
      if (m_use_old_generators)
        std::generate_n (v, len, [](void) { double x; F77_FUNC (dgennor, DGENNOR) (0.0, 1.0, x); return x; });
      else
        rand_normal<double> (len, v);
      break;

    case expon_dist:
      if (m_use_old_generators)
        std::generate_n (v, len, [](void) { double x; F77_FUNC (dgenexp, DGENEXP) (1.0, x); return x; });
      else
        rand_exponential<double> (len, v);
      break;

    case poisson_dist:
      if (m_use_old_generators)
        {
          if (a < 0.0 || ! math::isfinite (a))
            std::fill_n (v, len, numeric_limits<double>::NaN ());
          else
            {
              // workaround bug in ignpoi, by calling with different Mu
              double tmp;
              F77_FUNC (dignpoi, DIGNPOI) (a + 1, tmp);
              std::generate_n (v, len, [a](void) { double x; F77_FUNC (dignpoi, DIGNPOI) (a, x); return x; });
            }
        }
      else
        rand_poisson<double> (a, len, v);
      break;

    case gamma_dist:
      if (m_use_old_generators)
        {
          if (a <= 0.0 || ! math::isfinite (a))
            std::fill_n (v, len, numeric_limits<double>::NaN ());
          else
            std::generate_n (v, len, [a](void) { double x; F77_FUNC (dgengam, DGENGAM) (1.0, a, x); return x; });
        }
      else
        rand_gamma<double> (a, len, v);
      break;

    default:
      (*current_liboctave_error_handler)
        ("rand: invalid distribution ID = %d", m_current_distribution);
      break;
    }

  save_state ();

  return;
}

void rand::fill (octave_idx_type len, float *v, float a)
{
  if (len < 1)
    return;

  switch (m_current_distribution)
    {
    case uniform_dist:
      if (m_use_old_generators)
        std::generate_n (v, len, [](void) { float x; F77_FUNC (fgenunf, FGENUNF) (0.0f, 1.0f, x); return x; });
      else
        rand_uniform<float> (len, v);
      break;

    case normal_dist:
      if (m_use_old_generators)
        std::generate_n (v, len, [](void) { float x; F77_FUNC (fgennor, FGENNOR) (0.0f, 1.0f, x); return x; });
      else
        rand_normal<float> (len, v);
      break;

    case expon_dist:
      if (m_use_old_generators)
        std::generate_n (v, len, [](void) { float x; F77_FUNC (fgenexp, FGENEXP) (1.0f, x); return x; });
      else
        rand_exponential<float> (len, v);
      break;

    case poisson_dist:
      if (m_use_old_generators)
        {
          if (a < 0.0f || ! math::isfinite (a))
            std::fill_n (v, len, numeric_limits<float>::NaN ());
          else
            {
              // workaround bug in ignpoi, by calling with different Mu
              float tmp;
              F77_FUNC (fignpoi, FIGNPOI) (a + 1, tmp);
              std::generate_n (v, len, [a](void) { float x; F77_FUNC (fignpoi, FIGNPOI) (a, x); return x; });
            }
        }
      else
        rand_poisson<float> (a, len, v);
      break;

    case gamma_dist:
      if (m_use_old_generators)
        {
          if (a <= 0.0f || ! math::isfinite (a))
            std::fill_n (v, len, numeric_limits<float>::NaN ());
          else
            std::generate_n (v, len, [a](void) { float x; F77_FUNC (fgengam, FGENGAM) (1.0f, a, x); return x; });
        }
      else
        rand_gamma<float> (a, len, v);
      break;

    default:
      (*current_liboctave_error_handler)
        ("rand: invalid distribution ID = %d", m_current_distribution);
      break;
    }

  save_state ();

  return;
}

OCTAVE_END_NAMESPACE(octave)
