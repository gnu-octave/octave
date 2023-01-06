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

#if ! defined (octave_oct_rand_h)
#define octave_oct_rand_h 1

#include "octave-config.h"

#include <map>
#include <string>

#include "Array.h"
#include "dNDArray.h"
#include "fNDArray.h"
#include "lo-ieee.h"
#include "uint32NDArray.h"

//class dim_vector;

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTAVE_API rand
{
protected:

  OCTAVE_API rand (void);

public:

  ~rand (void) = default;

  static bool instance_ok (void);

  // Return the current seed.
  static double seed (void)
  {
    return (instance_ok ()
            ? m_instance->do_seed () : numeric_limits<double>::NaN ());
  }

  // Set the seed.
  static void seed (double s)
  {
    if (instance_ok ())
      m_instance->do_seed (s);
  }

  // Reset the seed.
  static void reset (void)
  {
    if (instance_ok ())
      m_instance->do_reset ();
  }

  // Return the current state.
  static uint32NDArray state (const std::string& d = "")
  {
    return instance_ok () ? m_instance->do_state (d) : uint32NDArray ();
  }

  // Set the current state/
  static void state (const uint32NDArray& s,
                     const std::string& d = "")
  {
    if (instance_ok ())
      m_instance->do_state (s, d);
  }

  // Reset the current state/
  static void reset (const std::string& d)
  {
    if (instance_ok ())
      m_instance->do_reset (d);
  }

  // Return the current distribution.
  static std::string distribution (void)
  {
    return instance_ok () ? m_instance->do_distribution () : "";
  }

  // Set the current distribution.  May be either "uniform" (the
  // default), "normal", "exponential", "poisson", or "gamma".
  static void distribution (const std::string& d)
  {
    if (instance_ok ())
      m_instance->do_distribution (d);
  }

  static void uniform_distribution (void)
  {
    if (instance_ok ())
      m_instance->do_uniform_distribution ();
  }

  static void normal_distribution (void)
  {
    if (instance_ok ())
      m_instance->do_normal_distribution ();
  }

  static void exponential_distribution (void)
  {
    if (instance_ok ())
      m_instance->do_exponential_distribution ();
  }

  static void poisson_distribution (void)
  {
    if (instance_ok ())
      m_instance->do_poisson_distribution ();
  }

  static void gamma_distribution (void)
  {
    if (instance_ok ())
      m_instance->do_gamma_distribution ();
  }

  // Return the next number from the sequence.
  static double scalar (double a = 1.0)
  {
    return (instance_ok ()
            ? m_instance->do_scalar (a) : numeric_limits<double>::NaN ());
  }

  // Return the next number from the sequence.
  static float float_scalar (float a = 1.0)
  {
    return (instance_ok ()
            ? m_instance->do_scalar (a) : numeric_limits<float>::NaN ());
  }

  // Return an array of numbers from the sequence.
  static Array<double> vector (octave_idx_type n, double a = 1.0)
  {
    return instance_ok () ? m_instance->do_vector (n, a) : Array<double> ();
  }

  // Return an array of numbers from the sequence.
  static Array<float> float_vector (octave_idx_type n, float a = 1.0)
  {
    return instance_ok () ? m_instance->do_vector (n, a) : Array<float> ();
  }

  // Return an N-dimensional array of numbers from the sequence,
  // filled in column major order.
  static NDArray nd_array (const dim_vector& dims, double a = 1.0)
  {
    return instance_ok () ? m_instance->do_nd_array (dims, a) : NDArray ();
  }

  // Return an N-dimensional array of numbers from the sequence,
  // filled in column major order.
  static FloatNDArray float_nd_array (const dim_vector& dims, float a = 1.0)
  {
    return (instance_ok ()
            ? m_instance->do_float_nd_array (dims, a) : FloatNDArray ());
  }

private:

  static rand *m_instance;

  static void cleanup_instance (void)
  { delete m_instance; m_instance = nullptr; }

  enum
  {
    unknown_dist,
    uniform_dist,
    normal_dist,
    expon_dist,
    poisson_dist,
    gamma_dist
  };

  // Current distribution of random numbers.
  int m_current_distribution;

  // If TRUE, use old RANLIB generators.  Otherwise, use Mersenne
  // Twister generator.
  bool m_use_old_generators;

  // Saved MT states.
  std::map<int, uint32NDArray> m_rand_states;

  // Return the current seed.
  OCTAVE_API double do_seed (void);

  // Set the seed.
  OCTAVE_API void do_seed (double s);

  // Reset the seed.
  OCTAVE_API void do_reset ();

  // Return the current state.
  OCTAVE_API uint32NDArray do_state (const std::string& d);

  // Set the current state/
  OCTAVE_API void do_state (const uint32NDArray& s, const std::string& d);

  // Reset the current state/
  OCTAVE_API void do_reset (const std::string& d);

  // Return the current distribution.
  OCTAVE_API std::string do_distribution (void);

  // Set the current distribution.  May be either "uniform" (the
  // default), "normal", "exponential", "poisson", or "gamma".
  OCTAVE_API void do_distribution (const std::string& d);

  OCTAVE_API void do_uniform_distribution (void);

  OCTAVE_API void do_normal_distribution (void);

  OCTAVE_API void do_exponential_distribution (void);

  OCTAVE_API void do_poisson_distribution (void);

  OCTAVE_API void do_gamma_distribution (void);

  // The following templates only make sense for double and float
  // types.

  template <typename T> OCTAVE_API T uniform (void);

  template <typename T> OCTAVE_API T normal (void);

  template <typename T> OCTAVE_API T exponential (void);

  template <typename T> OCTAVE_API T poisson (T a);

  template <typename T> OCTAVE_API T gamma (T a);

  // Return the next number from the sequence.
  template <typename T> OCTAVE_API T do_scalar (T a = 1);

  // Return an array of numbers from the sequence.
  template <typename T> OCTAVE_API Array<T>
  do_vector (octave_idx_type n, T a = 1);

  // Return an N-dimensional array of numbers from the sequence,
  // filled in column major order.
  OCTAVE_API NDArray do_nd_array (const dim_vector& dims, double a = 1.);

  // Return an N-dimensional array of numbers from the sequence,
  // filled in column major order.
  OCTAVE_API FloatNDArray
  do_float_nd_array (const dim_vector& dims, float a = 1.);

  // Some helper functions.

  OCTAVE_API void initialize_ranlib_generators (void);

  OCTAVE_API void initialize_mersenne_twister (void);

  OCTAVE_API uint32NDArray get_internal_state (void);

  OCTAVE_API void save_state (void);

  OCTAVE_API int get_dist_id (const std::string& d);

  OCTAVE_API void set_internal_state (const uint32NDArray& s);

  OCTAVE_API void switch_to_generator (int dist);

  OCTAVE_API void fill (octave_idx_type len, double *v, double a);

  OCTAVE_API void fill (octave_idx_type len, float *v, float a);
};

OCTAVE_END_NAMESPACE(octave)

#endif
