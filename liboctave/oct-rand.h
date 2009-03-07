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

#if !defined (octave_rand_h)
#define octave_rand_h 1

#include <map>
#include <string>

#include "dColVector.h"
#include "dMatrix.h"
#include "dNDArray.h"
#include "lo-ieee.h"

class
OCTAVE_API
octave_rand
{
protected:

  octave_rand (void);

public:

  ~octave_rand (void) { }

  static bool instance_ok (void);

  // Return the current seed.
  static double seed (void)
  {
    return instance_ok () ? instance->do_seed () : octave_NaN;
  }

  // Set the seed.
  static void seed (double s)
  {
    if (instance_ok ())
      instance->do_seed (s);
  }

  // Return the current state.
  static ColumnVector state (const std::string& d = std::string ())
  {
    return instance_ok () ? instance->do_state (d) : ColumnVector ();
  }

  // Set the current state/
  static void state (const ColumnVector &s,
		     const std::string& d = std::string ())
  {
    if (instance_ok ())
      instance->do_state (s, d);
  }
  
  // Return the current distribution.
  static std::string distribution (void)
  {
    return instance_ok () ? instance->do_distribution () : std::string ();
  }

  // Set the current distribution.  May be either "uniform" (the
  // default), "normal", "exponential", "poisson", or "gamma".
  static void distribution (const std::string& d)
  {
    if (instance_ok ())
      instance->do_distribution (d);
  }

  static void uniform_distribution (void)
  {
    if (instance_ok ())
      instance->do_uniform_distribution ();
  }

  static void normal_distribution (void)
  {
    if (instance_ok ())
      instance->do_normal_distribution ();
  }

  static void exponential_distribution (void)
  {
    if (instance_ok ())
      instance->do_exponential_distribution ();
  }

  static void poisson_distribution (void)
  {
    if (instance_ok ())
      instance->do_poisson_distribution ();
  }

  static void gamma_distribution (void)
  {
    if (instance_ok ())
      instance->do_gamma_distribution ();
  }

  // Return the next number from the sequence.
  static double scalar (double a = 1.0)
  {
    return instance_ok () ? instance->do_scalar (a) : octave_NaN;
  }

  // Return a matrix of numbers from the sequence, filled in column
  // major order.
  static Matrix matrix (octave_idx_type r, octave_idx_type c, double a = 1.0)
  {
    return instance_ok () ? instance->do_matrix (r, c, a) : Matrix ();
  }

  // Return an N-dimensional array of numbers from the sequence,
  // filled in column major order.
  static NDArray nd_array (const dim_vector& dims, double a = 1.0)
  {
    return instance_ok () ? instance->do_nd_array (dims, a) : NDArray ();
  }

  // Return an array of numbers from the sequence.
  static Array<double> vector (octave_idx_type n, double a = 1.0)
  {
    return instance_ok () ? instance->do_vector (n, a) : Array<double> ();
  }

private:

  static octave_rand *instance;

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
  int current_distribution;

  // If TRUE, use old RANLIB generators.  Otherwise, use Mersenne
  // Twister generator.
  bool use_old_generators;

  // Saved MT states.
  std::map<int, ColumnVector> rand_states;

  // Return the current seed.
  double do_seed (void);

  // Set the seed.
  void do_seed (double s);

  // Return the current state.
  ColumnVector do_state (const std::string& d);

  // Set the current state/
  void do_state (const ColumnVector &s, const std::string& d);
  
  // Return the current distribution.
  std::string do_distribution (void);

  // Set the current distribution.  May be either "uniform" (the
  // default), "normal", "exponential", "poisson", or "gamma".
  void do_distribution (const std::string& d);

  void do_uniform_distribution (void);

  void do_normal_distribution (void);

  void do_exponential_distribution (void);

  void do_poisson_distribution (void);

  void do_gamma_distribution (void);

  // Return the next number from the sequence.
  double do_scalar (double a = 1.);

  // Return a matrix of numbers from the sequence, filled in column
  // major order.
  Matrix do_matrix (octave_idx_type r, octave_idx_type c, double a = 1.);

  // Return an N-dimensional array of numbers from the sequence,
  // filled in column major order.
  NDArray do_nd_array (const dim_vector& dims, double a = 1.);

  // Return an array of numbers from the sequence.
  Array<double> do_vector (octave_idx_type n, double a = 1.);

  // Some helper functions.

  void initialize_ranlib_generators (void);

  void initialize_mersenne_twister (void);

  ColumnVector get_internal_state (void);

  void save_state (void);

  int get_dist_id (const std::string& d);

  void set_internal_state (const ColumnVector& s);

  void switch_to_generator (int dist);

  void fill (octave_idx_type len, double *v, double a);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
