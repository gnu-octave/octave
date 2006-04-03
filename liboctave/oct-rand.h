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

#if !defined (octave_rand_h)
#define octave_rand_h 1

#include <string>

#include "dColVector.h"
#include "dMatrix.h"
#include "dNDArray.h"

struct
octave_rand
{
  // Return the current seed.
  static double seed (void);

  // Set the seed.
  static void seed (double s);

  // Return the current state.
  static ColumnVector state (void);

  // Set the current state/
  static void state (const ColumnVector &s);
  
  // Return the current distribution.
  static std::string distribution (void);

  // Set the current distribution.  May be either "uniform" (the
  // default) or "normal".
  static void distribution (const std::string& d);

  static void uniform_distribution (void);

  static void normal_distribution (void);

  static void exponential_distribution (void);

  static void poisson_distribution (void);

  static void gamma_distribution (void);

  // Return the next number from the sequence.
  static double scalar (double a = 1.);

  // Return a matrix of numbers from the sequence, filled in column
  // major order.
  static Matrix matrix (octave_idx_type r, octave_idx_type c, double a = 1.);

  // Return an N-dimensional array of numbers from the sequence,
  // filled in column major order.
  static NDArray nd_array (const dim_vector& dims, double a = 1.);

  // Return an array of numbers from the sequence.
  static Array<double> vector (octave_idx_type n, double a = 1.);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
