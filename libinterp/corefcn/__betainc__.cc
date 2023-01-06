////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#include "defun.h"
#include "dNDArray.h"
#include "fNDArray.h"

OCTAVE_BEGIN_NAMESPACE(octave)
DEFUN (__betainc__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} __betainc__ (@var{x}, @var{a}, @var{b})
Continued fraction for incomplete beta function.
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  bool is_single = (args(0).is_single_type () || args(1).is_single_type ()
                    || args(2).is_single_type ());

  // Total number of scenarios: get maximum of length of all vectors
  int numel_x = args(0).numel ();
  int numel_a = args(1).numel ();
  int numel_b = args(2).numel ();
  int len = std::max (std::max (numel_x, numel_a), numel_b);

  octave_value_list retval;
  // Initialize output dimension vector
  dim_vector output_dv (len, 1);

  // Lentz's algorithm in two cases: single and double precision
  if (is_single)
    {
      // Initialize output and inputs
      FloatColumnVector output (output_dv);
      FloatNDArray x, a, b;

      if (numel_x == 1)
        x = FloatNDArray (output_dv, args(0).float_scalar_value ());
      else
        x = args(0).float_array_value ();


      if (numel_a == 1)
        a = FloatNDArray (output_dv, args(1).float_scalar_value ());
      else
        a = args(1).float_array_value ();

      if (numel_b == 1)
        b = FloatNDArray (output_dv, args(2).float_scalar_value ());
      else
        b = args(2).float_array_value ();

      // Initialize variables used in algorithm
      static const float tiny = math::exp2 (-50.0f);
      static const float eps = std::numeric_limits<float>::epsilon ();
      float xj, x2, y, Cj, Dj, aj, bj, Deltaj, alpha_j, beta_j;
      int j, maxit;
      maxit = 200;

      // Loop over all elements
      for (octave_idx_type i = 0; i < len; ++i)
        {
          // Catch Ctrl+C
          OCTAVE_QUIT;

          // Variable initialization for the current element
          xj = x(i);
          y = tiny;
          Cj = y;
          Dj = 0;
          aj = a(i);
          bj = b(i);
          Deltaj = 0;
          alpha_j = 1;
          beta_j = aj - (aj * (aj + bj)) / (aj + 1) * xj;
          x2 = xj * xj;
          j = 1;

          // Lentz's algorithm
          while ((std::abs ((Deltaj - 1)) > eps) && (j < maxit))
            {
              Dj = beta_j + alpha_j * Dj;
              if (Dj == 0)
                Dj = tiny;
              Cj = beta_j + alpha_j / Cj;
              if (Cj == 0)
                Cj = tiny;
              Dj = 1 / Dj;
              Deltaj = Cj * Dj;
              y *= Deltaj;
              alpha_j = ((aj + j - 1) * (aj + bj + j -1) * (bj - j) * j)
                        / ((aj + 2 * j - 1) * (aj + 2 * j - 1)) * x2;
              beta_j = aj + 2 * j + ((j * (bj - j)) / (aj + 2 * j - 1)
                                     - ((aj + j) * (aj + bj + j)) / (aj + 2 * j + 1)) * xj;
              j++;
            }

          output(i) = y;
        }

      retval(0) = output;
    }
  else
    {
      // Initialize output and inputs
      ColumnVector output (output_dv);
      NDArray x, a, b;

      if (numel_x == 1)
        x = NDArray (output_dv, args(0).scalar_value ());
      else
        x = args(0).array_value ();

      if (numel_a == 1)
        a = NDArray (output_dv, args(1).scalar_value ());
      else
        a = args(1).array_value ();

      if (numel_b == 1)
        b = NDArray (output_dv, args(2).scalar_value ());
      else
        b = args(2).array_value ();

      // Initialize variables used in algorithm
      static const double tiny = math::exp2 (-100.0);
      static const double eps = std::numeric_limits<double>::epsilon ();
      double xj, x2, y, Cj, Dj, aj, bj, Deltaj, alpha_j, beta_j;
      int j, maxit;
      maxit = 200;

      // Loop over all elements
      for (octave_idx_type i = 0; i < len; ++i)
        {
          // Catch Ctrl+C
          OCTAVE_QUIT;

          // Variable initialization for the current element
          xj = x(i);
          y = tiny;
          Cj = y;
          Dj = 0;
          aj = a(i);
          bj = b(i);
          Deltaj = 0;
          alpha_j = 1;
          beta_j = aj - (aj * (aj + bj)) / (aj + 1) * xj;
          x2 = xj * xj;
          j = 1;

          // Lentz's algorithm
          while ((std::abs ((Deltaj - 1)) > eps) && (j < maxit))
            {
              Dj = beta_j + alpha_j * Dj;
              if (Dj == 0)
                Dj = tiny;
              Cj = beta_j + alpha_j / Cj;
              if (Cj == 0)
                Cj = tiny;
              Dj = 1 / Dj;
              Deltaj = Cj * Dj;
              y *= Deltaj;
              alpha_j = ((aj + j - 1) * (aj + bj + j - 1) * (bj - j) * j)
                        / ((aj + 2 * j - 1) * (aj + 2 * j - 1)) * x2;
              beta_j = aj + 2 * j + ((j * (bj - j)) / (aj + 2 * j - 1)
                                     - ((aj + j) * (aj + bj + j)) / (aj + 2 * j + 1)) * xj;
              j++;
            }

          output(i) = y;
        }

      retval(0) = output;
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
