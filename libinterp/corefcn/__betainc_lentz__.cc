// Copyright (C) 2018 Stefan Schlögl
// Copyright (C) 2018 Michele Ginesi
//
// This file is part of Octave.
//
// Octave is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <http://www.gnu.org/licenses/>.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif
#include "defun.h"
#include "fNDArray.h"

DEFUN (__betainc_lentz__, args, , "Continued fraction for incomplete beta function")
{
  int nargin = args.length ();
  octave_value_list outargs;
  if (nargin != 4)
    print_usage ();
  else
    {
      // Values initialized in single precision
      FloatNDArray x_arg_s = args(0).array_value ();
      FloatNDArray a_arg_s = args(1).array_value ();
      FloatNDArray b_arg_s = args(2).array_value ();
      bool is_single = args(3).bool_value ();

      // total number of scenarios: get maximum of length of all vectors
      int len_x = x_arg_s.rows ();
      int len_a = a_arg_s.rows ();
      int len_b = b_arg_s.rows ();
      int len = std::max(len_x, len_a);
      len = std::max(len, len_b);

      // input checks
      if ((len_x != len) || (len_a != len) || (len_b != len))
        error("__betainc_lentz__: expecting arguments of same dimension");

      // initialize scenario dependent output:
      dim_vector dim_scen (len, 1);
      ColumnVector f (dim_scen);

      // Lentz's algorithm in two cases: double and single precision
      if (! is_single)
        {
          NDArray x_arg = args(0).array_value ();
          NDArray a_arg = args(1).array_value ();
          NDArray b_arg = args(2).array_value ();
          NDArray x (dim_scen);
          NDArray a (dim_scen);
          NDArray b (dim_scen);

          // initialize scenario dependent input values (idx either 0 or ii)
          if (len_x == 1)
            x.fill (x_arg(0));
          else
            x = x_arg;
          //
          if (len_a == 1)
            a.fill (a_arg(0));
          else
            a = a_arg;
          //
          if (len_b == 1)
            b.fill (b_arg(0));
          else
            b = b_arg;
          // Variables initialization
          static const double tiny = pow (2, -100);
          static const double eps = std::numeric_limits<double>::epsilon();
          double xj, x2, y, Cj, Dj, aj, bj, Deltaj, alpha_j, beta_j;
          int j, maxit;
          maxit = 200;
          // loop via all scenarios
          for (octave_idx_type ii = 0; ii < len; ++ii)
            {
              // catch ctrl + c
              OCTAVE_QUIT;
              // Variable initialization for the current element
              xj = x(ii);
              y = tiny;
              Cj = y;
              Dj = 0;
              aj = a(ii);
              bj = b(ii);
              Deltaj = 0;
              alpha_j = 1;
              beta_j = aj - (aj * (aj + bj)) / (aj + 1) * xj;
              x2 = xj * xj;
              j = 1;
              //Lentz's algorithm
              while((std::abs ((Deltaj - 1))  > eps) & (j < maxit))
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
                  alpha_j = ((aj + j - 1) * (aj + bj + j - 1) * (bj - j) * j) / ((aj + 2 * j - 1) * (aj + 2 * j - 1)) * x2;
                  beta_j = aj + 2 * j + ((j * (bj - j)) / (aj + 2 * j - 1) - ((aj + j) * (aj + bj + j)) / (aj + 2 * j + 1)) * xj;
                  j++;
                }
              if (! error_state)
                  f(ii) = y;
            }
          outargs(0) = f;
        }
      else
        {
          FloatNDArray x_s (dim_scen);
          FloatNDArray a_s (dim_scen);
          FloatNDArray b_s (dim_scen);

          // initialize scenario dependent input values (idx either 0 or ii)
          if (len_x == 1)
            x_s.fill (x_arg_s(0));
          else
            x_s = x_arg_s;
          //
          if (len_a == 1)
            a_s.fill (a_arg_s(0));
          else
            a_s = a_arg_s;
          //
          if (len_b == 1)
            b_s.fill (b_arg_s(0));
          else
            b_s = b_arg_s;
          // Variables initialization
          static const float tiny = pow (2, -50);
          static const float eps = std::numeric_limits<float>::epsilon();
          float xj, x2, y, Cj, Dj, aj, bj, Deltaj, alpha_j, beta_j;
          int j, maxit;
          maxit = 200;
          // loop via all scenarios
          for (octave_idx_type ii = 0; ii < len; ++ii)
            {
              // catch ctrl + c
              OCTAVE_QUIT;
              // Variable initialization for the current element
              xj = x_s(ii);
              y = tiny;
              Cj = y;
              Dj = 0;
              aj = a_s(ii);
              bj = b_s(ii);
              Deltaj = 0;
              alpha_j = 1;
              beta_j = aj - (aj * (aj + bj)) / (aj + 1) * xj;
              x2 = xj * xj;
              j = 1;
              //Lentz's algorithm
              while((std::abs ((Deltaj - 1))  > eps) & (j < maxit))
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
                  alpha_j = ((aj + j - 1) * (aj + bj + j -1) * (bj - j) * j) / ((aj + 2 * j - 1) * (aj + 2 * j - 1)) * x2;
                  beta_j = aj + 2 * j + ((j * (bj - j)) / (aj + 2 * j - 1) - ((aj + j) * (aj + bj + j)) / (aj + 2 * j + 1)) * xj;
                  j++;
                }
              if (! error_state)
                  f(ii) = y;
            }
          outargs(0) = f;
        }
    }
  return octave_value (outargs);
}
