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
#include "CNDArray.h"
#include "fCNDArray.h"
#include <complex>

DEFUN (__expint_lentz__, args, , "Continued fraction for the exponential integral")
{
  int nargin = args.length ();
  octave_value_list outargs;
  if (nargin != 2)
    print_usage ();
  else
    {
      // Value initialized in single precision
      FloatComplexNDArray x_arg_s = args(0).complex_array_value ();
      bool is_single = args(1).bool_value ();

      int len_x = x_arg_s.rows ();

      // initialize scenario dependent output:
      dim_vector dim_scen (len_x, 1);
      ComplexColumnVector f (dim_scen);

      // Lentz's algorithm in two cases: double and single precision

      if (! is_single)
        {
          ComplexNDArray x_arg = args(0).complex_array_value ();
          ComplexNDArray x (dim_scen);

          // initialize scenario dependent input values (idx either 0 or ii)
          if (len_x == 1)
            x.fill (x_arg(0));
          else
            x = x_arg;
          // Variables initialization
          static const std::complex<double> tiny = pow (2, -100);
          static const double eps = std::numeric_limits<double>::epsilon();
          std::complex<double> cone(1.0, 0.0);
          std::complex<double> czero(0.0, 0.0);
          std::complex<double> xj = x(0);
          std::complex<double> y = tiny;
          std::complex<double> Cj = y;
          std::complex<double> Dj = czero;
          std::complex<double> alpha_j = cone;
          std::complex<double> beta_j = xj;
          std::complex<double> Deltaj = czero;
          int j = 1;
          int maxit = 200;
          // loop via all scenarios
          for (octave_idx_type ii = 0; ii < len_x; ++ii)
            {
              // catch ctrl + c
              OCTAVE_QUIT;
              // Variable initialization for the current element
              xj = x(ii);
              y = tiny;
              Cj = y;
              Dj = czero;
              alpha_j = cone;
              beta_j = xj;
              Deltaj = czero;
              j = 1;
              //Lentz's algorithm
              while((std::abs (Deltaj - cone)  > eps) & (j < maxit))
                {
                  Dj = beta_j + alpha_j * Dj;
                  if (Dj == czero)
                    Dj = tiny;
                  Cj = beta_j + alpha_j / Cj;
                  if (Cj == czero)
                    Cj = tiny;
                  Dj = cone / Dj;
                  Deltaj = Cj * Dj;
                  y *= Deltaj;
                  alpha_j = floor ((j + 1) / 2);
                  if ((j % 2) == 0)
                    beta_j = xj;
                  else
                    beta_j = cone;
                  j++;
                }
              if (! error_state)
                f(ii) = y;
            }
          outargs(0) = f;
        }
      else
        {
          FloatComplexNDArray x_s (dim_scen);

          FloatComplexColumnVector f (dim_scen);

          // initialize scenario dependent input values (idx either 0 or ii)
          if (len_x == 1)
            x_s.fill (x_arg_s(0));
          else
            x_s = x_arg_s;
          // Variables initialization
          static const std::complex<float> tiny = pow (2, -50);
          static const float eps = std::numeric_limits<float>::epsilon();
          std::complex<float> cone(1.0, 0.0);
          std::complex<float> czero(0.0, 0.0);
          std::complex<float> xj = x_s(0);
          std::complex<float> y = tiny;
          std::complex<float> Cj = y;
          std::complex<float> Dj = czero;
          std::complex<float> alpha_j = cone;
          std::complex<float> beta_j = czero;
          std::complex<float> Deltaj = czero;
          int j = 1;
          int maxit = 100;
          // loop via all scenarios
          for (octave_idx_type ii = 0; ii < len_x; ++ii)
            {
              // catch ctrl + c
              OCTAVE_QUIT;
              // Variable initialization for the current element
              xj = x_s(ii);
              y = tiny;
              Cj = y;
              Dj = czero;
              alpha_j = cone;
              beta_j = xj;
              Deltaj = czero;
              j = 1;
              //Lentz's algorithm
              while((std::abs (Deltaj - cone)  > eps) & (j < maxit))
                {
                  Dj = beta_j + alpha_j * Dj;
                  if (Dj == czero)
                    Dj = tiny;
                  Cj = beta_j + alpha_j / Cj;
                  if (Cj == czero)
                    Cj = tiny;
                  Dj = cone / Dj;
                  Deltaj = Cj * Dj;
                  y *= Deltaj;
                  alpha_j = floor ((j + 1) / 2);
                  if ((j % 2) == 0)
                    beta_j = xj;
                  else
                    beta_j = cone;
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
