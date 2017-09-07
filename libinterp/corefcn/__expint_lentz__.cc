// Copyright (C) 2017 Michele Ginesi
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
#include <complex>

DEFUN (__expint_lentz__, args, , "Continued fraction for exponential integral function")
{
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();
  else
    {
      octave_value x_arg = args(0);
      if (x_arg.is_single_type ())
        {
          const std::complex<float> x = args(0).float_complex_value ();
          static const std::complex<float> tiny = pow (2, -100);
          static const float eps = std::numeric_limits<float>::epsilon();
          std::complex<float> cone(1.0, 0.0);
          std::complex<float> czero(0.0, 0.0);
          std::complex<float> f = tiny;
          std::complex<float> C = f;
          std::complex<float> D = czero;
          std::complex<float> alpha_m = cone;
          std::complex<float> beta_m = x;
          std::complex<float> Delta = czero;
          int m = 1;
          int maxit = 100;
          while((std::abs(Delta - cone) > eps) & (m < maxit))
            {
               D = beta_m + alpha_m * D;
               if (D == czero)
                 D = tiny;
               C = beta_m + alpha_m / C;
               if (C == czero)
                 C = tiny;
               D = cone / D;
               Delta = C * D;
               f *= Delta;
               alpha_m = floor ((m + 1) / 2);
               if ((m % 2) == 0)
                 beta_m = x;
               else
                 beta_m = cone;
               m++;
             }
           if (! error_state)
            return octave_value (f);
        }
      else
        {
          const std::complex<double> x = args(0).complex_value ();
          static const std::complex<double> tiny = pow (2, -100);
          static const double eps = std::numeric_limits<double>::epsilon();
          std::complex<double> cone(1.0, 0.0);
          std::complex<double> czero(0.0, 0.0);
          std::complex<double> f = tiny;
          std::complex<double> C = f;
          std::complex<double> D = czero;
          std::complex<double> alpha_m = cone;
          std::complex<double> beta_m = x;
          std::complex<double> Delta = czero;
          int m = 1;
          int maxit = 200;
          while((std::abs(Delta - cone) > eps) & (m < maxit))
            {
               D = beta_m + alpha_m * D;
               if (D == czero)
                 D = tiny;
               C = beta_m + alpha_m / C;
               if (C == czero)
                 C = tiny;
               D = cone / D;
               Delta = C * D;
               f *= Delta;
               alpha_m = floor ((m + 1) / 2);
               if ((m % 2) == 0)
                 beta_m = x;
               else
                 beta_m = cone;
               m++;
             }
           if (! error_state)
            return octave_value (f);
        }
      }
  return octave_value_list ();
}
