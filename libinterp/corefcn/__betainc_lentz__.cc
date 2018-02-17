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

DEFUN (__betainc_lentz__, args, , "Continued fraction for incomplete beta function")
{
  int nargin = args.length ();

  if (nargin != 3)
    print_usage ();
  else
    {
      octave_value x_arg = args(0);
      octave_value a_arg = args(1);
      octave_value b_arg = args(2);
      if (x_arg.is_single_type () || a_arg.is_single_type () || b_arg.is_single_type ())
        {
          const float x = args(0).float_value ();
          const float a = args(1).float_value ();
          const float b = args(2).float_value ();
          static const float tiny = pow (2, -50);
          static const float eps = std::numeric_limits<float>::epsilon();
          float f = tiny;
          float C = f;
          float D = 0;
          float alpha_m = 1;
          float beta_m = a - (a * (a+b)) / (a + 1) * x;
          float x2 = x * x;
          float Delta = 0;
          int m = 1;
          int maxit = 100;
          while((std::abs(Delta - 1) > eps) & (m < maxit))
            {
               D = beta_m + alpha_m * D;
               if (D == 0)
                 D = tiny;
               C = beta_m + alpha_m / C;
               if (C == 0)
                 C = tiny;
               D = 1 / D;
               Delta = C * D;
               f *= Delta;
               alpha_m = ((a + m - 1) * (a + b + m - 1) * (b - m) * m) / ((a + 2 * m - 1) * (a + 2 * m - 1)) * x2;
               beta_m = a + 2 * m + ((m * (b - m)) / (a + 2 * m - 1) - ((a + m) * (a + b + m)) / (a + 2 * m + 1)) * x;
               m++;
             }
           if (! error_state)
            return octave_value (f);
        }
      else
        {
          const double x = args(0).double_value ();
          const double a = args(1).double_value ();
          const double b = args(2).double_value ();
          static const double tiny = pow (2, -100);
          static const double eps = std::numeric_limits<double>::epsilon();
          double f = tiny;
          double C = f;
          double D = 0;
          double alpha_m = 1;
          double beta_m = a - (a * (a+b)) / (a + 1) * x;
          double x2 = x * x;
          double Delta = 0;
          int m = 1;
          int maxit = 200;
          while((std::abs(Delta - 1) > eps) & (m < maxit))
            {
               D = beta_m + alpha_m * D;
               if (D == 0)
                 D = tiny;
               C = beta_m + alpha_m / C;
               if (C == 0)
                 C = tiny;
               D = 1 / D;
               Delta = C * D;
               f *= Delta;
               alpha_m = ((a + m - 1) * (a + b + m - 1) * (b - m) * m) / ((a + 2 * m - 1) * (a + 2 * m - 1)) * x2;
               beta_m = a + 2 * m + ((m * (b - m)) / (a + 2 * m - 1) - ((a + m) * (a + b + m)) / (a + 2 * m + 1)) * x;
               m++;
             }
           if (! error_state)
            return octave_value (f);
        }
      }
  return octave_value_list ();
}
