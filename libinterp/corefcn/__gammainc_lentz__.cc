// Copyright (C) 2017 Nir Krakauer
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

DEFUN (__gammainc_lentz__, args, , "Continued fraction for incomplete gamma function")
{
  int nargin = args.length ();

  if (nargin != 2)
    print_usage ();
  else
    {
      octave_value x_arg = args(0);
      octave_value a_arg = args(1);
      if (x_arg.is_single_type () || a_arg.is_single_type ())
        {
          const float x = args(0).float_value ();
          const float a = args(1).float_value ();
          static const float tiny = pow (2, -50);
          static const float eps = std::numeric_limits<float>::epsilon();
          float y = tiny;
          float Cj = y;
          float Dj = 0;
          float bj = x - a + 1;
          float aj = a;
          float Deltaj = 0;
          int j = 1;
          int maxit = 200;
          while((std::abs((Deltaj - 1) / y)  > eps) & (j < maxit))
            {
               Cj = bj + aj/Cj;
               Dj = 1 / (bj + aj*Dj);
               Deltaj = Cj * Dj;
               y *= Deltaj;
               bj += 2;
               aj = j * (a - j);
               j++;
             }
           if (! error_state)
            return octave_value (y);
        }
      else
        {
          const double x = args(0).double_value ();
          const double a = args(1).double_value ();
          static const double tiny = pow (2, -100);
          static const double eps = std::numeric_limits<double>::epsilon();
          double y = tiny;
          double Cj = y;
          double Dj = 0;
          double bj = x - a + 1;
          double aj = a;
          double Deltaj = 0;
          int j = 1;
          int maxit = 200;
          while((std::abs((Deltaj - 1) / y)  > eps) & (j < maxit))
            {
              Cj = bj + aj/Cj;
              Dj = 1 / (bj + aj*Dj);
              Deltaj = Cj * Dj;
              y *= Deltaj;
              bj += 2;
              aj = j * (a - j);
              j++;
            }
          if (! error_state)
            return octave_value (y);
        }
      }
  return octave_value_list ();
}
