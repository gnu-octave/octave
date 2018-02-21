// Copyright (C) 2017 Nir Krakauer
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

DEFUN (__gammainc_lentz__, args, , "Continued fraction for incomplete gamma function")
{
  int nargin = args.length ();
  octave_value_list outargs;
  if (nargin != 3)
    print_usage ();
  else
    {
      FloatNDArray x_arg_s = args(0).array_value ();
      FloatNDArray a_arg_s = args(1).array_value ();
      bool is_single = args(2).bool_value ();

      // total number of scenarios: get maximum of length of all vectors
      int len_x = x_arg_s.rows ();
      int len_a = a_arg_s.rows ();
      int len = std::max(len_x,len_a);

      // input checks
      if (len_x > 1 && len_x != len)
        error("gammainc_lentz_vec: expecting x to be of length 1 or %d", len);
      if (len_a > 1 && len_a != len)
        error("gammainc_lentz_vec: expecting a to be of length 1 or %d", len);

      // initialize scenario dependent output:
      dim_vector dim_scen (len, 1);
      ColumnVector f (dim_scen);

      if (! is_single)
        {
          NDArray x_arg = args(0).array_value ();
          NDArray a_arg = args(1).array_value ();
          NDArray x (dim_scen);
          NDArray a (dim_scen);

          // initialize scenario dependent input values (idx either 0 or ii)
          if (len_x == 1)
            x.fill (x_arg(0));
          else
            x = x_arg;
          if (len_a == 1)
            a.fill (a_arg(0));
          else
            a = a_arg;
          static const double tiny = pow (2, -100);
          static const double eps = std::numeric_limits<double>::epsilon();
          double y, Cj, Dj, bj, aj, Deltaj;
          int j, maxit;
          maxit = 200;
          // loop via all scenarios
          for (octave_idx_type ii = 0; ii < len; ++ii)
            {
              // catch ctrl + c
              OCTAVE_QUIT;
              y = tiny;
              Cj = y;
              Dj = 0;
              bj = x(ii) - a(ii) + 1;
              aj = a(ii);
              Deltaj = 0;
              j = 1;

              while((std::abs ((Deltaj - 1) / y)  > eps) & (j < maxit))
                {
                  Cj = bj + aj/Cj;
                  Dj = 1 / (bj + aj*Dj);
                  Deltaj = Cj * Dj;
                  y *= Deltaj;
                  bj += 2;
                  aj = j * (a(ii) - j);
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

          // initialize scenario dependent input values (idx either 0 or ii)
          if (len_x == 1)
            x_s.fill (x_arg_s(0));
          else
          x_s = x_arg_s;
          if (len_a == 1)
            a_s.fill (a_arg_s(0));
          else
            a_s = a_arg_s;
          static const float tiny = pow (2, -50);
          static const float eps = std::numeric_limits<float>::epsilon();
          float y, Cj, Dj, bj, aj, Deltaj;
          int j, maxit;
          maxit = 200;
          // loop via all scenarios
          for (octave_idx_type ii = 0; ii < len; ++ii)
            {
              // catch ctrl + c
              OCTAVE_QUIT;
              y = tiny;
              Cj = y;
              Dj = 0;
              bj = x_s(ii) - a_s(ii) + 1;
              aj = a_s(ii);
              Deltaj = 0;
              j = 1;

              while((std::abs ((Deltaj - 1) / y)  > eps) & (j < maxit))
                {
                  Cj = bj + aj/Cj;
                  Dj = 1 / (bj + aj*Dj);
                  Deltaj = Cj * Dj;
                  y *= Deltaj;
                  bj += 2;
                  aj = j * (a_s(ii) - j);
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
