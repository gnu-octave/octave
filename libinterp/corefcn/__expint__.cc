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

#include "CNDArray.h"
#include "defun.h"
#include "fCNDArray.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (__expint__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} __expint__ (@var{x})
Continued fraction expansion for the exponential integral.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value_list retval;

  bool is_single = args(0).is_single_type ();

  int numel_x = args(0).numel ();

  // Initialize output dimension vector
  dim_vector output_dv (numel_x, 1);

  // Lentz's algorithm in two cases: single and double precision
  if (is_single)
    {
      // Initialize output and inputs
      FloatComplexColumnVector output (output_dv);
      FloatComplexNDArray x;

      if (numel_x == 1)
        x = FloatComplexNDArray (output_dv, args(0).float_complex_value ());
      else
        x = args(0).float_complex_array_value ();

      // Initialize variables used in algorithm
      static const FloatComplex tiny = math::exp2 (-50.0f);
      static const float eps = std::numeric_limits<float>::epsilon ();
      const FloatComplex cone (1.0, 0.0);
      const FloatComplex czero (0.0, 0.0);
      const int maxit = 100;

      // Loop over all elements
      for (octave_idx_type i = 0; i < numel_x; ++i)
        {
          // Catch Ctrl+C
          OCTAVE_QUIT;

          // Variable initialization for the current element
          FloatComplex xj = x(i);
          FloatComplex y = tiny;
          FloatComplex Cj = y;
          FloatComplex Dj = czero;
          FloatComplex alpha_j = cone;
          FloatComplex beta_j = xj;
          FloatComplex Deltaj = czero;
          int j = 1;

          // Lentz's algorithm
          while ((std::abs (Deltaj - cone)  > eps) && (j < maxit))
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
              alpha_j = (j + 1) / 2;
              if ((j % 2) == 0)
                beta_j = xj;
              else
                beta_j = cone;
              j++;
            }

          output(i) = y;
        }
      retval(0) = output;
    }
  else
    {
      // Initialize output and inputs
      ComplexColumnVector output (output_dv);
      ComplexNDArray x;

      if (numel_x == 1)
        x = ComplexNDArray (output_dv, args(0).complex_value ());
      else
        x = args(0).complex_array_value ();

      // Initialize variables used in algorithm
      static const Complex tiny = math::exp2 (-100.0);
      static const double eps = std::numeric_limits<double>::epsilon ();
      const Complex cone (1.0, 0.0);
      const Complex czero (0.0, 0.0);
      const int maxit = 200;

      // Loop over all scenarios
      for (octave_idx_type i = 0; i < numel_x; ++i)
        {
          // Catch Ctrl+C
          OCTAVE_QUIT;

          // Variable initialization for the current element
          Complex xj = x(i);
          Complex y = tiny;
          Complex Cj = y;
          Complex Dj = czero;
          Complex alpha_j = cone;
          Complex beta_j = xj;
          Complex Deltaj = czero;
          int j = 1;

          // Lentz's algorithm
          while ((std::abs (Deltaj - cone)  > eps) && (j < maxit))
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
              alpha_j = (j + 1) / 2;
              if ((j % 2) == 0)
                beta_j = xj;
              else
                beta_j = cone;
              j++;
            }

          output(i) = y;
        }

      retval(0) = output;
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
