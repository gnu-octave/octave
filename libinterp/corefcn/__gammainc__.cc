////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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
#include "fNDArray.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (__gammainc__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} __gammainc__ (@var{x}, @var{a})
Continued fraction for incomplete gamma function.
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  bool is_single = args(0).is_single_type () || args(1).is_single_type ();

  // Total number of scenarios: get maximum of length of all vectors
  int numel_x = args(0).numel ();
  int numel_a = args(1).numel ();
  int len = std::max (numel_x, numel_a);

  octave_value_list retval;
  // Initialize output dimension vector
  dim_vector output_dv (len, 1);

  // Lentz's algorithm in two cases: single and double precision
  if (is_single)
    {
      // Initialize output and inputs
      FloatColumnVector output (output_dv);
      FloatNDArray x, a;

      if (numel_x == 1)
        x = FloatNDArray (output_dv, args(0).float_scalar_value ());
      else
        x = args(0).float_array_value ();

      if (numel_a == 1)
        a = FloatNDArray (output_dv, args(1).float_scalar_value ());
      else
        a = args(1).float_array_value ();

      // Initialize variables used in algorithm
      static const float tiny = math::exp2 (-50.0f);
      static const float eps = std::numeric_limits<float>::epsilon();
      float y, Cj, Dj, bj, aj, Deltaj;
      int j, maxit;
      maxit = 200;

      // Loop over all elements
      for (octave_idx_type i = 0; i < len; ++i)
        {
          // Catch Ctrl+C
          OCTAVE_QUIT;

          // Variable initialization for the current element
          y = tiny;
          Cj = y;
          Dj = 0;
          bj = x(i) - a(i) + 1;
          aj = a(i);
          Deltaj = 0;
          j = 1;

          // Lentz's algorithm
          while ((std::abs ((Deltaj - 1) / y) > eps) && (j < maxit))
            {
              Cj = bj + aj/Cj;
              Dj = 1 / (bj + aj*Dj);
              Deltaj = Cj * Dj;
              y *= Deltaj;
              bj += 2;
              aj = j * (a(i) - j);
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
      NDArray x, a;

      if (numel_x == 1)
        x = NDArray (output_dv, args(0).scalar_value ());
      else
        x = args(0).array_value ();

      if (numel_a == 1)
        a = NDArray (output_dv, args(1).scalar_value ());
      else
        a = args(1).array_value ();

      // Initialize variables used in algorithm
      static const double tiny = math::exp2 (-100.0);
      static const double eps = std::numeric_limits<double>::epsilon();
      double y, Cj, Dj, bj, aj, Deltaj;
      int j, maxit;
      maxit = 200;

      // Loop over all elements
      for (octave_idx_type i = 0; i < len; ++i)
        {
          // Catch Ctrl+C
          OCTAVE_QUIT;

          // Variable initialization for the current element
          y = tiny;
          Cj = y;
          Dj = 0;
          bj = x(i) - a(i) + 1;
          aj = a(i);
          Deltaj = 0;
          j = 1;

          // Lentz's algorithm
          while ((std::abs ((Deltaj - 1) / y) > eps) && (j < maxit))
            {
              Cj = bj + aj/Cj;
              Dj = 1 / (bj + aj*Dj);
              Deltaj = Cj * Dj;
              y *= Deltaj;
              bj += 2;
              aj = j * (a(i) - j);
              j++;
            }

          output(i) = y;
        }

      retval(0) = output;
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
