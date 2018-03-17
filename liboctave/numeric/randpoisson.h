/*

Copyright (C) 2006-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

/* Original version written by Paul Kienzle distributed as free
   software in the in the public domain.  */

#if ! defined (octave_randpoisson_h)
#define octave_randpoisson_h 1

#include "octave-config.h"

namespace octave
{
  template <typename T> void rand_poisson (T L, octave_idx_type n, T *p);

  template <typename T> T rand_poisson (T L);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_poisson<double>' instead")
inline double
oct_randp (double L)
{
  return octave::rand_poisson (L);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_poisson<double>' instead")
inline void
oct_fill_randp (double L, octave_idx_type n, double *p)
{
  octave::rand_poisson (L, n, p);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_poisson<float>' instead")
inline float
oct_float_randp (float L)
{
  return octave::rand_poisson (L);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_poisson<float>' instead")
inline void
oct_fill_float_randp (float L, octave_idx_type n, float *p)
{
  octave::rand_poisson (L, n, p);
}

#endif
