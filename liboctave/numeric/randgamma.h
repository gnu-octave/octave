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

#if ! defined (octave_randgamma_h)
#define octave_randgamma_h 1

#include "octave-config.h"

namespace octave
{
  template <typename T>
  void
  rand_gamma (T a, octave_idx_type n, T *p);

  template <> void
  rand_gamma<double> (double a, octave_idx_type n, double  *p);

  template <> void
  rand_gamma<float> (float a, octave_idx_type n, float  *p);

  template <typename T>
  T
  rand_gamma (T a)
  {
    T retval;
    rand_gamma (a, 1, &retval);
    return retval;
  }
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_gamma<double>' instead")
inline double
oct_randg (double a)
{
  return octave::rand_gamma (a);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_gamma<float>' instead")
inline float
oct_float_randg (float a)
{
  return octave::rand_gamma (a);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_gamma<double>' instead")
inline void
oct_fill_randg (double a, octave_idx_type n, double *p)
{
  octave::rand_gamma (a, n, p);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::rand_gamma<float>' instead")
inline void
oct_fill_float_randg (float a, octave_idx_type n, float *p)
{
  octave::rand_gamma (a, n, p);
}

#endif
