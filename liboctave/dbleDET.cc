/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cmath>

#include "dbleDET.h"

bool
DET::value_will_overflow (void) const
{
  return base2
    ? (e2 + 1 > log2 (DBL_MAX) ? 1 : 0)
    : (e10 + 1 > log10 (DBL_MAX) ? 1 : 0);
}

bool
DET::value_will_underflow (void) const
{
  return base2
    ? (e2 - 1 < log2 (DBL_MIN) ? 1 : 0)
    : (e10 - 1 < log10 (DBL_MIN) ? 1 : 0);
}

void
DET::initialize10 (void)
{
  if (c2 != 0.0)
    {
      double etmp = e2 / log2 (10);
      e10 = static_cast<int> (round (etmp));
      etmp -= e10;
      c10 = c2 * pow (10.0, etmp);
    }
}

void
DET::initialize2 (void)
{
  if (c10 != 0.0)
    {
      double etmp = e10 / log10 (2);
      e2 = static_cast<int> (round (etmp));
      etmp -= e2;
      c2 = c10 * exp2 (etmp);
    }
}

double
DET::value (void) const
{
  return base2 ? c2 * exp2 (e2) : c10 * pow (10.0, e10);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
