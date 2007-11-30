/*

Copyright (C) 1994, 1995, 1996, 1997, 2002, 2003, 2004, 2005, 2006,
              2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include "dbleDET.h"
#include "lo-mappers.h"
#include "lo-math.h"

bool
DET::value_will_overflow (void) const
{
  return base2
    ? (e2 + 1 > xlog2 (DBL_MAX) ? 1 : 0)
    : (e10 + 1 > log10 (DBL_MAX) ? 1 : 0);
}

bool
DET::value_will_underflow (void) const
{
  return base2
    ? (e2 - 1 < xlog2 (DBL_MIN) ? 1 : 0)
    : (e10 - 1 < log10 (DBL_MIN) ? 1 : 0);
}

void
DET::initialize10 (void)
{
  if (c2 != 0.0)
    {
      double etmp = e2 / xlog2 (10);
      e10 = static_cast<int> (xround (etmp));
      etmp -= e10;
      c10 = c2 * pow (10.0, etmp);
    }
}

void
DET::initialize2 (void)
{
  if (c10 != 0.0)
    {
      double etmp = e10 / log10 (2.0);
      e2 = static_cast<int> (xround (etmp));
      etmp -= e2;
      c2 = c10 * xexp2 (etmp);
    }
}

double
DET::value (void) const
{
  return base2 ? c2 * xexp2 (e2) : c10 * pow (10.0, e10);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
