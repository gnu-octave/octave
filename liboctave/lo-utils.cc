// utils.cc
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <climits>

#include "lo-mappers.h"
#include "lo-utils.h"

// Convert X to the nearest integer value.  Should not pass NaN to
// this function.

int
NINT (double x)
{
  if (x > INT_MAX)
    return INT_MAX;
  else if (x < INT_MIN)
    return INT_MIN;
  else
    return (x > 0) ? ((int) (x + 0.5)) : ((int) (x - 0.5));
}

double
D_NINT (double x)
{
  if (xisinf (x) || xisnan (x))
    return x;
  else
    return floor (x + 0.5);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
