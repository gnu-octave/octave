/*

Copyright (C) 2002 John W. Eaton

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

#include <float.h>
#include <math.h>

#if defined (HAVE_FLOATINGPOINT_H)
#include <floatingpoint.h>
#endif

#if defined (HAVE_IEEEFP_H)
#include <ieeefp.h>
#endif

#if defined (HAVE_NAN_H)
#if defined (SCO)
#define _IEEE 1
#endif
#include <nan.h>
#if defined (SCO)
#undef _IEEE
#endif
#endif

#include "lo-ieee.h"

#if defined (SCO)

int
isnan (double x)
{
  return (IsNANorINF (x) && NaN (x) && ! IsINF (x)) ? 1 : 0;
}

int
isinf (double x)
{
  return (IsNANorINF (x) && IsINF (x)) ? 1 : 0;
}

#endif

int
lo_ieee_is_NA (double x)
{
  lo_ieee_double t;
  t.value = x;
  return (isnan (x) && t.word[lo_ieee_lw] == LO_IEEE_NA_LW) ? 1 : 0;
}

int
lo_ieee_is_NaN_or_NA (double x)
{
  return isnan (x);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
