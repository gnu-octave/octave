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

#include <cfloat>
#include <cmath>

#include "CmplxDET.h"
#include "oct-cmplx.h"

int
ComplexDET::value_will_overflow (void) const
{
  return det[1].real () + 1 > log10 (DBL_MAX) ? 1 : 0;
}

int
ComplexDET::value_will_underflow (void) const
{
  return det[1].real () - 1 < log10 (DBL_MIN) ? 1 : 0;
}

Complex
ComplexDET::coefficient (void) const
{
  return det[0];
}

int
ComplexDET::exponent (void) const
{
  return (int) (det[1].real ());
}

Complex
ComplexDET::value (void) const
{
  return det[0] * pow (10.0, det[1].real ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
