/* oct-math.h                                    -*- C -*- */
/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_oct_math_h)
#define octave_oct_math_h 1

#include <cmath>

#ifdef __cplusplus
extern "C" {
#endif

// Provide declarations for these whether they are actually missing or
// not, because they are not part of the standard math.h, and the
// g++/libg++ installation no longer provides declarations for them.

extern double acosh ();
extern double asinh ();
extern double atanh ();
extern double erf ();
extern double erfc ();
extern double lgamma ();
extern double gamma ();

#ifdef __cplusplus
}
#endif

#endif
