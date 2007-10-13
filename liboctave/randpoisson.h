/*

Copyright (C) 2006, 2007 John W. Eaton

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

/* Original version written by Paul Kienzle distributed as free
   software in the in the public domain.  */

#ifndef _RANDPOISSON_H

#include "oct-types.h"

#ifdef  __cplusplus
extern "C" {
#endif

extern double oct_randp (double L);
extern void oct_fill_randp (double L, octave_idx_type n, double *p);

#ifdef  __cplusplus
}
#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; End: ***
*/

