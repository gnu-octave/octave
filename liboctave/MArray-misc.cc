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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "MArray.h"
#include "MArray2.h"
#include "lo-error.h"

void
gripe_nonconformant (const char *op, int op1_len, int op2_len)
{
  (*current_liboctave_error_handler)
    ("%s: nonconformant arguments (op1 len: %d, op2 len: %d)",
     op, op1_len, op2_len);
}

void
gripe_nonconformant (const char *op, int op1_nr, int op1_nc,
		     int op2_nr, int op2_nc)
{
  (*current_liboctave_error_handler)
    ("%s: nonconformant arguments (op1 is %dx%d, op2 is %dx%d)",
     op, op1_nr, op1_nc, op2_nr, op2_nc);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
