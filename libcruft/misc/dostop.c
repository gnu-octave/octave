/* dostop.c                                              -*- C -*- */
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

#include <stdlib.h>
#include <string.h>

#include "f77-fcn.h"
#include "lo-error.h"

/* All the STOP statements in the Fortran routines have been replaced
   with a call to XSTOPX, defined in the file libcruft/misc/xstopx.f.

   The XSTOPX function calls this function, which will longjmp back to
   the entry point for the Fortran function that called us.   Then the
   calling function should do whatever cleanup is necessary. */

volatile void
#if defined (F77_APPEND_UNDERSCORE)
dostop_ (const char *s, const int *slen)
#else
dostop (const char *s, const int *slen)
#endif
{
  int len = *slen;
  if (len > 0)
    {
      char *tmp = malloc (len + 1);
      strncpy (tmp, s, len);
      (*current_liboctave_error_handler) ("%s", tmp);
      free (tmp);
    }

  longjmp (f77_context, 1);
}

/*
;;; Local Variables: ***
;;; mode: C ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
