/*

Copyright (C) 2000 John W. Eaton

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
#include <time.h>

void
octave_qsort (void *base, size_t n, size_t size,
	      int (*cmp) (const void *, const void *))
{
  qsort (base, n, size, cmp);
}

extern char *strptime ();

char *
oct_strptime (const char *buf, const char *format, struct tm *tm)
{
  return strptime (buf, format, tm);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
