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

#if !defined (octave_liboctave_utils_h)
#define octave_liboctave_utils_h 1

#include <cstdio>

#include <string>

extern int NINT (double x);
extern double D_NINT (double x);

extern char *strsave (const char *);

extern void octave_putenv (const std::string&, const std::string&);

extern std::string octave_fgets (std::FILE *);

extern "C" void octave_qsort (void *base, size_t n, size_t size,
			      int (*cmp) (const void *, const void *));

extern "C" char *oct_strptime (const char *buf, const char *format,
			       struct tm *tm);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
