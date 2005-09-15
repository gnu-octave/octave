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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_liboctave_utils_h)
#define octave_liboctave_utils_h 1

#include <cstdio>

#include <iostream>
#include <string>

#include "oct-cmplx.h"
#include "oct-types.h"

extern octave_idx_type NINTbig (double x);
extern int NINT (double x);
extern double D_NINT (double x);

extern char *strsave (const char *);

extern void octave_putenv (const std::string&, const std::string&);

extern std::string octave_fgets (std::FILE *);
extern std::string octave_fgetl (std::FILE *);

extern std::string octave_fgets (std::FILE *, bool& eof);
extern std::string octave_fgetl (std::FILE *, bool& eof);

extern "C" int octave_gethostname (char *, int);

extern "C" void octave_qsort (void *base, size_t n, size_t size,
			      int (*cmp) (const void *, const void *));

extern "C" char *oct_strptime (const char *buf, const char *format,
			       struct tm *tm);

extern double octave_read_double (std::istream& is);
extern Complex octave_read_complex (std::istream& is);

extern void octave_write_double (std::ostream& os, double dval);
extern void octave_write_complex (std::ostream& os, const Complex& cval);

#ifdef HAVE_LOADLIBRARY_API
#include <windows.h>
extern "C" void * octave_w32_library_search (HINSTANCE handle, const char *name);
#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
