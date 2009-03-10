/*

Copyright (C) 1996, 1997, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
              2007, 2008 John W. Eaton

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

#if !defined (octave_liboctave_utils_h)
#define octave_liboctave_utils_h 1

#include <cstdio>

#include <iosfwd>
#include <string>

#include "oct-cmplx.h"
#include "oct-types.h"
#include "syswait.h"

extern OCTAVE_API octave_idx_type NINTbig (double x);
extern OCTAVE_API octave_idx_type NINTbig (float x);
extern OCTAVE_API int NINT (double x);
extern OCTAVE_API int NINT (float x);
extern OCTAVE_API double D_NINT (double x);
extern OCTAVE_API float F_NINT (float x);

extern OCTAVE_API char *strsave (const char *);

extern OCTAVE_API void octave_putenv (const std::string&, const std::string&);

extern OCTAVE_API std::string octave_fgets (std::FILE *);
extern OCTAVE_API std::string octave_fgetl (std::FILE *);

extern OCTAVE_API std::string octave_fgets (std::FILE *, bool& eof);
extern OCTAVE_API std::string octave_fgetl (std::FILE *, bool& eof);

extern "C" OCTAVE_API int octave_gethostname (char *, int);

extern "C" OCTAVE_API void octave_qsort (void *base, size_t n, size_t size,
			      int (*cmp) (const void *, const void *));

extern "C" OCTAVE_API char *oct_strptime (const char *buf, const char *format,
			       struct tm *tm);

extern "C" OCTINTERP_API int octave_strcasecmp (const char *s1, const char *s2);

extern "C" OCTINTERP_API int octave_strncasecmp (const char *s1, const char *s2, size_t n);

extern OCTAVE_API double octave_read_double (std::istream& is);
extern OCTAVE_API Complex octave_read_complex (std::istream& is);

extern OCTAVE_API void octave_write_double (std::ostream& os, double dval);
extern OCTAVE_API void octave_write_complex (std::ostream& os, const Complex& cval);

extern OCTAVE_API float octave_read_float (std::istream& is);
extern OCTAVE_API FloatComplex octave_read_float_complex (std::istream& is);

extern OCTAVE_API void octave_write_float (std::ostream& os, float dval);
extern OCTAVE_API void octave_write_float_complex (std::ostream& os, const FloatComplex& cval);

#ifdef HAVE_LOADLIBRARY_API
#include <windows.h>
extern "C" OCTAVE_API void * octave_w32_library_search (HINSTANCE handle, const char *name);
#undef min
#undef max
#endif
#endif

extern "C" OCTAVE_API pid_t octave_waitpid (pid_t pid, int *status, int options);

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
