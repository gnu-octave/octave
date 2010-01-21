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

#include <iostream>
#include <string>

#include "oct-cmplx.h"
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

extern "C" OCTAVE_API int
octave_link (const char *old_name, const char *new_name);

extern "C" OCTAVE_API int
octave_symlink (const char *old_name, const char *new_name);

extern "C" OCTAVE_API int
octave_readlink (const char *name, char *buf, size_t size);

extern "C" OCTAVE_API int octave_mkdir (const char *name, mode_t mode);

extern "C" OCTAVE_API int octave_mkfifo (const char *name, mode_t mode);

extern "C" OCTAVE_API void octave_qsort (void *base, size_t n, size_t size,
			      int (*cmp) (const void *, const void *));

extern "C" OCTAVE_API int octave_rmdir (const char *name);

extern "C" OCTAVE_API int octave_rename (const char *from, const char *to);

extern "C" OCTAVE_API char *oct_strptime (const char *buf, const char *format,
                                          struct tm *tm);

extern "C" OCTINTERP_API int octave_strcasecmp (const char *s1, const char *s2);

extern "C" OCTINTERP_API int octave_strncasecmp (const char *s1, const char *s2, size_t n);

extern "C" OCTAVE_API char *
octave_tempnam (const char *pdir, const char *ppfx);

extern "C" OCTAVE_API mode_t octave_umask (mode_t);

extern "C" OCTAVE_API int octave_unlink (const char *name);

template <typename T>
T
octave_read_value (std::istream& is)
{
  T retval;
  is >> retval;
  return retval;
}

template <> OCTAVE_API double octave_read_value (std::istream& is);
template <> OCTAVE_API Complex octave_read_value (std::istream& is);
template <> OCTAVE_API float octave_read_value (std::istream& is);
template <> OCTAVE_API FloatComplex octave_read_value (std::istream& is);

// The next four functions are provided for backward compatibility.
inline double
octave_read_double (std::istream& is)
{
  return octave_read_value<double> (is);
}

inline Complex
octave_read_complex (std::istream& is)
{
  return octave_read_value<Complex> (is);
}

inline float
octave_read_float (std::istream& is)
{
  return octave_read_value<float> (is);
}

inline FloatComplex
octave_read_float_complex (std::istream& is)
{
  return octave_read_value<FloatComplex> (is);
}

extern OCTAVE_API void octave_write_double (std::ostream& os, double dval);
extern OCTAVE_API void octave_write_complex (std::ostream& os, const Complex& cval);

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
