////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_lo_utils_h)
#define octave_lo_utils_h 1

#include "octave-config.h"

#include <cstdio>

#include <iosfwd>
#include <string>

#include "oct-cmplx.h"
#include "quit.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Generic any/all test functionality with arbitrary predicate.

template <typename F, typename T, bool zero>
bool
any_all_test (F fcn, const T *m, octave_idx_type len)
{
  octave_idx_type i;

  for (i = 0; i < len - 3; i += 4)
    {
      octave_quit ();

      if (fcn (m[i]) != zero
          || fcn (m[i+1]) != zero
          || fcn (m[i+2]) != zero
          || fcn (m[i+3]) != zero)
        return ! zero;
    }

  octave_quit ();

  for (; i < len; i++)
    if (fcn (m[i]) != zero)
      return ! zero;

  return zero;
}

extern OCTAVE_API bool xis_int_or_inf_or_nan (double x);

template <typename T>
bool is_one_or_zero (const T& x)
{
  return x == T (0) || x == T (1);
}

template <typename T>
bool is_zero (const T& x)
{
  return x == T (0);
}

extern OCTAVE_API bool too_large_for_float (double x);

extern OCTAVE_API bool too_large_for_float (const Complex&  x);

extern OCTAVE_API bool is_int_or_inf_or_nan (float x);
extern OCTAVE_API bool too_large_for_float (float x);

extern OCTAVE_API char * strsave (const char *);

extern OCTAVE_API std::string fgets (std::FILE *);
extern OCTAVE_API std::string fgetl (std::FILE *);

extern OCTAVE_API std::string fgets (std::FILE *, bool& eof);
extern OCTAVE_API std::string fgetl (std::FILE *, bool& eof);

template <typename T> OCTAVE_API T read_value (std::istream& is);

template <> OCTAVE_API double read_value (std::istream& is);
template <> OCTAVE_API Complex read_value (std::istream& is);
template <> OCTAVE_API float read_value (std::istream& is);
template <> OCTAVE_API FloatComplex read_value (std::istream& is);

template <typename T> OCTAVE_API void write_value (std::ostream& os, const T& value);

template <> OCTAVE_API void write_value (std::ostream& os, const double& value);
template <> OCTAVE_API void write_value (std::ostream& os, const Complex& value);
template <> OCTAVE_API void write_value (std::ostream& os, const float& value);
template <> OCTAVE_API void write_value (std::ostream& os, const FloatComplex& value);

OCTAVE_BEGIN_NAMESPACE(math)

extern OCTAVE_API bool int_multiply_overflow (int a, int b, int *r);

extern OCTAVE_API bool
int_multiply_overflow (long int a, long int b, long int *r);

#if defined (OCTAVE_HAVE_LONG_LONG_INT)
extern OCTAVE_API bool
int_multiply_overflow (long long int a, long long int b, long long int *r);
#endif

extern OCTAVE_API bool
int_multiply_overflow (unsigned int a, unsigned int b, unsigned int *r);

extern OCTAVE_API bool
int_multiply_overflow (unsigned long int a, unsigned long int b,
                       unsigned long int *r);

#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
extern OCTAVE_API bool
int_multiply_overflow (unsigned long long int a, unsigned long long int b,
                       unsigned long long int *r);
#endif

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
template <typename F, typename T, bool zero>
OCTAVE_DEPRECATED (7, "use 'octave::any_all_test' instead")
bool
any_all_test (F fcn, const T *m, octave_idx_type len)
{
  return octave::any_all_test<F, T, zero> (fcn, m, len);
}

OCTAVE_DEPRECATED (7, "use 'octave::is_int_or_inf_or_nan' instead")
inline bool xis_int_or_inf_or_nan (double x)
{
  return octave::is_int_or_inf_or_nan (x);
}

template <typename T>
OCTAVE_DEPRECATED (7, "use 'octave::is_one_or_zero' instead")
bool
xis_one_or_zero (const T& x)
{
  return octave::is_one_or_zero (x);
}

template <typename T>
OCTAVE_DEPRECATED (7, "use 'octave::is_zero' instead")
bool
xis_zero (const T& x)
{
  return octave::is_zero (x);
}

OCTAVE_DEPRECATED (7, "use 'octave::' instead")
inline bool xtoo_large_for_float (double x)
{
  return octave::too_large_for_float (x);
}

OCTAVE_DEPRECATED (7, "use 'octave::' instead")
inline bool xtoo_large_for_float (const Complex&  x)
{
  return octave::too_large_for_float (x);
}

OCTAVE_DEPRECATED (7, "use 'octave::' instead")
inline bool xis_int_or_inf_or_nan (float x)
{
  return octave::is_int_or_inf_or_nan (x);
}

OCTAVE_DEPRECATED (7, "use 'octave::' instead")
inline bool xtoo_large_for_float (float x)
{
  return octave::too_large_for_float (x);
}

OCTAVE_DEPRECATED (7, "use 'octave::strsave' instead")
inline char * strsave (const char *s)
{
  return octave::strsave (s);
}

OCTAVE_DEPRECATED (7, "use 'octave::fgets' instead")
inline std::string octave_fgets (std::FILE *f)
{
  return octave::fgets (f);
}

OCTAVE_DEPRECATED (7, "use 'octave::fgetl' instead")
inline std::string octave_fgetl (std::FILE *f)
{
  return octave::fgetl (f);
}

OCTAVE_DEPRECATED (7, "use 'octave::fgets' instead")
inline std::string octave_fgets (std::FILE *f, bool& eof)
{
  return octave::fgets (f, eof);
}

OCTAVE_DEPRECATED (7, "use 'octave::fgetl' instead")
inline std::string octave_fgetl (std::FILE *f, bool& eof)
{
  return octave::fgetl (f, eof);
}

OCTAVE_DEPRECATED (7, "use 'octave::read_value<T>' instead")
inline double
octave_read_double (std::istream& is)
{
  return octave::read_value<double> (is);
}

OCTAVE_DEPRECATED (7, "use 'octave::read_value<T>' instead")
inline Complex
octave_read_complex (std::istream& is)
{
  return octave::read_value<Complex> (is);
}

OCTAVE_DEPRECATED (7, "use 'octave::read_value<T>' instead")
inline float
octave_read_float (std::istream& is)
{
  return octave::read_value<float> (is);
}

OCTAVE_DEPRECATED (7, "use 'octave::read_value<T>' instead")
inline FloatComplex
octave_read_float_complex (std::istream& is)
{
  return octave::read_value<FloatComplex> (is);
}

OCTAVE_DEPRECATED (7, "use 'octave::write_value<T>' instead")
inline void
octave_write_double (std::ostream& os, double value)
{
  octave::write_value<double> (os, value);
}

OCTAVE_DEPRECATED (7, "use 'octave::write_value<T>' instead")
inline void
octave_write_complex (std::ostream& os, const Complex& value)
{
  octave::write_value<Complex> (os, value);
}

OCTAVE_DEPRECATED (7, "use 'octave::write_value<T>' instead")
inline void
octave_write_float (std::ostream& os, float value)
{
  octave::write_value<float> (os, value);
}

OCTAVE_DEPRECATED (7, "use 'octave::write_value<T>' instead")
inline void
octave_write_float_complex (std::ostream& os, const FloatComplex& value)
{
  octave::write_value<FloatComplex> (os, value);
}
#endif

#endif
