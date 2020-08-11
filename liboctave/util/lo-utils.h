////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2020 The Octave Project Developers
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

#include "lo-cutils.h"
#include "oct-cmplx.h"
#include "quit.h"

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
extern OCTAVE_API bool xis_one_or_zero (double x);
extern OCTAVE_API bool xis_zero (double x);
extern OCTAVE_API bool xtoo_large_for_float (double x);

extern OCTAVE_API bool xtoo_large_for_float (const Complex&  x);

extern OCTAVE_API bool xis_int_or_inf_or_nan (float x);
extern OCTAVE_API bool xis_one_or_zero (float x);
extern OCTAVE_API bool xis_zero (float x);
extern OCTAVE_API bool xtoo_large_for_float (float x);

extern OCTAVE_API char * strsave (const char *);

extern OCTAVE_API std::string octave_fgets (std::FILE *);
extern OCTAVE_API std::string octave_fgetl (std::FILE *);

extern OCTAVE_API std::string octave_fgets (std::FILE *, bool& eof);
extern OCTAVE_API std::string octave_fgetl (std::FILE *, bool& eof);

namespace octave
{
  template <typename T>
  T
  read_value (std::istream& is)
  {
    T retval;
    is >> retval;
    return retval;
  }

  template <> double read_value (std::istream& is);
  template <> Complex read_value (std::istream& is);
  template <> float read_value (std::istream& is);
  template <> FloatComplex read_value (std::istream& is);

  template <typename T>
  void
  write_value (std::ostream& os, const T& value)
  {
    os << value;
  }

  template <> void write_value (std::ostream& os, const double& value);
  template <> void write_value (std::ostream& os, const Complex& value);
  template <> void write_value (std::ostream& os, const float& value);
  template <> void write_value (std::ostream& os, const FloatComplex& value);
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
