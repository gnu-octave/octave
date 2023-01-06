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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>
#include <cstring>

#include <complex>
#include <istream>
#include <limits>
#include <ostream>
#include <string>

#include "quit.h"

#include "intprops-wrappers.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-inttypes.h"

OCTAVE_BEGIN_NAMESPACE(octave)

bool is_int_or_inf_or_nan (double x)
{
  return math::isnan (x) || math::x_nint (x) == x;
}

bool too_large_for_float (double x)
{
  return (math::isfinite (x)
          && fabs (x) > std::numeric_limits<float>::max ());
}

bool too_large_for_float (const Complex& x)
{
  return (too_large_for_float (x.real ())
          || too_large_for_float (x.imag ()));
}

bool is_int_or_inf_or_nan (float x)
{
  return math::isnan (x) || math::x_nint (x) == x;
}

// Save a string.

char * strsave (const char *s)
{
  if (! s)
    return nullptr;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

std::string fgets (FILE *f)
{
  bool eof;
  return fgets (f, eof);
}

std::string fgets (FILE *f, bool& eof)
{
  eof = false;

  std::string retval;

  int grow_size = 1024;
  int max_size = grow_size;

  char *buf = static_cast<char *> (std::malloc (max_size));
  if (! buf)
    (*current_liboctave_error_handler) ("octave_fgets: unable to malloc %d bytes", max_size);

  char *bufptr = buf;
  int len = 0;

  do
    {
      if (std::fgets (bufptr, grow_size, f))
        {
          len = strlen (bufptr);

          if (len == grow_size - 1)
            {
              int tmp = bufptr - buf + grow_size - 1;
              grow_size *= 2;
              max_size += grow_size;
              auto tmpbuf = static_cast<char *> (std::realloc (buf, max_size));
              if (! tmpbuf)
                {
                  free (buf);
                  (*current_liboctave_error_handler) ("octave_fgets: unable to realloc %d bytes", max_size);
                }
              buf = tmpbuf;
              bufptr = buf + tmp;

              if (*(bufptr-1) == '\n')
                {
                  *bufptr = '\0';
                  retval = buf;
                }
            }
          else if (bufptr[len-1] != '\n')
            {
              bufptr[len++] = '\n';
              bufptr[len] = '\0';
              retval = buf;
            }
          else
            retval = buf;
        }
      else
        {
          if (len == 0)
            {
              eof = true;

              free (buf);

              buf = nullptr;
            }

          break;
        }
    }
  while (retval.empty ());

  free (buf);

  octave_quit ();

  return retval;
}

std::string fgetl (FILE *f)
{
  bool eof;
  return fgetl (f, eof);
}

std::string fgetl (FILE *f, bool& eof)
{
  std::string retval = fgets (f, eof);

  if (! retval.empty () && retval.back () == '\n')
    retval.pop_back ();

  return retval;
}

template <typename T>
T
read_value (std::istream& is)
{
  T retval;
  is >> retval;
  return retval;
}

template OCTAVE_API bool read_value<bool> (std::istream& is);
template OCTAVE_API octave_int8 read_value<octave_int8> (std::istream& is);
template OCTAVE_API octave_int16 read_value<octave_int16> (std::istream& is);
template OCTAVE_API octave_int32 read_value<octave_int32> (std::istream& is);
template OCTAVE_API octave_int64 read_value<octave_int64> (std::istream& is);
template OCTAVE_API octave_uint8 read_value<octave_uint8> (std::istream& is);
template OCTAVE_API octave_uint16 read_value<octave_uint16> (std::istream& is);
template OCTAVE_API octave_uint32 read_value<octave_uint32> (std::istream& is);
template OCTAVE_API octave_uint64 read_value<octave_uint64> (std::istream& is);

// Note that the caller is responsible for repositioning the stream on
// failure.

template <typename T>
T
read_inf_nan_na (std::istream& is, char c0)
{
  T val = 0.0;

  switch (c0)
    {
    case 'i': case 'I':
      {
        char c1 = is.get ();
        if (c1 == 'n' || c1 == 'N')
          {
            char c2 = is.get ();
            if (c2 == 'f' || c2 == 'F')
              val = std::numeric_limits<T>::infinity ();
            else
              is.setstate (std::ios::failbit);
          }
        else
          is.setstate (std::ios::failbit);
      }
      break;

    case 'n': case 'N':
      {
        char c1 = is.get ();
        if (c1 == 'a' || c1 == 'A')
          {
            char c2 = is.get ();
            if (c2 == 'n' || c2 == 'N')
              val = std::numeric_limits<T>::quiet_NaN ();
            else
              {
                val = numeric_limits<T>::NA ();
                if (c2 != std::istream::traits_type::eof ())
                  is.putback (c2);
                else
                  is.clear (is.rdstate () & ~std::ios::failbit);
              }
          }
        else
          is.setstate (std::ios::failbit);
      }
      break;

    default:
      (*current_liboctave_error_handler)
        ("read_inf_nan_na: invalid character '%c'", c0);
    }

  return val;
}

// Read a double value.  Discard any sign on NaN and NA.

template <typename T>
double
read_fp_value (std::istream& is)
{
  T val = 0.0;

  // FIXME: resetting stream position is likely to fail unless we are
  // reading from a file.
  std::streampos pos = is.tellg ();

  char c1 = ' ';

  while (isspace (c1))
    c1 = is.get ();

  bool neg = false;

  switch (c1)
    {
    case '-':
      neg = true;
      OCTAVE_FALLTHROUGH;

    case '+':
      {
        char c2 = 0;
        c2 = is.get ();
        if (c2 == 'i' || c2 == 'I' || c2 == 'n' || c2 == 'N')
          val = read_inf_nan_na<T> (is, c2);
        else
          {
            is.putback (c2);
            is >> val;
          }

        if (neg && ! is.fail ())
          val = -val;
      }
      break;

    case 'i': case 'I':
    case 'n': case 'N':
      val = read_inf_nan_na<T> (is, c1);
      break;

    default:
      is.putback (c1);
      is >> val;
      break;
    }

  std::ios::iostate status = is.rdstate ();
  if (status & std::ios::failbit)
    {
      // Convert MAX_VAL returned by C++ streams for very large numbers to Inf
      if (val == std::numeric_limits<T>::max ())
        {
          if (neg)
            val = -std::numeric_limits<T>::infinity ();
          else
            val = std::numeric_limits<T>::infinity ();
          is.clear (status & ~std::ios::failbit);
        }
      else
        {
          // True error.  Reset stream to original position and pass status on.
          is.clear ();
          is.seekg (pos);
          is.setstate (status);
        }
    }

  return val;
}

template <typename T>
std::complex<T>
read_cx_fp_value (std::istream& is)
{
  T re = 0.0;
  T im = 0.0;

  std::complex<T> cx = 0.0;

  char ch = ' ';

  while (isspace (ch))
    ch = is.get ();

  if (ch == '(')
    {
      re = read_value<T> (is);
      ch = is.get ();

      if (ch == ',')
        {
          im = read_value<T> (is);
          ch = is.get ();

          if (ch == ')')
            cx = std::complex<T> (re, im);
          else
            is.setstate (std::ios::failbit);
        }
      else if (ch == ')')
        cx = re;
      else
        is.setstate (std::ios::failbit);
    }
  else
    {
      is.putback (ch);
      cx = read_value<T> (is);
    }

  return cx;
}

// FIXME: Could we use traits and enable_if to avoid duplication in the
// following specializations?

template <> OCTAVE_API double read_value (std::istream& is)
{
                                          return read_fp_value<double> (is);
}

template <> OCTAVE_API Complex read_value (std::istream& is)
{
                                           return read_cx_fp_value<double> (is);
}

template <> OCTAVE_API float read_value (std::istream& is)
{
                                         return read_fp_value<float> (is);
}

template <> OCTAVE_API FloatComplex read_value (std::istream& is)
{
                                                return read_cx_fp_value<float> (is);
}

template <typename T>
void
write_value (std::ostream& os, const T& value)
{
  os << value;
}

template OCTAVE_API void
write_value<bool> (std::ostream& os, const bool& value);
template OCTAVE_API void
write_value<octave_int8> (std::ostream& os, const octave_int8& value);
template OCTAVE_API void
write_value<octave_int16> (std::ostream& os, const octave_int16& value);
template OCTAVE_API void
write_value<octave_int32> (std::ostream& os, const octave_int32& value);
template OCTAVE_API void
write_value<octave_int64> (std::ostream& os, const octave_int64& value);
template OCTAVE_API void
write_value<octave_uint8> (std::ostream& os, const octave_uint8& value);
template OCTAVE_API void
write_value<octave_uint16> (std::ostream& os, const octave_uint16& value);
template OCTAVE_API void
write_value<octave_uint32> (std::ostream& os, const octave_uint32& value);
template OCTAVE_API void
write_value<octave_uint64> (std::ostream& os, const octave_uint64& value);

// Note: precision is supposed to be managed outside of this function by
// setting stream parameters.

template <> OCTAVE_API void
write_value (std::ostream& os, const double& value)
{
  if (lo_ieee_is_NA (value))
    os << "NA";
  else if (lo_ieee_isnan (value))
    os << "NaN";
  else if (lo_ieee_isinf (value))
    os << (value < 0 ? "-Inf" : "Inf");
  else
    os << value;
}

template <> OCTAVE_API void
write_value (std::ostream& os, const Complex& value)
{
  os << '(';
  write_value<double> (os, real (value));
  os << ',';
  write_value<double> (os, imag (value));
  os << ')';
}

// Note: precision is supposed to be managed outside of this function by
// setting stream parameters.

template <> OCTAVE_API void
write_value (std::ostream& os, const float& value)
{
  if (lo_ieee_is_NA (value))
    os << "NA";
  else if (lo_ieee_isnan (value))
    os << "NaN";
  else if (lo_ieee_isinf (value))
    os << (value < 0 ? "-Inf" : "Inf");
  else
    os << value;
}

template <> OCTAVE_API void
write_value (std::ostream& os, const FloatComplex& value)
{
  os << '(';
  write_value<float> (os, real (value));
  os << ',';
  write_value<float> (os, imag (value));
  os << ')';
}

OCTAVE_BEGIN_NAMESPACE(math)

bool int_multiply_overflow (int a, int b, int *r)
{
  return octave_i_multiply_overflow_wrapper (a, b, r);
}

bool int_multiply_overflow (long int a, long int b, long int *r)
{
  return octave_li_multiply_overflow_wrapper (a, b, r);
}

#if defined (OCTAVE_HAVE_LONG_LONG_INT)
bool int_multiply_overflow (long long int a, long long int b,
                            long long int *r)
{
  return octave_lli_multiply_overflow_wrapper (a, b, r);
}
#endif

bool int_multiply_overflow (unsigned int a, unsigned int b,
                            unsigned int *r)
{
  return octave_ui_multiply_overflow_wrapper (a, b, r);
}

bool int_multiply_overflow (unsigned long int a, unsigned long int b,
                            unsigned long int *r)
{
  return octave_uli_multiply_overflow_wrapper (a, b, r);
}

#if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
bool int_multiply_overflow (unsigned long long int a,
                            unsigned long long int b,
                            unsigned long long int *r)
{
  return octave_ulli_multiply_overflow_wrapper (a, b, r);
}
#endif

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
