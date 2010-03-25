// utils.cc
/*

Copyright (C) 1996, 1997, 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008
              John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <limits>
#include <string>

#include <sys/types.h>
#include <unistd.h>

#include "quit.h"

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"

// Convert X to the nearest integer value.  Should not pass NaN to
// this function.

// Sometimes you need a large integer, but not always.

octave_idx_type
NINTbig (double x)
{
  if (x > std::numeric_limits<octave_idx_type>::max ())
    return std::numeric_limits<octave_idx_type>::max ();
  else if (x < std::numeric_limits<octave_idx_type>::min ())
    return std::numeric_limits<octave_idx_type>::min ();
  else
    return static_cast<octave_idx_type> ((x > 0) ? (x + 0.5) : (x - 0.5));
}

octave_idx_type
NINTbig (float x)
{
  if (x > std::numeric_limits<octave_idx_type>::max ())
    return std::numeric_limits<octave_idx_type>::max ();
  else if (x < std::numeric_limits<octave_idx_type>::min ())
    return std::numeric_limits<octave_idx_type>::min ();
  else
    return static_cast<octave_idx_type> ((x > 0) ? (x + 0.5) : (x - 0.5));
}

int
NINT (double x)
{
  if (x > std::numeric_limits<int>::max ())
    return std::numeric_limits<int>::max ();
  else if (x < std::numeric_limits<int>::min ())
    return std::numeric_limits<int>::min ();
  else
    return static_cast<int> ((x > 0) ? (x + 0.5) : (x - 0.5));
}

int
NINT (float x)
{
  if (x > std::numeric_limits<int>::max ())
    return std::numeric_limits<int>::max ();
  else if (x < std::numeric_limits<int>::min ())
    return std::numeric_limits<int>::min ();
  else
    return static_cast<int> ((x > 0) ? (x + 0.5) : (x - 0.5));
}

double
D_NINT (double x)
{
  if (xisinf (x) || xisnan (x))
    return x;
  else
    return floor (x + 0.5);
}

float
F_NINT (float x)
{
  if (xisinf (x) || xisnan (x))
    return x;
  else
    return floor (x + 0.5);
}

// Save a string.

char *
strsave (const char *s)
{
  if (! s)
    return 0;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

// This function was adapted from xputenv from Karl Berry's kpathsearch
// library.

// FIXME -- make this do the right thing if we don't have a
// SMART_PUTENV.

void
octave_putenv (const std::string& name, const std::string& value)
{
  int new_len = name.length () + value.length () + 2;

  char *new_item = static_cast<char*> (gnulib::malloc (new_len));

  sprintf (new_item, "%s=%s", name.c_str (), value.c_str ());

  // As far as I can see there's no way to distinguish between the
  // various errors; putenv doesn't have errno values.

  if (putenv (new_item) < 0)
    (*current_liboctave_error_handler) ("putenv (%s) failed", new_item);
}

std::string
octave_fgets (FILE *f)
{
  bool eof;
  return octave_fgets (f, eof);
}

std::string
octave_fgets (FILE *f, bool& eof)
{
  eof = false;

  std::string retval;

  int grow_size = 1024;
  int max_size = grow_size;

  char *buf = static_cast<char *> (gnulib::malloc (max_size));
  char *bufptr = buf;
  int len = 0;

  do
    {
      if (fgets (bufptr, grow_size, f))
        {
          len = strlen (bufptr);

          if (len == grow_size - 1)
            {
              int tmp = bufptr - buf + grow_size - 1;
              grow_size *= 2;
              max_size += grow_size;
              buf = static_cast<char *> (gnulib::realloc (buf, max_size));
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

              buf = 0;
            }

          break;
        }
    }
  while (retval.empty ());

  if (buf)
    free (buf);

  octave_quit ();

  return retval;
}

std::string
octave_fgetl (FILE *f)
{
  bool eof;
  return octave_fgetl (f, eof);
}

std::string
octave_fgetl (FILE *f, bool& eof)
{
  std::string retval = octave_fgets (f, eof);

  size_t len = retval.length ();

  if (retval[len-1] == '\n')
    retval.resize (len-1);

  return retval;
}

static inline double
read_inf_nan_na (std::istream& is, char c, char sign = '+')
{
  double d = 0.0;

  switch (c)
    {
    case 'i': case 'I':
      {
        c = is.get ();
        if (c == 'n' || c == 'N')
          {
            c = is.get ();
            if (c == 'f' || c == 'F')
              d = sign == '-' ? -octave_Inf : octave_Inf;
            else
              is.putback (c);
          }
        else
          is.putback (c);
      }
      break;

    case 'n': case 'N':
      {
        c = is.get ();
        if (c == 'a' || c == 'A')
          {
            c = is.get ();
            if (c == 'n' || c == 'N')
              d = octave_NaN;
            else
              {
                is.putback (c);
                d = octave_NA;
              }
          }
        else
          is.putback (c);
      }
      break;

    default:
      abort ();
    }

  return d;
}

template <>
double
octave_read_value (std::istream& is)
{
  double d = 0.0;

  char c1 = ' ';

  while (isspace (c1))
    c1 = is.get ();

  switch (c1)
    {
    case '-':
      {
        char c2 = 0;
        c2 = is.get ();
        if (c2 == 'i' || c2 == 'I')
          d = read_inf_nan_na (is, c2, c1);
        else
          {
            is.putback (c2);
            is.putback (c1);
            is >> d;
          }
      }
      break;

    case '+':
      {
        char c2 = 0;
        c2 = is.get ();
        if (c2 == 'i' || c2 == 'I')
          d = read_inf_nan_na (is, c2, c1);
        else
          {
            is.putback (c2);
            is.putback (c1);
            is >> d;
          }
      }
      break;

    case 'i': case 'I':
    case 'n': case 'N':
      d = read_inf_nan_na (is, c1);
      break;

    default:
      is.putback (c1);
      is >> d;
    }

  return d;
}

template <>
Complex
octave_read_value (std::istream& is)
{
  double re = 0.0, im = 0.0;

  Complex cx = 0.0;

  char ch = ' ';

  while (isspace (ch))
    ch = is.get ();

  if (ch == '(')
    {
      re = octave_read_value<double> (is);
      ch = is.get ();

      if (ch == ',')
        {
          im = octave_read_value<double> (is);
          ch = is.get ();

          if (ch == ')')
            cx = Complex (re, im);
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
      cx = octave_read_value<double> (is);
    }

  return cx;

}

static inline float
read_float_inf_nan_na (std::istream& is, char c, char sign = '+')
{
  float d = 0.0;

  switch (c)
    {
    case 'i': case 'I':
      {
        c = is.get ();
        if (c == 'n' || c == 'N')
          {
            c = is.get ();
            if (c == 'f' || c == 'F')
              d = sign == '-' ? -octave_Inf : octave_Inf;
            else
              is.putback (c);
          }
        else
          is.putback (c);
      }
      break;

    case 'n': case 'N':
      {
        c = is.get ();
        if (c == 'a' || c == 'A')
          {
            c = is.get ();
            if (c == 'n' || c == 'N')
              d = octave_NaN;
            else
              {
                is.putback (c);
                d = octave_NA;
              }
          }
        else
          is.putback (c);
      }
      break;

    default:
      abort ();
    }

  return d;
}

template <>
float
octave_read_value (std::istream& is)
{
  float d = 0.0;

  char c1 = ' ';

  while (isspace (c1))
    c1 = is.get ();

  switch (c1)
    {
    case '-':
      {
        char c2 = 0;
        c2 = is.get ();
        if (c2 == 'i' || c2 == 'I')
          d = read_float_inf_nan_na (is, c2, c1);
        else
          {
            is.putback (c2);
            is.putback (c1);
            is >> d;
          }
      }
      break;

    case '+':
      {
        char c2 = 0;
        c2 = is.get ();
        if (c2 == 'i' || c2 == 'I')
          d = read_float_inf_nan_na (is, c2, c1);
        else
          {
            is.putback (c2);
            is.putback (c1);
            is >> d;
          }
      }
      break;

    case 'i': case 'I':
    case 'n': case 'N':
      d = read_float_inf_nan_na (is, c1);
      break;

    default:
      is.putback (c1);
      is >> d;
    }

  return d;
}

template <>
FloatComplex
octave_read_value (std::istream& is)
{
  float re = 0.0, im = 0.0;

  FloatComplex cx = 0.0;

  char ch = ' ';

  while (isspace (ch))
    ch = is.get ();

  if (ch == '(')
    {
      re = octave_read_value<float> (is);
      ch = is.get ();

      if (ch == ',')
        {
          im = octave_read_value<float> (is);
          ch = is.get ();

          if (ch == ')')
            cx = FloatComplex (re, im);
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
      cx = octave_read_value<float> (is);
    }

  return cx;

}

void
octave_write_double (std::ostream& os, double d)
{
  if (lo_ieee_is_NA (d))
    os << "NA";
  else if (lo_ieee_isnan (d))
    os << "NaN";
  else if (lo_ieee_isinf (d))
    os << (d < 0 ? "-Inf" : "Inf");
  else
    os << d;
}

void
octave_write_complex (std::ostream& os, const Complex& c)
{
  os << "(";
  octave_write_double (os, real (c));
  os << ",";
  octave_write_double (os, imag (c));
  os << ")";
}

void
octave_write_float (std::ostream& os, float d)
{
  if (lo_ieee_is_NA (d))
    os << "NA";
  else if (lo_ieee_isnan (d))
    os << "NaN";
  else if (lo_ieee_isinf (d))
    os << (d < 0 ? "-Inf" : "Inf");
  else
    os << d;
}

void
octave_write_float_complex (std::ostream& os, const FloatComplex& c)
{
  os << "(";
  octave_write_float (os, real (c));
  os << ",";
  octave_write_float (os, imag (c));
  os << ")";
}
