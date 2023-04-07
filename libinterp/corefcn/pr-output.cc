////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#include <cmath>

#include <iomanip>
#include <limits>
#include <list>
#include <sstream>
#include <string>

#include "Array-util.h"
#include "CMatrix.h"
#include "Range.h"
#include "cmd-edit.h"
#include "dMatrix.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-cmplx.h"
#include "oct-string.h"
#include "quit.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "oct-stream.h"
#include "octave-preserve-stream-state.h"
#include "pager.h"
#include "parse.h"
#include "pr-flt-fmt.h"
#include "pr-output.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// TRUE means use a scaled fixed point format for 'format long' and
// 'format short'.
static bool Vfixed_point_format = false;

// TRUE means that the dimensions of empty objects should be printed
// like this: x = [](2x0).
bool Vprint_empty_dimensions = true;

// TRUE means that the rows of big matrices should be split into
// smaller slices that fit on the screen.
static bool Vsplit_long_rows = true;

// TRUE means don't do any fancy formatting.
static bool free_format = false;

// TRUE means print plus sign for nonzero, blank for zero.
static bool plus_format = false;

// First char for > 0, second for < 0, third for == 0.
static std::string plus_format_chars = "+- ";

// TRUE means always print in a rational approximation
static bool rat_format = false;

// Used to force the length of the rational approximation string for Frats
static int rat_string_len = -1;

// TRUE means always print like dollars and cents.
static bool bank_format = false;

// TRUE means print data in hexadecimal format.
static int hex_format = 0;

// TRUE means print data in binary-bit-pattern format.
static int bit_format = 0;

// TRUE means don't put newlines around the column number headers.
bool Vcompact_format = false;

// TRUE means use an e format.
static bool print_e = false;

// TRUE means use a g format.
static bool print_g = false;

// TRUE means print uppercase E in exponent field and A-F in hex format.
static bool uppercase_format = false;

// TRUE means use an engineering format.
static bool print_eng = false;

static int
calc_scale_exp (const int& x)
{
  if (! print_eng)
    return x;
  else
    return x - 3*static_cast<int> (x/3);

  // The expression above is equivalent to x - (x % 3).

  // According to the ISO specification for C++ the modulo operator is
  // compiler dependent if any of the arguments are negative.  Since
  // this function will need to work on negative arguments, and we want
  // to avoid portability issues, we re-implement the modulo function to
  // the desired behavior (truncation).  There may be a gnulib replacement.

  // ISO/IEC 14882:2003 : Programming languages -- C++. 5.6.4: ISO,
  // IEC. 2003 . "the binary % operator yields the remainder from the
  // division of the first expression by the second. .... If both
  // operands are nonnegative then the remainder is nonnegative; if not,
  // the sign of the remainder is implementation-defined".
}

template <typename T>
static inline int
engineering_exponent (T x)
{
  int ex = 0;

  if (x != 0)
    {
      T absval = (x < 0 ? -x : x);
      int logabsval = static_cast<int> (std::floor (log10 (absval)));

      // Avoid using modulo function with negative arguments for
      // portability.  See extended comment at calc_scale_exp

      if (logabsval < 0)
        ex = logabsval - 2 + ((-logabsval + 2) % 3);
      else
        ex = logabsval - (logabsval % 3);
    }

  return ex;
}

template <typename T>
static inline int
num_digits (T x)
{
  return 1 + (print_eng
              ? engineering_exponent (x)
              : static_cast<int> (std::floor (log10 (x))));
}

template <typename T>
int
pr_engineering_float<T>::exponent (void) const
{
  return engineering_exponent (m_val);
}

template <typename T>
T
pr_engineering_float<T>::mantissa (void) const
{
  return m_val / std::pow (static_cast<T> (10), exponent ());
}

template <typename T>
std::ostream&
operator << (std::ostream& os, const pr_engineering_float<T>& pef)
{
  octave::preserve_stream_state stream_state (os);

  float_format real_fmt = pef.m_ff;

  if (real_fmt.width () >= 0)
    os << std::setw (real_fmt.width () - real_fmt.exponent_width ());

  if (real_fmt.precision () >= 0)
    os << std::setprecision (real_fmt.precision ());

  os.flags (real_fmt.format_flags ());

  os << pef.mantissa ();

  int ex = pef.exponent ();
  if (ex < 0)
    {
      if (uppercase_format)
        os << std::setw (0) << "E-";
      else
        os << std::setw (0) << "e-";
      ex = -ex;
    }
  else
    {
      if (uppercase_format)
        os << std::setw (0) << "E+";
      else
        os << std::setw (0) << "e+";
    }

  os << std::setw (real_fmt.exponent_width () - 2)
     << std::setfill ('0') << ex;

  return os;
}

template <typename T>
std::ostream&
operator << (std::ostream& os, const pr_formatted_float<T>& pff)
{
  octave::preserve_stream_state stream_state (os);

  float_format real_fmt = pff.m_ff;

  if (real_fmt.width () >= 0)
    os << std::setw (real_fmt.width ());

  if (real_fmt.precision () >= 0)
    os << std::setprecision (real_fmt.precision ());

  os.flags (real_fmt.format_flags ());

  os << pff.m_val;

  return os;
}

template <typename T>
std::ostream&
operator << (std::ostream& os, const pr_rational_float<T>& prf)
{
  octave::preserve_stream_state stream_state (os);

  float_format real_fmt = prf.m_ff;
  bool have_neg_sign = prf.m_val < 0;

  int fw = (rat_string_len > 0 ? rat_string_len : real_fmt.width ());
  std::string s;

  if (have_neg_sign)
    s = rational_approx (prf.m_val, fw);
  else
    s = rational_approx (prf.m_val, fw-1);

  if (fw >= 0)
    os << std::setw (fw);

  os.flags (real_fmt.format_flags ());

  if (s == "0")
    s = '*';
  else if (fw > 0)
    {
      if (s.find ('/') != std::string::npos)
        {
          if (s.length () > (static_cast<unsigned int> (fw)))
            s = '*';
        }
      else
        {
          if (have_neg_sign)
            {
              if (s.length () > (static_cast<unsigned int> (fw) - 2))
                s = '*';
            }
          else
            {
              if (s.length () > (static_cast<unsigned int> (fw) - 3))
                s = '*';
            }
        }
    }

  os << s;

  return os;
}

template <typename T>
static inline T
pr_max_internal (const MArray<T>& m)
{
  // We expect a 2-d array.
  error_unless (m.ndims () == 2);

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  T result = std::numeric_limits<T>::lowest ();

  bool all_inf_or_nan = true;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        T val = m(i, j);
        if (! octave::math::isfinite (val))
          continue;

        all_inf_or_nan = false;

        if (val > result)
          result = val;
      }

  if (all_inf_or_nan)
    result = 0;

  return result;
}

template <typename T>
static inline T
pr_min_internal (const MArray<T>& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  T result = std::numeric_limits<T>::max ();

  bool all_inf_or_nan = true;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        T val = m(i, j);
        if (! octave::math::isfinite (val))
          continue;

        all_inf_or_nan = false;

        if (val < result)
          result = val;
      }

  if (all_inf_or_nan)
    result = 0;

  return result;
}

template <typename>
struct pr_output_traits
{
  static const int DIGITS10;
  static const int MAX_FIELD_WIDTH;
};

template <>
struct pr_output_traits<double>
{
  static const int DIGITS10;
  static const int MAX_FIELD_WIDTH;
};

const int pr_output_traits<double>::DIGITS10 = 16;
const int pr_output_traits<double>::MAX_FIELD_WIDTH = 21;

template <>
struct pr_output_traits<float>
{
  static const int DIGITS10;
  static const int MAX_FIELD_WIDTH;
};

const int pr_output_traits<float>::DIGITS10 = 8;
const int pr_output_traits<float>::MAX_FIELD_WIDTH = 13;

// FIXME: it would be nice to share more code among these functions,..

// Works for double and float.

template <typename T>
static inline float_display_format
make_real_format (int digits, bool inf_or_nan, bool int_only)
{
  float_format fmt;

  int prec = std::min (output_precision (), pr_output_traits<T>::DIGITS10);

  int fw = 0, ld = 0, rd = 0;

  if (rat_format)
    {
      fw = 0;
      rd = 0;
    }
  else if (bank_format)
    {
      fw = (digits < 0 ? 4 : digits + 3);
      if (inf_or_nan)
        fw = 3;
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (T);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (T);
      rd = 0;
    }
  else if (inf_or_nan)
    {
      fw = 3;
    }
  else if (int_only)
    {
      fw = digits;
      ld = digits;
      rd = 0;
    }
  else
    {
      if (digits > 0)
        {
          ld = digits;
          rd = (prec > digits ? prec - digits : prec);
        }
      else if (digits < 0)
        {
          ld = 1;
          rd = (prec > digits ? prec - digits : prec);
        }
      else
        {
          ld = 1;
          rd = (prec > digits ? prec - 1 : prec);
        }

      fw = ld + 1 + rd;
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e || print_g || print_eng
          || ld + rd > pr_output_traits<T>::DIGITS10
          || fw > pr_output_traits<T>::MAX_FIELD_WIDTH
          || ld + rd > (1.5 * prec)))
    {
      if (print_g)
        fmt = float_format (prec, prec);
      else
        {
          // e+ddd
          int ex = 5;

          if (print_eng)
            {
              // -ddd.
              fw = 1 + prec + ex;
              if (inf_or_nan)
                {
                  fw = 3;
                  ex = 0;
                }
              fmt = float_format (fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              // -d.
              fw = prec + ex;
              if (inf_or_nan)
                {
                  fw = 3;
                  ex = 0;
                }
              fmt = float_format (fw, ex, prec - 1, std::ios::scientific);
            }
        }
    }
  else if (! bank_format && (inf_or_nan || int_only))
    fmt = float_format (fw, ld);
  else
    fmt = float_format (fw, rd, std::ios::fixed);

  if (uppercase_format)
    fmt.uppercase ();

  return float_display_format (fmt);
}

// Works for double and float.

template <typename T>
float_display_format
make_scalar_format (const T& val)
{
  if (free_format)
    return float_display_format ();

  bool inf_or_nan = (octave::math::isinf (val) || octave::math::isnan (val));

  bool int_only = (! inf_or_nan && octave::math::x_nint (val) == val);

  T val_abs = (val < 0 ? -val : val);

  int digits = (inf_or_nan || val_abs == 0) ? 0 : num_digits (val_abs);

  return make_real_format<T> (digits, inf_or_nan, int_only);
}

template <>
float_display_format
make_format (const double& d)
{
  return make_scalar_format (d);
}

template <>
float_display_format
make_format (const float& f)
{
  return make_scalar_format (f);
}

template <typename T>
static inline float_display_format
make_real_matrix_format (int x_max, int x_min, bool inf_or_nan,
                         int int_or_inf_or_nan)
{
  T scale = ((x_max == 0 || int_or_inf_or_nan)
             ? 1 : std::pow (10.0, calc_scale_exp (x_max - 1)));

  float_format fmt;

  int prec = std::min (output_precision (), pr_output_traits<T>::DIGITS10);

  int fw = 0, ld = 0, rd = 0;

  if (rat_format)
    {
      fw = 9;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = (x_max > x_min ? x_max : x_min);
      fw = (digits <= 0 ? 5 : digits + 4);
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (T);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (T);
      rd = 0;
    }
  else if (Vfixed_point_format && ! print_g)
    {
      rd = prec - 1;
      fw = rd + 3;
      if (inf_or_nan && fw < 4)
        fw = 4;
    }
  else if (int_or_inf_or_nan)
    {
      int digits = (x_max > x_min ? x_max : x_min);
      fw = (digits <= 0 ? 2 : digits + 1);
      if (inf_or_nan && fw < 4)
        fw = 4;
      rd = fw;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max++;
        }
      else if (x_max < 0)
        {
          ld_max = 1;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max = -x_max + 1;
        }
      else
        {
          ld_max = 1;
          rd_max = (prec > 1 ? prec - 1 : prec);
          x_max = 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min++;
        }
      else if (x_min < 0)
        {
          ld_min = 1;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min = -x_min + 1;
        }
      else
        {
          ld_min = 1;
          rd_min = (prec > 1 ? prec - 1 : prec);
          x_min = 1;
        }

      ld = (ld_max > ld_min ? ld_max : ld_min);
      rd = (rd_max > rd_min ? rd_max : rd_min);

      fw = 1 + ld + 1 + rd;
      if (inf_or_nan && fw < 4)
        fw = 4;
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e || print_eng || print_g
          || (! Vfixed_point_format
              && (ld + rd > pr_output_traits<T>::DIGITS10
                  || fw > pr_output_traits<T>::MAX_FIELD_WIDTH
                  || ld + rd > (1.5 * prec)))))
    {
      if (print_g)
        fmt = float_format (prec+6, prec);
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              fw = 4 + prec + ex;
              if (inf_or_nan && fw < 6)
                fw = 6;
              fmt = float_format (fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              fw = 2 + prec + ex;
              if (inf_or_nan && fw < 4)
                fw = 4;
              fmt = float_format (fw, prec - 1, std::ios::scientific);
            }
        }
    }
  else if (! bank_format && int_or_inf_or_nan)
    fmt = float_format (fw, rd);
  else
    fmt = float_format (fw, rd, std::ios::fixed);

  if (uppercase_format)
    fmt.uppercase ();

  return float_display_format (scale, fmt);
}

template <typename MT>
static inline float_display_format
make_matrix_format (const MT& m)
{
  error_unless (m.ndims () == 2);

  if (free_format)
    return float_display_format ();

  bool inf_or_nan = m.any_element_is_inf_or_nan ();

  bool int_or_inf_or_nan = m.all_elements_are_int_or_inf_or_nan ();

  MT m_abs = m.abs ();

  typedef typename MT::element_type ELT_T;

  ELT_T max_abs = pr_max_internal (m_abs);
  ELT_T min_abs = pr_min_internal (m_abs);

  int x_max = (max_abs == 0 ? 0 : num_digits (max_abs));

  int x_min = (min_abs == 0 ? 0 : num_digits (min_abs));

  return make_real_matrix_format<ELT_T> (x_max, x_min, inf_or_nan,
                                         int_or_inf_or_nan);
}

template <>
float_display_format
make_format (const Matrix& m)
{
  return make_matrix_format (m);
}

template <>
float_display_format
make_format (const FloatMatrix& m)
{
  return make_matrix_format (m);
}

template <typename T>
static inline float_display_format
make_complex_format (int x_max, int x_min, int r_x,
                     bool inf_or_nan, int int_only)
{
  float_format r_fmt;
  float_format i_fmt;

  int prec = std::min (output_precision (), pr_output_traits<T>::DIGITS10);

  int i_fw = 0, r_fw = 0, ld = 0, rd = 0;

  if (rat_format)
    {
      i_fw = 0;
      r_fw = 0;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = r_x;
      i_fw = 0;
      r_fw = (digits <= 0 ? 5 : digits + 4);
      rd = 2;
    }
  else if (hex_format)
    {
      r_fw = 2 * sizeof (T);
      i_fw = 2 * sizeof (T);
      rd = 0;
    }
  else if (bit_format)
    {
      r_fw = 8 * sizeof (T);
      i_fw = 8 * sizeof (T);
      rd = 0;
    }
  else if (int_only)
    {
      int digits = (x_max > x_min ? x_max : x_min);
      i_fw = (digits <= 0 ? 1 : digits);
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
      ld = r_fw;
    }
  else  // ordinary case of floating point numeric values
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max++;
        }
      else if (x_max < 0)
        {
          ld_max = 1;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max = -x_max + 1;
        }
      else
        {
          ld_max = 1;
          rd_max = (prec > 1 ? prec - 1 : prec);
          x_max = 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min++;
        }
      else if (x_min < 0)
        {
          ld_min = 1;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min = -x_min + 1;
        }
      else
        {
          ld_min = 1;
          rd_min = (prec > 1 ? prec - 1 : prec);
          x_min = 1;
        }

      ld = (ld_max > ld_min ? ld_max : ld_min);
      rd = (rd_max > rd_min ? rd_max : rd_min);

      i_fw = ld + 1 + rd;
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e || print_eng || print_g
          || ld + rd > pr_output_traits<T>::DIGITS10
          || r_fw > pr_output_traits<T>::MAX_FIELD_WIDTH
          || i_fw > pr_output_traits<T>::MAX_FIELD_WIDTH
          || ld + rd > (1.5 * prec)))
    {
      if (print_g)
        {
          int width = prec + 6;
          r_fmt = float_format (width, prec);
          i_fmt = float_format (width, prec);
        }
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              i_fw = 3 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 5)
                {
                  i_fw = 5;
                  r_fw = 6;
                }
              r_fmt = float_format (r_fw, ex, prec - 1, std::ios::fixed);
              i_fmt = float_format (i_fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              i_fw = 1 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 3)
                {
                  i_fw = 3;
                  r_fw = 4;
                }
              r_fmt = float_format (r_fw, prec - 1, std::ios::scientific);
              i_fmt = float_format (i_fw, prec - 1, std::ios::scientific);
            }
        }

      if (uppercase_format)
        {
          r_fmt.uppercase ();
          i_fmt.uppercase ();
        }
    }
  else if (! bank_format && int_only)
    {
      r_fmt = float_format (r_fw, ld);
      i_fmt = float_format (i_fw, ld);
    }
  else
    {
      r_fmt = float_format (r_fw, rd, std::ios::fixed);
      i_fmt = float_format (i_fw, rd, std::ios::fixed);
    }

  return float_display_format (r_fmt, i_fmt);
}

template <typename T>
float_display_format
make_complex_scalar_format (const std::complex<T>& c)
{
  if (free_format)
    return float_display_format ();

  T rp = c.real ();
  T ip = c.imag ();

  bool inf_or_nan = (octave::math::isinf (c) || octave::math::isnan (c));

  bool int_only = (octave::math::x_nint (rp) == rp
                   && octave::math::x_nint (ip) == ip);

  T r_abs = (rp < 0 ? -rp : rp);
  T i_abs = (ip < 0 ? -ip : ip);

  int r_x = (r_abs == 0 ? 0 : num_digits (r_abs));
  int i_x = (i_abs == 0 ? 0 : num_digits (i_abs));

  int x_max, x_min;

  if (r_x > i_x)
    {
      x_max = r_x;
      x_min = i_x;
    }
  else
    {
      x_max = i_x;
      x_min = r_x;
    }

  return make_complex_format<T> (x_max, x_min, r_x, inf_or_nan, int_only);
}

template <>
float_display_format
make_format (const std::complex<double>& c)
{
  return make_complex_scalar_format (c);
}

template <>
float_display_format
make_format (const std::complex<float>& fc)
{
  return make_complex_scalar_format (fc);
}

template <typename T>
static inline float_display_format
make_complex_matrix_format (int x_max, int x_min, int r_x_max,
                            int r_x_min, bool inf_or_nan,
                            int int_or_inf_or_nan)
{
  T scale = ((x_max == 0 || int_or_inf_or_nan)
             ? 1 : std::pow (10.0, calc_scale_exp (x_max - 1)));

  float_format r_fmt;
  float_format i_fmt;

  int prec = std::min (output_precision (), pr_output_traits<T>::DIGITS10);

  int i_fw = 0, r_fw = 0, ld = 0, rd = 0;

  if (rat_format)
    {
      i_fw = 9;
      r_fw = 9;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = (r_x_max > r_x_min ? r_x_max : r_x_min);
      i_fw = 0;
      r_fw = (digits <= 0 ? 5 : digits + 4);
      rd = 2;
    }
  else if (hex_format)
    {
      r_fw = 2 * sizeof (T);
      i_fw = 2 * sizeof (T);
      rd = 0;
    }
  else if (bit_format)
    {
      r_fw = 8 * sizeof (T);
      i_fw = 8 * sizeof (T);
      rd = 0;
    }
  else if (Vfixed_point_format && ! print_g)
    {
      rd = prec - 1;
      i_fw = rd + 1;
      r_fw = i_fw + 2;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
    }
  else if (int_or_inf_or_nan)
    {
      int digits = (x_max > x_min ? x_max : x_min);
      i_fw = (digits <= 0 ? 1 : digits);
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
      rd = r_fw;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max++;
        }
      else if (x_max < 0)
        {
          ld_max = 1;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max = -x_max + 1;
        }
      else
        {
          ld_max = 1;
          rd_max = (prec > 1 ? prec - 1 : prec);
          x_max = 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min++;
        }
      else if (x_min < 0)
        {
          ld_min = 1;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min = -x_min + 1;
        }
      else
        {
          ld_min = 1;
          rd_min = (prec > 1 ? prec - 1 : prec);
          x_min = 1;
        }

      ld = (ld_max > ld_min ? ld_max : ld_min);
      rd = (rd_max > rd_min ? rd_max : rd_min);

      i_fw = ld + 1 + rd;
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e || print_eng || print_g
          || (! Vfixed_point_format
              && (ld + rd > pr_output_traits<T>::DIGITS10
                  || r_fw > pr_output_traits<T>::MAX_FIELD_WIDTH
                  || i_fw > pr_output_traits<T>::MAX_FIELD_WIDTH
                  || ld + rd > (1.5 * prec)))))
    {
      if (print_g)
        {
          int width = prec + 6;
          r_fmt = float_format (width, prec);
          i_fmt = float_format (width, prec);
        }
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              i_fw = 3 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 5)
                {
                  i_fw = 5;
                  r_fw = 6;
                }
              r_fmt = float_format (r_fw, ex, prec - 1, std::ios::fixed);
              i_fmt = float_format (i_fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              i_fw = 1 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 3)
                {
                  i_fw = 3;
                  r_fw = 4;
                }
              r_fmt = float_format (r_fw, prec - 1, std::ios::scientific);
              i_fmt = float_format (i_fw, prec - 1, std::ios::scientific);
            }
        }

      if (uppercase_format)
        {
          r_fmt.uppercase ();
          i_fmt.uppercase ();
        }
    }
  else if (! bank_format && int_or_inf_or_nan)
    {
      r_fmt = float_format (r_fw, rd);
      i_fmt = float_format (i_fw, rd);
    }
  else
    {
      r_fmt = float_format (r_fw, rd, std::ios::fixed);
      i_fmt = float_format (i_fw, rd, std::ios::fixed);
    }

  return float_display_format (scale, r_fmt, i_fmt);
}

template <typename CMT>
static inline float_display_format
make_complex_matrix_format (const CMT& cm)
{
  if (free_format)
    return float_display_format ();

  typedef typename CMT::real_matrix_type RMT;
  typedef typename CMT::real_elt_type ELT_T;

  RMT rp = real (cm);
  RMT ip = imag (cm);

  bool inf_or_nan = cm.any_element_is_inf_or_nan ();

  bool int_or_inf_or_nan = (rp.all_elements_are_int_or_inf_or_nan ()
                            && ip.all_elements_are_int_or_inf_or_nan ());

  RMT r_m_abs = rp.abs ();
  ELT_T r_max_abs = pr_max_internal (r_m_abs);
  ELT_T r_min_abs = pr_min_internal (r_m_abs);

  RMT i_m_abs = ip.abs ();
  ELT_T i_max_abs = pr_max_internal (i_m_abs);
  ELT_T i_min_abs = pr_min_internal (i_m_abs);

  int r_x_max = (r_max_abs == 0 ? 0 : num_digits (r_max_abs));

  int r_x_min = (r_min_abs == 0 ? 0 : num_digits (r_min_abs));

  int i_x_max = (i_max_abs == 0 ? 0 : num_digits (i_max_abs));

  int i_x_min = (i_min_abs == 0 ? 0 : num_digits (i_min_abs));

  int x_max = (r_x_max > i_x_max ? r_x_max : i_x_max);
  int x_min = (r_x_min > i_x_min ? r_x_min : i_x_min);

  return make_complex_matrix_format<ELT_T> (x_max, x_min, r_x_max, r_x_min,
         inf_or_nan, int_or_inf_or_nan);
}

template <>
float_display_format
make_format (const ComplexMatrix& cm)
{
  return make_complex_matrix_format (cm);
}

template <>
float_display_format
make_format (const FloatComplexMatrix& cm)
{
  return make_complex_matrix_format (cm);
}

template <>
float_display_format
make_format (const boolNDArray&)
{
  return float_display_format (float_format (1, 1));
}

template <typename T>
static inline float_display_format
make_range_format (int x_max, int x_min, int all_ints)
{
  double scale = ((x_max == 0 || all_ints)
                  ? 1 : std::pow (10.0, calc_scale_exp (x_max - 1)));

  float_format fmt;

  int prec = std::min (output_precision (), pr_output_traits<T>::DIGITS10);

  int fw = 0, ld = 0, rd = 0;

  if (rat_format)
    {
      fw = 9;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = (x_max > x_min ? x_max : x_min);
      fw = (digits < 0 ? 5 : digits + 4);
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (T);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (T);
      rd = 0;
    }
  else if (all_ints)
    {
      int digits = (x_max > x_min ? x_max : x_min);
      fw = digits + 1;
      rd = fw;
    }
  else if (Vfixed_point_format && ! print_g)
    {
      rd = prec - 1;
      fw = rd + 3;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max++;
        }
      else if (x_max < 0)
        {
          ld_max = 1;
          rd_max = (prec > x_max ? prec - x_max : prec);
          x_max = -x_max + 1;
        }
      else
        {
          ld_max = 1;
          rd_max = (prec > 1 ? prec - 1 : prec);
          x_max = 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min++;
        }
      else if (x_min < 0)
        {
          ld_min = 1;
          rd_min = (prec > x_min ? prec - x_min : prec);
          x_min = -x_min + 1;
        }
      else
        {
          ld_min = 1;
          rd_min = (prec > 1 ? prec - 1 : prec);
          x_min = 1;
        }

      ld = (ld_max > ld_min ? ld_max : ld_min);
      rd = (rd_max > rd_min ? rd_max : rd_min);

      fw = ld + rd + 3;
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e || print_eng || print_g
          || (! Vfixed_point_format
              && (ld + rd > pr_output_traits<T>::DIGITS10
                  || fw > pr_output_traits<T>::MAX_FIELD_WIDTH
                  || ld + rd > (1.5 * prec)))))
    {
      if (print_g)
        fmt = float_format (prec+6, prec);
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              fw = 5 + prec + ex;
              fmt = float_format (fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              fw = 3 + prec + ex;
              fmt = float_format (fw, prec - 1, std::ios::scientific);
            }
        }
    }
  else if (! bank_format && all_ints)
    fmt = float_format (fw, rd);
  else
    fmt = float_format (fw, rd, std::ios::fixed);

  if (uppercase_format)
    fmt.uppercase ();

  return float_display_format (scale, fmt);
}

template <>
float_display_format
make_format (const octave::range<double>& r)
{
  if (free_format)
    return float_display_format ();

  double r_min = r.base ();
  double r_max = r.limit ();

  if (r_max < r_min)
    {
      double tmp = r_max;
      r_max = r_min;
      r_min = tmp;
    }

  bool all_ints = r.all_elements_are_ints ();

  double max_abs = (r_max < 0 ? -r_max : r_max);
  double min_abs = (r_min < 0 ? -r_min : r_min);

  int x_max = (max_abs == 0 ? 0 : num_digits (max_abs));

  int x_min = (min_abs == 0 ? 0 : num_digits (min_abs));

  return make_range_format<double> (x_max, x_min, all_ints);
}

template <typename T>
union equiv
{
  T val;
  unsigned char i[sizeof (T)];
};

#define PRINT_CHAR_BITS(os, c)                  \
  do                                            \
    {                                           \
      unsigned char ctmp = c;                   \
      char stmp[9];                             \
      stmp[0] = (ctmp & 0x80) ? '1' : '0';      \
      stmp[1] = (ctmp & 0x40) ? '1' : '0';      \
      stmp[2] = (ctmp & 0x20) ? '1' : '0';      \
      stmp[3] = (ctmp & 0x10) ? '1' : '0';      \
      stmp[4] = (ctmp & 0x08) ? '1' : '0';      \
      stmp[5] = (ctmp & 0x04) ? '1' : '0';      \
      stmp[6] = (ctmp & 0x02) ? '1' : '0';      \
      stmp[7] = (ctmp & 0x01) ? '1' : '0';      \
      stmp[8] = '\0';                           \
      os << stmp;                               \
    }                                           \
  while (0)

#define PRINT_CHAR_BITS_SWAPPED(os, c)          \
  do                                            \
    {                                           \
      unsigned char ctmp = c;                   \
      char stmp[9];                             \
      stmp[0] = (ctmp & 0x01) ? '1' : '0';      \
      stmp[1] = (ctmp & 0x02) ? '1' : '0';      \
      stmp[2] = (ctmp & 0x04) ? '1' : '0';      \
      stmp[3] = (ctmp & 0x08) ? '1' : '0';      \
      stmp[4] = (ctmp & 0x10) ? '1' : '0';      \
      stmp[5] = (ctmp & 0x20) ? '1' : '0';      \
      stmp[6] = (ctmp & 0x40) ? '1' : '0';      \
      stmp[7] = (ctmp & 0x80) ? '1' : '0';      \
      stmp[8] = '\0';                           \
      os << stmp;                               \
    }                                           \
  while (0)

template <typename T>
static inline void
pr_any_float (std::ostream& os, const float_format& fmt, T val)
{
  // Unless explicitly asked for, always print in big-endian format
  // for hex and bit formats.
  //
  //   {bit,hex}_format == 1: print big-endian
  //   {bit,hex}_format == 2: print native

  int fw = fmt.width ();

  if (hex_format)
    {
      octave::preserve_stream_state stream_state (os);

      equiv<T> tmp;
      tmp.val = val;

      // Unless explicitly asked for, always print in big-endian format.

      // FIXME: Will bad things happen if we are interrupted before resetting
      //        the format flags and fill character?

      octave::mach_info::float_format flt_fmt
        = octave::mach_info::native_float_format ();

      os.fill ('0');
      if (uppercase_format)
        os.flags (std::ios::right | std::ios::hex | std::ios::uppercase);
      else
        os.flags (std::ios::right | std::ios::hex);

      if (hex_format > 1
          || flt_fmt == octave::mach_info::flt_fmt_ieee_big_endian)
        {
          for (std::size_t i = 0; i < sizeof (T); i++)
            os << std::setw (2) << static_cast<int> (tmp.i[i]);
        }
      else
        {
          for (int i = sizeof (T) - 1; i >= 0; i--)
            os << std::setw (2) << static_cast<int> (tmp.i[i]);
        }
    }
  else if (bit_format)
    {
      equiv<T> tmp;
      tmp.val = val;

      octave::mach_info::float_format flt_fmt
        = octave::mach_info::native_float_format ();

      if (flt_fmt == octave::mach_info::flt_fmt_ieee_big_endian)
        {
          for (std::size_t i = 0; i < sizeof (T); i++)
            PRINT_CHAR_BITS (os, tmp.i[i]);
        }
      else
        {
          if (bit_format > 1)
            {
              for (std::size_t i = 0; i < sizeof (T); i++)
                PRINT_CHAR_BITS (os, tmp.i[i]);
            }
          else
            {
              for (int i = sizeof (T) - 1; i >= 0; i--)
                PRINT_CHAR_BITS (os, tmp.i[i]);
            }
        }
    }
  else if (val == 0)
    {
      octave::preserve_stream_state stream_state (os);

      if (fw > 0)
        os << std::setw (fw) << "0";
      else
        os << "0";
    }
  else if (octave::math::isna (val))
    {
      octave::preserve_stream_state stream_state (os);

      if (fw > 0)
        os << std::setw (fw) << "NA";
      else
        os << "NA";
    }
  else if (rat_format)
    os << pr_rational_float<T> (fmt, val);
  else if (octave::math::isinf (val))
    {
      octave::preserve_stream_state stream_state (os);

      const char *s;
      if (val < 0)
        s = "-Inf";
      else
        s = "Inf";

      if (fw > 0)
        os << std::setw (fw) << s;
      else
        os << s;
    }
  else if (octave::math::isnan (val))
    {
      octave::preserve_stream_state stream_state (os);

      if (fw > 0)
        os << std::setw (fw) << "NaN";
      else
        os << "NaN";
    }
  else if (print_eng)
    os << pr_engineering_float<T> (fmt, val);
  else
    os << pr_formatted_float<T> (fmt, val);
}

template <typename T>
static inline void
pr_float (std::ostream& os, const float_display_format& fmt, T val)
{
  double scale = fmt.scale_factor ();

  if (Vfixed_point_format && ! (print_g || print_e) && scale != 1)
    val /= scale;

  pr_any_float (os, fmt.real_format (), val);
}

template <typename T>
static inline void
pr_imag_float (std::ostream& os, const float_display_format& fmt, T val)
{
  double scale = fmt.scale_factor ();

  if (Vfixed_point_format && ! (print_g || print_e) && scale != 1)
    val /= scale;

  pr_any_float (os, fmt.imag_format (), val);
}

template <typename T>
static inline void
pr_float (std::ostream& os, const float_display_format& fmt,
          const std::complex<T>& cval)
{
  T r = cval.real ();

  pr_float (os, fmt, r);

  if (! bank_format)
    {
      T i = cval.imag ();
      if (! (hex_format || bit_format) && lo_ieee_signbit (i))
        {
          os << " - ";
          i = -i;
          pr_imag_float (os, fmt, i);
        }
      else
        {
          if (hex_format || bit_format)
            os << "  ";
          else
            os << " + ";

          pr_imag_float (os, fmt, i);
        }
      os << 'i';
    }
}

static inline void
print_empty_matrix (std::ostream& os, octave_idx_type nr, octave_idx_type nc,
                    bool pr_as_read_syntax)
{
  error_unless (nr == 0 || nc == 0);

  if (pr_as_read_syntax)
    {
      if (nr == 0 && nc == 0)
        os << "[]";
      else
        os << "zeros (" << nr << ", " << nc << ')';
    }
  else
    {
      os << "[]";

      if (Vprint_empty_dimensions)
        os << '(' << nr << 'x' << nc << ')';
    }
}

static inline void
print_empty_nd_array (std::ostream& os, const dim_vector& dims,
                      bool pr_as_read_syntax)
{
  error_unless (dims.any_zero ());

  if (pr_as_read_syntax)
    os << "zeros (" << dims.str (',') << ')';
  else
    {
      os << "[]";

      if (Vprint_empty_dimensions)
        os << '(' << dims.str () << ')';
    }
}

static inline void
pr_scale_header (std::ostream& os, double scale)
{
  if (Vfixed_point_format && ! (print_g || print_e) && scale != 1)
    {
      octave::preserve_stream_state stream_state (os);

      os << "  "
         << std::setw (8) << std::setprecision (1)
         << std::setiosflags (std::ios::scientific | std::ios::left)
         << scale
         << "*\n";

      if (! Vcompact_format)
        os << "\n";
    }
}

static inline void
pr_col_num_header (std::ostream& os, octave_idx_type total_width, int max_width,
                   octave_idx_type lim, octave_idx_type col, int extra_indent)
{
  if (total_width > max_width && Vsplit_long_rows)
    {
      octave::preserve_stream_state stream_state (os);

      if (col != 0)
        {
          if (Vcompact_format)
            os << "\n";
          else
            os << "\n\n";
        }

      octave_idx_type num_cols = lim - col;

      os << std::setw (extra_indent) << "";

      if (num_cols == 1)
        os << " Column " << col + 1 << ":\n";
      else if (num_cols == 2)
        os << " Columns " << col + 1 << " and " << lim << ":\n";
      else
        os << " Columns " << col + 1 << " through " << lim << ":\n";

      if (! Vcompact_format)
        os << "\n";
    }
}

template <typename T>
static inline void
pr_plus_format (std::ostream& os, const T& val)
{
  if (val > T (0))
    os << plus_format_chars[0];
  else if (val < T (0))
    os << plus_format_chars[1];
  else
    os << plus_format_chars[2];
}

// FIXME: all this mess with abs is an attempt to avoid seeing
//
//   warning: comparison of unsigned expression < 0 is always false
//
// from GCC.  Isn't there a better way?

template <typename T>
static inline T
abs (T x)
{
  return x < 0 ? -x : x;
}

#define INSTANTIATE_ABS(T)                      \
  template T abs (T)

INSTANTIATE_ABS(int8_t);
INSTANTIATE_ABS(int16_t);
INSTANTIATE_ABS(int32_t);
INSTANTIATE_ABS(int64_t);

#define SPECIALIZE_UABS(T)                      \
  template <>                                   \
  inline T                                      \
  abs (T x)                                     \
  {                                             \
    return x;                                   \
  }

SPECIALIZE_UABS(uint8_t)
SPECIALIZE_UABS(uint16_t)
SPECIALIZE_UABS(uint32_t)
SPECIALIZE_UABS(uint64_t)

#define MAKE_INT_MATRIX_FORMAT(TYPE)                                    \
  template <>                                                           \
  float_display_format                                                  \
  make_format (const intNDArray<TYPE>& nda)                             \
  {                                                                     \
    bool isneg = false;                                                 \
    int digits = 0;                                                     \
                                                                        \
    for (octave_idx_type i = 0; i < nda.numel (); i++)                  \
      {                                                                 \
        int new_digits                                                  \
          = static_cast<int>                                            \
          (std::floor (log10 (double (abs (nda(i).value ()))) + 1));  \
                                                                        \
        if (new_digits > digits)                                        \
          digits = new_digits;                                          \
                                                                        \
        if (! isneg)                                                    \
          isneg = (abs (nda(i).value ()) != nda(i).value ());           \
      }                                                                 \
                                                                        \
    return float_display_format (float_format (digits + isneg, 0, 0));  \
  }

MAKE_INT_MATRIX_FORMAT (octave_int8)
MAKE_INT_MATRIX_FORMAT (octave_uint8)
MAKE_INT_MATRIX_FORMAT (octave_int16)
MAKE_INT_MATRIX_FORMAT (octave_uint16)
MAKE_INT_MATRIX_FORMAT (octave_int32)
MAKE_INT_MATRIX_FORMAT (octave_uint32)
MAKE_INT_MATRIX_FORMAT (octave_int64)
MAKE_INT_MATRIX_FORMAT (octave_uint64)

#define MAKE_INT_SCALAR_FORMAT(TYPE)                                    \
  template <>                                                           \
  float_display_format                                                  \
  make_format (const octave_int<TYPE>& val)                             \
  {                                                                     \
    bool isneg = false;                                                 \
    int digits                                                          \
      = static_cast<int>                                                \
      (std::floor (log10 (double (abs (val.value ()))) + 1));         \
                                                                        \
    isneg = (abs (val.value ()) != val.value ());                       \
                                                                        \
    return float_display_format (float_format (digits + isneg, 0, 0));  \
  }

MAKE_INT_SCALAR_FORMAT (int8_t)
MAKE_INT_SCALAR_FORMAT (uint8_t)
MAKE_INT_SCALAR_FORMAT (int16_t)
MAKE_INT_SCALAR_FORMAT (uint16_t)
MAKE_INT_SCALAR_FORMAT (int32_t)
MAKE_INT_SCALAR_FORMAT (uint32_t)
MAKE_INT_SCALAR_FORMAT (int64_t)
MAKE_INT_SCALAR_FORMAT (uint64_t)

void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       bool d, bool pr_as_read_syntax)
{
  octave_print_internal (os, fmt, octave_uint8 (d), pr_as_read_syntax);
}

void
octave_print_internal (std::ostream& os, bool d, bool pr_as_read_syntax)
{
  octave_print_internal (os, octave_uint8 (d), pr_as_read_syntax);
}

void
octave_print_internal (std::ostream&, const float_display_format&,
                       char, bool)
{
  panic_impossible ();
}

void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       double d, bool pr_as_read_syntax)
{
  if (pr_as_read_syntax)
    os << d;
  else if (plus_format)
    pr_plus_format (os, d);
  else
    {
      if (free_format)
        os << d;
      else
        pr_float (os, fmt, d);
    }
}

void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       float d, bool pr_as_read_syntax)
{
  if (pr_as_read_syntax)
    os << d;
  else if (plus_format)
    pr_plus_format (os, d);
  else
    {
      if (free_format)
        os << d;
      else
        pr_float (os, fmt, d);
    }
}

template <typename MT>
static inline void
octave_print_free (std::ostream& os, const MT& m, bool pr_as_read_syntax)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (pr_as_read_syntax)
    os << "[\n";

  for (octave_idx_type i = 0; i < nr; i++)
    {
      for (octave_idx_type j = 0; j < nc; j++)
        os << ' ' << m.elem (i, j);

      if (i < nr - 1)
        os << "\n";
    }

  if (pr_as_read_syntax)
    os << ']';
}

template <typename MT>
static inline void
pr_plus_format_matrix (std::ostream& os, const MT& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  for (octave_idx_type i = 0; i < nr; i++)
    {
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          pr_plus_format (os, m(i, j));
        }

      if (i < nr - 1)
        os << "\n";
    }
}

static inline int
get_column_width (const float_display_format& fmt)
{
  int r_fw = fmt.real_format().width ();
  int i_fw = fmt.imag_format().width ();

  int retval = r_fw + i_fw + 2;

  if (i_fw && ! (rat_format || bank_format || hex_format || bit_format))
    retval += 5;

  return retval;
}

template <typename MT>
static void
octave_print_matrix_internal (std::ostream& os, const MT& m,
                              bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    pr_plus_format_matrix (os, m);
  else
    {
      float_display_format fmt = make_format (m);
      int column_width = get_column_width (fmt);
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = octave::command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          octave_print_free (os, m, pr_as_read_syntax);
          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_idx_type col = 0;
              while (col < nc)
                {
                  octave_idx_type lim = (col + inc < nc ? col + inc : nc);

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      if (i == 0 && j == 0)
                        os << "[ ";
                      else
                        {
                          if (j > col)
                            os << ", ";
                          else
                            os << "  ";
                        }

                      pr_float (os, fmt, m(i, j));
                    }

                  col += inc;

                  if (col >= nc)
                    {
                      if (i == nr - 1)
                        os << " ]";
                      else
                        os << ";\n";
                    }
                  else
                    os << " ...\n";
                }
            }
        }
      else
        {
          octave::preserve_stream_state stream_state (os);

          pr_scale_header (os, fmt.scale_factor ());

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = (col + inc < nc ? col + inc : nc);

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      pr_float (os, fmt, m(i, j));
                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

template <typename DMT>
static void
octave_print_diag_matrix_internal (std::ostream& os, const DMT& m,
                                   bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    pr_plus_format_matrix (os, m);
  else
    {
      float_display_format fmt
        = make_format (typename DMT::full_matrix_type (m.diag ()));
      int column_width = get_column_width (fmt);
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = octave::command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          octave_print_free (os, m, pr_as_read_syntax);
          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          os << "diag (";

          octave_idx_type col = 0;
          while (col < nc)
            {
              octave_idx_type lim = (col + inc < nc ? col + inc : nc);

              for (octave_idx_type j = col; j < lim; j++)
                {
                  octave_quit ();

                  if (j == 0)
                    os << "[ ";
                  else
                    {
                      if (j > col)
                        os << ", ";
                      else
                        os << "  ";
                    }

                  pr_float (os, fmt, m(j, j));
                }

              col += inc;

              if (col >= nc)
                os << " ]";
              else
                os << " ...\n";
            }
          os << ')';
        }
      else
        {
          octave::preserve_stream_state stream_state (os);

          os << "Diagonal Matrix\n";
          if (! Vcompact_format)
            os << "\n";

          pr_scale_header (os, fmt.scale_factor ());

          // kluge.  Get the true width of a number.
          int zero_fw;
          {
            std::ostringstream tmp_oss;
            typename DMT::element_type zero = 0;
            pr_float (tmp_oss, fmt, zero);
            zero_fw = tmp_oss.str ().length ();
          }

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = (col + inc < nc ? col + inc : nc);

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      if (i == j)
                        pr_float (os, fmt, m(i, j));
                      else
                        os << std::setw (zero_fw) << '0';
                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

template <typename NDA_T, typename ELT_T, typename MAT_T>
void print_nd_array (std::ostream& os, const NDA_T& nda,
                     bool pr_as_read_syntax)
{

  if (nda.isempty ())
    print_empty_nd_array (os, nda.dims (), pr_as_read_syntax);
  else
    {

      int ndims = nda.ndims ();

      dim_vector dims = nda.dims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      for (octave_idx_type i = 0; i < m; i++)
        {
          octave_quit ();

          std::string nm = "ans";

          if (m > 1)
            {
              nm += "(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ',';
                  else
                    buf << ')';
                }

              nm += buf.str ();
            }

          Array<octave::idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = octave::idx_vector (':');
          idx(1) = octave::idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = octave::idx_vector (ra_idx(k));

          octave_value page
            = MAT_T (Array<ELT_T> (nda.index (idx), dim_vector (nr, nc)));

          if (i != m - 1)
            {
              page.print_with_name (os, nm);
            }
          else
            {
              page.print_name_tag (os, nm);
              page.print_raw (os);
            }

          NDA_T::increment_index (ra_idx, dims, 2);
        }
    }
}

void
octave_print_internal (std::ostream& os, const NDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, Matrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array <NDArray, double, Matrix> (os, nda, pr_as_read_syntax);
      break;
    }
}

void
octave_print_internal (std::ostream& os, const FloatNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, FloatMatrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array <FloatNDArray, float, FloatMatrix> (os, nda, pr_as_read_syntax);
      break;
    }
}

template <typename T>
static inline void
pr_plus_format (std::ostream& os, const std::complex<T>& c)
{
  T rp = c.real ();
  T ip = c.imag ();

  if (rp == 0)
    {
      if (ip == 0)
        os << ' ';
      else
        os << 'i';
    }
  else if (ip == 0)
    pr_plus_format (os, rp);
  else
    os << 'c';
}

extern void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       const Complex& c, bool pr_as_read_syntax)
{
  if (pr_as_read_syntax)
    os << c;
  else if (plus_format)
    pr_plus_format (os, c);
  else
    {
      if (free_format)
        os << c;
      else
        pr_float (os, fmt, c);
    }
}

void
octave_print_internal (std::ostream& os, const float_display_format& fmt,
                       const FloatComplex& c, bool pr_as_read_syntax)
{
  if (pr_as_read_syntax)
    os << c;
  else if (plus_format)
    pr_plus_format (os, c);
  else
    {
      if (free_format)
        os << c;
      else
        pr_float (os, fmt, c);
    }
}

void
octave_print_internal (std::ostream& os, const PermMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    pr_plus_format_matrix (os, m);
  else
    {
      int fw = 2;
      int column_width = fw + 2;
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = octave::command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          octave_print_free (os, m, pr_as_read_syntax);
          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          Array<octave_idx_type> pvec = m.col_perm_vec ();

          os << "eye (";
          os << ":, ";

          octave_idx_type col = 0;
          while (col < nc)
            {
              octave_idx_type lim = (col + inc < nc ? col + inc : nc);

              for (octave_idx_type j = col; j < lim; j++)
                {
                  octave_quit ();

                  if (j == 0)
                    os << "[ ";
                  else
                    {
                      if (j > col)
                        os << ", ";
                      else
                        os << "  ";
                    }

                  os << pvec (j);
                }

              col += inc;

              if (col >= nc)
                os << " ]";
              else
                os << " ...\n";
            }
          os << ')';
        }
      else
        {
          octave::preserve_stream_state stream_state (os);

          os << "Permutation Matrix\n";
          if (! Vcompact_format)
            os << "\n";

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = (col + inc < nc ? col + inc : nc);

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      os << std::setw (fw) << m(i, j);
                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

void
octave_print_internal (std::ostream& os, const ComplexNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, ComplexMatrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array <ComplexNDArray, Complex, ComplexMatrix>
      (os, nda, pr_as_read_syntax);
      break;
    }
}

void
octave_print_internal (std::ostream& os, const FloatComplexNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, FloatComplexMatrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array <FloatComplexNDArray, FloatComplex, FloatComplexMatrix>
      (os, nda, pr_as_read_syntax);
      break;
    }
}

// FIXME: write single precision versions of the printing functions.

void
octave_print_internal (std::ostream& os, const Matrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_matrix_internal (os, m, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_matrix_internal (os, m, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const DiagMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_diag_matrix_internal (os, m, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatDiagMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_diag_matrix_internal (os, m, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const ComplexMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_matrix_internal (os, cm, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatComplexMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_matrix_internal (os, cm, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const ComplexDiagMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_diag_matrix_internal (os, cm, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatComplexDiagMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_diag_matrix_internal (os, cm, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const octave::range<double>& r,
                       bool pr_as_read_syntax, int extra_indent)
{
  double base = r.base ();
  double increment = r.increment ();
  double limit = r.limit ();
  double final_value = r.final_value ();
  octave_idx_type num_elem = r.numel ();

  if (plus_format && ! pr_as_read_syntax)
    pr_plus_format_matrix (os, r);
  else
    {
      float_display_format fmt = make_format (r);

      if (pr_as_read_syntax)
        {
          if (free_format)
            {
              os << base << " : ";
              if (increment != 1)
                os << increment << " : ";
              os << limit;
            }
          else
            {
              pr_float (os, fmt, base);
              os << " : ";
              if (increment != 1)
                {
                  pr_float (os, fmt, increment);
                  os << " : ";
                }
              pr_float (os, fmt, limit);
            }
        }
      else
        {
          octave::preserve_stream_state stream_state (os);

          int column_width = get_column_width (fmt);
          octave_idx_type total_width = num_elem * column_width;
          octave_idx_type max_width = octave::command_editor::terminal_cols ();

          if (free_format)
            {
              os << ' ';
              for (octave_idx_type i = 0; i < num_elem; i++)
                os << ' ' << r.elem (i);
              return;
            }

          octave_idx_type inc = num_elem;
          if (total_width > max_width && Vsplit_long_rows)
            {
              inc = max_width / column_width;
              if (inc == 0)
                inc++;
            }

          max_width -= extra_indent;

          if (max_width < 0)
            max_width = 0;

          pr_scale_header (os, fmt.scale_factor ());

          octave_idx_type col = 0;
          while (col < num_elem)
            {
              octave_idx_type lim = (col + inc < num_elem ? col + inc
                                     : num_elem);

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              os << std::setw (extra_indent) << "";

              for (octave_idx_type i = col; i < lim; i++)
                {
                  octave_quit ();

                  double val;
                  if (i == 0)
                    val = base;
                  else
                    val = base + i * increment;

                  if (i == num_elem - 1)
                    val = final_value;

                  os << "  ";

                  pr_float (os, fmt, val);
                }

              col += inc;
            }
        }
    }
}

void
octave_print_internal (std::ostream& os, const boolMatrix& bm,
                       bool pr_as_read_syntax,
                       int extra_indent)
{
  uint8NDArray tmp (bm);
  octave_print_internal (os, tmp, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const boolNDArray& nda,
                       bool pr_as_read_syntax,
                       int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, boolMatrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array<boolNDArray, bool,
                     boolMatrix> (os, nda, pr_as_read_syntax);
      break;
    }
}

void
octave_print_internal (std::ostream& os, const charMatrix& chm,
                       bool pr_as_read_syntax,
                       int /* FIXME: extra_indent */,
                       bool pr_as_string)
{
  if (pr_as_string)
    {
      octave_idx_type nstr = chm.rows ();

      if (pr_as_read_syntax && nstr > 1)
        os << "[ ";

      if (nstr != 0)
        {
          for (octave_idx_type i = 0; i < nstr; i++)
            {
              octave_quit ();

              std::string row = chm.row_as_string (i);

              if (pr_as_read_syntax)
                {
                  os << '"' << octave::undo_string_escapes (row) << '"';

                  if (i < nstr - 1)
                    os << "; ";
                }
              else
                {
                  os << row;

                  if (i < nstr - 1)
                    os << "\n";
                }
            }
        }

      if (pr_as_read_syntax && nstr > 1)
        os << " ]";
    }
  else
    {
      os << "sorry, printing char matrices not implemented yet\n";
    }
}

void
octave_print_internal (std::ostream& os, const charNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent,
                       bool pr_as_string)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, charMatrix (nda),
                             pr_as_read_syntax, extra_indent, pr_as_string);
      break;

    default:
      print_nd_array <charNDArray, char, charMatrix> (os, nda,
          pr_as_read_syntax);
      break;
    }
}

void
octave_print_internal (std::ostream& os, const std::string& s,
                       bool pr_as_read_syntax, int extra_indent)
{
  Array<std::string> nda (dim_vector (1, 1), s);

  octave_print_internal (os, nda, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const Array<std::string>& nda,
                       bool pr_as_read_syntax, int /* extra_indent */)
{
  // FIXME: this mostly duplicates the code in the print_nd_array<>
  // function.  Can fix this with std::is_same from C++11.

  if (nda.isempty ())
    print_empty_nd_array (os, nda.dims (), pr_as_read_syntax);
  else if (nda.numel () == 1)
    {
      os << nda(0);
    }
  else
    {
      int ndims = nda.ndims ();

      dim_vector dims = nda.dims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      for (octave_idx_type i = 0; i < m; i++)
        {
          std::string nm = "ans";

          if (m > 1)
            {
              nm += "(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ',';
                  else
                    buf << ')';
                }

              nm += buf.str ();
            }

          Array<octave::idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = octave::idx_vector (':');
          idx(1) = octave::idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = octave::idx_vector (ra_idx(k));

          Array<std::string> page (nda.index (idx), dim_vector (nr, nc));

          // FIXME: need to do some more work to put these
          //        in neatly aligned columns...

          octave_idx_type n_rows = page.rows ();
          octave_idx_type n_cols = page.cols ();

          os << nm << " =\n";
          if (! Vcompact_format)
            os << "\n";

          for (octave_idx_type ii = 0; ii < n_rows; ii++)
            {
              for (octave_idx_type jj = 0; jj < n_cols; jj++)
                os << "  " << page(ii, jj);

              os << "\n";
            }

          if (i < m - 1)
            os << "\n";

          increment_index (ra_idx, dims, 2);
        }
    }
}

template <typename T>
class
octave_print_conv
{
public:
  typedef T print_conv_type;
};

#define PRINT_CONV(T1, T2)                      \
  template <>                                   \
  class                                         \
  octave_print_conv<T1>                         \
  {                                             \
  public:                                       \
    typedef T2 print_conv_type;                 \
  }

PRINT_CONV (octave_int8, octave_int16);
PRINT_CONV (octave_uint8, octave_uint16);

#undef PRINT_CONV

template <typename T>
static inline void
pr_int (std::ostream& os, const T& d, int fw = 0)
{
  std::size_t sz = d.byte_size ();
  const unsigned char *tmpi = d.iptr ();

  // Unless explicitly asked for, always print in big-endian
  // format for hex and bit formats.
  //
  //   {bit,hex}_format == 1: print big-endian
  //   {bit,hex}_format == 2: print native

  if (hex_format)
    {
      octave::preserve_stream_state stream_state (os);

      os.fill ('0');
      if (uppercase_format)
        os.flags (std::ios::right | std::ios::hex | std::ios::uppercase);
      else
        os.flags (std::ios::right | std::ios::hex);

      if (hex_format > 1 || octave::mach_info::words_big_endian ())
        {
          for (std::size_t i = 0; i < sz; i++)
            os << std::setw (2) << static_cast<int> (tmpi[i]);
        }
      else
        {
          for (int i = sz - 1; i >= 0; i--)
            os << std::setw (2) << static_cast<int> (tmpi[i]);
        }
    }
  else if (bit_format)
    {
      if (octave::mach_info::words_big_endian ())
        {
          for (std::size_t i = 0; i < sz; i++)
            PRINT_CHAR_BITS (os, tmpi[i]);
        }
      else
        {
          if (bit_format > 1)
            {
              for (std::size_t i = 0; i < sz; i++)
                PRINT_CHAR_BITS (os, tmpi[i]);
            }
          else
            {
              for (int i = sz - 1; i >= 0; i--)
                PRINT_CHAR_BITS (os, tmpi[i]);
            }
        }
    }
  else
    {
      octave::preserve_stream_state stream_state (os);

      os << std::setw (fw)
         << typename octave_print_conv<T>::print_conv_type (d);

      if (bank_format)
        os << ".00";
    }
}

template void
pr_int (std::ostream&, const octave_int8&, int);

template void
pr_int (std::ostream&, const octave_int16&, int);

template void
pr_int (std::ostream&, const octave_int32&, int);

template void
pr_int (std::ostream&, const octave_int64&, int);

template void
pr_int (std::ostream&, const octave_uint8&, int);

template void
pr_int (std::ostream&, const octave_uint16&, int);

template void
pr_int (std::ostream&, const octave_uint32&, int);

template void
pr_int (std::ostream&, const octave_uint64&, int);

template <typename T>
void
octave_print_internal_template (std::ostream& os,
                                const float_display_format& fmt,
                                const octave_int<T>& val, bool)
{
  if (plus_format)
    pr_plus_format (os, val);
  else
    {
      if (free_format)
        os << typename octave_print_conv<octave_int<T>>::print_conv_type (val);
      else
        {
          float_format r_fmt = fmt.real_format ();

          pr_int (os, val, r_fmt.width ());
        }
    }
}

#define PRINT_INT_SCALAR_INTERNAL(TYPE)                                 \
  void                                                                  \
  octave_print_internal (std::ostream& os,                              \
                         const float_display_format& fmt,               \
                         const octave_int<TYPE>& val, bool dummy)       \
  {                                                                     \
    octave_print_internal_template (os, fmt, val, dummy);               \
  }

PRINT_INT_SCALAR_INTERNAL (int8_t)
PRINT_INT_SCALAR_INTERNAL (uint8_t)
PRINT_INT_SCALAR_INTERNAL (int16_t)
PRINT_INT_SCALAR_INTERNAL (uint16_t)
PRINT_INT_SCALAR_INTERNAL (int32_t)
PRINT_INT_SCALAR_INTERNAL (uint32_t)
PRINT_INT_SCALAR_INTERNAL (int64_t)
PRINT_INT_SCALAR_INTERNAL (uint64_t)

template <typename T>
static inline void
octave_print_internal_template (std::ostream& os, const intNDArray<T>& nda,
                                bool pr_as_read_syntax, int extra_indent)
{
  // FIXME: this mostly duplicates the code in the print_nd_array<>
  // function.  Can fix this with std::is_same from C++11.

  if (nda.isempty ())
    print_empty_nd_array (os, nda.dims (), pr_as_read_syntax);
  else if (nda.numel () == 1)
    octave_print_internal_template (os, float_display_format (), nda(0),
                                    pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      int ndims = nda.ndims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      dim_vector dims = nda.dims ();

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      for (octave_idx_type i = 0; i < m; i++)
        {
          if (m > 1)
            {
              std::string nm = "ans(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ',';
                  else
                    buf << ')';
                }

              nm += buf.str ();

              os << nm << " =\n";
              if (! Vcompact_format)
                os << "\n";
            }

          Array<octave::idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = octave::idx_vector (':');
          idx(1) = octave::idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = octave::idx_vector (ra_idx(k));

          Array<T> page (nda.index (idx), dim_vector (nr, nc));

          for (octave_idx_type ii = 0; ii < nr; ii++)
            {
              for (octave_idx_type jj = 0; jj < nc; jj++)
                {
                  octave_quit ();

                  pr_plus_format (os, page(ii, jj));
                }

              if ((ii < nr - 1) || (i < m -1))
                os << "\n";
            }

          if (i < m - 1)
            {
              os << "\n";
              increment_index (ra_idx, dims, 2);
            }
        }
    }
  else
    {
      int ndims = nda.ndims ();

      dim_vector dims = nda.dims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      int fw = 0;
      if (hex_format)
        fw = 2 * nda(0).byte_size ();
      else if (bit_format)
        fw = nda(0).nbits ();
      else
        {
          bool isneg = false;
          int digits = 0;

          for (octave_idx_type i = 0; i < dims.numel (); i++)
            {
              int new_digits
                = static_cast<int>
                  (std::floor (log10 (double (abs (nda(i).value ()))) + 1));

              if (new_digits > digits)
                digits = new_digits;

              if (! isneg)
                isneg = (abs (nda(i).value ()) != nda(i).value ());
            }

          fw = digits + isneg;
        }

      int column_width = fw + (rat_format ?  0 : (bank_format ? 5 : 2));
      octave_idx_type total_width = nc * column_width;
      int max_width = octave::command_editor::terminal_cols () - extra_indent;
      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      for (octave_idx_type i = 0; i < m; i++)
        {
          if (m > 1)
            {
              std::string nm = "ans(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ',';
                  else
                    buf << ')';
                }

              nm += buf.str ();

              os << nm << " =\n";
              if (! Vcompact_format)
                os << "\n";
            }

          Array<octave::idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = octave::idx_vector (':');
          idx(1) = octave::idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = octave::idx_vector (ra_idx(k));

          Array<T> page (nda.index (idx), dim_vector (nr, nc));

          if (free_format)
            {
              if (pr_as_read_syntax)
                os << "[\n";

              for (octave_idx_type ii = 0; ii < nr; ii++)
                {
                  for (octave_idx_type jj = 0; jj < nc; jj++)
                    {
                      octave_quit ();
                      os << "  ";
                      os << typename octave_print_conv<T>::print_conv_type (page(ii, jj));
                    }
                  os << "\n";
                }

              if (pr_as_read_syntax)
                os << ']';
            }
          else
            {
              octave::preserve_stream_state stream_state (os);

              octave_idx_type n_rows = page.rows ();
              octave_idx_type n_cols = page.cols ();

              for (octave_idx_type col = 0; col < n_cols; col += inc)
                {
                  octave_idx_type lim = (col + inc < n_cols ? col + inc
                                         : n_cols);

                  pr_col_num_header (os, total_width, max_width, lim, col,
                                     extra_indent);

                  for (octave_idx_type ii = 0; ii < n_rows; ii++)
                    {
                      os << std::setw (extra_indent) << "";

                      for (octave_idx_type jj = col; jj < lim; jj++)
                        {
                          octave_quit ();
                          os << "  ";
                          pr_int (os, page(ii, jj), fw);
                        }
                      if ((ii < n_rows - 1) || (i < m -1))
                        os << "\n";
                    }
                }
            }

          if (i < m - 1)
            {
              os << "\n";
              increment_index (ra_idx, dims, 2);
            }
        }
    }
}

#define PRINT_INT_ARRAY_INTERNAL(TYPE)                                  \
  OCTINTERP_API void                                                    \
  octave_print_internal (std::ostream& os, const intNDArray<TYPE>& nda, \
                         bool pr_as_read_syntax, int extra_indent)      \
  {                                                                     \
    octave_print_internal_template (os, nda, pr_as_read_syntax, extra_indent); \
  }

PRINT_INT_ARRAY_INTERNAL (octave_int8)
PRINT_INT_ARRAY_INTERNAL (octave_uint8)
PRINT_INT_ARRAY_INTERNAL (octave_int16)
PRINT_INT_ARRAY_INTERNAL (octave_uint16)
PRINT_INT_ARRAY_INTERNAL (octave_int32)
PRINT_INT_ARRAY_INTERNAL (octave_uint32)
PRINT_INT_ARRAY_INTERNAL (octave_int64)
PRINT_INT_ARRAY_INTERNAL (octave_uint64)

void
octave_print_internal (std::ostream&, const Cell&, bool, int, bool)
{
  panic_impossible ();
}

void
octave_print_internal (std::ostream&, const octave_value&, bool)
{
  panic_impossible ();
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (rats, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{s} =} rats (@var{x})
@deftypefnx {} {@var{s} =} rats (@var{x}, @var{len})
Convert @var{x} into a rational approximation represented as a string.

A rational approximation to a floating point number is a simple fraction
with numerator @var{N} and denominator @var{D} such that
@code{@var{x} = @var{N}/@var{D}}.

The optional second argument defines the maximum length of the string
representing the elements of @var{x}.  By default, @var{len} is 9.

If the length of the smallest possible rational approximation exceeds
@var{len}, an asterisk (*) padded with spaces will be returned instead.

Example conversion from matrix to string, and back again.

@example
@group
r = rats (hilb (4));
x = str2num (r)
@end group
@end example

@seealso{rat, format}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value arg = args(0);

  if (! arg.isnumeric ())
    error ("rats: X must be numeric");

  if (arg.isempty ())
    return ovl (octave_value (""));

  // Convert to N-D arrays to 2-D arrays for Matlab compatibility
  if (arg.ndims () > 2)
    {
      dim_vector dv (arg.rows (), arg.numel () / arg.rows ());
      arg = arg.reshape (dv);
    }

  unwind_protect frame;

  frame.protect_var (rat_string_len);

  rat_string_len = 9;
  if (nargin == 2)
    rat_string_len = args(1).nint_value ();

  frame.protect_var (rat_format);

  rat_format = true;

  std::ostringstream buf;
  arg.print (buf);
  std::string s = buf.str ();

  std::list<std::string> lst;

  std::size_t n = 0;
  std::size_t s_len = s.length ();

  while (n < s_len)
    {
      std::size_t m = s.find ('\n',  n);

      if (m == std::string::npos)
        {
          lst.push_back (s.substr (n));
          break;
        }
      else
        {
          lst.push_back (s.substr (n, m - n));
          n = m + 1;
        }
    }

  return ovl (string_vector (lst));
}

/*
%!test <*56941>
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   format short;
%!   assert (rats (-2.0005, 10), "-4001/2000");
%!   assert (strtrim (rats (2.0005, 30)), "4001/2000");
%!   assert (pi - str2num (rats (pi, 30)), 0, 4 * eps);
%!   assert (e - str2num (rats (e, 30)), 0, 4 * eps);
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect

%!test <*57003>
%! x = ones (2,1,3);
%! s = rats (x,4);
%! assert (ndims (s) == 2);
%! assert (rows (s) == 2);
%! assert (columns (s) == 3 * 6);

%!assert <*57004> (rats ([]), '')

%!test <57704>
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   format short;
%!   assert (rats (2.0005, 9), "4001/2000");
%!   assert (rats (123, 2), " *");
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect

*/

DEFUN (disp, args, nargout,
       classes: cell char double function_handle int8 int16 int32 int64 logical single struct uint8 uint16 uint32 uint64
       doc: /* -*- texinfo -*-
@deftypefn  {} {} disp (@var{x})
@deftypefnx {} {@var{str} =} disp (@var{x})
Display the value of @var{x}.

For example:

@example
@group
disp ("The value of pi is:"), disp (pi)

     @print{} the value of pi is:
     @print{} 3.1416
@end group
@end example

@noindent
Note that the output from @code{disp} always ends with a newline.

If an output value is requested, @code{disp} prints nothing and returns the
formatted output in a string.
@seealso{fdisp}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value_list retval;

  octave_value arg = args(0);

  if (nargout == 0)
    arg.print (octave_stdout);
  else
    {
      std::ostringstream buf;
      arg.print (buf);
      retval = (octave_value (buf.str (), arg.is_dq_string () ? '"' : '\''));
    }

  return retval;
}

DEFMETHOD (fdisp, interp, args, ,
           classes: cell char double function_handle int8 int16 int32 int64 logical single struct uint8 uint16 uint32 uint64
           doc: /* -*- texinfo -*-
@deftypefn {} {} fdisp (@var{fid}, @var{x})
Display the value of @var{x} on the stream @var{fid}.

For example:

@example
@group
fdisp (stdout, "The value of pi is:"), fdisp (stdout, pi)

     @print{} the value of pi is:
     @print{} 3.1416
@end group
@end example

@noindent
Note that the output from @code{fdisp} always ends with a newline.
@seealso{disp}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  int fid = streams.get_file_number (args(0));

  stream os = streams.lookup (fid, "fdisp");

  std::ostream *osp = os.preferred_output_stream ();

  if (! osp)
    error ("fdisp: stream FID not open for writing");

  octave_value arg = args(1);
  arg.print (*osp);

  return ovl ();
}

/*
## FIXME: This test writes values to a file, but then never checks them.
%!test
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   format short
%!   fd = tmpfile ();
%!   for r = [0, Inf -Inf, NaN]
%!     for i = [0, Inf -Inf, NaN]
%!       fdisp (fd, complex (r, i));
%!     endfor
%!   endfor
%!   fclose (fd);
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect

%!test
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   foo.real = pi * ones (3,20,3);
%!   foo.complex = pi * ones (3,20,3) + 1i;
%!   foo.char = repmat ("- Hello World -", [3, 20]);
%!   foo.cell = {foo.real, foo.complex, foo.char};
%!   fields = fieldnames (foo);
%!   for f = 1:numel (fields)
%!     format loose;
%!     loose = disp (foo.(fields{f}));
%!     format compact;
%!     compact = disp (foo.(fields{f}));
%!     expected = strrep (loose, "\n\n", "\n");
%!     assert (expected, compact);
%!   endfor
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect
*/

DEFUN (display, args, ,
       classes: cell char double function_handle int8 int16 int32 int64 logical single struct uint8 uint16 uint32 uint64
       doc: /* -*- texinfo -*-
@deftypefn {} {} display (@var{obj})
Display the contents of the object @var{obj} prepended by its name.

The Octave interpreter calls the @code{display} function whenever it needs
to present a class on-screen.  Typically, this would be a statement which
does not end in a semicolon to suppress output.  For example:

@example
myclass (@dots{})
@end example

Or:

@example
myobj = myclass (@dots{})
@end example

In general, user-defined classes should overload the @code{disp} method to
avoid the default output:

@example
@group
myobj = myclass (@dots{})
  @result{} myobj =

  <class myclass>
@end group
@end example

When overloading the @code{display} method instead, one has to take care
of properly displaying the object's name.  This can be done by using the
@code{inputname} function.

@seealso{disp, class, subsref, subsasgn}
@end deftypefn */)
{
  int nargin = args.length ();

  // Matlab apparently accepts two arguments with the second set to the
  // inputname of the first.  This is undocumented, but we'll use it.
  // However, we never call display methods for classes with more than
  // one argument.

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string name;

  if (nargin == 2)
    name = args(1).xstring_value ("NAME must be a string");
  else
    {
      string_vector names = args.name_tags ();
      name = names(0);
    }

  // We are here because there is no overloaded display method for this
  // object type.

  octave_value value = args(0);

  // If print_name_tag displays a newline, then also print one after
  // disp is done.

  bool print_newlines = false;
  if (valid_identifier (name))
    print_newlines = value.print_name_tag (octave_stdout, name);

  // Use feval so that dispatch will also work for disp.

  feval ("disp", ovl (value));

  if (print_newlines)
    octave_stdout << std::endl;

  return ovl ();
}

/*
%!test
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   format short;
%!   str = evalc ("x = 1.1; display (x)");
%!   assert (str, "x = 1.1000\n");
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect

%!test
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   format short;
%!   str = evalc ("display (1.1)");
%!   assert (str, "1.1000\n");
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect

## Test input validation
%!error display ()
%!error display (1,2)
*/

static inline void
init_format_state (void)
{
  free_format = false;
  plus_format = false;
  rat_format = false;
  bank_format = false;
  hex_format = 0;
  bit_format = 0;
  print_e = false;
  print_g = false;
  print_eng = false;
}

static std::string format_string ("short");

static inline void
set_format_style (int argc, const string_vector& argv)
{
  if (--argc == 0)
    {
      // Special case of no options, reset to default values
      init_format_state ();
      set_output_prec (5);
      format_string = "short";
      Vcompact_format = false;
      uppercase_format = false;
      return;
    }

  int idx = 1;
  std::string format;

  unwind_protect frame;

  frame.protect_var (bank_format);
  frame.protect_var (bit_format);
  frame.protect_var (free_format);
  frame.protect_var (hex_format);
  frame.protect_var (plus_format);
  frame.protect_var (plus_format_chars);
  frame.protect_var (rat_format);
  frame.protect_var (print_e);
  frame.protect_var (print_eng);
  frame.protect_var (print_g);
  frame.protect_var (format_string);
  frame.protect_var (Vcompact_format);
  frame.protect_var (uppercase_format);
  int prec = output_precision ();
  frame.add ([=] (void) { set_output_prec (prec); });

  format = format_string;   // Initialize with existing value
  while (argc-- > 0)
    {
      std::string arg = argv[idx++];
      std::transform (arg.begin (), arg.end (), arg.begin (), tolower);

      if (arg == "default")
        {
          format = "short";
          init_format_state ();
          set_output_prec (5);
          Vcompact_format = false;
          uppercase_format = false;
        }
      else if (arg == "short")
        {
          format = arg;
          init_format_state ();
          if (argc > 0)
            {
              arg = argv[idx];
              if (arg == "e")
                {
                  print_e = true;
                  format.append (arg);
                  argc--; idx++;
                }
              else if (arg == "g")
                {
                  init_format_state ();
                  print_g = true;
                  format.append (arg);
                  argc--; idx++;
                }
              else if (arg == "eng")
                {
                  init_format_state ();
                  print_eng = true;
                  format.append (arg);
                  argc--; idx++;
                }
            }
          set_output_prec (5);
        }
      else if (arg == "shorte")
        {
          format = arg;
          init_format_state ();
          print_e = true;
          set_output_prec (5);
        }
      else if (arg == "shortg")
        {
          format = arg;
          init_format_state ();
          print_g = true;
          set_output_prec (5);
        }
      else if (arg == "shorteng")
        {
          format = arg;
          init_format_state ();
          print_eng = true;
          set_output_prec (5);
        }
      else if (arg == "long")
        {
          format = arg;
          init_format_state ();
          if (argc > 0)
            {
              arg = argv[idx];

              if (arg == "e")
                {
                  print_e = true;
                  format.append (arg);
                  argc--; idx++;
                }
              else if (arg == "g")
                {
                  print_g = true;
                  format.append (arg);
                  argc--; idx++;
                }
              else if (arg == "eng")
                {
                  print_eng = true;
                  format.append (arg);
                  argc--; idx++;
                }
            }
          set_output_prec (16);
        }
      else if (arg == "longe")
        {
          format = arg;
          init_format_state ();
          print_e = true;
          set_output_prec (16);
        }
      else if (arg == "longg")
        {
          format = arg;
          init_format_state ();
          print_g = true;
          set_output_prec (16);
        }
      else if (arg == "longeng")
        {
          format = arg;
          init_format_state ();
          print_eng = true;
          set_output_prec (16);
        }
      else if (arg == "hex")
        {
          format = arg;
          init_format_state ();
          hex_format = 1;
        }
      else if (arg == "native-hex")
        {
          format = arg;
          init_format_state ();
          hex_format = 2;
        }
      else if (arg == "bit")
        {
          format = arg;
          init_format_state ();
          bit_format = 1;
        }
      else if (arg == "native-bit")
        {
          format = arg;
          init_format_state ();
          bit_format = 2;
        }
      else if (arg == "+" || arg == "plus")
        {
          format = arg;
          init_format_state ();
          plus_format = true;
          if (argc > 0)
            {
              arg = argv[idx];

              if (arg.length () == 3)
                {
                  plus_format_chars = arg;
                  format.append (arg);
                  argc--; idx++;
                }
              else
                plus_format_chars = "+- ";
            }
          else
            plus_format_chars = "+- ";
        }
      else if (arg == "rat")
        {
          format = arg;
          init_format_state ();
          rat_format = true;
        }
      else if (arg == "bank")
        {
          format = arg;
          init_format_state ();
          bank_format = true;
        }
      else if (arg == "free")
        {
          format = arg;
          init_format_state ();
          free_format = true;
        }
      else if (arg == "none")
        {
          format = arg;
          init_format_state ();
          free_format = true;
        }
      else if (arg == "compact")
        Vcompact_format = true;
      else if (arg == "loose")
        Vcompact_format = false;
      else if (arg == "lowercase")
        uppercase_format = false;
      else if (arg == "uppercase")
        uppercase_format = true;
      else
        error ("format: unrecognized format state '%s'", arg.c_str ());
    }

  format_string = format;

  // If successful, discard unwind state information
  frame.discard ();
}

DEFUN (format, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} format
@deftypefnx {} {} format options
@deftypefnx {} {} format (@var{options})
@deftypefnx {} {[@var{format}, @var{formatspacing}, @var{uppercase}] =} format
Reset or specify the format of the output produced by @code{disp} and Octave's
normal echoing mechanism.

This command only affects the display of numbers, not how they are stored
or computed.  To change the internal representation from the default double use
one of the conversion functions such as @code{single}, @code{uint8},
@code{int64}, etc.  Any @code{format} options that change the number of
displayed significant digits will also be reflected by the
@code{output_precision} function.

By default, Octave displays 5 significant digits in a human readable form
(option @samp{short}, option @samp{lowercase}, and option @samp{loose} format
for matrices).  If @code{format} is invoked without any options, or the option
@samp{default} is specified, then this default format is restored.

Valid format options for floating point numbers are listed in the following
table.

@table @code
@item default
Restore the default format state described above.

@item short
Fixed point format with 5 significant figures (default).

@item long
Fixed point format with 16 significant figures.

As with the @samp{short} format, Octave will switch to an exponential @samp{e}
format if it is unable to format a matrix properly using the current format.

@item  shorte
@itemx longe
Exponential format.  The number to be represented is split between a mantissa
and an exponent (power of 10).  The mantissa has 5 significant digits in the
short format.  In the long format, double values are displayed with 16
significant digits and single values are displayed with 8.  For example,
with the @samp{shorte} format, @code{pi} is displayed as @code{3.1416e+00}.
Optionally, the trailing @samp{e} can be split into a second argument.

@item  shortg
@itemx longg
Optimally choose between fixed point and exponential format based on the
magnitude of the number.  For example, with the @samp{shortg} format,
@code{pi .^ [2; 4; 8; 16; 32]} is displayed as

@example
@group
ans =

      9.8696
      97.409
      9488.5
  9.0032e+07
  8.1058e+15
@end group
@end example

Optionally, the trailing @samp{g} can be split into a second argument.

@item  shorteng
@itemx longeng
Identical to @samp{shorte} or @samp{longe} but displays the value using an
engineering format, where the exponent is divisible by 3.  For example, with
the @samp{shorteng} format, @code{10 * pi} is displayed as @code{31.416e+00}.
Optionally, the trailing @samp{eng} can be split into a second argument.

@item  free
@itemx none
Print output in free format, without trying to line up columns of matrices on
the decimal point.  This is a raw format equivalent to the C++ code
@code{std::cout << @var{variable}}.  In general, the result is a presentation
with 6 significant digits where unnecessary precision (such as trailing zeros
for integers) is suppressed.  Complex numbers are formatted as numeric pairs
like this @samp{(0.60419, 0.60709)} instead of like this
@samp{0.60419 + 0.60709i}.
@end table

The following formats affect all numeric output (floating point and integer
types).

@table @asis
@item  @qcode{"+"}
@itemx @qcode{"+"} @qcode{"@var{chars}"}
@itemx @code{plus}
@itemx @code{plus @var{chars}}
Print a @samp{+} symbol for matrix elements greater than zero, a @samp{-}
symbol for elements less than zero, and a space for zero matrix elements.  This
format can be useful for examining the sparsity structure of a large matrix.
For very large matrices the function @code{spy} which plots the sparsity
pattern will be clearer.

The optional argument @var{chars} specifies a list of 3 characters to use for
printing values greater than zero, less than zero, and equal to zero.  For
example, with the format @qcode{"+" "+-."}, the matrix
@code{[1, 0, -1; -1, 0, 1]} is displayed as

@example
@group
ans =

+.-
-.+
@end group
@end example

@item bank
Print variable in a format appropriate for a currency (fixed format with two
digits to the right of the decimal point).  Only the real part of a variable is
displayed, as the imaginary part makes no sense for a currency.

@item bit
Print the bit representation of numbers in memory, always with the
most significant bit first.  For example, @code{pi} is printed like this:

@example
@group
0 10000000000 1001001000011111101101010100010001000010110100011000
@end group
@end example

where spaces have been added for clarity to show the sign bit, the 11-bit
exponent, and the 52-bit mantissa, in that order.  Together they represent
@code{pi} as an IEEE 754 double precision floating point number in the normal
form.  Single precision floating point numbers are analogous.

@item native-bit
Print the bit representation of numbers as stored in memory.  For big-endian
machines, this is identical to the @code{format bit} layout seen above.
For little-endian machines, it will print the bytes in the opposite order,
though bits within a byte will still be presented with the most significant
bit on the left.

For example, the value of @code{pi} in this format on x86-64 is:

@example
@group
00011000 00101101 01000100 01010100 11111011 00100001 00001001 01000000
@end group
@end example

shown here with spaces added for clarity.  Compare with the previous bit string
from @code{format bit} to see the grouping into bytes and their ordering.

@item hex
The same as @code{format bit} above, except that bits are grouped four at a
time into hexadecimal digits for brevity.  Thus @code{pi} is represented as:

@example
@group
400921fb54442d18
@end group
@end example

@item native-hex
The same as @code{format native-bit} above, except that bits are grouped four
at a time into hexadecimal digits for brevity.  Thus @code{pi} is represented
on an x86-64 as:

@example
@group
182d4454fb210940
@end group
@end example

@item rat
Print a rational approximation, i.e., values are approximated as the ratio of
small integers.  For example, with the @samp{rat} format, @code{pi} is
displayed as @code{355/113}.
@end table

The following two options affect the display of scientific and hex notations.

@table @code
@item lowercase (default)
Use a lowercase @samp{e} for the exponent character in scientific notation and
lowercase @samp{a-f} for the hex digits representing 10-15.

@item uppercase
Use an uppercase @samp{E} for the exponent character in scientific notation and
uppercase @samp{A-F} for the hex digits representing 10-15.
@end table

The following two options affect the display of all matrices.

@table @code
@item compact
Remove blank lines around column number labels and between matrices producing
more compact output with more data per page.

@item loose (default)
Insert blank lines above and below column number labels and between matrices to
produce a more readable output with less data per page.
@end table

If @code{format} is called with multiple competing options, the rightmost one
is used, except for @samp{default} which will override all other options.  In
case of an error the format remains unchanged.

If called with one to three output arguments, and no inputs, return the current
format, format spacing, and uppercase preference.  Specifying both outputs and
inputs will produce an error.

@seealso{fixed_point_format, output_precision, split_long_rows,
print_empty_dimensions, rats}
@end deftypefn */)
{
  octave_value_list retval (std::min (nargout, 2));

  int nargin = args.length ();

  if (nargout == 0)
    {
      int argc = nargin + 1;

      string_vector argv = args.make_argv ("format");

      set_format_style (argc, argv);
    }
  else
    {
      if (nargin > 0)
        warning ("format: cannot query and set format at the same time, ignoring set operation");

      if (nargout >= 3)
        retval(2) = (uppercase_format ? "uppercase" : "lowercase");

      if (nargout >= 2)
        retval(1) = (Vcompact_format ? "compact" : "loose");

      retval(0) = format_string;
    }

  return retval;
}

/*
%!test
%! [old_fmt, old_spacing, old_uppercase] = format ();
%! unwind_protect
%!   ## Test one of the formats
%!   format long e;
%!   format uppercase;
%!   str = disp (pi);
%!   assert (str, "3.141592653589793E+00\n");
%!   str = disp (single (pi));
%!   assert (str, "3.1415927E+00\n");
%!   new_fmt = format ();
%!   assert (new_fmt, "longe");
%!   ## Test resetting format (method #1)
%!   format compact;
%!   [~, new_spacing] = format ();
%!   assert (new_spacing, "compact");
%!   format;
%!   [new_fmt, new_spacing, new_case] = format ();
%!   assert (new_fmt, "short");
%!   assert (new_spacing, "loose");
%!   assert (new_case, "lowercase");
%!   ## Test resetting format (method #2)
%!   format compact uppercase long e;
%!   [new_fmt, new_spacing, new_case] = format ();
%!   assert (new_fmt, "longe");
%!   assert (new_spacing, "compact");
%!   assert (new_case, "uppercase");
%!   format ("default");
%!   [new_fmt, new_spacing, new_case] = format ();
%!   assert (new_fmt, "short");
%!   assert (new_spacing, "loose");
%!   assert (new_case, "lowercase");
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%!   format (old_uppercase);
%! end_unwind_protect

%!test <*53427>
%! [old_fmt, old_spacing] = format ();
%! unwind_protect
%!   format;          # reset format to short and loose
%!   format compact;  # set compact format
%!   format long;     # set long format
%!   [fmt, spacing] = format ();
%!   assert (fmt, "long");
%!   assert (spacing, "compact");
%! unwind_protect_cleanup
%!   format (old_fmt);
%!   format (old_spacing);
%! end_unwind_protect

## Test input validation
%!test
%! fail ("fmt = format ('long')", "warning", "cannot query and set format");

*/

DEFUN (fixed_point_format, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} fixed_point_format ()
@deftypefnx {} {@var{old_val} =} fixed_point_format (@var{new_val})
@deftypefnx {} {@var{old_val} =} fixed_point_format (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave will
use a scaled format to print matrix values.

The scaled format prints a scaling factor on the first line of output chosen
such that the largest matrix element can be written with a single leading
digit.  For example:

@example
@group
fixed_point_format (true)
logspace (1, 7, 5)'
ans =

  1.0e+07  *

  0.00000
  0.00003
  0.00100
  0.03162
  1.00000
@end group
@end example

@noindent
Notice that the first value appears to be 0 when it is actually 1.  Because
of the possibility for confusion you should be careful about enabling
@code{fixed_point_format}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{format, output_precision}
@end deftypefn */)
{
  return set_internal_variable (Vfixed_point_format, args, nargout,
                                "fixed_point_format");
}

DEFUN (print_empty_dimensions, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} print_empty_dimensions ()
@deftypefnx {} {@var{old_val} =} print_empty_dimensions (@var{new_val})
@deftypefnx {} {@var{old_val} =} print_empty_dimensions (@var{new_val}, "local")
Query or set the internal variable that controls whether the dimensions of
empty matrices are printed along with the empty matrix symbol, @samp{[]}.

For example, the expression

@example
zeros (3, 0)
@end example

@noindent
will print

@example
ans = [](3x0)
@end example

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{format}
@end deftypefn */)
{
  return set_internal_variable (Vprint_empty_dimensions, args, nargout,
                                "print_empty_dimensions");
}

DEFUN (split_long_rows, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} split_long_rows ()
@deftypefnx {} {@var{old_val} =} split_long_rows (@var{new_val})
@deftypefnx {} {@var{old_val} =} split_long_rows (@var{new_val}, "local")
Query or set the internal variable that controls whether rows of a matrix
may be split when displayed to a terminal window.

If the rows are split, Octave will display the matrix in a series of smaller
pieces, each of which can fit within the limits of your terminal width and
each set of rows is labeled so that you can easily see which columns are
currently being displayed.  For example:

@example
@group
octave:13> rand (2,10)
ans =

 Columns 1 through 6:

  0.75883  0.93290  0.40064  0.43818  0.94958  0.16467
  0.75697  0.51942  0.40031  0.61784  0.92309  0.40201

 Columns 7 through 10:

  0.90174  0.11854  0.72313  0.73326
  0.44672  0.94303  0.56564  0.82150
@end group
@end example

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{format}
@end deftypefn */)
{
  return set_internal_variable (Vsplit_long_rows, args, nargout,
                                "split_long_rows");
}

OCTAVE_END_NAMESPACE(octave)
