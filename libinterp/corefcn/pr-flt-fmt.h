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

#if ! defined (octave_pr_flt_fmt_h)
#define octave_pr_flt_fmt_h 1

#include "octave-config.h"

#include <iomanip>
#include <iosfwd>

template <typename T>
class pr_engineering_float;

template <typename T>
class pr_formatted_float;

template <typename T>
class pr_rational_float;

extern OCTINTERP_API int output_precision (void);

extern OCTINTERP_API void set_output_prec (int prec);

class
OCTINTERP_API
float_format
{
public:

  float_format (int w = 0, int p = output_precision (), int f = 0)
    : m_fw (w), m_ex (0), m_prec (p), m_fmt (f), m_up (0), m_sp (0) { }

  float_format (int w, int e, int p, int f)
    : m_fw (w), m_ex (e), m_prec (p), m_fmt (f), m_up (0), m_sp (0) { }

  float_format (const float_format&) = default;

  float_format& operator = (const float_format&) = default;

  ~float_format (void) = default;

  float_format& scientific (void)
  {
    m_fmt = std::ios::scientific;
    return *this;
  }

  float_format& fixed (void)
  {
    m_fmt = std::ios::fixed;
    return *this;
  }

  float_format& general (void)
  {
    m_fmt = 0;
    return *this;
  }

  float_format& uppercase (void)
  {
    m_up = std::ios::uppercase;
    return *this;
  }

  float_format& lowercase (void)
  {
    m_up = 0;
    return *this;
  }

  float_format& precision (int p)
  {
    m_prec = p;
    return *this;
  }

  float_format& width (int w)
  {
    m_fw = w;
    return *this;
  }

  float_format& exponent_width (int w)
  {
    m_ex = w;
    return *this;
  }

  float_format& trailing_zeros (bool tz = true)

  {
    m_sp = (tz ? std::ios::showpoint : 0);
    return *this;
  }

  std::ios::fmtflags format_flags (void) const
  {
    return static_cast<std::ios::fmtflags> (m_fmt | m_up | m_sp);
  }

  int format (void) const
  {
    return m_fmt;
  }

  bool is_scientific (void) const
  {
    return m_fmt == std::ios::scientific;
  }

  bool is_fixed (void) const
  {
    return m_fmt == std::ios::fixed;
  }

  bool is_general (void) const
  {
    return m_fmt == 0;
  }

  bool is_uppercase (void) const
  {
    return m_up == std::ios::uppercase;
  }

  bool is_lowercase (void) const
  {
    return m_up == 0;
  }

  int precision (void) const
  {
    return m_prec;
  }

  int width (void) const
  {
    return m_fw;
  }

  int exponent_width (void) const
  {
    return m_ex;
  }

  bool show_trailing_zeros (void) const
  {
    return m_sp == std::ios::showpoint;
  }

  template <typename T>
  friend std::ostream&
  operator << (std::ostream& os, const pr_engineering_float<T>& pef);

  template <typename T>
  friend std::ostream&
  operator << (std::ostream& os, const pr_formatted_float<T>& pff);

  template <typename T>
  friend std::ostream&
  operator << (std::ostream& os, const pr_rational_float<T>& prf);

private:

  // Field width.  Zero means as wide as necessary.
  int m_fw;

  // Exponent Field width.  Zero means as wide as necessary.
  int m_ex;

  // Precision.
  int m_prec;

  // Format.
  int m_fmt;

  // E or e.
  int m_up;

  // Show trailing zeros.
  int m_sp;
};

class
OCTINTERP_API
float_display_format
{
public:

  float_display_format (void) = default;

  float_display_format (double scale, const float_format& real_fmt,
                        const float_format& imag_fmt = float_format ())
    : m_scale (scale), m_real_fmt (real_fmt), m_imag_fmt (imag_fmt)
  { }

  explicit float_display_format (const float_format& real_fmt,
                                 const float_format& imag_fmt = float_format ())
    : m_scale (1.0), m_real_fmt (real_fmt), m_imag_fmt (imag_fmt)
  { }

  float_display_format (const float_display_format&) = default;

  float_display_format& operator = (const float_display_format&) = default;

  ~float_display_format (void) = default;

  double scale_factor (void) const { return m_scale; }

  float_format real_format (void) const { return m_real_fmt; }

  float_format imag_format (void) const { return m_imag_fmt; }

  void set_precision (int prec)
  {
    m_real_fmt.precision (prec);
    m_imag_fmt.precision (prec);
  }

private:

  double m_scale = 1.0;

  float_format m_real_fmt;

  float_format m_imag_fmt;
};

#endif
