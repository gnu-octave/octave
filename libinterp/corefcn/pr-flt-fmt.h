/*

Copyright (C) 1993-2017 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

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

extern int output_precision (void);

extern void set_output_prec (int prec);

class
float_format
{
public:

  float_format (int w = 0, int p = output_precision (), int f = 0)
    : fw (w), ex (0), prec (p), fmt (f), up (0), sp (0) { }

  float_format (int w, int e, int p, int f)
    : fw (w), ex (e), prec (p), fmt (f), up (0), sp (0) { }

  float_format (const float_format&) = default;

  float_format& operator = (const float_format&) = default;

  ~float_format (void) = default;

  float_format& scientific (void)
  {
    fmt = std::ios::scientific;
    return *this;
  }

  float_format& fixed (void)
  {
    fmt = std::ios::fixed;
    return *this;
  }

  float_format& general (void)
  {
    fmt = 0;
    return *this;
  }

  float_format& uppercase (void)
  {
    up = std::ios::uppercase;
    return *this;
  }

  float_format& lowercase (void)
  {
    up = 0;
    return *this;
  }

  float_format& precision (int p)
  {
    prec = p;
    return *this;
  }

  float_format& width (int w)
  {
    fw = w;
    return *this;
  }

  float_format& trailing_zeros (bool tz = true)

  {
    sp = (tz ? std::ios::showpoint : 0);
    return *this;
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

  // Field width.  Zero means as wide as necessary.
  int fw;

  // Exponent Field width.  Zero means as wide as necessary.
  int ex;

  // Precision.
  int prec;

  // Format.
  int fmt;

  // E or e.
  int up;

  // Show trailing zeros.
  int sp;
};

class
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
    m_real_fmt.prec = prec;
    m_imag_fmt.prec = prec;
  }

private:

  double m_scale;

  float_format m_real_fmt;

  float_format m_imag_fmt;
};

#endif
