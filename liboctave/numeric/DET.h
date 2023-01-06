////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#if ! defined (octave_DET_h)
#define octave_DET_h 1

#include "octave-config.h"

#include <cmath>

#include "lo-mappers.h"
#include "oct-cmplx.h"

template <typename T>
class
base_det
{
public:

  base_det (T c = 1, int e = 0)
    : m_c2 (), m_e2 ()
  {
    m_c2 = octave::math::log2 (c, m_e2);
    m_e2 += e;
  }

  base_det (T c, double e, double b)
    : m_c2 (), m_e2 ()
  {
    e *= octave::math::log2 (b);
    m_e2 = e;
    c *= octave::math::exp2 (e - m_e2);
    int f;
    m_c2 = octave::math::log2 (c, f);
    m_e2 += f;
  }

  base_det (const base_det& a) : m_c2 (a.m_c2), m_e2 (a.m_e2) { }

  base_det& operator = (const base_det& a)
  {
    m_c2 = a.m_c2;
    m_e2 = a.m_e2;
    return *this;
  }

  T coef (void) const { return m_c2; }
  int exp (void) const { return m_e2; }

  T value () const { return m_c2 * static_cast<T> (std::ldexp (1.0, m_e2)); }
  operator T () const { return value (); }

  base_det square () const { return base_det (m_c2*m_c2, m_e2+m_e2); }

  void operator *= (T t)
  {
    int e;
    m_c2 *= t;
    // Renormalize m_c2 to [0.5, 1), and find required change in exponent.
    m_c2 = octave::math::log2 (m_c2, e);
    m_e2 += e;
  }

private:

  T m_c2;
  int m_e2;
};

// Provide the old types by typedefs.
typedef base_det<double> DET;
typedef base_det<float> FloatDET;
typedef base_det<Complex> ComplexDET;
typedef base_det<FloatComplex> FloatComplexDET;

#endif
