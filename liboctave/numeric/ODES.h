////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2002-2023 The Octave Project Developers
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

#if ! defined (octave_ODES_h)
#define octave_ODES_h 1

#include "octave-config.h"

#include "ODESFunc.h"
#include "base-de.h"

class
ODES : public base_diff_eqn, public ODESFunc
{
public:

  ODES (void)
    : base_diff_eqn (), ODESFunc (), m_xdot (), m_theta () { }

  ODES (const ColumnVector& s, double tm, ODESFunc& f)
    : base_diff_eqn (s, tm), ODESFunc (f), m_xdot (s.numel (), 0.0), m_theta ()
  { }

  ODES (const ColumnVector& s, const ColumnVector& xtheta, double tm,
        ODESFunc& f)
    : base_diff_eqn (s, tm), ODESFunc (f), m_xdot (s.numel (), 0.0),
      m_theta (xtheta) { }

  ODES (const ODES& a)
    : base_diff_eqn (a), ODESFunc (a), m_xdot (a.m_xdot), m_theta (a.m_theta)
  { }

  ODES& operator = (const ODES& a)
  {
    if (this != &a)
      {
        base_diff_eqn::operator = (a);
        ODESFunc::operator = (a);

        m_xdot = a.m_xdot;
        m_theta = a.m_theta;
      }
    return *this;
  }

  ~ODES (void) = default;

  ColumnVector parameter_vector (void) { return m_theta; }

  OCTAVE_API void initialize (const ColumnVector& x, double t);

  OCTAVE_API void
  initialize (const ColumnVector& x, double t, const ColumnVector& theta);

protected:

  // State vector time derivatives.
  ColumnVector m_xdot;

  // Parameter vector.
  ColumnVector m_theta;
};

#endif
