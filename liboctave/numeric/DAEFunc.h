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

#if ! defined (octave_DAEFunc_h)
#define octave_DAEFunc_h 1

#include "octave-config.h"

#include "mx-fwd.h"

class
DAEFunc
{
public:

  typedef ColumnVector (*DAERHSFunc) (const ColumnVector& x,
                                      const ColumnVector& xdot,
                                      double t, octave_idx_type& ires);

  // This is really the form used by DASSL:
  //
  //   PD = DG/DY + CJ * DG/DYPRIME

  typedef Matrix (*DAEJacFunc) (const ColumnVector& x,
                                const ColumnVector& xdot,
                                double t, double cj);

  DAEFunc (void)
    : m_fcn (nullptr), m_jac (nullptr), m_reset (true) { }

  DAEFunc (DAERHSFunc f)
    : m_fcn (f), m_jac (nullptr), m_reset (true) { }

  DAEFunc (DAERHSFunc f, DAEJacFunc j)
    : m_fcn (f), m_jac (j), m_reset (true) { }

  DAEFunc (const DAEFunc& a)
    : m_fcn (a.m_fcn), m_jac (a.m_jac), m_reset (a.m_reset) { }

  DAEFunc& operator = (const DAEFunc& a)
  {
    if (this != &a)
      {
        m_fcn = a.m_fcn;
        m_jac = a.m_jac;
        m_reset = a.m_reset;
      }
    return *this;
  }

  virtual ~DAEFunc (void) = default;

  DAERHSFunc function (void) const { return m_fcn; }

  DAEFunc& set_function (DAERHSFunc f)
  {
    m_fcn = f;
    m_reset = true;
    return *this;
  }

  DAEJacFunc jacobian_function (void) const { return m_jac; }

  DAEFunc& set_jacobian_function (DAEJacFunc j)
  {
    m_jac = j;
    m_reset = true;
    return *this;
  }

protected:

  DAERHSFunc m_fcn;
  DAEJacFunc m_jac;

  // This variable is TRUE when this object is constructed, and also
  // after any internal data has changed.  Derived classes may use
  // this information (and change it) to know when to (re)initialize
  // their own internal data related to this object.

  bool m_reset;
};

#endif
