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

#if ! defined (octave_ODEFunc_h)
#define octave_ODEFunc_h 1

#include "octave-config.h"

#include "mx-fwd.h"

class
ODEFunc
{
public:

  typedef ColumnVector (*ODERHSFunc) (const ColumnVector&, double);
  typedef Matrix (*ODEJacFunc) (const ColumnVector&, double);

  ODEFunc (void)
    : m_fcn (nullptr), m_jac (nullptr), m_reset (true) { }

  ODEFunc (ODERHSFunc f)
    : m_fcn (f), m_jac (nullptr), m_reset (true) { }

  ODEFunc (ODERHSFunc f, ODEJacFunc j)
    : m_fcn (f), m_jac (j), m_reset (true) { }

  ODEFunc (const ODEFunc& a)
    : m_fcn (a.m_fcn), m_jac (a.m_jac), m_reset (true) { }

  ODEFunc& operator = (const ODEFunc& a)
  {
    if (this != &a)
      {
        m_fcn = a.m_fcn;
        m_jac = a.m_jac;
        m_reset = a.m_reset;
      }
    return *this;
  }

  virtual ~ODEFunc (void) = default;

  ODERHSFunc function (void) const { return m_fcn; }

  ODEFunc& set_function (ODERHSFunc f)
  {
    m_fcn = f;
    m_reset = true;
    return *this;
  }

  ODEJacFunc jacobian_function (void) const { return m_jac; }

  ODEFunc& set_jacobian_function (ODEJacFunc j)
  {
    m_jac = j;
    m_reset = true;
    return *this;
  }

protected:

  ODERHSFunc m_fcn;
  ODEJacFunc m_jac;

  // This variable is TRUE when this object is constructed, and also
  // after any internal data has changed.  Derived classes may use
  // this information (and change it) to know when to (re)initialize
  // their own internal data related to this object.

  bool m_reset;
};

#endif
