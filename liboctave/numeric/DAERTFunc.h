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

#if ! defined (octave_DAERTFunc_h)
#define octave_DAERTFunc_h 1

#include "octave-config.h"

#include "DAEFunc.h"

class
DAERTFunc : public DAEFunc
{
public:

  typedef ColumnVector (*DAERTConstrFunc) (const ColumnVector& x, double t);

  DAERTFunc (void)
    : DAEFunc (), m_constr (nullptr), m_reset (true) { }

  DAERTFunc (DAERHSFunc f)
    : DAEFunc (f), m_constr (nullptr), m_reset (true) { }

  DAERTFunc (DAERHSFunc f, DAEJacFunc j)
    : DAEFunc (f, j), m_constr (nullptr), m_reset (true) { }

  DAERTFunc (DAERHSFunc f, DAERTConstrFunc cf)
    : DAEFunc (f), m_constr (cf), m_reset (true) { }

  DAERTFunc (DAERHSFunc f, DAERTConstrFunc cf, DAEJacFunc j)
    : DAEFunc (f, j), m_constr (cf), m_reset (true) { }

  DAERTFunc (const DAERTFunc& a)
    : DAEFunc (a), m_constr (a.m_constr), m_reset (a.m_reset) { }

  DAERTFunc& operator = (const DAERTFunc& a)
  {
    if (this != &a)
      {
        DAEFunc::operator = (a);
        m_constr = a.m_constr;
        m_reset = a.m_reset;
      }
    return *this;
  }

  virtual ~DAERTFunc (void) = default;

  DAERTConstrFunc constraint_function (void) const { return m_constr; }

  DAERTFunc& set_constraint_function (DAERTConstrFunc cf)
  {
    m_constr = cf;
    m_reset = true;
    return *this;
  }

protected:

  DAERTConstrFunc m_constr;

  // This variable is TRUE when this object is constructed, and also
  // after any internal data has changed.  Derived classes may use
  // this information (and change it) to know when to (re)initialize
  // their own internal data related to this object.

  bool m_reset;
};

#endif
