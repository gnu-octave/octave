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

#if ! defined (octave_base_dae_h)
#define octave_base_dae_h 1

#include "octave-config.h"

#include "base-de.h"

class
base_diff_alg_eqn : public base_diff_eqn
{
public:

  base_diff_alg_eqn (void)
    : base_diff_eqn (), m_xdot () { }

  base_diff_alg_eqn (const ColumnVector& xx, double tt)
    : base_diff_eqn (xx, tt), m_xdot (xx.numel (), 0.0) { }

  base_diff_alg_eqn (const ColumnVector& xx, const ColumnVector& xxdot,
                     double tt)
    : base_diff_eqn (xx, tt), m_xdot (xxdot) { }

  base_diff_alg_eqn (const base_diff_alg_eqn& a)
    : base_diff_eqn (a), m_xdot (a.m_xdot) { }

  virtual ~base_diff_alg_eqn (void) = default;

  base_diff_alg_eqn& operator = (const base_diff_alg_eqn& a)
  {
    if (this != &a)
      {
        base_diff_eqn::operator = (a);
        m_xdot = a.m_xdot;
      }
    return *this;
  }

  void initialize (const ColumnVector& x0, double t0)
  {
    base_diff_eqn::initialize (x0, t0);
    m_xdot = ColumnVector (x0.numel (), 0.0);
  }

  void initialize (const ColumnVector& x0, const ColumnVector& xdot0,
                   double t0)
  {
    base_diff_eqn::initialize (x0, t0);
    m_xdot = xdot0;
  }

  ColumnVector state_derivative (void) { return m_xdot; }

protected:

  ColumnVector m_xdot;
};

#endif
