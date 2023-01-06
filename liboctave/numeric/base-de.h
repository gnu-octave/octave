////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_base_de_h)
#define octave_base_de_h 1

#include "octave-config.h"

#include <string>

#include "dColVector.h"

class
base_diff_eqn
{
public:

  base_diff_eqn (void)
    : m_x (), m_t (0.0), m_stop_time (0.0), m_stop_time_set (false),
      m_restart (true), m_integration_error (false), m_istate (0) { }

  base_diff_eqn (const ColumnVector& xx, double tt)
    : m_x (xx), m_t (tt), m_stop_time (0.0), m_stop_time_set (false),
      m_restart (true), m_integration_error (false), m_istate (0) { }

  base_diff_eqn (const base_diff_eqn& a)
    : m_x (a.m_x), m_t (a.m_t), m_stop_time (0.0), m_stop_time_set (false),
      m_restart (true), m_integration_error (false), m_istate (0) { }

  virtual ~base_diff_eqn (void) = default;

  base_diff_eqn& operator = (const base_diff_eqn& a)
  {
    if (this != &a)
      {
        m_x = a.m_x;
        m_t = a.m_t;
        m_stop_time = a.m_stop_time;
        m_stop_time_set = a.m_stop_time_set;
        m_restart = a.m_restart;
        m_integration_error = a.m_integration_error;
        m_istate = a.m_istate;
      }

    return *this;
  }

  void initialize (const ColumnVector& x0, double t0)
  {
    m_x = x0;
    m_t = t0;
    m_integration_error = false;
    m_istate = 0;
    force_restart ();
  }

  octave_idx_type size (void) const { return m_x.numel (); }

  ColumnVector state (void) const { return m_x; }

  double time (void) const { return m_t; }

  void set_stop_time (double tt)
  {
    m_stop_time_set = true;
    m_stop_time = tt;
    force_restart ();
  }

  void clear_stop_time (void)
  {
    m_stop_time_set = false;
    force_restart ();
  }

  virtual void force_restart (void) { m_restart = true; }

  bool integration_ok (void) const { return ! m_integration_error; }

  octave_idx_type integration_state (void) const { return m_istate; }

  virtual std::string error_message (void) const = 0;

protected:

  ColumnVector m_x;

  double m_t;

  double m_stop_time;

  bool m_stop_time_set;

  bool m_restart;

  bool m_integration_error;

  octave_idx_type m_istate;
};

#endif
