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

#if ! defined (octave_DASRT_h)
#define octave_DASRT_h 1

#include "octave-config.h"

#include <string>

#include "Array.h"
#include "DASRT-opts.h"
#include "dMatrix.h"

class
DASRT_result
{
public:

  DASRT_result (void)
    : m_x (), m_xdot (), m_t () { }

  DASRT_result (const Matrix& x, const Matrix& xdot, const ColumnVector& t)
    : m_x (x), m_xdot (xdot), m_t (t) { }

  DASRT_result (const DASRT_result& r)
    : m_x (r.m_x), m_xdot (r.m_xdot), m_t (r.m_t) { }

  DASRT_result& operator = (const DASRT_result& r)
  {
    if (this != &r)
      {
        m_x = r.m_x;
        m_xdot = r.m_xdot;
        m_t = r.m_t;
      }
    return *this;
  }

  ~DASRT_result (void) = default;

  Matrix state (void) const { return m_x; }
  Matrix deriv (void) const { return m_xdot; }
  ColumnVector times (void) const { return m_t; }

private:

  Matrix m_x;
  Matrix m_xdot;
  ColumnVector m_t;
};

class
OCTAVE_API
DASRT : public DAERT, public DASRT_options
{
public:

  DASRT (void)
    : DAERT (), DASRT_options (), m_initialized (false),
      m_liw (0), m_lrw (0), m_ng (0), m_info (), m_iwork (), m_jroot (),
      m_rwork (), m_abs_tol (), m_rel_tol ()
  { }

  DASRT (const ColumnVector& s, double tm, DAERTFunc& f)
    : DAERT (s, tm, f), DASRT_options (), m_initialized (false),
      m_liw (0), m_lrw (0), m_ng (0), m_info (), m_iwork (), m_jroot (),
      m_rwork (), m_abs_tol (), m_rel_tol ()
  { }

  DASRT (const ColumnVector& s, const ColumnVector& deriv,
         double tm, DAERTFunc& f)
    : DAERT (s, deriv, tm, f), DASRT_options (), m_initialized (false),
      m_liw (0), m_lrw (0), m_ng (0), m_info (), m_iwork (), m_jroot (),
      m_rwork (), m_abs_tol (), m_rel_tol ()
  { }

  ~DASRT (void) = default;

  DASRT_result integrate (const ColumnVector& tout);

  DASRT_result integrate (const ColumnVector& tout,
                          const ColumnVector& tcrit);

  std::string error_message (void) const;

private:

  bool m_initialized;

  octave_f77_int_type m_liw;
  octave_f77_int_type m_lrw;

  octave_f77_int_type m_ng;

  Array<octave_f77_int_type> m_info;
  Array<octave_f77_int_type> m_iwork;
  Array<octave_f77_int_type> m_jroot;

  Array<double> m_rwork;

  Array<double> m_abs_tol;
  Array<double> m_rel_tol;

  void integrate (double t);
};

#endif
