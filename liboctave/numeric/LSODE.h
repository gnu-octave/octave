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

#if ! defined (octave_LSODE_h)
#define octave_LSODE_h 1

#include "octave-config.h"

#include <string>

#include "Array.h"
#include "LSODE-opts.h"

class
OCTAVE_API
LSODE : public ODE, public LSODE_options
{
public:

  LSODE (void)
    : ODE (), LSODE_options (), m_initialized (false), m_method_flag (0),
      m_itask (0), m_iopt (0), m_itol (0), m_liw (0), m_lrw (0),
      m_iwork (), m_rwork (), m_rel_tol (0.0), m_abs_tol () { }

  LSODE (const ColumnVector& s, double tm, const ODEFunc& f)
    : ODE (s, tm, f), LSODE_options (), m_initialized (false),
      m_method_flag (0), m_itask (0), m_iopt (0), m_itol (0), m_liw (0),
      m_lrw (0), m_iwork (), m_rwork (), m_rel_tol (0.0), m_abs_tol () { }

  ~LSODE (void) = default;

  ColumnVector do_integrate (double t);

  Matrix do_integrate (const ColumnVector& tout);

  Matrix do_integrate (const ColumnVector& tout, const ColumnVector& tcrit);

  std::string error_message (void) const;

private:

  bool m_initialized;

  octave_f77_int_type m_method_flag;
  octave_f77_int_type m_itask;
  octave_f77_int_type m_iopt;
  octave_f77_int_type m_itol;

  octave_f77_int_type m_liw;
  octave_f77_int_type m_lrw;

  Array<octave_f77_int_type> m_iwork;
  Array<double> m_rwork;

  double m_rel_tol;

  Array<double> m_abs_tol;
};

#endif
