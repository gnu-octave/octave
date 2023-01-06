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

#if ! defined (octave_DASSL_h)
#define octave_DASSL_h 1

#include "octave-config.h"

#include <string>

#include "mx-fwd.h"

#include "Array.h"
#include "DASSL-opts.h"

class
OCTAVE_API
DASSL : public DAE, public DASSL_options
{
public:

  DASSL (void)
    : DAE (), DASSL_options (), m_initialized (false), m_liw (0), m_lrw (0),
      m_info (), m_iwork (), m_rwork (), m_abs_tol (), m_rel_tol () { }

  DASSL (const ColumnVector& s, double tm, DAEFunc& f)
    : DAE (s, tm, f), DASSL_options (), m_initialized (false), m_liw (0),
      m_lrw (0), m_info (), m_iwork (), m_rwork (), m_abs_tol (), m_rel_tol ()
  { }

  DASSL (const ColumnVector& s, const ColumnVector& deriv,
         double tm, DAEFunc& f)
    : DAE (s, deriv, tm, f), DASSL_options (), m_initialized (false),
      m_liw (0), m_lrw (0), m_info (), m_iwork (), m_rwork (), m_abs_tol (),
      m_rel_tol () { }

  ~DASSL (void) = default;

  ColumnVector do_integrate (double t);

  Matrix do_integrate (const ColumnVector& tout);

  Matrix do_integrate (const ColumnVector& tout, const ColumnVector& tcrit);

  Matrix integrate (const ColumnVector& tout, Matrix& xdot_out);

  Matrix integrate (const ColumnVector& tout, Matrix& xdot_out,
                    const ColumnVector& tcrit);

  std::string error_message (void) const;

private:

  bool m_initialized;

  octave_f77_int_type m_liw;
  octave_f77_int_type m_lrw;

  Array<octave_f77_int_type> m_info;
  Array<octave_f77_int_type> m_iwork;

  Array<double> m_rwork;

  Array<double> m_abs_tol;
  Array<double> m_rel_tol;
};

#endif
