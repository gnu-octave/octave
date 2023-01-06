////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_EIG_h)
#define octave_EIG_h 1

#include "octave-config.h"

#include <iosfwd>

#include "mx-fwd.h"

#include "CColVector.h"
#include "CMatrix.h"

class
OCTAVE_API
EIG
{
  friend class Matrix;
  friend class ComplexMatrix;

public:

  EIG (void) : m_lambda (), m_v (), m_w () { }

  EIG (const Matrix& a, bool calc_rev = true,
       bool calc_lev = true, bool balance = true)
    : m_lambda (), m_v (), m_w ()
  {
    init (a, calc_rev, calc_lev, balance);
  }

  EIG (const Matrix& a, octave_idx_type& info,
       bool calc_rev = true, bool calc_lev = true, bool balance = true)
    : m_lambda (), m_v (), m_w ()
  {
    info = init (a, calc_rev, calc_lev, balance);
  }

  EIG (const Matrix& a, const Matrix& b,
       bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : m_lambda (), m_v (), m_w ()
  {
    init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const Matrix& a, const Matrix& b, octave_idx_type& info,
       bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : m_lambda (), m_v (), m_w ()
  {
    info = init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const ComplexMatrix& a, bool calc_rev = true,
       bool calc_lev = true, bool balance = true)
    : m_lambda (), m_v (), m_w ()
  {
    init (a, calc_rev, calc_lev, balance);
  }

  EIG (const ComplexMatrix& a, octave_idx_type& info,
       bool calc_rev = true, bool calc_lev = true, bool balance = true)
    : m_lambda (), m_v (), m_w ()
  {
    info = init (a, calc_rev, calc_lev, balance);
  }

  EIG (const ComplexMatrix& a, const ComplexMatrix& b,
       bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : m_lambda (), m_v (), m_w ()
  {
    init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const ComplexMatrix& a, const ComplexMatrix& b,
       octave_idx_type& info, bool calc_rev = true, bool calc_lev = true,
       bool force_qz = false)
    : m_lambda (), m_v (), m_w ()
  {
    info = init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const EIG& a) : m_lambda (a.m_lambda), m_v (a.m_v), m_w (a.m_w) { }

  EIG& operator = (const EIG& a)
  {
    if (this != &a)
      {
        m_lambda = a.m_lambda;
        m_v = a.m_v;
        m_w = a.m_w;
      }
    return *this;
  }

  ~EIG (void) = default;

  ComplexColumnVector eigenvalues (void) const { return m_lambda; }
  ComplexMatrix right_eigenvectors (void) const { return m_v; }
  ComplexMatrix left_eigenvectors (void) const { return m_w; }

  friend std::ostream&  operator << (std::ostream& os, const EIG& a);

private:

  ComplexColumnVector m_lambda;
  ComplexMatrix m_v;
  ComplexMatrix m_w;

  octave_idx_type init (const Matrix& a, bool calc_rev, bool calc_lev,
                        bool balance);

  octave_idx_type init (const Matrix& a, const Matrix& b,
                        bool calc_rev, bool calc_lev, bool force_qz);

  octave_idx_type init (const ComplexMatrix& a, bool calc_rev,
                        bool calc_lev, bool balance);

  octave_idx_type init (const ComplexMatrix& a, const ComplexMatrix& b,
                        bool calc_rev, bool calc_lev, bool force_qz);

  octave_idx_type symmetric_init (const Matrix& a, bool calc_rev,
                                  bool calc_lev);

  octave_idx_type symmetric_init (const Matrix& a, const Matrix& b,
                                  bool calc_rev, bool calc_lev);

  octave_idx_type hermitian_init (const ComplexMatrix& a,
                                  bool calc_rev, bool calc_lev);

  octave_idx_type hermitian_init (const ComplexMatrix& a,
                                  const ComplexMatrix& b,
                                  bool calc_rev, bool calc_lev);

};

#endif
