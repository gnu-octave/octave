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

#if ! defined (octave_aepbalance_h)
#define octave_aepbalance_h 1

#include "octave-config.h"

#include <algorithm>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename MT>
class
aepbalance
{
public:

  typedef typename MT::real_column_vector_type VT;

  aepbalance (void) : m_balanced_mat (), m_scale (), m_ilo (), m_ihi (), m_job () { }

  OCTAVE_API aepbalance (const MT& a, bool noperm = false, bool noscal = false);

  aepbalance (const aepbalance& a)
    : m_balanced_mat (a.m_balanced_mat), m_scale (a.m_scale),
      m_ilo(a.m_ilo), m_ihi(a.m_ihi), m_job(a.m_job)
  { }

  aepbalance& operator = (const aepbalance& a)
  {
    if (this != &a)
      {
        m_balanced_mat = a.m_balanced_mat;
        m_scale = a.m_scale;
        m_ilo = a.m_ilo;
        m_ihi = a.m_ihi;
        m_job = a.m_job;
      }

    return *this;
  }

  virtual ~aepbalance (void) = default;

  OCTAVE_API MT balancing_matrix (void) const;

  MT balanced_matrix (void) const
  {
    return m_balanced_mat;
  }

  VT permuting_vector (void) const
  {
    octave_idx_type n = m_balanced_mat.rows ();

    VT pv (n);

    for (octave_idx_type i = 0; i < n; i++)
      pv(i) = i+1;

    for (octave_idx_type i = n-1; i >= m_ihi; i--)
      {
        octave_idx_type j = m_scale(i) - 1;
        std::swap (pv(i), pv(j));
      }

    for (octave_idx_type i = 0; i < m_ilo-1; i++)
      {
        octave_idx_type j = m_scale(i) - 1;
        std::swap (pv(i), pv(j));
      }

    return pv;
  }

  VT scaling_vector (void) const
  {
    octave_idx_type n = m_balanced_mat.rows ();

    VT scv (n);

    for (octave_idx_type i = 0; i < m_ilo-1; i++)
      scv(i) = 1;

    for (octave_idx_type i = m_ilo-1; i < m_ihi; i++)
      scv(i) = m_scale(i);

    for (octave_idx_type i = m_ihi; i < n; i++)
      scv(i) = 1;

    return scv;
  }

protected:

  MT m_balanced_mat;
  VT m_scale;
  octave_idx_type m_ilo;
  octave_idx_type m_ihi;
  char m_job;
};

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
