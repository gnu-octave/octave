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

#if ! defined (octave_qr_h)
#define octave_qr_h 1

#include "octave-config.h"

#include "Array-fwd.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
qr
{
public:

  typedef typename T::element_type ELT_T;
  typedef typename T::row_vector_type RV_T;
  typedef typename T::column_vector_type CV_T;

  enum type
  {
    std,
    raw,
    economy
  };

  qr (void) : m_q (), m_r () { }

  qr (const T& a, type qr_type = qr::std)
    : m_q (), m_r ()
  {
    init (a, qr_type);
  }

  OCTAVE_API qr (const T& m_q, const T& m_r);

  qr (const qr& a) : m_q (a.m_q), m_r (a.m_r) { }

  qr& operator = (const qr& a)
  {
    if (this != &a)
      {
        m_q = a.m_q;
        m_r = a.m_r;
      }

    return *this;
  }

  virtual ~qr (void) = default;

  T Q (void) const { return m_q; }

  T R (void) const { return m_r; }

  OCTAVE_API type get_type (void) const;

  OCTAVE_API bool regular (void) const;

  OCTAVE_API void init (const T& a, type qr_type);

  OCTAVE_API void update (const CV_T& u, const CV_T& v);

  OCTAVE_API void update (const T& u, const T& v);

  OCTAVE_API void insert_col (const CV_T& u, octave_idx_type j);

  OCTAVE_API void insert_col (const T& u, const Array<octave_idx_type>& j);

  OCTAVE_API void delete_col (octave_idx_type j);

  OCTAVE_API void delete_col (const Array<octave_idx_type>& j);

  OCTAVE_API void insert_row (const RV_T& u, octave_idx_type j);

  OCTAVE_API void delete_row (octave_idx_type j);

  OCTAVE_API void shift_cols (octave_idx_type i, octave_idx_type j);

protected:

  T m_q;
  T m_r;

  OCTAVE_API void
  form (octave_idx_type n, T& afact, ELT_T *tau, type qr_type);
};

extern OCTAVE_API void warn_qrupdate_once (void);

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
