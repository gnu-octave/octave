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

#if ! defined (octave_qrp_h)
#define octave_qrp_h 1

#include "octave-config.h"

#include "PermMatrix.h"
#include "qr.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
qrp : public qr<T>
{
public:

  typedef typename T::real_row_vector_type RV_T;

  typedef typename qr<T>::type type;

  qrp (void) : qr<T> (), m_p () { }

  OCTAVE_API qrp (const T&, type = qr<T>::std);

  qrp (const qrp& a) : qr<T> (a), m_p (a.m_p) { }

  qrp& operator = (const qrp& a)
  {
    if (this != &a)
      {
        qr<T>::operator = (a);
        m_p = a.m_p;
      }

    return *this;
  }

  ~qrp (void) = default;

  OCTAVE_API void init (const T&, type = qr<T>::std);

  PermMatrix P (void) const { return m_p; }

  OCTAVE_API RV_T Pvec (void) const;

private:

  PermMatrix m_p;
};

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
