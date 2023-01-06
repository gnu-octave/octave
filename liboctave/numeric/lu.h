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

#if ! defined (octave_lu_h)
#define octave_lu_h 1

#include "octave-config.h"

#include "mx-fwd.h"

#include "Array.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
lu
{
public:

  typedef typename T::column_vector_type VT;
  typedef typename T::element_type ELT_T;

  lu (void)
    : m_a_fact (), m_L (), m_ipvt () { }

  OCTAVE_API lu (const T& a);

  lu (const lu& a)
    : m_a_fact (a.m_a_fact), m_L (a.m_L), m_ipvt (a.m_ipvt) { }

  OCTAVE_API lu (const T& l, const T& u, const PermMatrix& p);

  lu& operator = (const lu& a)
  {
    if (this != &a)
      {
        m_a_fact = a.m_a_fact;
        m_L = a.m_L;
        m_ipvt = a.m_ipvt;
      }

    return *this;
  }

  virtual ~lu (void) = default;

  OCTAVE_API bool packed (void) const;

  OCTAVE_API void unpack (void);

  OCTAVE_API T L (void) const;

  OCTAVE_API T U (void) const;

  OCTAVE_API T Y (void) const;

  OCTAVE_API PermMatrix P (void) const;

  OCTAVE_API ColumnVector P_vec (void) const;

  OCTAVE_API bool regular (void) const;

  OCTAVE_API void update (const VT& u, const VT& v);

  OCTAVE_API void update (const T& u, const T& v);

  OCTAVE_API void update_piv (const VT& u, const VT& v);

  OCTAVE_API void update_piv (const T& u, const T& v);

protected:

  // The result of getp is passed to other Octave Matrix functions,
  // so we use octave_idx_type.
  OCTAVE_API Array<octave_idx_type> getp (void) const;

  T m_a_fact;
  T m_L;

  // This is internal storage that is passed to Fortran,
  // so we need a Fortran INTEGER.
  Array<octave_f77_int_type> m_ipvt;
};

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
