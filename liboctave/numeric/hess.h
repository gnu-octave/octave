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

#if ! defined (octave_hess_h)
#define octave_hess_h 1

#include "octave-config.h"

#include <iosfwd>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
hess
{
public:

  hess (void)
    : m_hess_mat (), m_unitary_hess_mat ()
  { }

  hess (const T& a)
    : m_hess_mat (), m_unitary_hess_mat ()
  {
    init (a);
  }

  hess (const T& a, octave_idx_type& info)
    : m_hess_mat (), m_unitary_hess_mat ()
  {
    info = init (a);
  }

  hess (const hess& a)
    : m_hess_mat (a.m_hess_mat), m_unitary_hess_mat (a.m_unitary_hess_mat)
  { }

  hess& operator = (const hess& a)
  {
    if (this != &a)
      {
        m_hess_mat = a.m_hess_mat;
        m_unitary_hess_mat = a.m_unitary_hess_mat;
      }

    return *this;
  }

  ~hess (void) = default;

  T hess_matrix (void) const { return m_hess_mat; }

  T unitary_hess_matrix (void) const { return m_unitary_hess_mat; }

private:

  T m_hess_mat;
  T m_unitary_hess_mat;

  OCTAVE_API octave_idx_type init (const T& a);
};

template <typename T>
extern OCTAVE_API std::ostream&
operator << (std::ostream& os, const hess<T>& a);

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
