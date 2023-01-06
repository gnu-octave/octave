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

#if ! defined (octave_gepbalance_h)
#define octave_gepbalance_h 1

#include "octave-config.h"

#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
gepbalance
{
public:

  typedef typename T::real_matrix_type RT;

  gepbalance (void)
    : m_balanced_mat (), m_balanced_mat2 (), m_balancing_mat (),
      m_balancing_mat2 ()
  { }

  gepbalance (const T& a, const T& b, const std::string& job)
    : m_balanced_mat (), m_balanced_mat2 (), m_balancing_mat (),
      m_balancing_mat2 ()
  {
    init (a, b, job);
  }

  gepbalance (const gepbalance& a)
    : m_balanced_mat (a.m_balanced_mat),
      m_balanced_mat2 (a.m_balanced_mat2),
      m_balancing_mat (a.m_balancing_mat),
      m_balancing_mat2 (a.m_balancing_mat2)
  { }

  gepbalance& operator = (const gepbalance& a)
  {
    if (this != &a)
      {
        m_balanced_mat = a.m_balanced_mat;
        m_balanced_mat2 = a.m_balanced_mat2;
        m_balancing_mat = a.m_balancing_mat;
        m_balancing_mat2 = a.m_balancing_mat2;
      }

    return *this;
  }

  ~gepbalance (void) = default;

  T balanced_matrix (void) const { return m_balanced_mat; }

  T balanced_matrix2 (void) const { return m_balanced_mat2; }

  RT balancing_matrix (void) const { return m_balancing_mat; }

  RT balancing_matrix2 (void) const { return m_balancing_mat2; }

private:

  T m_balanced_mat;
  T m_balanced_mat2;
  RT m_balancing_mat;
  RT m_balancing_mat2;

  OCTAVE_API octave_idx_type
  init (const T& a, const T& b, const std::string& job);
};

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
