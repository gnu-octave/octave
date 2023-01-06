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

#if ! defined (octave_schur_h)
#define octave_schur_h 1

#include "octave-config.h"

#include <string>

// FIXME: Don't really need these for compiling schur.h, but it messes
// up compilation in liboctave/array if these are not present.
#include "CMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fMatrix.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
schur
{
public:

  schur (void) : m_schur_mat (), m_unitary_schur_mat () { }

  schur (const T& a, const std::string& ord, bool calc_unitary = true)
    : m_schur_mat (), m_unitary_schur_mat ()
  {
    init (a, ord, calc_unitary);
  }

  schur (const T& a, const std::string& ord, octave_f77_int_type& info,
         bool calc_unitary = true)
    : m_schur_mat (), m_unitary_schur_mat ()
  {
    info = init (a, ord, calc_unitary);
  }

  // This one should really be protected or private but we need it in
  // rsf2csf and I don't see how to make that function a friend of
  // this class.
  schur (const T& s, const T& u) : m_schur_mat (s), m_unitary_schur_mat (u)
  { }

  schur (const schur& a)
    : m_schur_mat (a.m_schur_mat),
      m_unitary_schur_mat (a.m_unitary_schur_mat)
  { }

  schur& operator = (const schur& a)
  {
    if (this != &a)
      {
        m_schur_mat = a.m_schur_mat;
        m_unitary_schur_mat = a.m_unitary_schur_mat;
      }

    return *this;
  }

  ~schur (void) = default;

  T schur_matrix (void) const { return m_schur_mat; }

  T unitary_schur_matrix (void) const { return m_unitary_schur_mat; }

protected:

private:

  T m_schur_mat;
  T m_unitary_schur_mat;

  OCTAVE_API octave_f77_int_type
  init (const T& a, const std::string& ord, bool calc_unitary);
};

template <typename RT, typename AT>
extern OCTAVE_API schur<RT>
rsf2csf (const AT& s, const AT& u);

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
