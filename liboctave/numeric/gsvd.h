////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1997-2023 The Octave Project Developers
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

#if ! defined (octave_gsvd_h)
#define octave_gsvd_h 1

#include "octave-config.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
OCTAVE_API
gsvd
{
public:

  enum class Type
  {
    std,
    economy,
    sigma_only
  };

  gsvd (void) : m_sigmaA (), m_sigmaB (), m_left_smA (), m_left_smB (), m_right_sm ()
  { }

  gsvd (const T& a, const T& b,
        gsvd::Type gsvd_type = gsvd<T>::Type::std);

  gsvd (const gsvd& a)
    : m_type (a.m_type),
      m_sigmaA (a.m_sigmaA), m_sigmaB (a.m_sigmaB),
      m_left_smA (a.m_left_smA), m_left_smB (a.m_left_smB), m_right_sm (a.m_right_sm)
  { }

  gsvd& operator = (const gsvd& a)
  {
    if (this != &a)
      {
        m_type = a.m_type;
        m_sigmaA = a.m_sigmaA;
        m_sigmaB = a.m_sigmaB;
        m_left_smA = a.m_left_smA;
        m_left_smB = a.m_left_smB;
        m_right_sm = a.m_right_sm;
      }

    return *this;
  }

  ~gsvd (void) = default;

  typename T::real_matrix_type
  singular_values_A (void) const { return m_sigmaA; }

  typename T::real_matrix_type
  singular_values_B (void) const { return m_sigmaB; }

  T left_singular_matrix_A (void) const;
  T left_singular_matrix_B (void) const;

  T right_singular_matrix (void) const;

private:
  typedef typename T::value_type P;
  typedef typename T::real_matrix_type real_matrix;

  void ggsvd (char& jobu, char& jobv, char& jobq, octave_f77_int_type m,
              octave_f77_int_type n, octave_f77_int_type p,
              octave_f77_int_type& k, octave_f77_int_type& l,
              P *tmp_dataA, octave_f77_int_type m1,
              P *tmp_dataB, octave_f77_int_type p1,
              real_matrix& alpha, real_matrix& beta,
              P *u, octave_f77_int_type nrow_u,
              P *v, octave_f77_int_type nrow_v,
              P *q, octave_f77_int_type nrow_q,
              P *work, octave_f77_int_type lwork,
              octave_f77_int_type *iwork,
              octave_f77_int_type& info);

  //--------

  gsvd::Type m_type;
  real_matrix m_sigmaA, m_sigmaB;
  T m_left_smA, m_left_smB;
  T m_right_sm;
};

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
