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

#if ! defined (octave_svd_h)
#define octave_svd_h 1

#include "octave-config.h"

#include <vector>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
class
OCTAVE_API
svd
{
public:

  typedef typename T::real_diag_matrix_type DM_T;

  enum class Type
  {
    std,
    economy,
    sigma_only
  };

  enum class Driver
  {
    GESVD,
    GESDD,
    GEJSV
  };

  svd (void)
    : m_type (), m_driver (), m_left_sm (), m_sigma (), m_right_sm ()
  { }

  svd (const T& a, svd::Type type = svd::Type::std,
       svd::Driver driver = svd::Driver::GESVD);

  svd (const svd& a)
    : m_type (a.m_type), m_driver (a.m_driver), m_left_sm (a.m_left_sm),
      m_sigma (a.m_sigma), m_right_sm (a.m_right_sm)
  { }

  svd& operator = (const svd& a)
  {
    if (this != &a)
      {
        m_type = a.m_type;
        m_left_sm = a.m_left_sm;
        m_sigma = a.m_sigma;
        m_right_sm = a.m_right_sm;
        m_driver = a.m_driver;
      }

    return *this;
  }

  ~svd (void) = default;

  T left_singular_matrix (void) const;

  DM_T singular_values (void) const { return m_sigma; }

  T right_singular_matrix (void) const;

private:

  typedef typename T::element_type P;
  typedef typename DM_T::element_type DM_P;

  svd::Type m_type;
  svd::Driver m_driver;

  T m_left_sm;
  DM_T m_sigma;
  T m_right_sm;

  void gesvd (char& jobu, char& jobv, octave_f77_int_type m,
              octave_f77_int_type n, P *tmp_data, octave_f77_int_type m1,
              DM_P *s_vec, P *u, P *vt, octave_f77_int_type nrow_vt1,
              std::vector<P>& work, octave_f77_int_type& lwork,
              octave_f77_int_type& info);

  void gesdd (char& jobz, octave_f77_int_type m, octave_f77_int_type n,
              P *tmp_data, octave_f77_int_type m1, DM_P *s_vec, P *u,
              P *vt, octave_f77_int_type nrow_vt1, std::vector<P>& work,
              octave_f77_int_type& lwork, octave_f77_int_type *iwork,
              octave_f77_int_type& info);

  void gejsv (char& joba, char& jobu, char& jobv, char& jobr, char& jobt,
              char& jobp, octave_f77_int_type m, octave_f77_int_type n,
              P *tmp_data, octave_f77_int_type m1, DM_P *s_vec, P *u,
              P *v, octave_f77_int_type nrow_v1, std::vector<P>& work,
              octave_f77_int_type& lwork,
              std::vector<octave_f77_int_type>& iwork,
              octave_f77_int_type& info);
};

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
