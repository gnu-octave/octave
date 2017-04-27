/*

Copyright (C) 2016-2017 Carnë Draug
Copyright (C) 1994-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_svd_h)
#define octave_svd_h 1

#include "octave-config.h"

#include <vector>

namespace octave
{
  namespace math
  {
    template <typename T>
    class
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
        GESDD
      };

      svd (void)
        : m_type (), m_driver (), left_sm (), sigma (), right_sm ()
      { }

      svd (const T& a, svd::Type type = svd::Type::std,
           svd::Driver driver = svd::Driver::GESVD);

      svd (const svd& a)
        : m_type (a.m_type), m_driver (a.m_driver), left_sm (a.left_sm),
          sigma (a.sigma), right_sm (a.right_sm)
      { }

      svd& operator = (const svd& a)
      {
        if (this != &a)
          {
            m_type = a.m_type;
            left_sm = a.left_sm;
            sigma = a.sigma;
            right_sm = a.right_sm;
            m_driver = a.m_driver;
          }

        return *this;
      }

      ~svd (void) = default;

      T left_singular_matrix (void) const;

      DM_T singular_values (void) const { return sigma; }

      T right_singular_matrix (void) const;

    private:

      typedef typename T::element_type P;
      typedef typename DM_T::element_type DM_P;

      svd::Type m_type;
      svd::Driver m_driver;

      T left_sm;
      DM_T sigma;
      T right_sm;

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
    };
  }
}

#endif
