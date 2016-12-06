/*

Copyright (C) 2016 Barbara Lócsi
Copyright (C) 2006 Pascal Dupuis <Pascal.Dupuis@uclouvain.be>
Copyright (C) 1996, 1997 John W. Eaton

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

#if ! defined (octave_gsvd_h)
#define octave_gsvd_h 1

#include "octave-config.h"

namespace octave
{
  namespace math
  {
    template <typename T>
    class
    gsvd
    {
    public:

      enum class Type
      {
        std,
        economy,
        sigma_only
      };

      gsvd (void) : sigmaA (), sigmaB (), left_smA (), left_smB (), right_sm ()
      { }

      gsvd (const T& a, const T& b,
            gsvd::Type gsvd_type = gsvd<T>::Type::economy);

      gsvd (const gsvd& a)
        : type (a.type),
          sigmaA (a.sigmaA), sigmaB (a.sigmaB),
          left_smA (a.left_smA), left_smB (a.left_smB), right_sm (a.right_sm),
          R(a.R) { }

      gsvd& operator = (const gsvd& a)
      {
        if (this != &a)
          {
            type = a.type;
            sigmaA = a.sigmaA;
            sigmaB = a.sigmaB;
            left_smA = a.left_smA;
            left_smB = a.left_smB;
            right_sm = a.right_sm;
            R = a.R;
          }

        return *this;
      }

      ~gsvd (void) = default;

      typename T::real_diag_matrix_type
      singular_values_A (void) const { return sigmaA; }

      typename T::real_diag_matrix_type
      singular_values_B (void) const { return sigmaB; }

      T left_singular_matrix_A (void) const;
      T left_singular_matrix_B (void) const;

      T right_singular_matrix (void) const;
      T R_matrix (void) const;

    private:
      typedef typename T::value_type P;
      typedef typename T::real_matrix_type real_matrix;

      gsvd::Type type;
      typename T::real_diag_matrix_type sigmaA, sigmaB;
      T left_smA, left_smB;
      T right_sm, R;

      void ggsvd (char& jobu, char& jobv, char& jobq, octave_idx_type m,
                  octave_idx_type n, octave_idx_type p, octave_idx_type& k,
                  octave_idx_type& l, P *tmp_dataA, octave_idx_type m1,
                  P *tmp_dataB, octave_idx_type p1, real_matrix& alpha,
                  real_matrix& beta, P *u, octave_idx_type nrow_u, P *v,
                  octave_idx_type nrow_v, P *q, octave_idx_type nrow_q, T& work,
                  octave_idx_type* iwork, octave_idx_type& info);
    };
  }
}

#endif
